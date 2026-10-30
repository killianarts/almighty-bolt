(defpackage #:almighty-bolt/platform/macos/view
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:define-callback
                #:define-runtime-class
                #:define-startup
                #:ivar-offset
                #:instance-slot
                #:register-object
                #:registered-object
                #:objc-class
                #:lisp-string)
  (:import-from #:almighty-bolt/clock
                #:now-ns)
  (:import-from #:almighty-bolt/event
                #:make-text-input-event
                #:make-composition-event
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-super+
                #:+modifier-control+)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:run-on-main)
  (:import-from #:almighty-bolt/platform/macos/ns-event
                #:pointer-event-from-ns-event
                #:wheel-event-from-ns-event
                #:key-event-from-ns-event)
  (:export #:make-metal-view
           #:metal-view-object
           #:metal-view-device
           #:request-draw))

(in-package #:almighty-bolt/platform/macos/view)

;;; A Metal view: an MTKView that draws only when asked, and tells Lisp about
;;; drawing, resizing and input. The class is made at run time as a subclass of
;;; MTKView that is its own delegate, so that no C is needed. Each method finds
;;; the Lisp side of its object through an integer kept in an instance variable.

(defstruct (metal-view (:constructor %make-metal-view))
  (object 0 :type (unsigned-byte 64))
  (device 0 :type (unsigned-byte 64))
  (index 0 :type fixnum)
  (on-draw #'identity :type function)
  (on-resize (lambda (view width height) (declare (ignore view width height)))
   :type function)
  (event-handler #'identity :type function)
  ;; Called when the input method asks where the caret is, or whether text is wanted. Returns
  ;; whether the app takes text input and the caret's x, y, width and height in the view.
  (text-input (lambda () (values nil 0f0 0f0 0f0 0f0)) :type function)
  ;; Text input: whether an input method composition is going on, what the text of it is, and the
  ;; key that is being interpreted, which a command is given to the app as.
  (marked nil)
  (marked-text "" :type string)
  (pending-key nil))

(defvar *view-slot-offset* 0
  "Offset of the instance variable that holds a view's index.")

(declaim (inline view-of))
(defun view-of (object)
  (registered-object (instance-slot object *view-slot-offset*)))

;;; Drawing and resizing (MTKViewDelegate)

(define-callback view-draw sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (sender sb-alien:unsigned-long))
  (let ((view (view-of self)))
    (funcall (metal-view-on-draw view) view)))

(define-callback view-drawable-size-will-change sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (sender sb-alien:unsigned-long)
     (width sb-alien:double) (height sb-alien:double))
  ;; The CGSize argument arrives in the registers of two doubles.
  (let ((view (view-of self)))
    (funcall (metal-view-on-resize view) view width height)))

;;; Answers to AppKit's questions about the view

(define-callback view-yes sb-alien:unsigned-char
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  1)

(define-callback view-yes-for-event sb-alien:unsigned-char
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (event sb-alien:unsigned-long))
  1)

;;; Input

(defmacro define-pointer-callback (name kind)
  `(define-callback ,name sb-alien:void
       ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (event sb-alien:unsigned-long))
     (let ((view (view-of self)))
       (funcall (metal-view-event-handler view)
                (pointer-event-from-ns-event event self ,kind)))))

(define-pointer-callback view-mouse-down :down)
(define-pointer-callback view-mouse-up :up)
(define-pointer-callback view-mouse-moved :move)
(define-pointer-callback view-mouse-entered :enter)
(define-pointer-callback view-mouse-exited :leave)

(define-callback view-scroll-wheel sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (event sb-alien:unsigned-long))
  (funcall (metal-view-event-handler (view-of self))
           (wheel-event-from-ns-event event self)))

(define-callback view-key-down sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (event sb-alien:unsigned-long))
  (let* ((view (view-of self))
         (key (key-event-from-ns-event event :down)))
    (if (and (funcall (metal-view-text-input view))
             (not (or (modifier-down-p (key-event-modifiers key) +modifier-super+)
                      (modifier-down-p (key-event-modifiers key) +modifier-control+))))
        ;; The app takes text. The key goes to the input method, which answers by inserting text,
        ;; composing, or, for a key that is a command (an arrow, Return), by calling
        ;; DOCOMMANDBYSELECTOR:, which gives the app the key as it is.
        (progn
          (setf (metal-view-pending-key view) key)
          (unwind-protect
               (msg-send :void self "interpretKeyEvents:"
                         (:id (msg-send :id (objc-class "NSArray") "arrayWithObject:" (:id event))))
            (setf (metal-view-pending-key view) nil)))
        (funcall (metal-view-event-handler view) key))))

(define-callback view-key-up sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (event sb-alien:unsigned-long))
  (funcall (metal-view-event-handler (view-of self))
           (key-event-from-ns-event event :up)))

;;; Text input (NSTextInputClient). While the app takes text (its context says so), a key goes to the
;;; input method, and what comes back is a TEXT-INPUT-EVENT for text that is to be inserted as it is
;;; and COMPOSITION-EVENTs for text that is being composed (an input method's preedit, with a
;;; selection in it) and that was then committed; keys that are commands are given as key events.
;;; Ranges are in UTF-16 units, as Cocoa has them, and the views of the app do not use them: a
;;; replacement range is ignored, since the app has its own caret and selection.

(defconstant +not-found+ (1- (expt 2 63)) "NSNotFound.")

(defun plain-string (object)
  "The Lisp string of OBJECT, an NSString or an NSAttributedString."
  (let ((string (if (msg-send :bool object "isKindOfClass:" (:id (objc-class "NSAttributedString")))
                    (msg-send :id object "string")
                    object)))
    (lisp-string string)))

(defun utf-16-length (text &optional (end (length text)))
  "How many UTF-16 units the first END characters of TEXT take."
  (loop :for i :below end
        :sum (if (> (char-code (char text i)) #xFFFF) 2 1)))

(defun characters-in (text units)
  "How many characters of TEXT are in its first UNITS UTF-16 units."
  (let ((count 0) (taken 0))
    (loop :for character :across text
          :while (< taken units)
          :do (incf taken (if (> (char-code character) #xFFFF) 2 1))
              (incf count))
    count))

(defvar *range-result* nil
  "Where the methods that return an NSRange put it. A callback returns a structure by value, and
this is a place for it that is not allocated again for each call.")
(defvar *rect-result* nil
  "The same for the methods that return an NSRect.")

(define-startup text-input-results
  (setf *range-result* (sb-alien:make-alien (sb-alien:struct almighty-bolt/platform/macos/objc::ns-range))
        *rect-result* (sb-alien:make-alien (sb-alien:struct almighty-bolt/platform/macos/objc::cg-rect))))

(defun range-result (location length)
  (setf (sb-alien:slot *range-result* 'almighty-bolt/platform/macos/objc::location) location
        (sb-alien:slot *range-result* 'almighty-bolt/platform/macos/objc::length) length)
  (sb-alien:deref *range-result*))

(defun rect-result (x y width height)
  (let ((origin (sb-alien:slot *rect-result* 'almighty-bolt/platform/macos/objc::origin))
        (size (sb-alien:slot *rect-result* 'almighty-bolt/platform/macos/objc::size)))
    (setf (sb-alien:slot origin 'almighty-bolt/platform/macos/objc::x) x
          (sb-alien:slot origin 'almighty-bolt/platform/macos/objc::y) y
          (sb-alien:slot size 'almighty-bolt/platform/macos/objc::width) width
          (sb-alien:slot size 'almighty-bolt/platform/macos/objc::height) height))
  (sb-alien:deref *rect-result*))

(define-callback view-insert-text sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (string sb-alien:unsigned-long)
     (location sb-alien:unsigned-long) (length sb-alien:unsigned-long))
  (let* ((view (view-of self))
         (text (plain-string string)))
    (cond ((metal-view-marked view)
           ;; The composition ends with this text.
           (setf (metal-view-marked view) nil
                 (metal-view-marked-text view) "")
           (funcall (metal-view-event-handler view)
                    (make-composition-event :timestamp (now-ns) :kind :committed :text text)))
          (t
           (funcall (metal-view-event-handler view)
                    (make-text-input-event :timestamp (now-ns) :text text))))))

(define-callback view-set-marked-text sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (string sb-alien:unsigned-long)
     (selected-location sb-alien:unsigned-long) (selected-length sb-alien:unsigned-long)
     (replacement-location sb-alien:unsigned-long) (replacement-length sb-alien:unsigned-long))
  (let* ((view (view-of self))
         (text (plain-string string))
         (started (not (metal-view-marked view))))
    (cond ((zerop (length text))
           ;; The composition was emptied: it is over, with nothing in it.
           (when (metal-view-marked view)
             (setf (metal-view-marked view) nil
                   (metal-view-marked-text view) "")
             (funcall (metal-view-event-handler view)
                      (make-composition-event :timestamp (now-ns) :kind :committed :text ""))))
          (t
           (setf (metal-view-marked view) t
                 (metal-view-marked-text view) text)
           (let* ((start (min (length text) (characters-in text selected-location)))
                  (end (min (length text) (characters-in text (+ selected-location selected-length)))))
             (funcall (metal-view-event-handler view)
                      (make-composition-event :timestamp (now-ns)
                                              :kind (if started :started :changed)
                                              :text text :selection-start start
                                              :selection-length (- end start))))))))

(define-callback view-unmark-text sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  (let ((view (view-of self)))
    (when (metal-view-marked view)
      ;; Unmarking keeps the text as it was typed: it is committed.
      (let ((text (metal-view-marked-text view)))
        (setf (metal-view-marked view) nil
              (metal-view-marked-text view) "")
        (funcall (metal-view-event-handler view)
                 (make-composition-event :timestamp (now-ns) :kind :committed :text text))))))

(define-callback view-has-marked-text sb-alien:unsigned-char
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  (if (metal-view-marked (view-of self)) 1 0))

(sb-alien:define-alien-callable view-marked-range (sb-alien:struct almighty-bolt/platform/macos/objc::ns-range)
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  cmd
  (let ((view (view-of self)))
    (if (metal-view-marked view)
        (range-result 0 (utf-16-length (metal-view-marked-text view)))
        (range-result +not-found+ 0))))

(sb-alien:define-alien-callable view-selected-range (sb-alien:struct almighty-bolt/platform/macos/objc::ns-range)
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  self cmd
  ;; The app has its own selection, which the input method is not told of.
  (range-result +not-found+ 0))

(define-callback view-valid-attributes sb-alien:unsigned-long
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  (msg-send :id (objc-class "NSArray") "array"))

(define-callback view-substring sb-alien:unsigned-long
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long)
     (location sb-alien:unsigned-long) (length sb-alien:unsigned-long)
     (actual sb-alien:unsigned-long))
  ;; The app's text is not given to the input method (no reconversion).
  0)

(define-callback view-character-index sb-alien:unsigned-long
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long)
     (x sb-alien:double) (y sb-alien:double))
  0)

(sb-alien:define-alien-callable view-first-rect (sb-alien:struct almighty-bolt/platform/macos/objc::cg-rect)
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long)
     (location sb-alien:unsigned-long) (length sb-alien:unsigned-long)
     (actual sb-alien:unsigned-long))
  cmd location length actual
  (handler-case
      (multiple-value-bind (active x y width height) (funcall (metal-view-text-input (view-of self)))
        (declare (ignore active))
        ;; The caret is in the view's own coordinates (the view is flipped, with its origin at
        ;; the top left); the input method wants the place on the screen.
        (multiple-value-bind (wx wy ww wh)
            (msg-send :rect self "convertRect:toView:" (:rect x y width height) (:id 0))
          (let ((window (msg-send :id self "window")))
            (if (zerop window)
                (rect-result wx wy ww wh)
                (multiple-value-bind (sx sy sw sh)
                    (msg-send :rect window "convertRectToScreen:" (:rect wx wy ww wh))
                  (rect-result sx sy sw sh))))))
    (error (condition)
      (format *error-output* "~&Error in the callback view-first-rect: ~a~%" condition)
      (rect-result 0d0 0d0 0d0 0d0))))

(define-callback view-do-command sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (selector sb-alien:unsigned-long))
  ;; A key that the input method did not take as text, such as an arrow or Return, is a key.
  (let* ((view (view-of self))
         (key (metal-view-pending-key view)))
    (when key
      (setf (metal-view-pending-key view) nil)
      (funcall (metal-view-event-handler view) key))))

;;; The class. Dragging and the right and other buttons are reported as the
;;; same pointer events (a drag is a move with buttons down), so their methods
;;; use the same callbacks.

(define-runtime-class *view-class* "AlmightyBoltView" "MTKView"
  :ivars '(("lispSlot" 8 3 "Q"))
  :methods '(("drawInMTKView:" "v@:@" view-draw)
             ("mtkView:drawableSizeWillChange:" "v@:@{CGSize=dd}"
              view-drawable-size-will-change)
             ("isFlipped" "B@:" view-yes)
             ("acceptsFirstResponder" "B@:" view-yes)
             ("acceptsFirstMouse:" "B@:@" view-yes-for-event)
             ("mouseDown:" "v@:@" view-mouse-down)
             ("rightMouseDown:" "v@:@" view-mouse-down)
             ("otherMouseDown:" "v@:@" view-mouse-down)
             ("mouseUp:" "v@:@" view-mouse-up)
             ("rightMouseUp:" "v@:@" view-mouse-up)
             ("otherMouseUp:" "v@:@" view-mouse-up)
             ("mouseDragged:" "v@:@" view-mouse-moved)
             ("rightMouseDragged:" "v@:@" view-mouse-moved)
             ("otherMouseDragged:" "v@:@" view-mouse-moved)
             ("mouseMoved:" "v@:@" view-mouse-moved)
             ("mouseEntered:" "v@:@" view-mouse-entered)
             ("mouseExited:" "v@:@" view-mouse-exited)
             ("scrollWheel:" "v@:@" view-scroll-wheel)
             ("keyDown:" "v@:@" view-key-down)
             ("keyUp:" "v@:@" view-key-up)
             ;; NSTextInputClient
             ("insertText:replacementRange:" "v@:@{_NSRange=QQ}" view-insert-text)
             ("setMarkedText:selectedRange:replacementRange:" "v@:@{_NSRange=QQ}{_NSRange=QQ}"
              view-set-marked-text)
             ("unmarkText" "v@:" view-unmark-text)
             ("hasMarkedText" "B@:" view-has-marked-text)
             ("markedRange" "{_NSRange=QQ}@:" view-marked-range)
             ("selectedRange" "{_NSRange=QQ}@:" view-selected-range)
             ("validAttributesForMarkedText" "@@:" view-valid-attributes)
             ("attributedSubstringForProposedRange:actualRange:" "@@:{_NSRange=QQ}^{_NSRange=QQ}"
              view-substring)
             ("characterIndexForPoint:" "Q@:{CGPoint=dd}" view-character-index)
             ("firstRectForCharacterRange:actualRange:"
              "{CGRect={CGPoint=dd}{CGSize=dd}}@:{_NSRange=QQ}^{_NSRange=QQ}" view-first-rect)
             ("doCommandBySelector:" "v@::" view-do-command))
  :protocols '("NSTextInputClient"))

(define-startup view-slot-offset
  (setf *view-slot-offset* (ivar-offset *view-class* "lispSlot")))

(defconstant +tracking-options+
  (logior 1       ; NSTrackingMouseEnteredAndExited
          2       ; NSTrackingMouseMoved
          #x80    ; NSTrackingActiveAlways
          #x200)  ; NSTrackingInVisibleRect, so the area follows the view's size
  "Options for the tracking area that delivers mouse moves.")

(defun make-metal-view (&key device (width 480) (height 320) on-draw on-resize event-handler
                          text-input)
  "A Metal view WIDTH by HEIGHT points that draws only when REQUEST-DRAW is called.
DEVICE is the Metal device to draw with (the system default if not given). ON-DRAW
is called with the view when it is to draw, ON-RESIZE with the view and its new
drawable size in pixels, and EVENT-HANDLER with each pointer, wheel and key event, and the
text and composition events of the input method. TEXT-INPUT is a function that says whether
the app takes text and where its caret is: it returns true or false and then the x, y, width and
height of the caret in the view. Call on the main thread."
  (let* ((device (or device
                     (sb-alien:alien-funcall
                      (sb-alien:extern-alien "MTLCreateSystemDefaultDevice"
                                             (function sb-alien:unsigned-long)))))
         (object (msg-send :id (msg-send :id *view-class* "alloc")
                           "initWithFrame:device:"
                           (:rect 0 0 width height) (:id device)))
         (view (%make-metal-view :object object :device device)))
    (when on-draw (setf (metal-view-on-draw view) on-draw))
    (when on-resize (setf (metal-view-on-resize view) on-resize))
    (when event-handler (setf (metal-view-event-handler view) event-handler))
    (when text-input (setf (metal-view-text-input view) text-input))
    (setf (metal-view-index view) (register-object view)
          (instance-slot object *view-slot-offset*) (metal-view-index view))
    (msg-send :void object "setDelegate:" (:id object))
    ;; Paused, with setNeedsDisplay enabled, a view draws only when asked.
    (msg-send :void object "setPaused:" (:bool t))
    (msg-send :void object "setEnableSetNeedsDisplay:" (:bool t))
    (let ((area (msg-send :id (msg-send :id (objc-class "NSTrackingArea") "alloc")
                          "initWithRect:options:owner:userInfo:"
                          (:rect 0 0 0 0) (:ulong +tracking-options+) (:id object) (:id 0))))
      (msg-send :void object "addTrackingArea:" (:id area)))
    view))

(defun request-draw (view)
  "Ask VIEW to draw one frame. Safe from any thread."
  (run-on-main (lambda ()
                 (msg-send :void (metal-view-object view) "setNeedsDisplay:" (:bool t)))))
