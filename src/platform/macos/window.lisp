(defpackage #:almighty-bolt/platform/macos/window
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:main-queue #:almighty-bolt/platform/macos/main-queue)
                    (#:clock #:almighty-bolt/clock)
                    (#:event #:almighty-bolt/event))
  (:export #:make-window
           #:window-object
           #:set-content-view
           #:show-window
           #:close-window
           #:resize-window-content
           #:window-number
           #:window-scale-factor
           #:window-frame))

(in-package #:almighty-bolt/platform/macos/window)

;;; A window and its delegate. The delegate turns what AppKit tells it about the
;;; window (resized, moved to a screen with another scale, gained or lost focus,
;;; closing) into the library's events and calls, all on the main thread.

(defstruct (window (:constructor %make-window))
  (object 0 :type (unsigned-byte 64))
  (index 0 :type fixnum)
  (event-handler #'identity :type function)
  (on-close (lambda ()) :type function))

(defconstant +style-titled+ 1)
(defconstant +style-closable+ 2)
(defconstant +style-miniaturizable+ 4)
(defconstant +style-resizable+ 8)

(declaim (inline window-of))
(defun window-of (delegate offset)
  (objc:registered-object (objc:instance-slot delegate offset)))

(defvar *delegate-slot-offset* 0
  "Offset of the instance variable that holds a delegate's window index.")

(defun content-size (window)
  "The width and height of WINDOW's content in points, as two values."
  (let ((view (objc:msg-send :id (window-object window) "contentView")))
    (multiple-value-bind (x y width height) (objc:msg-send :rect view "bounds")
      (declare (ignore x y))
      (values width height))))

(objc:define-callback window-will-close sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (funcall (window-on-close (window-of self *delegate-slot-offset*))))

(objc:define-callback window-did-resize sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (let ((window (window-of self *delegate-slot-offset*)))
    (multiple-value-bind (width height) (content-size window)
      (funcall (window-event-handler window)
               (event:make-resize-event :timestamp (clock:now-ns) :width width :height height)))))

(objc:define-callback window-did-change-backing-properties sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (let ((window (window-of self *delegate-slot-offset*)))
    (funcall (window-event-handler window)
             (event:make-scale-event :timestamp (clock:now-ns)
                                     :scale (objc:msg-send :double (window-object window)
                                                           "backingScaleFactor")))))

(objc:define-callback window-did-become-key sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (funcall (window-event-handler (window-of self *delegate-slot-offset*))
           (event:make-focus-event :timestamp (clock:now-ns) :focused-p t)))

(objc:define-callback window-did-resign-key sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (funcall (window-event-handler (window-of self *delegate-slot-offset*))
           (event:make-focus-event :timestamp (clock:now-ns) :focused-p nil)))

(objc:define-runtime-class *delegate-class* "AlmightyBoltWindowDelegate" "NSObject"
  :ivars '(("lispSlot" 8 3 "Q"))
  :methods '(("windowWillClose:" "v@:@" window-will-close)
             ("windowDidResize:" "v@:@" window-did-resize)
             ("windowDidChangeBackingProperties:" "v@:@"
              window-did-change-backing-properties)
             ("windowDidBecomeKey:" "v@:@" window-did-become-key)
             ("windowDidResignKey:" "v@:@" window-did-resign-key)))

(objc:define-startup window-delegate-offset
  (setf *delegate-slot-offset* (objc:ivar-offset *delegate-class* "lispSlot")))

(defun make-window (&key (title "") (width 480) (height 320)
                      (event-handler #'identity) (on-close (lambda ())))
  "A window whose content area is WIDTH by HEIGHT points, not yet shown. EVENT-HANDLER
is called with each resize, scale and focus event; ON-CLOSE with no arguments when
the window is closing. Call on the main thread."
  (let* ((object (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSWindow") "alloc")
                                "initWithContentRect:styleMask:backing:defer:"
                                (:rect 0 0 width height)
                                (:ulong (logior +style-titled+ +style-closable+
                                                +style-miniaturizable+ +style-resizable+))
                                (:ulong 2)   ; NSBackingStoreBuffered
                                (:bool nil)))
         (window (%make-window :object object :event-handler event-handler :on-close on-close))
         (delegate (objc:msg-send :id (objc:msg-send :id *delegate-class* "alloc") "init"))
         (ns-title (objc:ns-string title)))
    (setf (window-index window) (objc:register-object window)
          (objc:instance-slot delegate *delegate-slot-offset*) (window-index window))
    (objc:msg-send :void object "setReleasedWhenClosed:" (:bool nil))
    (objc:msg-send :void object "setTitle:" (:id ns-title))
    (objc:msg-send :void object "setDelegate:" (:id delegate))
    (objc:release ns-title)
    window))

(defun set-content-view (window view-object)
  "Make the NSView VIEW-OBJECT fill WINDOW."
  (objc:msg-send :void (window-object window) "setContentView:" (:id view-object)))

;;; --- where a window opens ---
;;;
;;; A window is put where its PLACEMENT says, a property list. :TOP, :LEFT, :BOTTOM and :RIGHT are
;;; as CSS's absolute positioning has them: points from that edge of the screen's usable area (below
;;; the menu bar and beside the Dock), :LEFT before :RIGHT and :TOP before :BOTTOM when both are given,
;;; and the middle along an axis that has neither. :BESIDE is another window, which it opens next to
;;; on its :SIDE (:RIGHT, :LEFT, :ABOVE or :BELOW), :GAP points away, with the edges lined up, or on
;;; the other side if there is no room. :CASCADE is another window, below and to the right of which
;;; it opens, as a Mac app's new windows do. Wherever it goes, it is kept on the screen.
;;;
;;; AppKit counts up from the bottom of the main screen; the rectangles here are as it gives them.

(defconstant +cascade-step+ 24d0 "How far down and right of the window before a new one opens.")

(defun window-frame (window)
  "WINDOW's frame, with its title bar, as four values: x, y (from the top of the screen it is on, down)
and width and height, in points. Call on the main thread."
  (let ((object (window-object window)))
    (multiple-value-bind (x y w h) (objc:msg-send :rect object "frame")
      (multiple-value-bind (vx vy vw vh) (visible-frame (objc:msg-send :id object "screen"))
        (declare (ignore vx vw))
        (values x (- (+ vy vh) (+ y h)) w h)))))

(defun visible-frame (screen)
  "The part of SCREEN (an NSScreen, or 0 for the main one) that windows go in, as AppKit's x, y, width
and height."
  (objc:msg-send :rect (if (zerop screen) (objc:msg-send :id (objc:objc-class "NSScreen") "mainScreen") screen)
                 "visibleFrame"))

(defun place-window (window placement)
  "Put WINDOW where PLACEMENT says (see above). Call on the main thread, before it is shown."
  (destructuring-bind (&key top left bottom right beside (side :right) (gap 12) cascade)
      placement
    (let* ((object (window-object window))
           (other (or beside cascade))
           (screen (if other (objc:msg-send :id (window-object other) "screen") 0)))
      (multiple-value-bind (vx vy vw vh) (visible-frame screen)
        (multiple-value-bind (fx fy w h) (objc:msg-send :rect object "frame")
          (declare (ignore fx fy))
          (let ((gap (float gap 1d0))
                ;; The top left corner, in AppKit's coordinates.
                (tx (+ vx (/ (- vw w) 2)))
                (ty (- (+ vy vh) (/ (- vh h) 2))))
            (cond
              (beside
               (multiple-value-bind (ox oy ow oh) (objc:msg-send :rect (window-object beside) "frame")
                 (flet ((fits-p (x y) (and (<= vx x) (<= (+ x w) (+ vx vw))
                                           (<= (+ vy h) y) (<= y (+ vy vh)))))
                   (let ((spots (list :right (list (+ ox ow gap) (+ oy oh))
                                      :left (list (- ox gap w) (+ oy oh))
                                      :below (list ox (- oy gap))
                                      :above (list ox (+ oy oh gap h))))
                         (other-side (ecase side
                                       (:right :left) (:left :right) (:below :above) (:above :below))))
                     (destructuring-bind (x y) (if (apply #'fits-p (getf spots side))
                                                   (getf spots side)
                                                   (if (apply #'fits-p (getf spots other-side))
                                                       (getf spots other-side)
                                                       (getf spots side)))
                       (setf tx x ty y))))))
              (cascade
               (multiple-value-bind (ox oy ow oh) (objc:msg-send :rect (window-object cascade) "frame")
                 (declare (ignore ow))
                 (setf tx (+ ox +cascade-step+)
                       ty (- (+ oy oh) +cascade-step+))))
              (t
               (cond (left (setf tx (+ vx left)))
                     (right (setf tx (- (+ vx vw) right w))))
               (cond (top (setf ty (- (+ vy vh) top)))
                     (bottom (setf ty (+ vy bottom h))))))
            ;; On the screen: as far as it fits, from the top left.
            (setf tx (max vx (min tx (- (+ vx vw) w)))
                  ty (min (+ vy vh) (max ty (+ vy h))))
            (objc:msg-send :void object "setFrameTopLeftPoint:" (:point tx ty))))))))

(defun show-window (window &optional placement)
  "Put WINDOW where PLACEMENT says (see PLACE-WINDOW) and bring it to the front."
  (place-window window placement)
  (objc:msg-send :void (window-object window) "makeKeyAndOrderFront:" (:id 0)))

(defun close-window (window)
  "Close WINDOW as if the user had pressed its close button. Safe from any thread."
  (main-queue:run-on-main (lambda ()
                            (objc:msg-send :void (window-object window) "performClose:" (:id 0)))))

(defun resize-window-content (window width height)
  "Resize WINDOW so that its content is WIDTH by HEIGHT points. Safe from any thread."
  (main-queue:run-on-main (lambda ()
                            (objc:msg-send :void (window-object window) "setContentSize:"
                                           (:size width height)))))

(defun window-number (window)
  "The window server's number for WINDOW, as events carry it."
  (objc:msg-send :long (window-object window) "windowNumber"))

(defun window-scale-factor (window)
  "Device pixels per point on the screen WINDOW is on."
  (objc:msg-send :double (window-object window) "backingScaleFactor"))
