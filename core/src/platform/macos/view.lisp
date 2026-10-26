(defpackage #:almighty-bolt-core/platform/macos/view
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:objc-class
                #:define-callback
                #:define-runtime-class
                #:define-startup
                #:ivar-offset
                #:instance-slot
                #:register-object
                #:registered-object)
  (:import-from #:almighty-bolt-core/platform/macos/main-queue
                #:run-on-main)
  (:import-from #:almighty-bolt-core/platform/macos/ns-event
                #:pointer-event-from-ns-event
                #:wheel-event-from-ns-event
                #:key-event-from-ns-event)
  (:export #:make-metal-view
           #:metal-view-object
           #:metal-view-device
           #:request-draw))

(in-package #:almighty-bolt-core/platform/macos/view)

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
  (event-handler #'identity :type function))

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
  (funcall (metal-view-event-handler (view-of self))
           (key-event-from-ns-event event :down)))

(define-callback view-key-up sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (event sb-alien:unsigned-long))
  (funcall (metal-view-event-handler (view-of self))
           (key-event-from-ns-event event :up)))

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
             ("keyUp:" "v@:@" view-key-up)))

(define-startup view-slot-offset
  (setf *view-slot-offset* (ivar-offset *view-class* "lispSlot")))

(defconstant +tracking-options+
  (logior 1       ; NSTrackingMouseEnteredAndExited
          2       ; NSTrackingMouseMoved
          #x80    ; NSTrackingActiveAlways
          #x200)  ; NSTrackingInVisibleRect, so the area follows the view's size
  "Options for the tracking area that delivers mouse moves.")

(defun make-metal-view (&key device (width 480) (height 320) on-draw on-resize event-handler)
  "A Metal view WIDTH by HEIGHT points that draws only when REQUEST-DRAW is called.
DEVICE is the Metal device to draw with (the system default if not given). ON-DRAW
is called with the view when it is to draw, ON-RESIZE with the view and its new
drawable size in pixels, and EVENT-HANDLER with each pointer, wheel and key event.
Call on the main thread."
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
