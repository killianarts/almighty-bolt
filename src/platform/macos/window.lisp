(defpackage #:almighty-bolt/platform/macos/window
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:ns-string
                #:release
                #:define-callback
                #:define-runtime-class
                #:define-startup
                #:ivar-offset
                #:instance-slot
                #:register-object
                #:registered-object)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:run-on-main)
  (:import-from #:almighty-bolt/clock
                #:now-ns)
  (:import-from #:almighty-bolt/event
                #:make-resize-event
                #:make-scale-event
                #:make-focus-event)
  (:export #:make-window
           #:window-object
           #:set-content-view
           #:show-window
           #:close-window
           #:resize-window-content
           #:window-number
           #:window-scale-factor))

(in-package #:almighty-bolt/platform/macos/window)

;;; A window and its delegate. The delegate turns what AppKit tells it about the
;;; window (resized, moved to a screen with another scale, gained or lost focus,
;;; closing) into core events and calls, all on the main thread.

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
  (registered-object (instance-slot delegate offset)))

(defvar *delegate-slot-offset* 0
  "Offset of the instance variable that holds a delegate's window index.")

(defun content-size (window)
  "The width and height of WINDOW's content in points, as two values."
  (let ((view (msg-send :id (window-object window) "contentView")))
    (multiple-value-bind (x y width height) (msg-send :rect view "bounds")
      (declare (ignore x y))
      (values width height))))

(define-callback window-will-close sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (funcall (window-on-close (window-of self *delegate-slot-offset*))))

(define-callback window-did-resize sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (let ((window (window-of self *delegate-slot-offset*)))
    (multiple-value-bind (width height) (content-size window)
      (funcall (window-event-handler window)
               (make-resize-event :timestamp (now-ns) :width width :height height)))))

(define-callback window-did-change-backing-properties sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (let ((window (window-of self *delegate-slot-offset*)))
    (funcall (window-event-handler window)
             (make-scale-event :timestamp (now-ns)
                               :scale (msg-send :double (window-object window)
                                                "backingScaleFactor")))))

(define-callback window-did-become-key sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (funcall (window-event-handler (window-of self *delegate-slot-offset*))
           (make-focus-event :timestamp (now-ns) :focused-p t)))

(define-callback window-did-resign-key sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (notification sb-alien:unsigned-long))
  (funcall (window-event-handler (window-of self *delegate-slot-offset*))
           (make-focus-event :timestamp (now-ns) :focused-p nil)))

(define-runtime-class *delegate-class* "AlmightyBoltWindowDelegate" "NSObject"
  :ivars '(("lispSlot" 8 3 "Q"))
  :methods '(("windowWillClose:" "v@:@" window-will-close)
             ("windowDidResize:" "v@:@" window-did-resize)
             ("windowDidChangeBackingProperties:" "v@:@"
              window-did-change-backing-properties)
             ("windowDidBecomeKey:" "v@:@" window-did-become-key)
             ("windowDidResignKey:" "v@:@" window-did-resign-key)))

(define-startup window-delegate-offset
  (setf *delegate-slot-offset* (ivar-offset *delegate-class* "lispSlot")))

(defun make-window (&key (title "") (width 480) (height 320)
                      (event-handler #'identity) (on-close (lambda ())))
  "A window whose content area is WIDTH by HEIGHT points, not yet shown. EVENT-HANDLER
is called with each resize, scale and focus event; ON-CLOSE with no arguments when
the window is closing. Call on the main thread."
  (let* ((object (msg-send :id (msg-send :id (objc-class "NSWindow") "alloc")
                           "initWithContentRect:styleMask:backing:defer:"
                           (:rect 0 0 width height)
                           (:ulong (logior +style-titled+ +style-closable+
                                           +style-miniaturizable+ +style-resizable+))
                           (:ulong 2)   ; NSBackingStoreBuffered
                           (:bool nil)))
         (window (%make-window :object object :event-handler event-handler :on-close on-close))
         (delegate (msg-send :id (msg-send :id *delegate-class* "alloc") "init"))
         (ns-title (ns-string title)))
    (setf (window-index window) (register-object window)
          (instance-slot delegate *delegate-slot-offset*) (window-index window))
    (msg-send :void object "setReleasedWhenClosed:" (:bool nil))
    (msg-send :void object "setTitle:" (:id ns-title))
    (msg-send :void object "setDelegate:" (:id delegate))
    (release ns-title)
    window))

(defun set-content-view (window view-object)
  "Make the NSView VIEW-OBJECT fill WINDOW."
  (msg-send :void (window-object window) "setContentView:" (:id view-object)))

(defun show-window (window)
  "Centre WINDOW on the screen and bring it to the front."
  (msg-send :void (window-object window) "center")
  (msg-send :void (window-object window) "makeKeyAndOrderFront:" (:id 0)))

(defun close-window (window)
  "Close WINDOW as if the user had pressed its close button. Safe from any thread."
  (run-on-main (lambda ()
                 (msg-send :void (window-object window) "performClose:" (:id 0)))))

(defun resize-window-content (window width height)
  "Resize WINDOW so that its content is WIDTH by HEIGHT points. Safe from any thread."
  (run-on-main (lambda ()
                 (msg-send :void (window-object window) "setContentSize:"
                           (:size width height)))))

(defun window-number (window)
  "The window server's number for WINDOW, as events carry it."
  (msg-send :long (window-object window) "windowNumber"))

(defun window-scale-factor (window)
  "Device pixels per point on the screen WINDOW is on."
  (msg-send :double (window-object window) "backingScaleFactor"))
