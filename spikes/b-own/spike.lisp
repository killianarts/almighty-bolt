(defpackage #:almighty-bolt-spikes/b-own
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:objc-class
                #:ns-string
                #:release)
  (:import-from #:almighty-bolt-core/platform/macos/app
                #:run-application)
  (:import-from #:almighty-bolt-core/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt-core/platform/macos/window
                #:make-window
                #:set-content-view
                #:show-window
                #:close-window
                #:resize-window-content
                #:window-number)
  (:import-from #:almighty-bolt-core/platform/macos/view
                #:make-metal-view
                #:metal-view-object
                #:metal-view-device
                #:request-draw)
  (:import-from #:almighty-bolt-core/event
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:key-event
                #:key-event-kind
                #:key-event-logical-key
                #:resize-event
                #:scale-event
                #:focus-event)
  (:local-nicknames (#:acceptance #:almighty-bolt-spikes/acceptance)
                    (#:ring #:almighty-bolt-core/ring)
                    (#:clock #:almighty-bolt-core/clock))
  (:export #:main
           #:b-rig-window
           #:b-rig-view))

(in-package #:almighty-bolt-spikes/b-own)

;;; Spike B: the acceptance script against our own Objective-C bindings. No C,
;;; no cl-nextstep: the classes AppKit calls are made at run time, in Lisp.

(defun call-default-device ()
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "MTLCreateSystemDefaultDevice" (function sb-alien:unsigned-long))))

;;; How long a part of the draw callback took, over all the draws: the total,
;;; the longest, how many were timed, and how many took over a millisecond. Four
;;; fixnums in one array, so noting a span allocates nothing.

(defconstant +slow-span-ns+ 1000000)

(defun make-span-stats ()
  (make-array 4 :element-type 'fixnum :initial-element 0))

(declaim (inline note-span))
(defun note-span (stats ns)
  (declare (type (simple-array fixnum (4)) stats) (type fixnum ns))
  (incf (aref stats 0) ns)
  (when (> ns (aref stats 1))
    (setf (aref stats 1) ns))
  (incf (aref stats 2))
  (when (> ns +slow-span-ns+)
    (incf (aref stats 3))))

(defun report-span (label stats)
  (destructuring-bind (total longest count slow) (coerce stats 'list)
    (when (plusp count)
      (format t "~36a mean ~,1f us, max ~,1f us, over 1 ms in ~d of ~d draws~%"
              label (/ total count 1000.0) (/ longest 1000.0) slow count))))

(defclass b-rig ()
  ((ring :initform (ring:make-frame-ring :capacity 4096) :reader b-rig-ring)
   ;; The draw callback in two parts: asking the view for its drawable, which can
   ;; wait for the display, and encoding and committing the frame.
   (drawable-spans :initform (make-span-stats) :reader b-rig-drawable-spans)
   (encode-spans :initform (make-span-stats) :reader b-rig-encode-spans)
   (counters :initform (acceptance:make-counters) :reader b-rig-counters)
   (input :initform (acceptance:make-input-log) :reader b-rig-input)
   (window :accessor b-rig-window)
   (view :accessor b-rig-view)
   (queue :accessor b-rig-queue)
   (frame-function :initform nil :accessor b-rig-frame-function)
   ;; Events the shim could not deliver at all.
   (window-events :initform (list :resize 0 :scale 0 :focus 0) :accessor b-rig-window-events)))

(defmethod acceptance:rig-ring ((rig b-rig)) (b-rig-ring rig))
(defmethod acceptance:rig-counters ((rig b-rig)) (b-rig-counters rig))
(defmethod acceptance:rig-input ((rig b-rig)) (b-rig-input rig))

;;; The callbacks, all on the main thread

(defun clear-frame (rig view)
  "The simplest frame: begin a pass that clears to the view's clear colour, and present.
The time to get the drawable and the time to encode and commit are noted apart."
  (let* ((object (metal-view-object view))
         (asked (clock:now-ns))
         (descriptor (msg-send :id object "currentRenderPassDescriptor"))
         (drawable (msg-send :id object "currentDrawable"))
         (got (clock:now-ns)))
    (note-span (b-rig-drawable-spans rig) (- got asked))
    (unless (or (zerop descriptor) (zerop drawable))
      (let* ((buffer (msg-send :id (b-rig-queue rig) "commandBuffer"))
             (encoder (msg-send :id buffer "renderCommandEncoderWithDescriptor:"
                                (:id descriptor))))
        (msg-send :void encoder "endEncoding")
        (msg-send :void buffer "presentDrawable:" (:id drawable))
        (msg-send :void buffer "commit")))
    (note-span (b-rig-encode-spans rig) (- (clock:now-ns) got))))

(defun draw (rig view)
  (ring:with-recorded-frame ((b-rig-ring rig))
    (funcall (the function (b-rig-frame-function rig)) rig view))
  (sb-ext:atomic-incf (acceptance:counters-draws (b-rig-counters rig))))

(defun resized (rig view width height)
  (declare (ignore view))
  (let ((counters (b-rig-counters rig)))
    (setf (acceptance:counters-width counters) (round width)
          (acceptance:counters-height counters) (round height))
    (sb-ext:atomic-incf (acceptance:counters-reshapes counters))))

(defun note-input (rig event)
  (let ((input (b-rig-input rig)))
    (etypecase event
      (pointer-event
       (let ((position (list (pointer-event-x event) (pointer-event-y event))))
         (ecase (pointer-event-kind event)
           (:down (setf (acceptance:input-log-mouse-down input) position))
           (:move (setf (acceptance:input-log-mouse-moved input) position))
           (:up (setf (acceptance:input-log-mouse-up input) position))
           ((:enter :leave) nil))))
      (key-event
       (when (eq (key-event-kind event) :down)
         (setf (acceptance:input-log-key-down input) (key-event-logical-key event)))))))

(defun note-window-event (rig event)
  (let ((events (b-rig-window-events rig)))
    (etypecase event
      (resize-event (incf (getf events :resize)))
      (scale-event (incf (getf events :scale)))
      (focus-event (incf (getf events :focus))))
    (setf (b-rig-window-events rig) events)))

;;; The rig

(defmethod acceptance:request-redraw ((rig b-rig))
  (request-draw (b-rig-view rig)))

(defmethod acceptance:resize-content ((rig b-rig) width height)
  (resize-window-content (b-rig-window rig) width height))

(defmethod acceptance:close-window ((rig b-rig))
  (close-window (b-rig-window rig)))

(defun make-mouse-event (type x y window-number)
  "A mouse event at (X, Y) in window coordinates, built by hand as AppKit would."
  (msg-send :id (objc-class "NSEvent")
            "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"
            (:long type) (:point x y) (:ulong 0) (:double 0d0) (:long window-number)
            (:id 0) (:long 0) (:long 1) (:float 1f0)))

(defun make-key-event (character window-number)
  "A key-down event for CHARACTER, built by hand as AppKit would."
  (let ((characters (ns-string (string character))))
    (prog1 (msg-send :id (objc-class "NSEvent")
                     "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"
                     (:long 10) (:point 0 0) (:ulong 0) (:double 0d0) (:long window-number)
                     (:id 0) (:id characters) (:id characters) (:bool nil) (:ushort 0))
      (release characters))))

(defmethod acceptance:synthesize-input ((rig b-rig))
  ;; The view reports positions from the top left, so a point 50 below the top
  ;; edge is height - 50 above the bottom edge, which is how AppKit gives them.
  (call-on-main
   (lambda ()
     (let* ((view (metal-view-object (b-rig-view rig)))
            (number (window-number (b-rig-window rig))))
       (multiple-value-bind (x y width height) (msg-send :rect view "bounds")
         (declare (ignore x y width))
         (msg-send :void view "mouseDown:" (:id (make-mouse-event 1 100 (- height 50) number)))
         (msg-send :void view "mouseMoved:" (:id (make-mouse-event 5 120 (- height 60) number)))
         (msg-send :void view "mouseUp:" (:id (make-mouse-event 2 120 (- height 60) number)))
         (msg-send :void view "keyDown:" (:id (make-key-event #\a number))))))))

(defun extra-report (rig more)
  (let ((events (b-rig-window-events rig))
        (uptime (msg-send :double (msg-send :id (objc-class "NSProcessInfo") "processInfo")
                          "systemUptime")))
    (format t "window events (the shim has none): resize ~d, scale ~d, focus ~d~%"
            (getf events :resize) (getf events :scale) (getf events :focus))
    (format t "application active, window key:     ~a, ~a~%"
            (msg-send :bool (msg-send :id (objc-class "NSApplication") "sharedApplication") "isActive")
            (msg-send :bool (almighty-bolt-core/platform/macos/window::window-object
                             (b-rig-window rig))
                      "isKeyWindow"))
    (format t "scale factor of the window:         ~,1f~%"
            (msg-send :double (almighty-bolt-core/platform/macos/window::window-object
                               (b-rig-window rig))
                      "backingScaleFactor"))
    (report-span "getting the drawable (all draws):" (b-rig-drawable-spans rig))
    (report-span "encoding and committing:" (b-rig-encode-spans rig))
    (format t "NSProcessInfo uptime vs core clock: ~,3f ms apart~%"
            (/ (abs (- (* uptime 1d9) (clock:now-ns))) 1d6))
    (when more (funcall more rig))))

(defun main (&key (frames 300) (title "Spike B: own bindings, paused MTKView")
               make-frame-function more-report)
  "Open the window, run the acceptance script and quit. Does not return.
MAKE-FRAME-FUNCTION, if given, is called with the rig and the view once they exist
and returns the function (of the rig and the view) that draws a frame; by default a
frame only clears. MORE-REPORT is called with the rig to print extra lines."
  (let ((rig (make-instance 'b-rig)))
    (run-application
     (lambda ()
       (let* ((window (make-window :title "Spike B: own bindings" :width 480 :height 320
                                   :event-handler (lambda (event) (note-window-event rig event))))
              (view (make-metal-view :device (call-default-device)
                                     :width 480 :height 320
                                     :on-draw (lambda (view) (draw rig view))
                                     :on-resize (lambda (view width height)
                                                  (resized rig view width height))
                                     :event-handler (lambda (event) (note-input rig event)))))
         (setf (b-rig-window rig) window
               (b-rig-view rig) view
               (b-rig-queue rig) (msg-send :id (metal-view-device view) "newCommandQueue"))
         (setf (b-rig-frame-function rig)
               (if make-frame-function
                   (funcall make-frame-function rig view)
                   #'clear-frame))
         (msg-send :void (metal-view-object view) "setClearColor:" (:color 0.1 0.2 0.4 1.0))
         (set-content-view window (metal-view-object view))
         (show-window window)
         (sb-thread:make-thread
          (lambda ()
            (handler-case
                (acceptance:run-acceptance rig :frames frames :title title
                                               :extra-report (lambda ()
                                                               (extra-report rig more-report)))
              (error (condition)
                (format t "~&Spike B failed: ~a~%" condition)
                (finish-output)
                (sb-ext:exit :code 1 :abort t))))
          :name "spike B driver"))))))
