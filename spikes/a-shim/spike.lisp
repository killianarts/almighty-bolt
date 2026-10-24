(defpackage #:almighty-bolt-spikes/a-shim
  (:use #:cl)
  (:import-from #:cffi
                #:null-pointer-p)
  (:local-nicknames (#:ns #:cl-nextstep)
                    (#:acceptance #:almighty-bolt-spikes/acceptance)
                    (#:ring #:almighty-bolt-core/ring))
  (:export #:main))

(in-package #:almighty-bolt-spikes/a-shim)

;;; Spike A: the acceptance script against cl-nextstep. The rig is the view
;;; itself: a paused MTKView that draws only on setNeedsDisplay.

(defclass spike-view (ns:mtk-view)
  ((ring :initform (ring:make-frame-ring :capacity 4096) :reader view-ring)
   (counters :initform (acceptance:make-counters) :reader view-counters)
   (input :initform (acceptance:make-input-log) :reader view-input)
   (queue :accessor view-queue)
   (window :initarg :window :initform nil :accessor view-window)))

(defmethod ns:init ((view spike-view))
  (call-next-method)
  ;; The default is to draw continuously. Paused with setNeedsDisplay enabled
  ;; draws only when asked.
  (ns:objc view "setPaused:" :bool t)
  (ns:objc view "setEnableSetNeedsDisplay:" :bool t)
  (mtl:clear-color view 0.1 0.2 0.4 1.0)
  (setf (view-queue view) (mtl:make-command-queue (ns:device view))))

(defmethod ns:draw ((view spike-view))
  (ring:with-recorded-frame ((view-ring view))
    (let ((descriptor (ns:current-render-pass-descriptor view))
          (drawable (ns:current-drawable view)))
      (unless (or (null-pointer-p descriptor) (null-pointer-p drawable))
        (let* ((buffer (mtl:get-command-buffer (view-queue view)))
               (encoder (mtl:get-render-command-encoder buffer descriptor)))
          (mtl:end-encoding encoder)
          (mtl:present-drawable buffer drawable)
          (mtl:commit buffer)))))
  (sb-ext:atomic-incf (acceptance:counters-draws (view-counters view))))

(defmethod ns:reshape ((view spike-view))
  (let ((counters (view-counters view)))
    (setf (acceptance:counters-width counters) (ns:width view)
          (acceptance:counters-height counters) (ns:height view))
    (sb-ext:atomic-incf (acceptance:counters-reshapes counters))))

;;; These only run because the C source of the shim's Metal view was patched to
;;; forward input (spikes/a-shim/metal-view-input.patch). Unpatched, the view
;;; never calls them. The shim reports positions from the bottom left.

(defmethod ns:mouse-down ((view spike-view) event x y)
  (declare (ignore event))
  (setf (acceptance:input-log-mouse-down (view-input view)) (list x y)))

(defmethod ns:mouse-moved ((view spike-view) event x y)
  (declare (ignore event))
  (setf (acceptance:input-log-mouse-moved (view-input view)) (list x y)))

(defmethod ns:mouse-up ((view spike-view) event x y)
  (declare (ignore event))
  (setf (acceptance:input-log-mouse-up (view-input view)) (list x y)))

(defmethod ns:key-down ((view spike-view) event key-code)
  (declare (ignore event))
  (setf (acceptance:input-log-key-down (view-input view)) (code-char key-code)))

;;; The rig

(defmethod acceptance:rig-ring ((view spike-view)) (view-ring view))
(defmethod acceptance:rig-counters ((view spike-view)) (view-counters view))
(defmethod acceptance:rig-input ((view spike-view)) (view-input view))

(defmethod acceptance:request-redraw ((view spike-view))
  (ns:with-event-loop ()
    (ns:objc view "setNeedsDisplay:" :bool t)))

(defmethod acceptance:resize-content ((view spike-view) width height)
  (ns:with-event-loop ()
    (sb-alien:with-alien ((size (sb-alien:struct ns:size)))
      (setf (sb-alien:slot size 'ns:width) (float width 1d0)
            (sb-alien:slot size 'ns:height) (float height 1d0))
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend"
                              (function sb-alien:void
                                        sb-alien:system-area-pointer
                                        sb-alien:system-area-pointer
                                        (sb-alien:struct ns:size)))
       ;; cl-nextstep does not export COCOA-REF, the NSWindow behind a window.
       (ns::cocoa-ref (view-window view)) (ns:sel "setContentSize:") size))))

(defmethod acceptance:close-window ((view spike-view))
  (ns:with-event-loop ()
    (ns:window-close (view-window view))))

(defun make-mouse-event (type x y window-number)
  "A mouse event at (X, Y) in window coordinates, built by hand as AppKit would."
  (sb-alien:with-alien ((point (sb-alien:struct ns:point)))
    (setf (sb-alien:slot point 'ns::x) (float x 1d0)
          (sb-alien:slot point 'ns::y) (float y 1d0))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "objc_msgSend"
                            (function sb-alien:system-area-pointer
                                      sb-alien:system-area-pointer
                                      sb-alien:system-area-pointer
                                      sb-alien:long
                                      (sb-alien:struct ns:point)
                                      sb-alien:unsigned-long
                                      sb-alien:double
                                      sb-alien:long
                                      sb-alien:system-area-pointer
                                      sb-alien:long
                                      sb-alien:long
                                      sb-alien:single-float))
     (ns:cls "NSEvent")
     (ns:sel "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:")
     type point 0 0d0 window-number (cffi:null-pointer) 0 1 1f0)))

(defun make-key-event (character window-number)
  "A key-down event for CHARACTER, built by hand as AppKit would."
  (sb-alien:with-alien ((point (sb-alien:struct ns:point)))
    (setf (sb-alien:slot point 'ns::x) 0d0
          (sb-alien:slot point 'ns::y) 0d0)
    (let ((characters (ns:autorelease (ns:make-ns-string (string character)))))
      (sb-alien:alien-funcall
       (sb-alien:extern-alien "objc_msgSend"
                              (function sb-alien:system-area-pointer
                                        sb-alien:system-area-pointer
                                        sb-alien:system-area-pointer
                                        sb-alien:long
                                        (sb-alien:struct ns:point)
                                        sb-alien:unsigned-long
                                        sb-alien:double
                                        sb-alien:long
                                        sb-alien:system-area-pointer
                                        sb-alien:system-area-pointer
                                        sb-alien:system-area-pointer
                                        sb-alien:unsigned-char
                                        sb-alien:unsigned-short))
       (ns:cls "NSEvent")
       (ns:sel "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:")
       10 point 0 0d0 window-number (cffi:null-pointer) characters characters 0 0))))

(defmethod acceptance:synthesize-input ((view spike-view))
  (ns:with-event-loop (:waitp t)
    (let ((number (ns:objc (view-window view) "windowNumber" :long)))
      (ns:objc view "mouseDown:" :pointer (make-mouse-event 1 100 50 number))
      (ns:objc view "mouseMoved:" :pointer (make-mouse-event 5 120 60 number))
      (ns:objc view "mouseUp:" :pointer (make-mouse-event 2 120 60 number))
      (ns:objc view "keyDown:" :pointer (make-key-event #\a number)))))


(defun main (&key (frames 300))
  "Open the window, run the acceptance script and quit. Does not return."
  (push (lambda ()
          (let* ((window (make-instance 'ns:window :title "Spike A: cl-nextstep"
                                                   :w 480 :h 320 :close-fn #'ns:quit))
                 (view (make-instance 'spike-view :w 480 :h 320 :window window)))
            (setf (ns:content-view window) view)
            (ns:objc window "center")
            (ns:window-show window)
            (sb-thread:make-thread
             (lambda ()
               (handler-case
                   (acceptance:run-acceptance view :frames frames
                                                   :title "Spike A: cl-nextstep, paused MTKView")
                 (error (condition)
                   (format t "~&Spike A failed: ~a~%" condition)
                   (finish-output)
                   (sb-ext:exit :code 1 :abort t))))
             :name "spike A driver")))
        ns:*startup-hooks*)
  (ns:start-event-loop))
