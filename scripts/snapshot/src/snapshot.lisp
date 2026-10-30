(defpackage #:almighty-bolt-snapshot/snapshot
  (:use #:cl)
  (:import-from #:almighty-bolt/machine/png
                #:write-png)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt/internals
                #:lookup-target
                #:make-pointer-event
                #:make-key-event
                #:now-ns)
  (:import-from #:almighty-bolt/platform/macos/host
                #:host-variant
                #:make-host
                #:host-acquire
                #:host-renderer
                #:host-context
                #:run-frame
                #:host-deliver-event)
  (:import-from #:almighty-bolt/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt/render/metal/target
                #:+pixel-format-bgra8-unorm+
                #:make-offscreen-target
                #:offscreen-target-descriptor
                #:offscreen-target-width
                #:offscreen-target-height
                #:read-pixels)
  (:import-from #:almighty-bolt/render/metal/renderer
                #:wait-for-last-frame)
  (:export #:with-snapshot-host
           #:draw-frame
           #:write-snapshot
           #:deliver
           #:click-control
           #:type-string
           #:key-press))

(in-package #:almighty-bolt-snapshot/snapshot)

;;; Draw a variant into an off-screen texture with the real renderer and real text, and
;;; write what it drew as a PNG that can be looked at. The same host as the window's, so
;;; a snapshot is what a window would show (without the window's chrome). Events can be
;;; delivered to the variant between frames, to see it hovered, pressed or typed into.

(defvar *size* '(400 300 2)
  "The width and height in points and the scale of the host being drawn.")

(defmacro with-snapshot-host ((host target variant &key (width 400) (height 300) (scale 2)
                                                         (capacity 65536))
                              &body body)
  "Run BODY with HOST, a host for VARIANT, drawing into TARGET, an off-screen texture of
WIDTH by HEIGHT points at SCALE pixels a point."
  `(with-masked-float-traps
     (with-autorelease-pool
       (let* ((device (default-device))
              (,host (make-host ,variant device +pixel-format-bgra8-unorm+
                                :capacity ,capacity :scale ,scale))
              (,target (make-offscreen-target device (* ,width ,scale) (* ,height ,scale))))
         (let ((pass (offscreen-target-descriptor ,target)))
           (setf (host-acquire ,host) (lambda () (values pass 0))))
         (let ((*size* (list ,width ,height ,scale)))
           ,@body)))))

(defun draw-frame (host &optional (frames 1))
  "Draw FRAMES frames of HOST, then wait for the GPU."
  (destructuring-bind (width height scale) *size*
    (dotimes (i frames)
      (with-autorelease-pool
        (run-frame host (float width 1f0) (float height 1f0) (float scale 1f0)))
      (wait-for-last-frame (host-renderer host)))))

(defun write-snapshot (target path)
  "Write what TARGET holds to PATH as a PNG."
  (let ((pixels (read-pixels target)))
    (write-png path (offscreen-target-width target) (offscreen-target-height target) pixels)))

(defun deliver (host event)
  "Offer EVENT to the host's variant, as the window would."
  (host-deliver-event host event))

(defun click-control (host id &key (fx 0.5) (fy 0.5))
  "Click the control ID of HOST's variant, at the fraction FX, FY across and down it, and
draw the frame that follows."
  (multiple-value-bind (x y width height) (lookup-target (host-variant host) id)
    (unless x (error "The app shows no control ~s." id))
    (let ((px (float (+ x (* fx width)) 1d0))
          (py (float (+ y (* fy height)) 1d0)))
      (dolist (step '((:move nil 0) (:down 0 1) (:up 0 0)))
        (deliver host (make-pointer-event :timestamp (now-ns) :kind (first step) :x px :y py
                                          :button (second step) :buttons (third step)))
        (draw-frame host)))))

(defun key-press (host character &key (modifiers 0))
  (deliver host (make-key-event :timestamp (now-ns) :kind :down :logical-key character
                                :modifiers modifiers))
  (draw-frame host))

(defun type-string (host string)
  (loop :for character :across string
        :do (key-press host character)))
