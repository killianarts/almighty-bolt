(defpackage #:almighty-bolt-snapshot/snapshot
  (:use #:cl)
  (:local-nicknames (#:png #:almighty-bolt/machine/png)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:core #:almighty-bolt/internals)
                    (#:host #:almighty-bolt/platform/macos/host)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:target #:almighty-bolt/render/metal/target)
                    (#:renderer #:almighty-bolt/render/metal/renderer))
  (:export #:with-snapshot-host
           #:draw-frame
           #:write-snapshot
           #:deliver
           #:click-control
           #:type-string
           #:key-press))

(in-package #:almighty-bolt-snapshot/snapshot)

;;; Draw an app into an off-screen texture with the real renderer and real text, and
;;; write what it drew as a PNG that can be looked at. The same host as the window's, so
;;; a snapshot is what a window would show (without the window's chrome). Events can be
;;; delivered to the app between frames, to see it hovered, pressed or typed into.

(defvar *size* '(400 300 2)
  "The width and height in points and the scale of the host being drawn.")

(defmacro with-snapshot-host ((host target instance &key (width 400) (height 300) (scale 2)
                                                         (capacity 65536))
                              &body body)
  "Run BODY with HOST, a host for INSTANCE, drawing into TARGET, an off-screen texture of
WIDTH by HEIGHT points at SCALE pixels a point."
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let* ((device (device:default-device))
              (,host (host:make-host ,instance device target:+pixel-format-bgra8-unorm+
                                     :capacity ,capacity :scale ,scale))
              (,target (target:make-offscreen-target device (* ,width ,scale) (* ,height ,scale))))
         (let ((pass (target:offscreen-target-descriptor ,target)))
           (setf (host:host-acquire ,host) (lambda () (values pass 0))))
         (let ((*size* (list ,width ,height ,scale)))
           ,@body)))))

(defun draw-frame (host &optional (frames 1))
  "Draw FRAMES frames of HOST, then wait for the GPU."
  (destructuring-bind (width height scale) *size*
    (dotimes (i frames)
      (objc:with-autorelease-pool
        (host:run-frame host (float width 1f0) (float height 1f0) (float scale 1f0)))
      (renderer:wait-for-last-frame (host:host-renderer host)))))

(defun write-snapshot (target path)
  "Write what TARGET holds to PATH as a PNG."
  (let ((pixels (target:read-pixels target)))
    (png:write-png path (target:offscreen-target-width target) (target:offscreen-target-height target) pixels)))

(defun deliver (host event)
  "Offer EVENT to the host's app, as the window would."
  (host:host-deliver-event host event))

(defun click-control (host id &key (fx 0.5) (fy 0.5))
  "Click the control ID of HOST's app, at the fraction FX, FY across and down it, and
draw the frame that follows."
  (multiple-value-bind (x y width height) (core:lookup-target (host:host-app host) id)
    (unless x (error "The app shows no control ~s." id))
    (let ((px (float (+ x (* fx width)) 1d0))
          (py (float (+ y (* fy height)) 1d0)))
      (dolist (step '((:move nil 0) (:down 0 1) (:up 0 0)))
        (deliver host (core:make-pointer-event :timestamp (core:now-ns) :kind (first step) :x px :y py
                                               :button (second step) :buttons (third step)))
        (draw-frame host)))))

(defun key-press (host character &key (modifiers 0))
  (deliver host (core:make-key-event :timestamp (core:now-ns) :kind :down :logical-key character
                                     :modifiers modifiers))
  (draw-frame host))

(defun type-string (host string)
  (loop :for character :across string
        :do (key-press host character)))
