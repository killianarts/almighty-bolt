(defpackage #:almighty-bolt-snapshot/snapshot
  (:use #:cl)
  (:import-from #:almighty-bolt-snapshot/png
                #:write-png)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt-core/platform/macos/host
                #:make-host
                #:host-acquire
                #:host-renderer
                #:host-context
                #:run-frame
                #:host-deliver-event)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt-core/render/metal/target
                #:+pixel-format-bgra8-unorm+
                #:make-offscreen-target
                #:offscreen-target-descriptor
                #:offscreen-target-width
                #:offscreen-target-height
                #:read-pixels)
  (:import-from #:almighty-bolt-core/render/metal/renderer
                #:wait-for-last-frame)
  (:export #:with-snapshot-host
           #:draw-frame
           #:write-snapshot
           #:deliver))

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
