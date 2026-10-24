(defpackage #:almighty-bolt-spikes/c-metal
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:with-autorelease-pool
                #:with-masked-float-traps)
  (:import-from #:almighty-bolt-core/platform/macos/view
                #:metal-view-object
                #:metal-view-device)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt-core/render/metal/target
                #:+pixel-format-bgra8-unorm+
                #:make-offscreen-target
                #:offscreen-target-descriptor)
  (:import-from #:almighty-bolt-core/render/metal/renderer
                #:make-renderer
                #:render-display-list
                #:wait-for-last-frame
                #:last-frame-gpu-ns
                #:renderer-shader-compile-ns
                #:renderer-pipeline-ns)
  (:import-from #:almighty-bolt-core/display-list
                #:make-display-list
                #:clear-display-list
                #:push-rounded-rect
                #:pack-color)
  (:local-nicknames (#:b #:almighty-bolt-spikes/b-own)
                    (#:clock #:almighty-bolt-core/clock))
  (:export #:main
           #:benchmark))

(in-package #:almighty-bolt-spikes/c-metal)

;;; Spike C: the display list drawn by the Metal renderer. In a window, behind
;;; the same acceptance script as A and B; and without one, in a loop that times
;;; the renderer and counts what it allocates.

(defun build-scene (display-list frame count width height)
  "Fill DISPLAY-LIST with COUNT rounded squares that drift as FRAME advances."
  (declare (type fixnum frame count width height))
  (clear-display-list display-list)
  (let ((room-x (- width 24))
        (room-y (- height 24)))
    (dotimes (i count)
      (push-rounded-rect display-list
                         (float (mod (+ (* i 37) (* frame 2)) room-x) 1f0)
                         (float (mod (+ (* i 53) frame) room-y) 1f0)
                         24f0 24f0
                         (float (+ 4 (mod i 7)) 1f0)
                         (pack-color (mod (* i 53) 256) (mod (* i 97) 256)
                                     (mod (* i 193) 256) 200)))))

;;; In a window

(defvar *renderer* nil
  "The window's renderer, kept for the report.")

(defun make-frame-function (rig view)
  (declare (ignore rig))
  (let* ((object (metal-view-object view))
         (renderer (make-renderer (metal-view-device view)
                                  (msg-send :ulong object "colorPixelFormat")
                                  :capacity 1024))
         (display-list (make-display-list :capacity 1024))
         (frame 0))
    (declare (type fixnum frame))
    (setf *renderer* renderer)
    (lambda (rig view)
      (declare (ignore rig view))
      (let ((pass (msg-send :id object "currentRenderPassDescriptor"))
            (drawable (msg-send :id object "currentDrawable")))
        (unless (or (zerop pass) (zerop drawable))
          (multiple-value-bind (x y width height) (msg-send :rect object "bounds")
            (declare (ignore x y))
            (let ((scale (/ (msg-send :size object "drawableSize") width)))
              (build-scene display-list frame 200 (floor width) (floor height))
              (incf frame)
              (render-display-list renderer display-list pass drawable width height scale))))))))

(defun more-report (rig)
  (declare (ignore rig))
  (when *renderer*
    (wait-for-last-frame *renderer*)
    (format t "shader compile, pipeline:           ~,1f ms, ~,1f ms~%"
            (/ (renderer-shader-compile-ns *renderer*) 1d6)
            (/ (renderer-pipeline-ns *renderer*) 1d6))
    (format t "GPU time of the last frame (200):   ~,3f ms~%"
            (/ (last-frame-gpu-ns *renderer*) 1d6))))

(defun main (&key (frames 300))
  "Open the window, run the acceptance script with the renderer, and quit."
  (b:main :frames frames
          :title "Spike C: Metal renderer, paused MTKView"
          :make-frame-function #'make-frame-function
          :more-report #'more-report))

;;; Without a window

(defun run-case (renderer target display-list count frames)
  "Draw FRAMES frames of COUNT commands into TARGET. Returns the mean microseconds
to build the display list, to encode and commit, and for the GPU, and the bytes
consed a frame."
  (declare (type fixnum count frames))
  (let ((pass (offscreen-target-descriptor target))
        (build 0) (encode 0) (gpu 0)
        (before (sb-ext:get-bytes-consed)))
    (declare (type fixnum build encode gpu))
    (dotimes (frame frames)
      (with-autorelease-pool
        (let ((t0 (clock:now-ns)))
          (build-scene display-list frame count 480 320)
          (let ((t1 (clock:now-ns)))
            (render-display-list renderer display-list pass 0 480 320 2)
            (let ((t2 (clock:now-ns)))
              (wait-for-last-frame renderer)
              (incf build (- t1 t0))
              (incf encode (- t2 t1))
              (incf gpu (last-frame-gpu-ns renderer)))))))
    (let ((consed (- (sb-ext:get-bytes-consed) before)))
      (values (/ build frames 1000.0) (/ encode frames 1000.0) (/ gpu frames 1000.0)
              (/ consed frames 1.0)))))

(defun benchmark (&key (frames 1000) (counts '(10 100 1000 10000)))
  "Time the renderer without a window and print a table."
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((device (default-device))
             (renderer (make-renderer device +pixel-format-bgra8-unorm+ :capacity 16384))
             (target (make-offscreen-target device 960 640))
             (display-list (make-display-list :capacity 16384)))
        (format t "~&--- Spike C: the Metal renderer without a window (960 x 640 pixels) ---~%")
        (format t "shader compile ~,1f ms, pipeline ~,1f ms (the first run on this machine)~%"
                (/ (renderer-shader-compile-ns renderer) 1d6)
                (/ (renderer-pipeline-ns renderer) 1d6))
        (format t "~8a ~12a ~14a ~12a ~14a~%"
                "commands" "build (us)" "encode (us)" "GPU (us)" "bytes consed/frame")
        (run-case renderer target display-list 100 50)       ; warm up
        (dolist (count counts)
          (multiple-value-bind (build encode gpu consed)
              (run-case renderer target display-list count frames)
            (format t "~8d ~12,1f ~14,1f ~12,1f ~14,1f~%" count build encode gpu consed)))
        (finish-output)))))
