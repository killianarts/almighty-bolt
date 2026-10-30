(defpackage #:almighty-bolt-core/render/metal/renderer
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:release)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:make-command-queue)
  (:import-from #:almighty-bolt-core/render/metal/rect-pipeline
                #:+instance-bytes+
                #:make-rect-pipeline)
  (:import-from #:almighty-bolt-core/render/metal/glyph-atlas
                #:make-glyph-atlas
                #:glyph-atlas-texture
                #:glyph-atlas-uv-buffer)
  (:import-from #:almighty-bolt-core/display-list
                #:display-list
                #:display-list-count
                #:display-list-kind
                #:display-list-x
                #:display-list-y
                #:display-list-width
                #:display-list-height
                #:display-list-parameter
                #:display-list-color
                #:+command-clip+
                #:+command-unclip+)
  (:export #:renderer
           #:make-renderer
           #:renderer-capacity
           #:renderer-atlas
           #:renderer-shader-compile-ns
           #:renderer-pipeline-ns
           #:render-display-list
           #:wait-for-last-frame
           #:last-frame-gpu-ns))

(in-package #:almighty-bolt-core/render/metal/renderer)

;;; Draws a display list with Metal. A frame copies the commands into a shared
;;; buffer, encodes one instanced draw for all of them (or, if the list clips, one for
;;; each stretch of commands that has the same clip, in order, each with a scissor
;;; rectangle), and commits. At most one
;;; frame is in flight: a frame first waits for the one before it, which has
;;; usually finished long ago. (The wait exists because one instance buffer is
;;; reused. Several in flight would need several buffers and a way to know
;;; when each is free again. A completion handler, which is an Objective-C
;;; block, is the usual way; waiting on the command buffer that last used each
;;; buffer should also work. Neither is built or tried yet.)

(defconstant +max-runs+ 4096
  "The most stretches of commands with one clip that a frame may have.")
(defconstant +max-clip-depth+ 32
  "The most clips that may be on at once.")

(defstruct (renderer (:constructor %make-renderer))
  (device 0 :type (unsigned-byte 64))
  (queue 0 :type (unsigned-byte 64))
  (pipeline 0 :type (unsigned-byte 64))
  (buffer 0 :type (unsigned-byte 64))
  (contents 0 :type (unsigned-byte 64))
  (capacity 0 :type fixnum :read-only t)
  (atlas nil :read-only t)
  (last-command-buffer 0 :type (unsigned-byte 64))
  (shader-compile-ns 0 :type fixnum :read-only t)
  (pipeline-ns 0 :type fixnum :read-only t)
  ;; The stretches of commands that share a clip, from the last frame copied: the
  ;; index of the first instance, how many, and the scissor rectangle in pixels (x, y,
  ;; width, height), and how many stretches there are.
  (run-firsts (make-array +max-runs+ :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)) :read-only t)
  (run-counts (make-array +max-runs+ :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)) :read-only t)
  (run-scissors (make-array (* 4 +max-runs+) :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)) :read-only t)
  (run-count 0 :type fixnum)
  ;; Whether the last frame copied had a clip in it.
  (clipped nil :type boolean)
  ;; The clips that are on, outermost first, as pixel rectangles (x0 y0 x1 y1).
  (clip-stack (make-array (* 4 +max-clip-depth+) :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)) :read-only t))

(defun make-renderer (device pixel-format &key (capacity 65536) (scale 2))
  "A renderer on DEVICE for targets of PIXEL-FORMAT that draws up to CAPACITY
commands a frame. Compiles the shaders, which takes a few milliseconds. Its glyph
atlas is rasterized at SCALE pixels a point, which is the scale that text must be
drawn at."
  (multiple-value-bind (pipeline compile-ns pipeline-ns)
      (make-rect-pipeline device pixel-format)
    (let ((buffer (msg-send :id device "newBufferWithLength:options:"
                            (:ulong (* capacity +instance-bytes+)) (:ulong 0))))
      (%make-renderer :device device
                      :queue (make-command-queue device)
                      :pipeline pipeline
                      :buffer buffer
                      :contents (msg-send :id buffer "contents")
                      :capacity capacity
                      :atlas (make-glyph-atlas device :scale scale)
                      :shader-compile-ns compile-ns
                      :pipeline-ns pipeline-ns))))

(defun wait-for-last-frame (renderer)
  "Block until the GPU has finished the last frame that RENDERER committed."
  (let ((previous (renderer-last-command-buffer renderer)))
    (unless (zerop previous)
      (msg-send :void previous "waitUntilCompleted"))))

(defun last-frame-gpu-ns (renderer)
  "Nanoseconds the GPU took for the last frame, once it has finished."
  (let ((buffer (renderer-last-command-buffer renderer)))
    (if (zerop buffer)
        0
        (values (round (* 1d9 (- (the double-float (msg-send :double buffer "GPUEndTime"))
                                 (the double-float (msg-send :double buffer "GPUStartTime")))))))))

(defun copy-commands (renderer display-list width height scale)
  "Write the drawing commands of DISPLAY-LIST into the renderer's instance buffer, and
note the stretches of them that share a clip. Returns how many instances there are."
  (declare (type renderer renderer) (type display-list display-list)
           (type single-float width height scale)
           (optimize (speed 1) (safety 1)))
  (let* ((count (display-list-count display-list))
         (sap (sb-sys:int-sap (renderer-contents renderer)))
         (firsts (renderer-run-firsts renderer))
         (counts (renderer-run-counts renderer))
         (scissors (renderer-run-scissors renderer))
         (stack (renderer-clip-stack renderer))
         (instances 0)
         (runs 0)
         (depth 0)
         (run-first 0)
         (clipped nil)
         ;; The clip that is on now, in pixels: from x0, y0 up to x1, y1.
         (x0 0) (y0 0)
         (x1 (round (* width scale))) (y1 (round (* height scale))))
    (declare (type fixnum instances runs depth run-first x0 y0 x1 y1))
    (when (> count (renderer-capacity renderer))
      (error "The display list has ~d commands but the renderer holds ~d."
             count (renderer-capacity renderer)))
    (flet ((end-run ()
             ;; Note the instances since the last stretch ended as a stretch with the
             ;; clip that is on.
             (when (> instances run-first)
               (when (>= runs +max-runs+)
                 (error "A frame has more than ~d stretches with one clip." +max-runs+))
               (setf (aref firsts runs) run-first
                     (aref counts runs) (- instances run-first)
                     (aref scissors (* 4 runs)) x0
                     (aref scissors (+ (* 4 runs) 1)) y0
                     (aref scissors (+ (* 4 runs) 2)) (max 0 (- x1 x0))
                     (aref scissors (+ (* 4 runs) 3)) (max 0 (- y1 y0)))
               (incf runs))
             (setf run-first instances)))
      (dotimes (i count)
        (let ((kind (display-list-kind display-list i)))
          (cond
            ((= kind +command-clip+)
             (end-run)
             (setf clipped t)
             (when (>= depth +max-clip-depth+)
               (error "More than ~d clips are on at once." +max-clip-depth+))
             (setf (aref stack (* 4 depth)) x0
                   (aref stack (+ (* 4 depth) 1)) y0
                   (aref stack (+ (* 4 depth) 2)) x1
                   (aref stack (+ (* 4 depth) 3)) y1)
             (incf depth)
             (let ((left (floor (* scale (display-list-x display-list i))))
                   (top (floor (* scale (display-list-y display-list i))))
                   (right (ceiling (* scale (+ (display-list-x display-list i)
                                               (display-list-width display-list i)))))
                   (bottom (ceiling (* scale (+ (display-list-y display-list i)
                                                (display-list-height display-list i))))))
               (setf x0 (max x0 left) y0 (max y0 top)
                     x1 (min x1 right) y1 (min y1 bottom))))
            ((= kind +command-unclip+)
             (end-run)
             (when (plusp depth)
               (decf depth)
               (setf x0 (aref stack (* 4 depth))
                     y0 (aref stack (+ (* 4 depth) 1))
                     x1 (aref stack (+ (* 4 depth) 2))
                     y1 (aref stack (+ (* 4 depth) 3)))))
            (t
             (let ((offset (* instances +instance-bytes+)))
               (setf (sb-sys:sap-ref-single sap offset) (display-list-x display-list i)
                     (sb-sys:sap-ref-single sap (+ offset 4)) (display-list-y display-list i)
                     (sb-sys:sap-ref-single sap (+ offset 8)) (display-list-width display-list i)
                     (sb-sys:sap-ref-single sap (+ offset 12)) (display-list-height display-list i)
                     (sb-sys:sap-ref-single sap (+ offset 16)) (display-list-parameter display-list i)
                     (sb-sys:sap-ref-32 sap (+ offset 20)) (display-list-color display-list i)
                     (sb-sys:sap-ref-32 sap (+ offset 24)) kind
                     (sb-sys:sap-ref-32 sap (+ offset 28)) 0))
             (incf instances)))))
      (end-run))
    (setf (renderer-run-count renderer) runs
          (renderer-clipped renderer) clipped)
    instances))

(defun set-scissor (encoder x y width height)
  "Clip what ENCODER draws next to the pixels from X, Y, WIDTH by HEIGHT. The rectangle is
four words and so is passed by address, as the region of the pixel read-back is."
  (sb-alien:with-alien ((rectangle (array sb-alien:unsigned-long 4)))
    (setf (sb-alien:deref rectangle 0) x
          (sb-alien:deref rectangle 1) y
          (sb-alien:deref rectangle 2) width
          (sb-alien:deref rectangle 3) height)
    (msg-send :void encoder "setScissorRect:"
              (:pointer (sb-sys:sap-int (sb-alien:alien-sap rectangle))))))

(defun render-display-list (renderer display-list pass drawable width height scale)
  "Draw DISPLAY-LIST with the render pass descriptor PASS (a window's current one,
or an off-screen target's), then present DRAWABLE unless it is zero. WIDTH and
HEIGHT are the size of the drawing area in points and SCALE the pixels per point.
Returns the command buffer. Must run inside an autorelease pool."
  (declare (type renderer renderer))
  (wait-for-last-frame renderer)
  (let* ((count (copy-commands renderer display-list (float width 1f0) (float height 1f0)
                               (float scale 1f0)))
         (buffer (msg-send :id (renderer-queue renderer) "commandBuffer"))
         (encoder (msg-send :id buffer "renderCommandEncoderWithDescriptor:" (:id pass))))
    (msg-send :void encoder "setRenderPipelineState:" (:id (renderer-pipeline renderer)))
    (msg-send :void encoder "setVertexBuffer:offset:atIndex:"
              (:id (renderer-buffer renderer)) (:ulong 0) (:ulong 0))
    ;; The glyph atlas: where each glyph lies in the texture, and the texture.
    (msg-send :void encoder "setVertexBuffer:offset:atIndex:"
              (:id (glyph-atlas-uv-buffer (renderer-atlas renderer))) (:ulong 0) (:ulong 2))
    (msg-send :void encoder "setFragmentTexture:atIndex:"
              (:id (glyph-atlas-texture (renderer-atlas renderer))) (:ulong 0))
    ;; The frame constants: the size in points, the scale, and a pad word. Four
    ;; floats are 16 bytes, which Metal takes as bytes in the command.
    (sb-alien:with-alien ((constants (array sb-alien:single-float 4)))
      (setf (sb-alien:deref constants 0) (float width 1f0)
            (sb-alien:deref constants 1) (float height 1f0)
            (sb-alien:deref constants 2) (float scale 1f0)
            (sb-alien:deref constants 3) 0f0)
      (msg-send :void encoder "setVertexBytes:length:atIndex:"
                (:pointer (sb-sys:sap-int (sb-alien:alien-sap constants)))
                (:ulong 16) (:ulong 1)))
    (cond
      ((zerop count))
      ((not (renderer-clipped renderer))
       ;; A strip of four vertices (a quad) for each of COUNT instances.
       (msg-send :void encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
                 (:ulong 4) (:ulong 0) (:ulong 4) (:ulong count)))
      (t
       ;; The same, a stretch at a time, each clipped to its scissor rectangle. A stretch
       ;; that is clipped to nothing is not drawn.
       (let ((firsts (renderer-run-firsts renderer))
             (counts (renderer-run-counts renderer))
             (scissors (renderer-run-scissors renderer)))
         (dotimes (run (renderer-run-count renderer))
           (let ((x (aref scissors (* 4 run)))
                 (y (aref scissors (+ (* 4 run) 1)))
                 (w (aref scissors (+ (* 4 run) 2)))
                 (h (aref scissors (+ (* 4 run) 3))))
             (when (and (plusp w) (plusp h))
               (set-scissor encoder x y w h)
               (msg-send :void encoder
                         "drawPrimitives:vertexStart:vertexCount:instanceCount:baseInstance:"
                         (:ulong 4) (:ulong 0) (:ulong 4)
                         (:ulong (aref counts run)) (:ulong (aref firsts run)))))))))
    (msg-send :void encoder "endEncoding")
    (unless (zerop drawable)
      (msg-send :void buffer "presentDrawable:" (:id drawable)))
    (msg-send :void buffer "commit")
    ;; Keep the command buffer to wait on at the next frame.
    (msg-send :id buffer "retain")
    (let ((previous (renderer-last-command-buffer renderer)))
      (unless (zerop previous)
        (release previous)))
    (setf (renderer-last-command-buffer renderer) buffer)
    buffer))
