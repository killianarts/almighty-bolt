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
                #:display-list-color)
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
;;; buffer, encodes one instanced draw for all of them, and commits. At most one
;;; frame is in flight: a frame first waits for the one before it, which has
;;; usually finished long ago. (Keeping several in flight needs a completion
;;; handler, which is an Objective-C block; that waits until it is needed.)

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
  (pipeline-ns 0 :type fixnum :read-only t))

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

(defun copy-commands (renderer display-list)
  "Write the commands of DISPLAY-LIST into the renderer's instance buffer."
  (declare (type renderer renderer) (type display-list display-list)
           (optimize (speed 1) (safety 1)))
  (let ((count (display-list-count display-list))
        (sap (sb-sys:int-sap (renderer-contents renderer))))
    (when (> count (renderer-capacity renderer))
      (error "The display list has ~d commands but the renderer holds ~d."
             count (renderer-capacity renderer)))
    (dotimes (i count)
      (let ((offset (* i +instance-bytes+)))
        (setf (sb-sys:sap-ref-single sap offset) (display-list-x display-list i)
              (sb-sys:sap-ref-single sap (+ offset 4)) (display-list-y display-list i)
              (sb-sys:sap-ref-single sap (+ offset 8)) (display-list-width display-list i)
              (sb-sys:sap-ref-single sap (+ offset 12)) (display-list-height display-list i)
              (sb-sys:sap-ref-single sap (+ offset 16)) (display-list-parameter display-list i)
              (sb-sys:sap-ref-32 sap (+ offset 20)) (display-list-color display-list i)
              (sb-sys:sap-ref-32 sap (+ offset 24)) (display-list-kind display-list i)
              (sb-sys:sap-ref-32 sap (+ offset 28)) 0)))
    count))

(defun render-display-list (renderer display-list pass drawable width height scale)
  "Draw DISPLAY-LIST with the render pass descriptor PASS (a window's current one,
or an off-screen target's), then present DRAWABLE unless it is zero. WIDTH and
HEIGHT are the size of the drawing area in points and SCALE the pixels per point.
Returns the command buffer. Must run inside an autorelease pool."
  (declare (type renderer renderer))
  (wait-for-last-frame renderer)
  (let* ((count (copy-commands renderer display-list))
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
    (when (plusp count)
      ;; A strip of four vertices (a quad) for each of COUNT instances.
      (msg-send :void encoder "drawPrimitives:vertexStart:vertexCount:instanceCount:"
                (:ulong 4) (:ulong 0) (:ulong 4) (:ulong count)))
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
