(defpackage #:almighty-bolt-core/render/metal/target
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:export #:+pixel-format-bgra8-unorm+
           #:offscreen-target
           #:make-offscreen-target
           #:offscreen-target-descriptor
           #:offscreen-target-width
           #:offscreen-target-height
           #:read-pixels))

(in-package #:almighty-bolt-core/render/metal/target)

;;; An off-screen colour texture to draw into and read back, so that rendering
;;; can be tested and measured without a window.

(defconstant +pixel-format-bgra8-unorm+ 80
  "MTLPixelFormatBGRA8Unorm, the format of a window's drawable.")

(defconstant +usage-render-target+ 4)
(defconstant +storage-shared+ 0
  "Memory that both the CPU and the GPU can see (on Apple silicon).")
(defconstant +load-action-clear+ 2)
(defconstant +store-action-store+ 1)

(defstruct (offscreen-target (:constructor %make-offscreen-target))
  (texture 0 :type (unsigned-byte 64))
  (descriptor 0 :type (unsigned-byte 64))
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defun make-offscreen-target (device width height
                              &key (pixel-format +pixel-format-bgra8-unorm+)
                                (clear-color '(0d0 0d0 0d0 1d0)))
  "A WIDTH by HEIGHT pixel texture on DEVICE and a render pass that clears it to
CLEAR-COLOR (red green blue alpha) and keeps the result."
  (let* ((texture-descriptor
           (msg-send :id (objc-class "MTLTextureDescriptor")
                     "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                     (:ulong pixel-format) (:ulong width) (:ulong height) (:bool nil)))
         (texture (progn
                    (msg-send :void texture-descriptor "setUsage:" (:ulong +usage-render-target+))
                    (msg-send :void texture-descriptor "setStorageMode:" (:ulong +storage-shared+))
                    (msg-send :id device "newTextureWithDescriptor:" (:id texture-descriptor))))
         (pass (msg-send :id (objc-class "MTLRenderPassDescriptor") "renderPassDescriptor"))
         (attachment (msg-send :id (msg-send :id pass "colorAttachments")
                               "objectAtIndexedSubscript:" (:ulong 0))))
    (destructuring-bind (red green blue alpha) clear-color
      (msg-send :void attachment "setTexture:" (:id texture))
      (msg-send :void attachment "setLoadAction:" (:ulong +load-action-clear+))
      (msg-send :void attachment "setStoreAction:" (:ulong +store-action-store+))
      (msg-send :void attachment "setClearColor:" (:color red green blue alpha)))
    (msg-send :id pass "retain")
    (%make-offscreen-target :texture texture :descriptor pass :width width :height height)))

(defun read-pixels (target)
  "The pixels of TARGET as a vector of bytes, four per pixel (blue, green, red,
alpha), row by row from the top. Call after the GPU has finished drawing."
  (let* ((width (offscreen-target-width target))
         (height (offscreen-target-height target))
         (bytes-per-row (* 4 width))
         (pixels (make-array (* bytes-per-row height) :element-type '(unsigned-byte 8))))
    ;; MTLRegion is six words (x y z, width height depth): larger than 16 bytes,
    ;; so it is passed by address.
    (sb-alien:with-alien ((region (array sb-alien:unsigned-long 6)))
      (loop :for value :in (list 0 0 0 width height 1)
            :for i :from 0
            :do (setf (sb-alien:deref region i) value))
      (sb-sys:with-pinned-objects (pixels)
        (msg-send :void (offscreen-target-texture target)
                  "getBytes:bytesPerRow:fromRegion:mipmapLevel:"
                  (:pointer (sb-sys:sap-int (sb-sys:vector-sap pixels)))
                  (:ulong bytes-per-row)
                  (:pointer (sb-sys:sap-int (sb-alien:alien-sap region)))
                  (:ulong 0))))
    pixels))
