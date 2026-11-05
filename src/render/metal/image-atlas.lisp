(defpackage #:almighty-bolt/render/metal/image-atlas
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:import-from #:almighty-bolt/app-instance
                #:glyphs-full)
  (:import-from #:almighty-bolt/image
                #:image
                #:image-id
                #:image-pixel-width
                #:image-pixel-height
                #:image-pixels)
  (:export #:image-atlas
           #:make-image-atlas
           #:image-atlas-texture
           #:image-atlas-uv-buffer
           #:image-atlas-count
           #:image-atlas-full
           #:image-too-large
           #:clear-image-atlas
           #:atlas-image-index))

(in-package #:almighty-bolt/render/metal/image-atlas)

;;; Images packed into one colour texture, as glyphs are into the glyph atlas, so that an image is
;;; one textured quad and a frame is still one draw. An image is copied in the first time it is
;;; drawn and found by its id after that. Images are sampled smoothly, so that one drawn larger or
;;; smaller than its pixels is filtered; the shader keeps the texture coordinates half a texel
;;; inside each image's own box, where the filter reads that image's texels only, so images can lie
;;; edge to edge without one bleeding into the next.
;;;
;;; As with the glyph atlas, a full atlas signals IMAGE-ATLAS-FULL (a GLYPHS-FULL), and the frame
;;; is built again on an emptied atlas. An image that would not fit even in an empty one signals
;;; IMAGE-TOO-LARGE instead.

(defconstant +pixel-format-rgba8-unorm+ 70)
(defconstant +usage-shader-read+ 1)
(defconstant +storage-shared+ 0)

(define-condition image-atlas-full (glyphs-full)
  ((what :initarg :what :reader image-atlas-full-what))
  (:report (lambda (condition stream)
             (format stream "The image atlas is full (~a)." (image-atlas-full-what condition)))))

(define-condition image-too-large (error)
  ((width :initarg :width :reader image-too-large-width)
   (height :initarg :height :reader image-too-large-height)
   (size :initarg :size :reader image-too-large-size))
  (:report (lambda (condition stream)
             (format stream "An image of ~d by ~d pixels cannot be drawn: the image atlas is ~d ~
                             pixels a side."
                     (image-too-large-width condition) (image-too-large-height condition)
                     (image-too-large-size condition)))))

(defstruct (image-atlas (:constructor %make-image-atlas))
  (texture 0 :type (unsigned-byte 64) :read-only t)
  (uv-buffer 0 :type (unsigned-byte 64) :read-only t)
  (uv-contents 0 :type (unsigned-byte 64) :read-only t)
  (size 0 :type fixnum :read-only t)
  (capacity 0 :type fixnum :read-only t)
  (count 0 :type fixnum)
  (table (make-hash-table :test 'eql) :read-only t)
  (shelf-x 0 :type fixnum)
  (shelf-y 0 :type fixnum)
  (shelf-height 0 :type fixnum))

(defun make-image-atlas (device &key (size 2048) (capacity 1024))
  "An empty image atlas on DEVICE: a SIZE by SIZE colour texture and room for CAPACITY images."
  (let* ((descriptor (msg-send :id (objc-class "MTLTextureDescriptor")
                               "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                               (:ulong +pixel-format-rgba8-unorm+) (:ulong size) (:ulong size)
                               (:bool nil)))
         (texture (progn
                    (msg-send :void descriptor "setUsage:" (:ulong +usage-shader-read+))
                    (msg-send :void descriptor "setStorageMode:" (:ulong +storage-shared+))
                    (msg-send :id device "newTextureWithDescriptor:" (:id descriptor))))
         (uv-buffer (msg-send :id device "newBufferWithLength:options:"
                              (:ulong (* capacity 16)) (:ulong 0))))
    ;; The texture is not cleared: only the boxes images were copied into are ever sampled.
    (%make-image-atlas :texture texture
                       :uv-buffer uv-buffer
                       :uv-contents (msg-send :id uv-buffer "contents")
                       :size size
                       :capacity capacity)))

(defun clear-image-atlas (atlas)
  "Forget every image in ATLAS, so that it has all its room again. The GPU must not be drawing
from it."
  (clrhash (image-atlas-table atlas))
  (setf (image-atlas-count atlas) 0
        (image-atlas-shelf-x atlas) 0
        (image-atlas-shelf-y atlas) 0
        (image-atlas-shelf-height atlas) 0)
  atlas)

(defun upload (atlas x y width height pixels)
  "Copy the WIDTH by HEIGHT pixels PIXELS, four bytes each, into the texture with their top left at
X, Y."
  (sb-alien:with-alien ((region (array sb-alien:unsigned-long 6)))
    (loop :for value :in (list x y 0 width height 1)
          :for i :from 0
          :do (setf (sb-alien:deref region i) value))
    (sb-sys:with-pinned-objects (pixels)
      (msg-send :void (image-atlas-texture atlas) "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
                (:pointer (sb-sys:sap-int (sb-alien:alien-sap region)))
                (:ulong 0)
                (:pointer (sb-sys:sap-int (sb-sys:vector-sap pixels)))
                (:ulong (* 4 width))))))

(defun allocate-space (atlas width height)
  "Find room for a WIDTH by HEIGHT image on the current shelf or a new one, and return its x and y."
  (let ((size (image-atlas-size atlas))
        (w width)
        (h height))
    (when (> (+ (image-atlas-shelf-x atlas) w) size)
      (setf (image-atlas-shelf-y atlas) (+ (image-atlas-shelf-y atlas)
                                           (image-atlas-shelf-height atlas))
            (image-atlas-shelf-x atlas) 0
            (image-atlas-shelf-height atlas) 0))
    (when (> (+ (image-atlas-shelf-y atlas) h) size)
      (error 'image-atlas-full :what "no room for another image"))
    (let ((x (image-atlas-shelf-x atlas))
          (y (image-atlas-shelf-y atlas)))
      (incf (image-atlas-shelf-x atlas) w)
      (setf (image-atlas-shelf-height atlas) (max (image-atlas-shelf-height atlas) h))
      (values x y))))

(defun add-image (atlas image)
  "Copy IMAGE into the atlas and return its index."
  (let ((index (image-atlas-count atlas))
        (width (image-pixel-width image))
        (height (image-pixel-height image))
        (size (image-atlas-size atlas)))
    (when (or (> width size) (> height size))
      (error 'image-too-large :width width :height height :size size))
    (when (>= index (image-atlas-capacity atlas))
      (error 'image-atlas-full :what "no room for another image's place"))
    (multiple-value-bind (x y) (allocate-space atlas width height)
      (upload atlas x y width height (image-pixels image))
      (let ((sap (sb-sys:int-sap (image-atlas-uv-contents atlas)))
            (offset (* index 16))
            (size (float size 1f0)))
        ;; The texture coordinates of the image: left, top, right, bottom.
        (setf (sb-sys:sap-ref-single sap offset) (/ x size)
              (sb-sys:sap-ref-single sap (+ offset 4)) (/ y size)
              (sb-sys:sap-ref-single sap (+ offset 8)) (/ (+ x width) size)
              (sb-sys:sap-ref-single sap (+ offset 12)) (/ (+ y height) size))))
    (setf (image-atlas-count atlas) (1+ index))
    index))

(defun atlas-image-index (atlas image)
  "The index of IMAGE in ATLAS, copying it in first if it is not there yet. Allocates nothing for
an image already there."
  (declare (type image-atlas atlas) (type image image))
  (let ((table (image-atlas-table atlas))
        (id (image-id image)))
    (or (gethash id table)
        (setf (gethash id table) (add-image atlas image)))))
