(defpackage #:almighty-bolt/render/metal/glyph-atlas
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:import-from #:almighty-bolt/text/glyph-raster
                #:rasterize-glyph
                #:glyph-bitmap-pixels
                #:glyph-bitmap-width
                #:glyph-bitmap-height
                #:glyph-bitmap-bearing-x
                #:glyph-bitmap-bearing-y)
  (:export #:glyph-atlas
           #:make-glyph-atlas
           #:glyph-atlas-texture
           #:glyph-atlas-uv-buffer
           #:glyph-atlas-scale
           #:glyph-atlas-count
           #:glyph-atlas-full
           #:atlas-glyph-index
           #:atlas-bearing-x
           #:atlas-bearing-y
           #:atlas-width
           #:atlas-height))

(in-package #:almighty-bolt/render/metal/glyph-atlas)

;;; Rasterized glyphs packed into one grey-scale texture, so that drawing text is
;;; a textured quad per glyph. A glyph is rasterized the first time it is wanted
;;; and found by (font, glyph) after that; a lookup that finds its glyph
;;; allocates nothing. Glyphs are rasterized at a fixed number of pixels a point
;;; (the atlas's scale) and must be drawn at that scale, one texel to a pixel.
;;;
;;; The atlas never evicts: when it is full, asking for another glyph is an error.
;;; Glyphs are not positioned between pixels: the draw code snaps them to whole
;;; pixels.

(defconstant +pixel-format-r8-unorm+ 10)
(defconstant +usage-shader-read+ 1)
(defconstant +storage-shared+ 0)

(define-condition glyph-atlas-full (error)
  ((what :initarg :what :reader glyph-atlas-full-what))
  (:report (lambda (condition stream)
             (format stream "The glyph atlas is full (~a)." (glyph-atlas-full-what condition)))))

(defstruct (glyph-atlas (:constructor %make-glyph-atlas))
  (texture 0 :type (unsigned-byte 64) :read-only t)
  (uv-buffer 0 :type (unsigned-byte 64) :read-only t)
  (uv-contents 0 :type (unsigned-byte 64) :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (scale 2f0 :type single-float :read-only t)
  (capacity 0 :type fixnum :read-only t)
  (count 0 :type fixnum)
  (table (make-hash-table :test 'eql) :read-only t)
  (bearing-xs nil :type (simple-array single-float (*)) :read-only t)
  (bearing-ys nil :type (simple-array single-float (*)) :read-only t)
  (widths nil :type (simple-array single-float (*)) :read-only t)
  (heights nil :type (simple-array single-float (*)) :read-only t)
  (shelf-x 0 :type fixnum)
  (shelf-y 0 :type fixnum)
  (shelf-height 0 :type fixnum))

(defun make-glyph-atlas (device &key (width 1024) (height 1024) (capacity 4096) (scale 2))
  "An empty atlas on DEVICE: a WIDTH by HEIGHT texture and room for CAPACITY glyphs,
rasterized at SCALE pixels a point."
  (let* ((descriptor (msg-send :id (objc-class "MTLTextureDescriptor")
                               "texture2DDescriptorWithPixelFormat:width:height:mipmapped:"
                               (:ulong +pixel-format-r8-unorm+) (:ulong width) (:ulong height)
                               (:bool nil)))
         (texture (progn
                    (msg-send :void descriptor "setUsage:" (:ulong +usage-shader-read+))
                    (msg-send :void descriptor "setStorageMode:" (:ulong +storage-shared+))
                    (msg-send :id device "newTextureWithDescriptor:" (:id descriptor))))
         (uv-buffer (msg-send :id device "newBufferWithLength:options:"
                              (:ulong (* capacity 16)) (:ulong 0)))
         (atlas (%make-glyph-atlas
                 :texture texture
                 :uv-buffer uv-buffer
                 :uv-contents (msg-send :id uv-buffer "contents")
                 :width width
                 :height height
                 :scale (float scale 1f0)
                 :capacity capacity
                 :bearing-xs (make-array capacity :element-type 'single-float)
                 :bearing-ys (make-array capacity :element-type 'single-float)
                 :widths (make-array capacity :element-type 'single-float)
                 :heights (make-array capacity :element-type 'single-float))))
    ;; A new texture's contents are not defined; start from empty.
    (upload atlas 0 0 width height
            (make-array (* width height) :element-type '(unsigned-byte 8) :initial-element 0))
    atlas))

(defun upload (atlas x y width height pixels)
  "Copy the WIDTH by HEIGHT bytes PIXELS into the texture with its top left at X, Y."
  ;; An MTLRegion is six words (x y z, width height depth): larger than 16 bytes, so
  ;; it is passed by address.
  (sb-alien:with-alien ((region (array sb-alien:unsigned-long 6)))
    (loop :for value :in (list x y 0 width height 1)
          :for i :from 0
          :do (setf (sb-alien:deref region i) value))
    (sb-sys:with-pinned-objects (pixels)
      (msg-send :void (glyph-atlas-texture atlas) "replaceRegion:mipmapLevel:withBytes:bytesPerRow:"
                (:pointer (sb-sys:sap-int (sb-alien:alien-sap region)))
                (:ulong 0)
                (:pointer (sb-sys:sap-int (sb-sys:vector-sap pixels)))
                (:ulong width)))))

(defun allocate-space (atlas width height)
  "Find room for a WIDTH by HEIGHT bitmap on the current shelf or a new one, and
return its x and y."
  (when (> (+ (glyph-atlas-shelf-x atlas) width) (glyph-atlas-width atlas))
    (setf (glyph-atlas-shelf-y atlas) (+ (glyph-atlas-shelf-y atlas)
                                         (glyph-atlas-shelf-height atlas))
          (glyph-atlas-shelf-x atlas) 0
          (glyph-atlas-shelf-height atlas) 0))
  (when (or (> width (glyph-atlas-width atlas))
            (> (+ (glyph-atlas-shelf-y atlas) height) (glyph-atlas-height atlas)))
    (error 'glyph-atlas-full :what "no room for another bitmap"))
  (let ((x (glyph-atlas-shelf-x atlas))
        (y (glyph-atlas-shelf-y atlas)))
    (incf (glyph-atlas-shelf-x atlas) width)
    (setf (glyph-atlas-shelf-height atlas) (max (glyph-atlas-shelf-height atlas) height))
    (values x y)))

(defun add-glyph (atlas font-address glyph)
  "Rasterize GLYPH of the font at FONT-ADDRESS into the atlas. Returns its index, or
-1 if the glyph has no ink."
  (let ((bitmap (rasterize-glyph font-address glyph (glyph-atlas-scale atlas))))
    (if (null bitmap)
        -1
        (let ((index (glyph-atlas-count atlas))
              (width (glyph-bitmap-width bitmap))
              (height (glyph-bitmap-height bitmap))
              (scale (glyph-atlas-scale atlas)))
          (when (>= index (glyph-atlas-capacity atlas))
            (error 'glyph-atlas-full :what "no room for another glyph"))
          (multiple-value-bind (x y) (allocate-space atlas width height)
            (upload atlas x y width height (glyph-bitmap-pixels bitmap))
            (let ((sap (sb-sys:int-sap (glyph-atlas-uv-contents atlas)))
                  (offset (* index 16))
                  (atlas-width (float (glyph-atlas-width atlas) 1f0))
                  (atlas-height (float (glyph-atlas-height atlas) 1f0)))
              ;; The texture coordinates of the bitmap: left, top, right, bottom.
              (setf (sb-sys:sap-ref-single sap offset) (/ x atlas-width)
                    (sb-sys:sap-ref-single sap (+ offset 4)) (/ y atlas-height)
                    (sb-sys:sap-ref-single sap (+ offset 8)) (/ (+ x width) atlas-width)
                    (sb-sys:sap-ref-single sap (+ offset 12)) (/ (+ y height) atlas-height)))
            (setf (aref (glyph-atlas-bearing-xs atlas) index) (glyph-bitmap-bearing-x bitmap)
                  (aref (glyph-atlas-bearing-ys atlas) index) (glyph-bitmap-bearing-y bitmap)
                  (aref (glyph-atlas-widths atlas) index) (/ width scale)
                  (aref (glyph-atlas-heights atlas) index) (/ height scale)))
          (setf (glyph-atlas-count atlas) (1+ index))
          index))))

(declaim (inline atlas-key))
(defun atlas-key (font-address glyph)
  (declare (type (unsigned-byte 46) font-address) (type (unsigned-byte 16) glyph))
  (logior (ash font-address 16) glyph))

(defun atlas-glyph-index (atlas font-address glyph)
  "The index of GLYPH of the font at FONT-ADDRESS in ATLAS, rasterizing it first if it
is not there yet, or -1 if it has no ink. Allocates nothing for a glyph already
there."
  (declare (type glyph-atlas atlas))
  (let ((key (atlas-key font-address glyph))
        (table (glyph-atlas-table atlas)))
    (or (gethash key table)
        (setf (gethash key table) (add-glyph atlas font-address glyph)))))

;;; What the draw code needs to know about a glyph: where its bitmap goes relative
;;; to the glyph's origin, and how big it is, in points.

(declaim (inline atlas-bearing-x atlas-bearing-y atlas-width atlas-height))
(defun atlas-bearing-x (atlas index) (aref (glyph-atlas-bearing-xs atlas) index))
(defun atlas-bearing-y (atlas index) (aref (glyph-atlas-bearing-ys atlas) index))
(defun atlas-width (atlas index) (aref (glyph-atlas-widths atlas) index))
(defun atlas-height (atlas index) (aref (glyph-atlas-heights atlas) index))
