(defpackage #:almighty-bolt/render/image-file
  (:use #:cl)
  (:import-from #:almighty-bolt/text/core-foundation
                #:ccall
                #:cf-constant
                #:cf-release
                #:cf-string)
  (:import-from #:almighty-bolt/image
                #:make-premultiplied-image)
  (:export #:load-image))

(in-package #:almighty-bolt/render/image-file)

;;; Reading an image file with Image I/O, which reads what the system reads: PNG, JPEG, GIF (its
;;; first frame), TIFF, HEIC, BMP and the rest. The picture is drawn by Core Graphics into a bitmap
;;; of sRGB pixels, premultiplied, red first: the space the theme's colours are written in and the
;;; renderer draws them in, so that a pixel of #ff0000 in the file is drawn as #ff0000 is.

(sb-alien:load-shared-object "/System/Library/Frameworks/ImageIO.framework/ImageIO")

(defconstant +premultiplied-last-big-endian+ (logior 1 (ash 4 12))
  "kCGImageAlphaPremultipliedLast with kCGBitmapByteOrder32Big: the bytes red, green, blue and
alpha, in that order.")

(defun scale-from-name (file)
  "The pixels a point that FILE's name says: 2 for one whose name ends in @2x (as in
\"icon@2x.png\"), 3 for @3x, and otherwise 1."
  (let ((name (pathname-name file)))
    (cond ((and name (>= (length name) 3) (string-equal "@2x" name :start2 (- (length name) 3))) 2)
          ((and name (>= (length name) 3) (string-equal "@3x" name :start2 (- (length name) 3))) 3)
          (t 1))))

(defun load-image (path &key scale)
  "The image in the file at PATH, as MAKE-IMAGE makes one. SCALE is how many pixels it has to a
point; by default 2 if the file's name ends in @2x, 3 if in @3x, and otherwise 1. Read an image
once, not each frame: each one read is a new image to copy to the GPU."
  (let ((file (probe-file path)))
    (unless file
      (error "There is no image file ~a." path))
    (when scale
      (check-type scale (real (0))))
    (let* ((cf-path (cf-string (namestring file)))
           (url (ccall "CFURLCreateWithFileSystemPath" :ulong
                       (:ulong 0) (:ulong cf-path) (:long 0) (:bool nil)))
           (source (ccall "CGImageSourceCreateWithURL" :ulong (:ulong url) (:ulong 0)))
           (picture (if (zerop source)
                        0
                        (ccall "CGImageSourceCreateImageAtIndex" :ulong
                               (:ulong source) (:ulong 0) (:ulong 0)))))
      (cf-release cf-path)
      (cf-release url)
      (unless (zerop source) (cf-release source))
      (when (zerop picture)
        (error "The file ~a is not an image that can be read." (namestring file)))
      (unwind-protect
           (let* ((width (ccall "CGImageGetWidth" :ulong (:ulong picture)))
                  (height (ccall "CGImageGetHeight" :ulong (:ulong picture)))
                  (pixels (make-array (* 4 width height) :element-type '(unsigned-byte 8)
                                                         :initial-element 0))
                  (space (ccall "CGColorSpaceCreateWithName" :ulong
                                (:ulong (cf-constant "kCGColorSpaceSRGB")))))
             (sb-sys:with-pinned-objects (pixels)
               (let ((context (ccall "CGBitmapContextCreate" :ulong
                                     (:pointer (sb-sys:sap-int (sb-sys:vector-sap pixels)))
                                     (:ulong width) (:ulong height) (:ulong 8) (:ulong (* 4 width))
                                     (:ulong space) (:uint +premultiplied-last-big-endian+))))
                 (ccall "CGColorSpaceRelease" :void (:ulong space))
                 (when (zerop context)
                   (error "Core Graphics could not make a bitmap for ~a." (namestring file)))
                 (ccall "CGContextDrawImage" :void (:ulong context)
                        (:double 0d0) (:double 0d0)
                        (:double (float width 1d0)) (:double (float height 1d0))
                        (:ulong picture))
                 (ccall "CGContextRelease" :void (:ulong context))))
             (make-premultiplied-image width height pixels (or scale (scale-from-name file))))
        (cf-release picture)))))
