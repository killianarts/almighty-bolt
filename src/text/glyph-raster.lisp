(defpackage #:almighty-bolt/text/glyph-raster
  (:use #:cl)
  (:import-from #:almighty-bolt/text/core-foundation
                #:ccall)
  (:export #:glyph-bitmap
           #:glyph-bitmap-pixels
           #:glyph-bitmap-width
           #:glyph-bitmap-height
           #:glyph-bitmap-bearing-x
           #:glyph-bitmap-bearing-y
           #:rasterize-glyph))

(in-package #:almighty-bolt/text/glyph-raster)

;;; Draw one glyph with Core Graphics into a grey-scale coverage bitmap (0 for
;;; no ink, 255 for full ink), at a given number of pixels a point. Core Text
;;; does the drawing, so the result looks as the system draws text, except that
;;; the colour fringes of LCD smoothing are off: the coverage is the same
;;; whatever colour the text is later drawn in.
;;;
;;; The bitmap's edges are a whole number of pixels from the glyph's origin, so
;;; the origin, and with it the baseline, is on a pixel boundary of the bitmap.
;;; Drawn with its origin on a whole pixel, every glyph of a line then stands on
;;; the same baseline. If the bitmap were cut to the ink's own box instead, the
;;; baseline would fall a different fraction of a pixel into each glyph's bitmap,
;;; and rounding the bitmaps to whole pixels would set some letters a little
;;; higher or lower than others. The origin has to be on a whole pixel for Core
;;; Graphics too: it drops the fraction of a glyph's origin, so one that is not
;;; on a whole pixel would move the ink in the bitmap, by up to a pixel and by a
;;; different amount for each glyph.

(defconstant +padding+ 2
  "Empty pixels kept round the glyph. The antialiased edge of the ink reaches a
little past the box that Core Text reports for it, so a pixel is not enough.")

(defconstant +bitmap-info-alpha-only+ 7 "kCGImageAlphaOnly.")

(defstruct (glyph-bitmap (:constructor %make-glyph-bitmap))
  "A rasterized glyph. The pixels are WIDTH by HEIGHT bytes, row by row from the
top. BEARING-X is how far, in points, the bitmap's left edge lies to the right of
the glyph's origin, and BEARING-Y how far its top edge lies above the baseline;
at the scale the glyph was rasterized at, both are whole numbers of pixels."
  (pixels (make-array 0 :element-type '(unsigned-byte 8))
   :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (width 0 :type fixnum :read-only t)
  (height 0 :type fixnum :read-only t)
  (bearing-x 0f0 :type single-float :read-only t)
  (bearing-y 0f0 :type single-float :read-only t))

(defun glyph-bounds (ct-font glyph)
  "The box that GLYPH's ink fills, in points from its origin (y up): x, y, width
and height as four double floats."
  (sb-alien:with-alien ((glyph-id (sb-alien:unsigned 16))
                        (bounds (array sb-alien:double 4)))
    (setf glyph-id glyph)
    ;; The result is the union of the boxes; we want the box itself, which is
    ;; written to the array.
    (ccall "CTFontGetBoundingRectsForGlyphs" :void
           (:ulong ct-font)
           (:uint 0)                    ; kCTFontOrientationDefault
           (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr glyph-id))))
           (:pointer (sb-sys:sap-int (sb-alien:alien-sap bounds)))
           (:long 1))
    (values (sb-alien:deref bounds 0) (sb-alien:deref bounds 1)
            (sb-alien:deref bounds 2) (sb-alien:deref bounds 3))))

(defun rasterize-glyph (ct-font glyph scale)
  "Draw GLYPH of CT-FONT at SCALE pixels a point and return a GLYPH-BITMAP, or NIL if
the glyph has no ink (a space)."
  (multiple-value-bind (x y width height) (glyph-bounds ct-font glyph)
    (when (or (<= width 0d0) (<= height 0d0))
      (return-from rasterize-glyph nil))
    (let* ((scale (float scale 1d0))
           ;; The bitmap's edges, in whole pixels from the origin (y up): the ink's box
           ;; rounded outwards, and a margin.
           (left (- (floor (* x scale)) +padding+))
           (bottom (- (floor (* y scale)) +padding+))
           (right (+ (ceiling (* (+ x width) scale)) +padding+))
           (top (+ (ceiling (* (+ y height) scale)) +padding+))
           (pixel-width (- right left))
           (pixel-height (- top bottom))
           (pixels (make-array (* pixel-width pixel-height)
                               :element-type '(unsigned-byte 8) :initial-element 0)))
      (sb-alien:with-alien ((glyph-id (sb-alien:unsigned 16))
                            (origin (array sb-alien:double 2)))
        (setf glyph-id glyph
              ;; The origin is a whole number of pixels from the bitmap's bottom left.
              (sb-alien:deref origin 0) (/ (- left) scale)
              (sb-alien:deref origin 1) (/ (- bottom) scale))
        (sb-sys:with-pinned-objects (pixels)
          (let ((context (ccall "CGBitmapContextCreate" :ulong
                                (:pointer (sb-sys:sap-int (sb-sys:vector-sap pixels)))
                                (:ulong pixel-width) (:ulong pixel-height)
                                (:ulong 8) (:ulong pixel-width)
                                (:ulong 0)      ; no colour space: alpha only
                                (:uint +bitmap-info-alpha-only+))))
            (when (zerop context)
              (error "Core Graphics could not make a bitmap context."))
            (ccall "CGContextScaleCTM" :void (:ulong context) (:double scale) (:double scale))
            (ccall "CGContextSetShouldAntialias" :void (:ulong context) (:bool t))
            (ccall "CGContextSetAllowsFontSmoothing" :void (:ulong context) (:bool nil))
            (ccall "CGContextSetShouldSmoothFonts" :void (:ulong context) (:bool nil))
            (ccall "CTFontDrawGlyphs" :void
                   (:ulong ct-font)
                   (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr glyph-id))))
                   (:pointer (sb-sys:sap-int (sb-alien:alien-sap origin)))
                   (:ulong 1)
                   (:ulong context))
            (ccall "CGContextRelease" :void (:ulong context)))))
      (%make-glyph-bitmap :pixels pixels
                          :width pixel-width
                          :height pixel-height
                          :bearing-x (float (/ left scale) 1f0)
                          :bearing-y (float (/ top scale) 1f0)))))
