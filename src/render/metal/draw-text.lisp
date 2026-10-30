(defpackage #:almighty-bolt/render/metal/draw-text
  (:use #:cl)
  (:import-from #:almighty-bolt/text/line
                #:text-line-glyph-count
                #:text-line-glyphs
                #:text-line-glyph-fonts
                #:text-line-xs
                #:text-line-ys)
  (:import-from #:almighty-bolt/render/metal/glyph-atlas
                #:glyph-atlas-scale
                #:atlas-glyph-index
                #:atlas-bearing-x
                #:atlas-bearing-y
                #:atlas-width
                #:atlas-height)
  (:import-from #:almighty-bolt/display-list
                #:push-glyph)
  (:export #:push-line))

(in-package #:almighty-bolt/render/metal/draw-text)

;;; Turn a shaped line into glyph commands.

(defun push-line (display-list atlas line x baseline color)
  "Append a glyph command for each glyph of LINE, which starts at X and whose
baseline is BASELINE (points, y down), in COLOR. Glyphs are snapped to whole
pixels so that the atlas bitmaps are sampled one texel to a pixel: the pen position and the
baseline are rounded, once, and then each glyph's bitmap is placed by its own whole-pixel
offsets from them, so that no glyph is rounded a different way from its neighbours. A glyph
not yet in the atlas is rasterized now. Returns the number of commands added (a space has none)."
  (declare (type single-float x baseline))
  (let* ((scale (glyph-atlas-scale atlas))
         (inverse (/ 1f0 scale))
         (glyphs (text-line-glyphs line))
         (fonts (text-line-glyph-fonts line))
         (xs (text-line-xs line))
         (ys (text-line-ys line))
         (added 0))
    (declare (type fixnum added))
    (dotimes (i (text-line-glyph-count line))
      (let ((index (atlas-glyph-index atlas (aref fonts i) (aref glyphs i))))
        (when (>= index 0)
          (let* ((pen (round (* scale (+ x (aref xs i)))))
                 (base (round (* scale (- baseline (aref ys i)))))
                 (left (* inverse (+ pen (round (* scale (atlas-bearing-x atlas index))))))
                 (top (* inverse (- base (round (* scale (atlas-bearing-y atlas index)))))))
            (push-glyph display-list left top
                        (atlas-width atlas index) (atlas-height atlas index)
                        index color)
            (incf added)))))
    added))
