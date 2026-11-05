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

(defun push-line (display-list atlas line x baseline color &optional turn)
  "Append a glyph command for each glyph of LINE, which starts at X and whose
baseline is BASELINE (points, y down), in COLOR. Glyphs are snapped to whole
pixels so that the atlas bitmaps are sampled one texel to a pixel: the pen position and the
baseline are rounded, once, and then each glyph's bitmap is placed by its own whole-pixel
offsets from them, so that no glyph is rounded a different way from its neighbours. A glyph
not yet in the atlas is rasterized now. Returns the number of commands added (a space has none).

TURN :UP turns the line a quarter to the left about the point X, BASELINE, where it starts, so that
it reads upwards from there, the tops of its letters to the left; :DOWN turns it a quarter to the
right, reading downwards with their tops to the right. Each glyph is the same bitmap as upright, so a
turned line is the upright one's pixels turned."
  (declare (type single-float x baseline))
  (let* ((scale (glyph-atlas-scale atlas))
         (inverse (/ 1f0 scale))
         (glyphs (text-line-glyphs line))
         (fonts (text-line-glyph-fonts line))
         (xs (text-line-xs line))
         (ys (text-line-ys line))
         (start-x (round (* scale x)))
         (start-y (round (* scale baseline)))
         (added 0))
    (declare (type fixnum added))
    (dotimes (i (text-line-glyph-count line))
      (let ((index (atlas-glyph-index atlas (aref fonts i) (aref glyphs i))))
        (when (>= index 0)
          (let ((width (atlas-width atlas index))
                (height (atlas-height atlas index))
                (bearing-x (round (* scale (atlas-bearing-x atlas index))))
                (bearing-y (round (* scale (atlas-bearing-y atlas index)))))
            (if (null turn)
                (let* ((pen (round (* scale (+ x (aref xs i)))))
                       (base (round (* scale (- baseline (aref ys i))))))
                  (push-glyph display-list (* inverse (+ pen bearing-x)) (* inverse (- base bearing-y))
                              width height index color))
                ;; Along the line from its start, and across it from the baseline to the bitmap's
                ;; top edge (up the letters is less), in pixels.
                (let ((along (+ (round (* scale (aref xs i))) bearing-x))
                      (across (- (+ (round (* scale (aref ys i))) bearing-y)))
                      (long (round (* scale width)))
                      (short (round (* scale height))))
                  (ecase turn
                    (:up (push-glyph display-list (* inverse (+ start-x across))
                                     (* inverse (- start-y along long))
                                     height width index color :up))
                    (:down (push-glyph display-list (* inverse (- start-x across short))
                                       (* inverse (+ start-y along))
                                       height width index color :down)))))
            (incf added)))))
    added))
