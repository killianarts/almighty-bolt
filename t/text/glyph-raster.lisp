(defpackage #:almighty-bolt/t/text/glyph-raster
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:font #:almighty-bolt/text/font)
                    (#:line #:almighty-bolt/text/line)
                    (#:raster #:almighty-bolt/text/glyph-raster)))

(in-package #:almighty-bolt/t/text/glyph-raster)

;;; Drawing a glyph with Core Graphics into a coverage bitmap.

(defmacro with-glyph ((bitmap text scale) &body body)
  "Bind BITMAP to the rasterization of the first glyph of TEXT in Helvetica 20 at SCALE."
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let* ((font (font:make-font "Helvetica" 20))
              (line (line:get-line font ,text))
              (,bitmap (raster:rasterize-glyph (aref (line:text-line-glyph-fonts line) 0)
                                               (aref (line:text-line-glyphs line) 0)
                                               ,scale)))
         ,@body))))

(defun ink-in-row (bitmap row)
  "How many pixels of ROW (0 is the top) have any ink."
  (let ((width (raster:glyph-bitmap-width bitmap))
        (pixels (raster:glyph-bitmap-pixels bitmap)))
    (loop :for x :below width :count (plusp (aref pixels (+ (* row width) x))))))

(define-test a-glyph-has-ink-and-an-empty-margin ()
  (with-glyph (bitmap "H" 1)
    (let ((width (raster:glyph-bitmap-width bitmap))
          (height (raster:glyph-bitmap-height bitmap))
          (pixels (raster:glyph-bitmap-pixels bitmap)))
      (assert-eql (* width height) (length pixels))
      (assert-eql 255 (reduce #'max pixels))
      ;; The first and last rows and columns are the margin.
      (assert-eql 0 (ink-in-row bitmap 0))
      (assert-eql 0 (ink-in-row bitmap (1- height)))
      (dotimes (row height)
        (assert-eql 0 (aref pixels (* row width)))
        (assert-eql 0 (aref pixels (+ (* row width) (1- width))))))))

(defun first-and-last-ink-rows (bitmap)
  "The first and last rows (0 is the top) that have any ink."
  (let ((rows (loop :for row :below (raster:glyph-bitmap-height bitmap)
                    :when (plusp (ink-in-row bitmap row)) :collect row)))
    (values (first rows) (car (last rows)))))

(define-test the-top-of-a-glyph-is-the-first-row ()
  ;; A T is wide at the top and a narrow stem below. If the bitmap were upside
  ;; down, the wide row would be last.
  (with-glyph (bitmap "T" 1)
    (multiple-value-bind (first last) (first-and-last-ink-rows bitmap)
      (assert-true (> (ink-in-row bitmap first) (* 2 (ink-in-row bitmap last)))
                   (list (ink-in-row bitmap first) (ink-in-row bitmap last))))))

(define-test twice-the-scale-is-about-twice-the-size ()
  ;; Compare the ink, without the margin, which is the same at every scale.
  (with-glyph (one "H" 1)
    (with-glyph (two "H" 2)
      (flet ((ink-size (bitmap) (- (raster:glyph-bitmap-width bitmap) 4)))
        (assert-true (<= (abs (- (ink-size two) (* 2 (ink-size one)))) 2)
                     (list (ink-size one) (ink-size two))))
      (flet ((ink-size (bitmap) (- (raster:glyph-bitmap-height bitmap) 4)))
        (assert-true (<= (abs (- (ink-size two) (* 2 (ink-size one)))) 2)
                     (list (ink-size one) (ink-size two)))))))

(define-test the-bearing-places-the-bitmap-against-the-baseline ()
  ;; A capital H at 20 points is about 14 points tall and sits on the baseline.
  ;; The bitmap has a margin of two pixels, so its top is a little over 14 points
  ;; above the baseline and its bottom is a little under it.
  (with-glyph (bitmap "H" 1)
    (let ((top (raster:glyph-bitmap-bearing-y bitmap))
          (bottom (- (raster:glyph-bitmap-bearing-y bitmap)
                     (raster:glyph-bitmap-height bitmap))))
      (assert-true (< 14 top 19) top)
      (assert-true (< -4 bottom 0) bottom)
      ;; The left edge of the ink is a little right of the origin, the margin a
      ;; little left of it.
      (assert-true (< -3 (raster:glyph-bitmap-bearing-x bitmap) 0)
                   (raster:glyph-bitmap-bearing-x bitmap)))))

(define-test a-space-has-no-ink ()
  (with-glyph (bitmap " " 1)
    (assert-false bitmap)))
