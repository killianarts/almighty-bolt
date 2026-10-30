(defpackage #:almighty-bolt-stress/rows/paint
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:context-font
                #:context-measure
                #:context-push-text
                #:push-rect
                #:push-rounded-rect
                #:pack-color)
  (:import-from #:almighty-bolt-stress/rows/model
                #:+row-count+
                #:+row-height+
                #:+visible-rows+
                #:+list-width+
                #:+list-height+
                #:+bar-width+
                #:+thumb-height+
                #:thumb-top
                #:row-id-text
                #:row-name-text
                #:row-value-text)
  (:export #:paint-rows))

(in-package #:almighty-bolt-stress/rows/paint)

;;; How the list looks, the same in every variant: a dark ground with a lighter stripe on every
;;; second row, the selected row in blue, the number of the row, a name and a value across, and the
;;; bar at the right with its thumb.

(defparameter +ground+ (pack-color 30 33 46))
(defparameter +stripe+ (pack-color 38 42 58))
(defparameter +selected+ (pack-color 72 104 200))
(defparameter +text+ (pack-color 232 234 242))
(defparameter +dim+ (pack-color 150 156 176))
(defparameter +track+ (pack-color 24 27 40))
(defparameter +thumb+ (pack-color 110 150 235))

(defun paint-rows (context list x y width height top selected)
  "Append the commands that draw the list in the area X, Y, WIDTH by HEIGHT, scrolled to row TOP,
with row SELECTED (or NIL) selected."
  (declare (ignore width height))
  (let ((font (context-font context "Helvetica" 14)))
    (multiple-value-bind (text-width ascent descent) (context-measure context font "Hg")
      (declare (ignore text-width))
      (let ((baseline (+ (/ (- +row-height+ (+ ascent descent)) 2) ascent))
            (right (+ x (- +list-width+ +bar-width+ 12))))
        (push-rect list x y +list-width+ +list-height+ +ground+)
        (dotimes (slot +visible-rows+)
          (let ((index (+ top slot))
                (row-y (+ y (* slot +row-height+))))
            (when (< index +row-count+)
              (cond ((eql index selected)
                     (push-rect list x row-y (- +list-width+ +bar-width+) +row-height+ +selected+))
                    ((oddp index)
                     (push-rect list x row-y (- +list-width+ +bar-width+) +row-height+ +stripe+)))
              (context-push-text context list font (row-id-text index) (+ x 8) (+ row-y baseline)
                                 (if (eql index selected) +text+ +dim+))
              (context-push-text context list font (row-name-text index) (+ x 120) (+ row-y baseline)
                                 +text+)
              (let ((value (row-value-text index)))
                (context-push-text context list font value
                                   (- right (context-measure context font value))
                                   (+ row-y baseline) +text+)))))
        (push-rect list (+ x (- +list-width+ +bar-width+)) y +bar-width+ +list-height+ +track+)
        (push-rounded-rect list (+ x (- +list-width+ +bar-width+) 2) (+ y (thumb-top top))
                           (- +bar-width+ 4) +thumb-height+ 4 +thumb+)))))
