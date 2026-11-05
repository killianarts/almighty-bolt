(defpackage #:almighty-bolt-stress/rows/paint
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:model #:almighty-bolt-stress/rows/model))
  (:export #:paint-rows))

(in-package #:almighty-bolt-stress/rows/paint)

;;; How the list looks: a dark ground with a lighter stripe on every
;;; second row, the selected row in blue, the number of the row, a name and a value across, and the
;;; bar at the right with its thumb.

(defparameter +ground+ (bolt:pack-color 30 33 46))
(defparameter +stripe+ (bolt:pack-color 38 42 58))
(defparameter +selected+ (bolt:pack-color 72 104 200))
(defparameter +text+ (bolt:pack-color 232 234 242))
(defparameter +dim+ (bolt:pack-color 150 156 176))
(defparameter +track+ (bolt:pack-color 24 27 40))
(defparameter +thumb+ (bolt:pack-color 110 150 235))

(defun paint-rows (context list x y width height top selected)
  "Append the commands that draw the list in the area X, Y, WIDTH by HEIGHT, scrolled to row TOP,
with row SELECTED (or NIL) selected."
  (declare (ignore width height))
  (let ((font (bolt:context-font context "Helvetica" 14)))
    (multiple-value-bind (text-width ascent descent) (bolt:context-measure context font "Hg")
      (declare (ignore text-width))
      (let ((baseline (+ (/ (- model:+row-height+ (+ ascent descent)) 2) ascent))
            (right (+ x (- model:+list-width+ model:+bar-width+ 12))))
        (bolt:push-rect list x y model:+list-width+ model:+list-height+ +ground+)
        (dotimes (slot model:+visible-rows+)
          (let ((index (+ top slot))
                (row-y (+ y (* slot model:+row-height+))))
            (when (< index model:+row-count+)
              (cond ((eql index selected)
                     (bolt:push-rect list x row-y (- model:+list-width+ model:+bar-width+) model:+row-height+ +selected+))
                    ((oddp index)
                     (bolt:push-rect list x row-y (- model:+list-width+ model:+bar-width+) model:+row-height+ +stripe+)))
              (bolt:context-push-text context list font (model:row-id-text index) (+ x 8) (+ row-y baseline)
                                      (if (eql index selected) +text+ +dim+))
              (bolt:context-push-text context list font (model:row-name-text index) (+ x 120) (+ row-y baseline)
                                      +text+)
              (let ((value (model:row-value-text index)))
                (bolt:context-push-text context list font value
                                        (- right (bolt:context-measure context font value))
                                        (+ row-y baseline) +text+)))))
        (bolt:push-rect list (+ x (- model:+list-width+ model:+bar-width+)) y model:+bar-width+ model:+list-height+ +track+)
        (bolt:push-rounded-rect list (+ x (- model:+list-width+ model:+bar-width+) 2) (+ y (model:thumb-top top))
                                (- model:+bar-width+ 4) model:+thumb-height+ 4 +thumb+)))))
