(defpackage #:almighty-bolt-stress/dashboard/paint
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:push-rect
                #:pack-color)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+samples+
                #:+series+
                #:dashboard-head
                #:dashboard-history)
  (:export #:paint-chart))

(in-package #:almighty-bolt-stress/dashboard/paint)

;;; How the dashboard's chart looks, the same in every variant: a dark ground and, for each of the
;;; three series, a short bar at the height of each of the last 240 steps, the oldest at the left.

(defparameter +ground+ (pack-color 30 33 46))
(defparameter +colors+ (vector (pack-color 120 220 160) (pack-color 150 200 255)
                               (pack-color 240 180 110)))

(defun paint-chart (list x y width height dashboard)
  "Append the commands that draw DASHBOARD's history in the area X, Y, WIDTH by HEIGHT."
  (declare (type single-float x y width height))
  (push-rect list x y width height +ground+)
  (let* ((history (dashboard-history dashboard))
         (head (dashboard-head dashboard))
         (column (/ width +samples+))
         (room (- height 4f0)))
    (declare (type (simple-array single-float (*)) history))
    (dotimes (series +series+)
      (let ((color (svref +colors+ series)))
        (dotimes (slot +samples+)
          (let ((sample (aref history (+ (* series +samples+) (mod (+ head slot) +samples+)))))
            (push-rect list (+ x (* slot column)) (+ y (- height 4f0 (* sample room)))
                       column 2f0 color)))))))
