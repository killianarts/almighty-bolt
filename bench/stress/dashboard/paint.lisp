(defpackage #:almighty-bolt-stress/dashboard/paint
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:model #:almighty-bolt-stress/dashboard/model))
  (:export #:paint-chart))

(in-package #:almighty-bolt-stress/dashboard/paint)

;;; How the dashboard's chart looks: a dark ground and, for each of the
;;; three series, a short bar at the height of each of the last 240 steps, the oldest at the left.

(defparameter +ground+ (bolt:pack-color 30 33 46))
(defparameter +colors+ (vector (bolt:pack-color 120 220 160) (bolt:pack-color 150 200 255)
                               (bolt:pack-color 240 180 110)))

(defun paint-chart (list x y width height dashboard)
  "Append the commands that draw DASHBOARD's history in the area X, Y, WIDTH by HEIGHT."
  (declare (type single-float x y width height))
  (bolt:push-rect list x y width height +ground+)
  (let* ((history (model:dashboard-history dashboard))
         (head (model:dashboard-head dashboard))
         (column (/ width model:+samples+))
         (room (- height 4f0)))
    (declare (type (simple-array single-float (*)) history))
    (dotimes (series model:+series+)
      (let ((color (svref +colors+ series)))
        (dotimes (slot model:+samples+)
          (let ((sample (aref history (+ (* series model:+samples+) (mod (+ head slot) model:+samples+)))))
            (bolt:push-rect list (+ x (* slot column)) (+ y (- height 4f0 (* sample room)))
                            column 2f0 color)))))))
