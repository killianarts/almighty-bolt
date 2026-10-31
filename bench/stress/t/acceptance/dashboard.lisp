(defpackage #:almighty-bolt-stress/t/acceptance/dashboard
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:m #:almighty-bolt-stress/dashboard/model)))

(in-package #:almighty-bolt-stress/t/acceptance/dashboard)

;;; The dashboard: 36 tiles, each with a name, a value (:tile-N-value) and a bar (:tile-N-bar), and
;;; a chart (:chart). Every frame is a new step of the model, so every value changes in every
;;; frame, and the app asks for the next frame all the time. The driver's clock starts at one
;;; second, which is step 59.

(defparameter +first-step+ 59)

(defun expected (tile step)
  (m:index-text (m:value-index tile step)))

(defun shown (driver tile)
  (d:text-in driver (m:tile-value-id tile)))

(d:define-acceptance every-tile-shows-the-value-of-the-step-the-time-is-in :dashboard
  (dotimes (tile m:+tiles+)
    (assert-equal (expected tile +first-step+) (shown driver tile) tile)))

(d:define-acceptance the-values-change-as-time-passes :dashboard
  (d:advance-time driver 0.5)
  (let ((step (m:step-at 1500000000)))
    (assert-eql 89 step)
    (dotimes (tile m:+tiles+)
      (assert-equal (expected tile step) (shown driver tile) tile))))

(d:define-acceptance a-frame-in-the-same-step-changes-nothing :dashboard
  (let ((before (loop :for tile :below m:+tiles+ :collect (shown driver tile))))
    ;; Ten nanoseconds more, which are still in the step that began at 0.983 s.
    (d:advance-time driver 1d-8)
    (assert-equal before (loop :for tile :below m:+tiles+ :collect (shown driver tile)))))

(d:define-acceptance a-run-of-frames-follows-the-steps :dashboard
  (dotimes (i 10)
    (d:advance-time driver (/ m:+frame-ns+ 1d9)))
  (let ((step (m:step-at (+ 1000000000 (* 10 m:+frame-ns+)))))
    (dotimes (tile 6)
      (assert-equal (expected tile step) (shown driver tile) tile))))

(d:define-acceptance the-dashboard-asks-for-the-next-frame-in-every-frame :dashboard
  (assert-true (d:frame-requested-p driver))
  (d:advance-time driver m:+frame-ns+ :frames 1)
  (assert-true (d:frame-requested-p driver)))

(defun fill-width (driver tile)
  "The width of the second shape in the bar of TILE, which is what is filled."
  (let ((shapes (d:shapes-in driver (m:tile-bar-id tile))))
    (and (second shapes) (fourth (second shapes)))))

(d:define-acceptance a-bar-is-as-full-as-its-value :dashboard
  (let ((rows (sort (loop :for tile :below m:+tiles+
                          :collect (cons (m:value-index tile +first-step+) (fill-width driver tile)))
                    #'< :key #'car)))
    ;; The fuller the value, the wider the fill, over all 36 tiles.
    (loop :for (a b) :on rows
          :while b
          :do (assert-true (<= (or (cdr a) 0) (or (cdr b) 0))))
    (assert-true (> (reduce #'max (mapcar (lambda (row) (or (cdr row) 0)) rows))
                    (* 2 (reduce #'min (mapcar (lambda (row) (or (cdr row) 0)) rows)))))))

(d:define-acceptance the-chart-has-a-ground-and-a-bar-for-each-series-and-step :dashboard
  (d:advance-time driver 0.1)
  (assert-eql (+ 1 (* m:+series+ m:+samples+)) (length (d:shapes-in driver :chart))))

(d:define-acceptance the-tiles-are-in-a-grid-of-six-by-six :dashboard
  (let ((rectangles (loop :for tile :below m:+tiles+
                          :collect (d:rectangle driver (m:tile-value-id tile)))))
    ;; Six distinct places across and six down, and every one inside the window.
    (assert-eql 6 (length (remove-duplicates (mapcar (lambda (r) (round (first r))) rectangles))))
    (assert-eql 6 (length (remove-duplicates (mapcar (lambda (r) (round (second r))) rectangles))))
    (dolist (r rectangles)
      (assert-true (<= 0 (first r)))
      (assert-true (<= (+ (first r) (third r)) 960))
      (assert-true (<= (+ (second r) (fourth r)) 680)))))
