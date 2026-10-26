(defpackage #:almighty-bolt-stress/t/dashboard
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-stress/dashboard/model)
                    (#:p #:almighty-bolt-stress/dashboard/paint)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-stress/t/dashboard)

;;; The dashboard's data: values that are a function of the tile and the step, the texts of
;;; them, the history of the chart, and the painter. All of it allocates nothing in a step.

(define-test a-value-is-a-number-of-tenths-from-0-to-999 ()
  (dotimes (tile m:+tiles+)
    (dotimes (step 500)
      (assert-true (<= 0 (m:value-index tile step) 999)))))

(define-test a-value-is-the-same-for-the-same-tile-and-step-and-changes-with-the-step ()
  (assert-eql (m:value-index 7 123) (m:value-index 7 123))
  (assert-true (> (length (remove-duplicates (loop :for step :below 100
                                                   :collect (m:value-index 7 step))))
                  20))
  (assert-true (> (length (remove-duplicates (loop :for tile :below m:+tiles+
                                                   :collect (m:value-index tile 50))))
                  10)))

(define-test the-text-of-a-value-is-its-tenths ()
  (assert-equal "0.0" (m:index-text 0))
  (assert-equal "0.7" (m:index-text 7))
  (assert-equal "42.3" (m:index-text 423))
  (assert-equal "99.9" (m:index-text 999)))

(define-test a-step-time-is-a-frame-at-sixty-a-second ()
  (assert-eql 0 (m:step-at 0))
  (assert-eql 0 (m:step-at (1- m:+frame-ns+)))
  (assert-eql 1 (m:step-at m:+frame-ns+))
  ;; The frame is 16.666667 ms, a little over a sixtieth of a second.
  (assert-eql 59 (m:step-at 1000000000))
  (assert-eql 60 (m:step-at 1000000020)))

(define-test advancing-sets-every-tile-and-says-whether-the-step-was-new ()
  (let ((dashboard (m:make-dashboard)))
    (assert-true (m:advance-dashboard dashboard 10))
    (dotimes (tile m:+tiles+)
      (assert-eql (m:value-index tile 10) (aref (m:dashboard-indices dashboard) tile))
      (assert-equal (m:index-text (m:value-index tile 10)) (m:value-text dashboard tile)))
    (assert-false (m:advance-dashboard dashboard 10))
    (assert-true (m:advance-dashboard dashboard 11))
    (assert-eql 11 (m:dashboard-step dashboard))))

(define-test a-bar-is-the-value-as-a-fraction ()
  (let ((dashboard (m:make-dashboard)))
    (m:advance-dashboard dashboard 3)
    (dotimes (tile m:+tiles+)
      (assert-true (< (abs (- (m:tile-fraction dashboard tile)
                              (/ (m:value-index tile 3) 999f0)))
                      1e-6)))))

(define-test the-history-keeps-the-last-240-steps-oldest-at-the-head ()
  (let ((dashboard (m:make-dashboard)))
    (dotimes (step 250)
      (m:advance-dashboard dashboard step))
    (assert-eql 10 (m:dashboard-head dashboard))
    ;; Series 0 of step 249 is the mean of tiles 0, 3, 6, ... (a series is every third tile).
    (let ((expected (/ (loop :for tile :from 0 :below m:+tiles+ :by 3
                             :sum (m:value-index tile 249))
                       12000f0))
          (slot (mod 249 m:+samples+)))
      (assert-true (< (abs (- expected (aref (m:dashboard-history dashboard) slot))) 1e-5)))))

(define-test advancing-a-step-allocates-nothing ()
  (let ((dashboard (m:make-dashboard)))
    (dotimes (step 100) (m:advance-dashboard dashboard step))
    (let ((before (sb-ext:get-bytes-consed)))
      (loop :for step :from 100 :below 20100
            :do (m:advance-dashboard dashboard step))
      (assert-true (< (- (sb-ext:get-bytes-consed) before) 200000)))))

(define-test the-chart-is-a-ground-and-a-bar-for-every-series-and-step ()
  (let ((dashboard (m:make-dashboard))
        (list (core:make-display-list :capacity 4096)))
    (dotimes (step 300) (m:advance-dashboard dashboard step))
    (p:paint-chart list 0f0 0f0 936f0 90f0 dashboard)
    (assert-eql (+ 1 (* m:+series+ m:+samples+)) (core:display-list-count list))
    ;; Every bar is inside the area.
    (dotimes (i (core:display-list-count list))
      (assert-true (<= 0f0 (core:display-list-x list i)))
      (assert-true (<= (+ (core:display-list-x list i) (core:display-list-width list i)) 936.01f0))
      (assert-true (<= 0f0 (core:display-list-y list i)))
      (assert-true (<= (+ (core:display-list-y list i) (core:display-list-height list i)) 90.01f0)))))

(define-test painting-the-chart-allocates-nothing ()
  (let ((dashboard (m:make-dashboard))
        (list (core:make-display-list :capacity 4096)))
    (dotimes (step 300) (m:advance-dashboard dashboard step))
    (p:paint-chart list 0f0 0f0 936f0 90f0 dashboard)
    (let ((before (sb-ext:get-bytes-consed)))
      (dotimes (i 2000)
        (core:clear-display-list list)
        (p:paint-chart list 0f0 0f0 936f0 90f0 dashboard))
      (assert-true (< (- (sb-ext:get-bytes-consed) before) 500000)))))
