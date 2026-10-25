(defpackage #:almighty-bolt-core/t/wheel-lock
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:event #:almighty-bolt-core/event)
                    (#:lock #:almighty-bolt-core/wheel-lock)))

(in-package #:almighty-bolt-core/t/wheel-lock)

;;; Locking a trackpad gesture to one axis. The deltas are in points and the answers in lines,
;;; 24 points to a line. The swipes are the ones Micah's trackpad sent (A-004).

(defconstant +ms+ 1000000)

(defun swipe (lock start steps &key (precise t))
  "Feed LOCK the STEPS, each (dx dy), 10 ms apart from START ms, and return the answers as a
list of (dx dy) in lines."
  (loop :for (dx dy) :in steps
        :for ms :from start :by 10
        :collect (multiple-value-list
                  (lock:lock-wheel lock (event:make-wheel-event
                                         :timestamp (* ms +ms+) :dx (float dx 1d0)
                                         :dy (float dy 1d0) :precise-p precise)))))

(defun column (answers index)
  (mapcar (lambda (answer) (nth index answer)) answers))

(define-test a-calm-horizontal-swipe-with-a-downward-drift-moves-across-only ()
  (let ((answers (swipe (lock:make-axis-lock) 0
                        '((-1 0) (-2 -1) (-3 -2) (-3 -3) (-4 -3) (-6 -5) (-10 -5) (-18 -4)))))
    (assert-true (every #'zerop (column answers 1)))
    (assert-equal (mapcar (lambda (x) (/ x 24d0)) '(-1 -2 -3 -3 -4 -6 -10 -18))
                  (column answers 0))))

(define-test a-vertical-swipe-with-a-sideways-drift-moves-down-only ()
  (let ((answers (swipe (lock:make-axis-lock) 0 '((0 1) (1 2) (0 2) (-2 3) (0 3) (-1 4) (0 2)))))
    (assert-true (every #'zerop (column answers 0)))
    (assert-equal (mapcar (lambda (y) (/ y 24d0)) '(1 2 2 3 3 4 2)) (column answers 1))))

(define-test the-first-small-movements-go-the-way-they-lead-before-the-axis-is-chosen ()
  ;; 1 and 0 add up to 1 across and nothing down: across. Then 0 and 5 make down the larger,
  ;; and from then on only down counts, though the 1 across was already given.
  (let ((answers (swipe (lock:make-axis-lock) 0 '((1 0) (0 5) (2 1)))))
    (assert-equal '(1d0 0d0)
                  (list (* 24 (first (first answers))) (second (first answers))))
    (assert-equal '(0d0 5d0)
                  (list (first (second answers)) (* 24 (second (second answers)))))
    (assert-eql 0d0 (first (third answers)))))

(define-test a-pause-ends-the-gesture-and-the-next-one-chooses-again ()
  (let ((lock (lock:make-axis-lock)))
    (assert-true (every #'zerop (column (swipe lock 0 '((-5 0) (-5 0) (-5 1))) 1)))
    ;; The next swipe starts 1 s later and goes down.
    (let ((answers (swipe lock 1000 '((0 5) (1 5) (1 5)))))
      (assert-true (every #'zerop (column answers 0)))
      (assert-false (every #'zerop (column answers 1))))))

(define-test the-axis-stays-through-the-momentum-that-follows-a-flick ()
  (let ((answers (swipe (lock:make-axis-lock) 0
                        '((0 20) (0 18) (3 15) (4 12) (3 9) (2 6) (1 3) (1 2)))))
    (assert-true (every #'zerop (column answers 0)))))

(define-test a-wheels-lines-pass-through-with-both-axes ()
  (let ((answers (swipe (lock:make-axis-lock) 0 '((2 3) (1 -4)) :precise nil)))
    (assert-equal '((2d0 3d0) (1d0 -4d0)) answers)))
