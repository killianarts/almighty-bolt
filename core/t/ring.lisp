(defpackage #:almighty-bolt-core/t/ring
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:ring #:almighty-bolt-core/ring)
                    (#:clock #:almighty-bolt-core/clock)))

(in-package #:almighty-bolt-core/t/ring)

(defvar *sink* nil
  "Keeps allocations in tests from being optimized away.")

(defun collect-frames (frame-ring)
  (let ((frames '()))
    (ring:map-frames (lambda (start end before after)
                       (push (list start end before after) frames))
                     frame-ring)
    (nreverse frames)))

;;; --- recording ---

(define-test an-empty-ring-holds-nothing ()
  (let ((frame-ring (ring:make-frame-ring :capacity 4)))
    (assert-eql 4 (ring:frame-ring-capacity frame-ring))
    (assert-eql 0 (ring:frame-ring-count frame-ring))
    (assert-eql 0 (ring:frame-ring-total frame-ring))
    (assert-equal '() (collect-frames frame-ring))))

(define-test records-come-back-oldest-first ()
  (let ((frame-ring (ring:make-frame-ring :capacity 4)))
    (ring:record-frame frame-ring 10 20 100 150)
    (ring:record-frame frame-ring 30 45 150 150)
    (assert-eql 2 (ring:frame-ring-count frame-ring))
    (assert-equal '((10 20 100 150) (30 45 150 150)) (collect-frames frame-ring))
    (assert-eql 10 (ring:frame-start-ns frame-ring 0))
    (assert-eql 45 (ring:frame-end-ns frame-ring 1))
    (assert-eql 10 (ring:frame-duration-ns frame-ring 0))
    (assert-eql 15 (ring:frame-duration-ns frame-ring 1))
    (assert-eql 50 (ring:frame-bytes-consed frame-ring 0))
    (assert-eql 0 (ring:frame-bytes-consed frame-ring 1))
    (assert-eql 100 (ring:frame-bytes-consed-before frame-ring 0))
    (assert-eql 150 (ring:frame-bytes-consed-after frame-ring 0))))

(define-test a-full-ring-overwrites-the-oldest-record ()
  (let ((frame-ring (ring:make-frame-ring :capacity 3)))
    (dotimes (i 5)
      (ring:record-frame frame-ring i (+ i 1) 0 0))
    (assert-eql 3 (ring:frame-ring-count frame-ring))
    (assert-eql 5 (ring:frame-ring-total frame-ring))
    (assert-equal '((2 3 0 0) (3 4 0 0) (4 5 0 0)) (collect-frames frame-ring))))

(define-test clearing-forgets-records ()
  (let ((frame-ring (ring:make-frame-ring :capacity 3)))
    (dotimes (i 5)
      (ring:record-frame frame-ring i i 0 0))
    (ring:clear-frame-ring frame-ring)
    (assert-eql 0 (ring:frame-ring-count frame-ring))
    (assert-eql 0 (ring:frame-ring-total frame-ring))
    (ring:record-frame frame-ring 7 8 0 0)
    (assert-equal '((7 8 0 0)) (collect-frames frame-ring))))

(define-test reading-a-missing-record-is-an-error ()
  (let ((frame-ring (ring:make-frame-ring :capacity 3)))
    (ring:record-frame frame-ring 1 2 0 0)
    (assert-error 'error (ring:frame-start-ns frame-ring 1))
    (assert-error 'error (ring:frame-start-ns frame-ring -1))))

(define-test the-capacity-must-be-positive ()
  (assert-error 'type-error (ring:make-frame-ring :capacity 0)))

;;; --- recording a frame around code ---

(define-test with-recorded-frame-times-and-counts-the-body ()
  (let ((frame-ring (ring:make-frame-ring :capacity 4)))
    (ring:with-recorded-frame (frame-ring)
      (sleep 0.02)
      ;; Large objects are charged as soon as they are allocated.
      (setf *sink* (make-array 200000 :element-type '(unsigned-byte 64))))
    (assert-eql 1 (ring:frame-ring-count frame-ring))
    (assert-true (>= (ring:frame-duration-ns frame-ring 0) 18000000))
    (assert-true (>= (ring:frame-bytes-consed frame-ring 0) 1600000))))

(define-test with-recorded-frame-returns-the-values-of-the-body ()
  (let ((frame-ring (ring:make-frame-ring :capacity 2)))
    (assert-equal '(1 2 3)
                  (multiple-value-list
                   (ring:with-recorded-frame (frame-ring) (values 1 2 3))))))

(define-test with-recorded-frame-uses-the-core-clock ()
  (let ((frame-ring (ring:make-frame-ring :capacity 2))
        (virtual (clock:make-virtual-clock 5000)))
    (clock:with-time-source ((clock:virtual-clock-source virtual))
      (ring:with-recorded-frame (frame-ring)
        (clock:advance-virtual-clock virtual 250)))
    (assert-eql 5000 (ring:frame-start-ns frame-ring 0))
    (assert-eql 5250 (ring:frame-end-ns frame-ring 0))))

;;; --- no allocation ---

(define-test recording-does-not-allocate ()
  (let ((frame-ring (ring:make-frame-ring :capacity 1024))
        (before (sb-ext:get-bytes-consed)))
    (dotimes (i 500000)
      (ring:record-frame frame-ring i (+ i 10) 100 200))
    (assert-true (< (- (sb-ext:get-bytes-consed) before) 1000000))))

(define-test with-recorded-frame-does-not-allocate ()
  (let ((frame-ring (ring:make-frame-ring :capacity 1024))
        (before (sb-ext:get-bytes-consed)))
    (dotimes (i 200000)
      (ring:with-recorded-frame (frame-ring)
        nil))
    (assert-true (< (- (sb-ext:get-bytes-consed) before) 1000000))))
