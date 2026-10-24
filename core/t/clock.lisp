(defpackage #:almighty-bolt-core/t/clock
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:clock #:almighty-bolt-core/clock)))

(in-package #:almighty-bolt-core/t/clock)

;;; --- real clock ---

(define-test now-ns-is-positive-and-never-goes-backwards ()
  (let ((previous (clock:now-ns)))
    (assert-true (plusp previous))
    (dotimes (i 10000)
      (let ((now (clock:now-ns)))
        (assert-true (>= now previous))
        (setf previous now)))))

(define-test now-ns-measures-a-sleep-in-nanoseconds ()
  (let* ((start (clock:now-ns))
         (elapsed (progn (sleep 0.05) (- (clock:now-ns) start))))
    ;; A 50 ms sleep: at least about 50 ms, and well under a second.
    (assert-true (>= elapsed 45000000))
    (assert-true (< elapsed 1000000000))))

(define-test now-ns-does-not-allocate ()
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i 200000)
      (clock:now-ns))
    ;; One call that allocated even 16 bytes would add 3.2 MB.
    (assert-true (< (- (sb-ext:get-bytes-consed) before) 1000000))))

;;; --- virtual clock ---

(define-test with-time-source-replaces-the-core-clock ()
  (let ((virtual (clock:make-virtual-clock 1000)))
    (clock:with-time-source ((clock:virtual-clock-source virtual))
      (assert-eql 1000 (clock:now-ns))
      (assert-eql 1500 (clock:advance-virtual-clock virtual 500))
      (assert-eql 1500 (clock:now-ns)))
    ;; Outside the form the real clock is back.
    (assert-true (> (clock:now-ns) 1500))))

(define-test virtual-clock-only-moves-when-advanced ()
  (let ((virtual (clock:make-virtual-clock)))
    (clock:with-time-source ((clock:virtual-clock-source virtual))
      (assert-eql 0 (clock:now-ns))
      (sleep 0.01)
      (assert-eql 0 (clock:now-ns)))))

(define-test advance-virtual-clock-rejects-negative-steps ()
  (assert-error 'type-error
                (clock:advance-virtual-clock (clock:make-virtual-clock) -1)))
