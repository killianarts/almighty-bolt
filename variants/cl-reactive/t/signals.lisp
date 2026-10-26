(defpackage #:almighty-bolt-variant-reactive/t/signals
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:s #:almighty-bolt-variant-reactive/signals)))

(in-package #:almighty-bolt-variant-reactive/t/signals)

(defun watcher (thunk)
  "An observer that counts how often it is told, and runs THUNK to read with. Returns the
observer and a function that gives the count."
  (let* ((count 0)
         (observer (s:make-observer (lambda () (incf count)))))
    (s:call-observing observer thunk)
    (values observer (lambda () count))))

(define-test a-signal-holds-and-changes-its-value ()
  (let ((signal (s:make-signal 1)))
    (assert-eql 1 (s:value signal))
    (setf (s:value signal) 2)
    (assert-eql 2 (s:value signal))
    (s:update signal #'1+)
    (assert-eql 3 (s:peek signal))))

(define-test an-observer-is-told-of-a-signal-it-read ()
  (let ((signal (s:make-signal 1)))
    (multiple-value-bind (observer count) (watcher (lambda () (s:value signal)))
      (declare (ignore observer))
      (setf (s:value signal) 2)
      (assert-eql 1 (funcall count))
      (setf (s:value signal) 3)
      (assert-eql 2 (funcall count)))))

(define-test writing-the-same-value-tells-no-one ()
  (let ((signal (s:make-signal (copy-seq "a"))))
    (multiple-value-bind (observer count) (watcher (lambda () (s:value signal)))
      (declare (ignore observer))
      ;; A new string that looks the same is a new value (the test is EQL).
      (setf (s:value signal) (copy-seq "a"))
      (assert-eql 1 (funcall count))
      ;; The very same object, and a number that is the same, are not.
      (setf (s:value signal) (s:peek signal))
      (assert-eql 1 (funcall count))))
  (let ((number (s:make-signal 5)))
    (multiple-value-bind (observer count) (watcher (lambda () (s:value number)))
      (declare (ignore observer))
      (setf (s:value number) 5)
      (assert-eql 0 (funcall count)))))

(define-test a-signal-that-was-not-read-tells-nobody ()
  (let ((read (s:make-signal 1))
        (unread (s:make-signal 1)))
    (multiple-value-bind (observer count) (watcher (lambda () (s:value read)))
      (declare (ignore observer))
      (setf (s:value unread) 2)
      (assert-eql 0 (funcall count)))))

(define-test a-new-run-replaces-what-an-observer-depends-on ()
  (let ((a (s:make-signal 1))
        (b (s:make-signal 1))
        (use-a t))
    (multiple-value-bind (observer count)
        (watcher (lambda () (if use-a (s:value a) (s:value b))))
      (setf (s:value b) 2)
      (assert-eql 0 (funcall count))                ; it reads a, not b
      (setf use-a nil)
      (s:call-observing observer (lambda () (s:value b)))
      (setf (s:value a) 9)
      (assert-eql 0 (funcall count))                ; and now b, not a
      (setf (s:value b) 3)
      (assert-eql 1 (funcall count)))))

(define-test a-disposed-observer-hears-nothing ()
  (let ((signal (s:make-signal 1)))
    (multiple-value-bind (observer count) (watcher (lambda () (s:value signal)))
      (s:dispose-observer observer)
      (setf (s:value signal) 2)
      (assert-eql 0 (funcall count)))))

(define-test untracked-reads-are-not-dependencies ()
  (let ((signal (s:make-signal 1)))
    (multiple-value-bind (observer count) (watcher (lambda () (s:untracked (s:value signal))))
      (declare (ignore observer))
      (setf (s:value signal) 2)
      (assert-eql 0 (funcall count)))))

(define-test a-computed-is-worked-out-when-read-and-kept ()
  (let* ((runs 0)
         (a (s:make-signal 2))
         (double (s:computed (incf runs) (* 2 (s:value a)))))
    (assert-eql 0 runs)
    (assert-eql 4 (s:value double))
    (assert-eql 4 (s:value double))
    (assert-eql 1 runs)
    (setf (s:value a) 5)
    (assert-eql 1 runs)                              ; not worked out until read
    (assert-eql 10 (s:value double))
    (assert-eql 2 runs)))

(define-test a-computed-tells-its-readers-once-it-is-stale ()
  (let* ((a (s:make-signal 1))
         (double (s:computed (* 2 (s:value a)))))
    (multiple-value-bind (observer count) (watcher (lambda () (s:value double)))
      (declare (ignore observer))
      (setf (s:value a) 2)
      (assert-eql 1 (funcall count))
      ;; Stale already, so more writes before it is read tell no one more.
      (setf (s:value a) 3)
      (assert-eql 1 (funcall count))
      (assert-eql 6 (s:value double)))))

(define-test computeds-can-depend-on-computeds ()
  (let* ((a (s:make-signal 1))
         (b (s:computed (+ 1 (s:value a))))
         (c (s:computed (* 10 (s:value b)))))
    (assert-eql 20 (s:value c))
    (setf (s:value a) 4)
    (assert-eql 50 (s:value c))))

;;; --- tracking makes no garbage when an observer reads the same things again ---

(define-test an-observer-that-reads-the-same-signals-again-makes-no-garbage ()
  ;; A component is described again for every frame in which its signals change, and each read
  ;; used to make two conses.
  (let* ((a (s:make-signal 1)) (b (s:make-signal 2)) (c (s:make-signal 3))
         (observer (s:make-observer (lambda ()))))
    (let ((thunk (lambda () (+ (s:value a) (s:value b) (s:value c)))))
     (flet ((run ()
             (s:call-observing observer thunk)))
      (dotimes (i 100) (run))
      (let ((before (sb-ext:get-bytes-consed)))
        (dotimes (i 20000) (run))
        ;; 60,000 reads. Two conses each would be 2 MB.
        (assert-true (< (- (sb-ext:get-bytes-consed) before) 200000)
                     (- (sb-ext:get-bytes-consed) before)))))))

(define-test observers-in-any-place-of-a-signals-list-can-stop-reading-it ()
  ;; The observers of a signal are in the order they began reading; the first, a middle one and
  ;; the last each stop, and the others are still told, and only once.
  (let* ((signal (s:make-signal 0))
         (counts (make-array 4 :initial-element 0))
         (observers (loop :for i :below 4
                          :collect (let ((i i))
                                     (s:make-observer (lambda () (incf (aref counts i))))))))
    (dolist (observer observers)
      (s:call-observing observer (lambda () (s:value signal))))
    (setf (s:value signal) 1)
    (assert-equalp #(1 1 1 1) counts)
    ;; The one that read last is first in the list, then the others in turn.
    (dolist (stopping (list (fourth observers) (second observers) (first observers)))
      (s:call-observing stopping (lambda () nil))
      (fill counts 0)
      (setf (s:value signal) (1+ (s:peek signal)))
      (assert-eql 0 (aref counts (position stopping observers))))
    (assert-equalp #(0 0 1 0) counts)
    ;; An observer that stopped can read it again, and is told.
    (s:call-observing (first observers) (lambda () (s:value signal)))
    (fill counts 0)
    (setf (s:value signal) 100)
    (assert-equalp #(1 0 1 0) counts)))

(define-test a-run-that-reads-other-signals-moves-the-observer-to-them ()
  (let* ((signals (loop :repeat 5 :collect (s:make-signal 0)))
         (told 0)
         (observer (s:make-observer (lambda () (incf told)))))
    ;; Each run reads two of the five, walking along them, so that its spare conses are reused
    ;; for different signals every time.
    (dotimes (run 10)
      (let ((read (list (nth (mod run 5) signals) (nth (mod (+ run 1) 5) signals))))
        (s:call-observing observer (lambda () (dolist (signal read) (s:value signal))))
        (setf told 0)
        (loop :for signal :in signals
              :do (setf (s:value signal) (1+ (s:peek signal))))
        ;; Two of the five were read, so it is told twice.
        (assert-eql 2 told run)))))
