(defpackage #:almighty-bolt-core/t/redefinition
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:redefinition #:almighty-bolt-core/redefinition)))

(in-package #:almighty-bolt-core/t/redefinition)

;;; The definitions below are made with EVAL, as an editor's compile of one form makes them.

(defun define (form)
  "EVAL the definition FORM. A function defined twice is the point of these tests, so SBCL's
warning that it was is not shown."
  (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
    (eval form)))

(defun wait-until (predicate &optional (seconds 3))
  "True once PREDICATE is, checking every few milliseconds, false after SECONDS."
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop :until (funcall predicate)
          :do (when (> (get-internal-real-time) end)
                (return-from wait-until nil))
              (sleep 0.005))
    t))

(defmacro with-watcher ((calls &key (quiet 0.05)) &body body)
  "Run BODY with CALLS a list in a cons that holds the time of each call of a watcher, newest
first, and the watcher stopped afterwards."
  `(let* ((,calls (list nil))
          (watcher (redefinition:watch-redefinitions
                    (lambda () (push (get-internal-real-time) (car ,calls)))
                    :quiet ,quiet)))
     (unwind-protect (progn ,@body)
       (redefinition:stop-watching-redefinitions watcher))))

(define-test a-new-definition-calls-the-function-once ()
  (with-watcher (calls)
    (define '(defun redefinition-test-function-1 () 1))
    (assert-true (wait-until (lambda () (car calls))))
    (sleep 0.2)
    (assert-eql 1 (length (car calls)))))

(define-test a-redefinition-calls-it-again ()
  (with-watcher (calls)
    (define '(defun redefinition-test-function-2 () 1))
    (assert-true (wait-until (lambda () (= 1 (length (car calls))))))
    (define '(defun redefinition-test-function-2 () 2))
    (assert-true (wait-until (lambda () (= 2 (length (car calls))))))))

(define-test a-burst-of-definitions-is-one-call ()
  ;; A compiled file defines its functions one after the other. The calls are made while the
  ;; definitions go on, and the quiet time is the least that may pass between the last of them
  ;; and the call, so one call that comes after it.
  (with-watcher (calls :quiet 0.15)
    (let ((last nil))
      (dotimes (i 6)
        (define `(defun ,(intern (format nil "REDEFINITION-TEST-BURST-~d" i)) () ,i))
        (setf last (get-internal-real-time))
        (sleep 0.02))
      (assert-true (wait-until (lambda () (car calls))))
      (sleep 0.4)
      (assert-eql 1 (length (car calls)))
      (assert-true (>= (- (first (car calls)) last)
                       (* 0.1 internal-time-units-per-second))))))

(define-test the-call-waits-for-the-quiet-time ()
  (with-watcher (calls :quiet 0.3)
    (let ((start (get-internal-real-time)))
      (define '(defun redefinition-test-function-3 () 1))
      (assert-true (wait-until (lambda () (car calls))))
      (assert-true (>= (- (first (car calls)) start)
                       (* 0.25 internal-time-units-per-second))))))

(define-test a-redefined-method-calls-the-function ()
  (define '(defgeneric redefinition-test-generic (x)))
  (with-watcher (calls)
    (define '(defmethod redefinition-test-generic ((x integer)) 1))
    (assert-true (wait-until (lambda () (car calls))))))

(define-test a-redefined-setf-function-calls-the-function ()
  (with-watcher (calls)
    (define '(defun (setf redefinition-test-place) (value) value))
    (assert-true (wait-until (lambda () (car calls))))))

(define-test sbcl-definitions-do-not-call-the-function ()
  (with-watcher (calls)
    (sb-ext:without-package-locks
      (setf (fdefinition (intern "REDEFINITION-TEST-NAME" "SB-IMPL")) (lambda ())))
    (sleep 0.3)
    (assert-false (car calls))
    ;; The watch goes on: the program's own definition is seen.
    (define '(defun redefinition-test-function-7 () 1))
    (assert-true (wait-until (lambda () (car calls))))))

(define-test a-stopped-watcher-is-not-called ()
  (let ((calls (list nil)))
    (let ((watcher (redefinition:watch-redefinitions (lambda () (push t (car calls)))
                                                      :quiet 0.02)))
      (redefinition:stop-watching-redefinitions watcher))
    (define '(defun redefinition-test-function-4 () 1))
    (sleep 0.3)
    (assert-false (car calls))))

(define-test stopping-takes-the-hook-out ()
  (let ((before (length sb-int:*setf-fdefinition-hook*))
        (watcher (redefinition:watch-redefinitions (lambda ()))))
    (assert-eql (1+ before) (length sb-int:*setf-fdefinition-hook*))
    (redefinition:stop-watching-redefinitions watcher)
    (assert-eql before (length sb-int:*setf-fdefinition-hook*))))

(define-test an-error-in-the-function-does-not-end-the-watch ()
  (let* ((calls 0)
         (watcher (redefinition:watch-redefinitions
                   (lambda ()
                     (incf calls)
                     (error "the first call fails"))
                   :quiet 0.03))
         (*error-output* (make-broadcast-stream)))
    (unwind-protect
         (progn
           (define '(defun redefinition-test-function-5 () 1))
           (assert-true (wait-until (lambda () (= 1 calls))))
           (define '(defun redefinition-test-function-5 () 2))
           (assert-true (wait-until (lambda () (= 2 calls)))))
      (redefinition:stop-watching-redefinitions watcher))))

(define-test a-watcher-may-stop-itself ()
  (let* ((watcher nil)
         (done nil))
    (setf watcher (redefinition:watch-redefinitions
                   (lambda ()
                     (redefinition:stop-watching-redefinitions watcher)
                     (setf done t))
                   :quiet 0.02))
    (define '(defun redefinition-test-function-6 () 1))
    (assert-true (wait-until (lambda () done)))))

;;; --- what counts ---

(define-test sbcl-names-do-not-count ()
  (assert-true (redefinition:sbcl-name-p 'sb-impl::%defun))
  (assert-true (redefinition:sbcl-name-p '(setf sb-impl::%defun)))
  (assert-true (redefinition:sbcl-name-p (list 'sb-pcl::fast-method 'sb-impl::%defun '(t))))
  (assert-true (redefinition:sbcl-name-p (list 'sb-pcl::slow-method 'sb-kernel::%fun-name
                                               '(t t)))))

(define-test the-programs-names-count ()
  (assert-false (redefinition:sbcl-name-p 'redefinition-test-function-1))
  (assert-false (redefinition:sbcl-name-p '(setf redefinition-test-place)))
  (assert-false (redefinition:sbcl-name-p (list 'sb-pcl::fast-method 'redefinition-test-generic
                                                '(integer))))
  (assert-false (redefinition:sbcl-name-p (list 'sb-pcl::slow-method 'redefinition-test-generic
                                                :before '(integer))))
  (assert-false (redefinition:sbcl-name-p (make-symbol "UNINTERNED")))
  (assert-false (redefinition:sbcl-name-p 'print-object)))
