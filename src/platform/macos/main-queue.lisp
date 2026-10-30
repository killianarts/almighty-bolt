(defpackage #:almighty-bolt/platform/macos/main-queue
  (:use #:cl)
  (:export #:on-main-thread-p
           #:run-on-main
           #:call-on-main))

(in-package #:almighty-bolt/platform/macos/main-queue)

;;; AppKit may only be used from the process's main thread, which is where
;;; the event loop runs. Any other thread (the REPL's, a test driver) hands it
;;; work through Grand Central Dispatch's main queue. The work is a Lisp
;;; function; GCD is given a small integer naming it, and a callable that turns
;;; the integer back into the function and calls it.

(defvar *thunks* (make-hash-table))
(defvar *next-id* 0)
(defvar *lock* (sb-thread:make-mutex :name "main queue"))

(defun on-main-thread-p ()
  "True if the calling thread is the main thread."
  (eq sb-thread:*current-thread* (sb-thread:main-thread)))

(sb-alien:define-alien-callable run-thunk sb-alien:void
    ((context sb-alien:unsigned-long))
  ;; An error must not unwind into GCD's frames, so report it and carry on.
  (let ((thunk (sb-thread:with-mutex (*lock*)
                 (prog1 (gethash context *thunks*)
                   (remhash context *thunks*)))))
    (when thunk
      (handler-case (funcall thunk)
        (error (condition)
          (format *error-output* "~&Error in work run on the main thread: ~a~%" condition)
          (finish-output *error-output*))))))

(defun main-queue-address ()
  (or (sb-sys:find-dynamic-foreign-symbol-address "_dispatch_main_q")
      (error "Cannot find the main dispatch queue.")))

(defun enqueue (thunk synchronously)
  "Give THUNK to the main queue. With SYNCHRONOUSLY, return only after it ran."
  (let ((id (sb-thread:with-mutex (*lock*)
              (let ((id (incf *next-id*)))
                (setf (gethash id *thunks*) thunk)
                id)))
        (queue (main-queue-address))
        (function (sb-sys:sap-int
                   (sb-alien:alien-sap (sb-alien:alien-callable-function 'run-thunk)))))
    (if synchronously
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "dispatch_sync_f"
                                (function sb-alien:void sb-alien:unsigned-long
                                          sb-alien:unsigned-long sb-alien:unsigned-long))
         queue id function)
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "dispatch_async_f"
                                (function sb-alien:void sb-alien:unsigned-long
                                          sb-alien:unsigned-long sb-alien:unsigned-long))
         queue id function))))

(defun run-on-main (thunk)
  "Call THUNK on the main thread, soon. Returns at once, unless the caller is
the main thread, in which case THUNK has run when this returns."
  (if (on-main-thread-p)
      (funcall thunk)
      (enqueue thunk nil))
  nil)

(defun call-on-main (thunk)
  "Call THUNK on the main thread and return its values, waiting for it. Must not
be called from a thread that the main thread is itself waiting for."
  (if (on-main-thread-p)
      (funcall thunk)
      (let ((values nil))
        (enqueue (lambda () (setf values (multiple-value-list (funcall thunk)))) t)
        (values-list values))))
