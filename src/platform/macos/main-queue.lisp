(defpackage #:almighty-bolt/platform/macos/main-queue
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:selector
                #:define-callback
                #:define-runtime-class
                #:define-startup)
  (:export #:on-main-thread-p
           #:run-on-main
           #:run-on-main-later
           #:run-on-main-in-a-turn
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

(defun run-on-main-later (thunk)
  "Call THUNK on the main thread after what it is doing now, even if the caller is the main thread:
for work that must not run inside a frame (a panel that runs a loop of its own). Returns at once."
  (enqueue thunk nil)
  nil)

;;; Work that runs a loop of its own (the system's file panels, which wait for the person) must not
;;; run from the main queue: the queue is serial, and nothing else given to it would run until the
;;; loop ended, so the REPL and an agent could do nothing in the window meanwhile. It is started from
;;; a timer of the main thread's run loop instead, and the loop it runs then goes on serving the queue.

(define-callback later-run sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (number sb-alien:unsigned-long))
  (let* ((id (msg-send :long number "longValue"))
         (thunk (sb-thread:with-mutex (*lock*)
                  (prog1 (gethash id *thunks*)
                    (remhash id *thunks*)))))
    (when thunk
      (funcall thunk))))

(define-runtime-class *later-class* "AlmightyBoltLater" "NSObject"
  :methods '(("run:" "v@:@" later-run)))

(defvar *later* nil "The object whose run: the timers call.")

(define-startup later-object
  (setf *later* (msg-send :id (msg-send :id *later-class* "alloc") "init")))

(defun run-on-main-in-a-turn (thunk)
  "Call THUNK on the main thread at the next turn of its run loop, from a timer and not from the
main queue, so that the main queue goes on being served while THUNK runs a loop of its own (a modal
panel). Call on the main thread; returns at once."
  (let ((id (sb-thread:with-mutex (*lock*)
              (let ((id (incf *next-id*)))
                (setf (gethash id *thunks*) thunk)
                id))))
    (msg-send :void *later* "performSelector:withObject:afterDelay:"
              (:ulong (selector "run:"))
              (:id (msg-send :id (objc-class "NSNumber") "numberWithLong:" (:long id)))
              (:double 0d0))
    nil))

(defun call-on-main (thunk)
  "Call THUNK on the main thread and return its values, waiting for it. Must not
be called from a thread that the main thread is itself waiting for."
  (if (on-main-thread-p)
      (funcall thunk)
      (let ((values nil))
        (enqueue (lambda () (setf values (multiple-value-list (funcall thunk)))) t)
        (values-list values))))
