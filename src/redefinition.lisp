(defpackage #:almighty-bolt/redefinition
  (:use #:cl)
  (:export #:watch-redefinitions
           #:stop-watching-redefinitions
           #:call-with-redefinitions-held
           #:with-redefinitions-held
           #:sbcl-name-p))

(in-package #:almighty-bolt/redefinition)

;;; Telling a running program that its code changed. SBCL calls the
;;; functions in SB-INT:*SETF-FDEFINITION-HOOK* with the name whenever a function is defined,
;;; whether the definition comes from a compiled file, from COMPILE or EVAL, or from an
;;; editor's compile of one form, so no polling is needed and no file is watched. This file is
;;; the only one that knows about the hook.
;;;
;;; A hook is called once for each function, and a compiled file or a system defines many, so a
;;; watcher waits until the definitions stop for a moment and then calls its function once. A
;;; system that is loaded again compiles one file while it defines nothing, which can be longer than
;;; that moment, so the code that loads it holds the watchers until it is done (WITH-REDEFINITIONS-
;;; HELD), and they call their functions once after.

(defstruct (watcher (:constructor %make-watcher (function quiet)) (:copier nil))
  (function nil :type function :read-only t)
  ;; How long the definitions must be quiet before FUNCTION is called, in internal time units.
  (quiet 0 :type (integer 0) :read-only t)
  (lock (sb-thread:make-mutex :name "redefinition watcher") :read-only t)
  (queue (sb-thread:make-waitqueue) :read-only t)
  ;; When the latest definition that FUNCTION has not been told of was made, or NIL.
  (changed nil)
  (stopped nil)
  (hook nil)
  (thread nil))

(defvar *hook-lock* (sb-thread:make-mutex :name "redefinition hooks")
  "Guards the list of hooks, which a watcher starting and one stopping both change, and the
number of holds.")

(defvar *holds* 0
  "How many calls of WITH-REDEFINITIONS-HELD are in progress, on any thread.")

(defun call-with-redefinitions-held (function)
  "Call FUNCTION. While it runs no watcher calls its function, however long the definitions
are quiet; after it, a watcher that saw a definition calls its function once."
  (sb-thread:with-mutex (*hook-lock*)
    (incf *holds*))
  (unwind-protect (funcall function)
    (sb-thread:with-mutex (*hook-lock*)
      (decf *holds*))))

(defmacro with-redefinitions-held (&body body)
  "Run BODY (CALL-WITH-REDEFINITIONS-HELD): the code that loads many files and does not want
the app made again from half of them."
  `(call-with-redefinitions-held (lambda () ,@body)))

(defun sbcl-name-p (name)
  "True for the name of something SBCL defines for itself: a symbol of one of its SB-
packages, or the method of one of its generic functions. SBCL defines such functions while
a program runs, as it first calls a generic function, and none of them is the program's code."
  (typecase name
    (symbol (let ((package (symbol-package name)))
              (and package (eql 0 (search "SB-" (package-name package))))))
    (cons (cond ((eq (car name) 'setf) (sbcl-name-p (second name)))
                ((not (and (symbolp (car name)) (sbcl-name-p (car name)))) nil)
                ;; (sb-pcl::fast-method foo ...) is a method of FOO, which is the program's
                ;; if FOO is.
                ((member (symbol-name (car name)) '("FAST-METHOD" "SLOW-METHOD") :test #'string=)
                 (sbcl-name-p (second name)))
                (t t)))
    (t t)))

(defun note-definition (watcher)
  (sb-thread:with-mutex ((watcher-lock watcher))
    (setf (watcher-changed watcher) (get-internal-real-time))
    (sb-thread:condition-broadcast (watcher-queue watcher))))

(defun wait-until-quiet (watcher)
  "Wait for a definition and then for the quiet time after the last one. True when it has
passed, false if the watcher was stopped first."
  (let ((lock (watcher-lock watcher))
        (queue (watcher-queue watcher)))
    (loop
      (let ((remaining
              (sb-thread:with-mutex (lock)
                (loop :until (or (watcher-stopped watcher) (watcher-changed watcher))
                      :do (sb-thread:condition-wait queue lock))
                (when (watcher-stopped watcher)
                  (return-from wait-until-quiet nil))
                (let ((remaining (- (+ (watcher-changed watcher) (watcher-quiet watcher))
                                    (get-internal-real-time))))
                  ;; A hold is not over, so look again soon.
                  (when (and (<= remaining 0) (plusp *holds*))
                    (setf remaining (round (* 0.05 internal-time-units-per-second))))
                  ;; Forgetting the change in the same step that finds it quiet means that a
                  ;; definition made a moment later is a new one, not lost.
                  (when (<= remaining 0)
                    (setf (watcher-changed watcher) nil))
                  remaining))))
        (when (<= remaining 0)
          (return t))
        (sleep (/ remaining internal-time-units-per-second))))))

(defun watch (watcher)
  (loop :while (wait-until-quiet watcher)
        :do (handler-case (funcall (watcher-function watcher))
              (error (condition)
                (let ((*print-pretty* nil))
                  (format *error-output* "~&A function that watches definitions failed: ~a~%"
                          condition))
                (finish-output *error-output*)))))

(defun watch-redefinitions (function &key (quiet 0.15))
  "Call FUNCTION, with no arguments, on a thread of its own, once after functions are defined
or redefined and none has been for QUIET seconds. Returns the watcher, for
STOP-WATCHING-REDEFINITIONS. SBCL's own definitions do not count (SBCL-NAME-P)."
  (let* ((watcher (%make-watcher function (round (* quiet internal-time-units-per-second))))
         (hook (lambda (name new-definition)
                 (declare (ignore new-definition))
                 (unless (sbcl-name-p name)
                   (note-definition watcher)))))
    (setf (watcher-hook watcher) hook
          (watcher-thread watcher) (sb-thread:make-thread #'watch :arguments (list watcher)
                                                                  :name "redefinition watcher"))
    (sb-thread:with-mutex (*hook-lock*)
      (push hook sb-int:*setf-fdefinition-hook*))
    watcher))

(defun stop-watching-redefinitions (watcher)
  "Stop WATCHER: its function is not called again, and the thread ends."
  (sb-thread:with-mutex (*hook-lock*)
    (setf sb-int:*setf-fdefinition-hook*
          (remove (watcher-hook watcher) sb-int:*setf-fdefinition-hook*)))
  (sb-thread:with-mutex ((watcher-lock watcher))
    (setf (watcher-stopped watcher) t)
    (sb-thread:condition-broadcast (watcher-queue watcher)))
  ;; The function may be the one that stops it, and a thread cannot wait for itself.
  (unless (eq (watcher-thread watcher) sb-thread:*current-thread*)
    (sb-thread:join-thread (watcher-thread watcher) :default nil :timeout 2))
  nil)
