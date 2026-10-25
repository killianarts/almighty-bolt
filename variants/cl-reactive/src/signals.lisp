(defpackage #:almighty-bolt-variant-reactive/signals
  (:use #:cl)
  (:shadow #:signal)
  (:export #:signal
           #:make-signal
           #:value
           #:update
           #:peek
           #:computed
           #:make-computed
           #:observer
           #:make-observer
           #:observer-sources
           #:call-observing
           #:dispose-observer
           #:untracked))

(in-package #:almighty-bolt-variant-reactive/signals)

;;; Reactive state. A SIGNAL holds a value. Reading it with VALUE inside something that is
;;; being observed records that the observer depends on it; writing it with (SETF VALUE)
;;; tells every observer that depended on it, unless the new value is EQL to the old one.
;;; An OBSERVER is what is told: it has a function to call when a source it read changes,
;;; and the sources it read in its last run, which a new run replaces. A COMPUTED is a
;;; value worked out from other signals, kept until one of them changes, and then worked
;;; out again when it is next read: it is a source to its readers and an observer of what
;;; it reads. Nothing is worked out when a signal is written; observers are only told,
;;; and the variant decides when to run them (before the next frame).

(defvar *observer* nil
  "The observer that is running, whose reads are recorded, or NIL.")

(defstruct (observer (:constructor make-observer (on-change)) (:copier nil))
  (on-change nil :type function :read-only t)
  (sources '() :type list)
  (disposed nil :type boolean))

(defstruct (source (:constructor nil) (:copier nil))
  (observers '() :type list))

(defstruct (signal (:include source) (:constructor %make-signal (%value)) (:copier nil))
  (%value nil))

(defun make-signal (initial)
  "A signal holding INITIAL."
  (%make-signal initial))

(defun track (source)
  (let ((observer *observer*))
    (when (and observer (not (observer-disposed observer)))
      (pushnew source (observer-sources observer) :test #'eq)
      (pushnew observer (source-observers source) :test #'eq))))

(defun notify (source)
  "Tell every observer of SOURCE that it changed."
  (dolist (observer (copy-list (source-observers source)))
    (unless (observer-disposed observer)
      (funcall (observer-on-change observer)))))

(defgeneric value (source)
  (:documentation "The value of a signal or a computed, which is recorded as a read by
whatever observer is running."))

(defmethod value ((signal signal))
  (track signal)
  (signal-%value signal))

(defun (setf value) (new signal)
  "Put NEW in SIGNAL and tell its observers, unless it is the value it already has."
  (unless (eql new (signal-%value signal))
    (setf (signal-%value signal) new)
    (notify signal))
  new)

(defun peek (signal)
  "The value of SIGNAL without recording a read."
  (signal-%value signal))

(defun update (signal function)
  "Replace the value of SIGNAL by the result of calling FUNCTION on it."
  (setf (value signal) (funcall function (signal-%value signal))))

(defun forget-sources (observer)
  (dolist (source (observer-sources observer))
    (setf (source-observers source) (delete observer (source-observers source) :test #'eq)))
  (setf (observer-sources observer) '()))

(defun call-observing (observer thunk)
  "Call THUNK, recording what it reads as the sources of OBSERVER, in place of those of
its last run. Returns what THUNK returns."
  (forget-sources observer)
  (let ((*observer* observer))
    (funcall thunk)))

(defun dispose-observer (observer)
  "Stop OBSERVER from being told anything again."
  (forget-sources observer)
  (setf (observer-disposed observer) t))

(defmacro untracked (&body body)
  "Run BODY without recording what it reads."
  `(let ((*observer* nil))
     ,@body))

;;; --- computed values ---

(defstruct (computed (:include source) (:constructor %make-computed (thunk))
                     (:copier nil))
  (thunk nil :type function :read-only t)
  (%value nil)
  (stale t :type boolean)
  (observer nil))

(defun make-computed (thunk)
  "A value worked out by calling THUNK, kept until something THUNK read changes, and then
worked out again the next time it is read."
  (let ((computed (%make-computed thunk)))
    (setf (computed-observer computed)
          (make-observer (lambda ()
                           (unless (computed-stale computed)
                             (setf (computed-stale computed) t)
                             (notify computed)))))
    computed))

(defmacro computed (&body body)
  `(make-computed (lambda () ,@body)))

(defmethod value ((computed computed))
  (track computed)
  (when (computed-stale computed)
    (setf (computed-%value computed)
          (call-observing (computed-observer computed) (computed-thunk computed))
          (computed-stale computed) nil))
  (computed-%value computed))
