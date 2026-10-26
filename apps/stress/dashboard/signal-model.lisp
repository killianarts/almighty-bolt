(defpackage #:almighty-bolt-stress/dashboard/signal-model
  (:use #:cl)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+tiles+
                #:value-index
                #:make-dashboard
                #:advance-dashboard)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:computed
                #:value)
  (:export #:make-signal-dashboard
           #:signal-dashboard-model
           #:current-step
           #:tile-index
           #:advance-signal-dashboard))

(in-package #:almighty-bolt-stress/dashboard/signal-model)

;;; The dashboard's data made of signals, for the reactive variant (the experiment of
;;; notes/builder-macros.org). What the screen is at is one signal, the step; the value of a tile is a
;;; computed of it, worked out by the same function that the shared model uses, so that a step is
;;; written once and every tile that reads it is told. The history that the chart draws is not a
;;; value that a signal can hold, so the shared model still keeps it, and is brought to a step
;;; before the signal is.

(defstruct (signal-dashboard (:constructor %make-signal-dashboard (step model indices)))
  step
  (model nil :read-only t)
  (indices nil :type simple-vector :read-only t))

(defun make-signal-dashboard ()
  (let ((step (make-signal 0))
        (model (make-dashboard))
        (indices (make-array +tiles+)))
    (advance-dashboard model 0)
    (dotimes (tile +tiles+)
      (let ((tile tile))
        (setf (svref indices tile) (computed (value-index tile (value step))))))
    (%make-signal-dashboard step model indices)))

(defun current-step (dashboard)
  "The step the screen is at; reading it makes the reader depend on it."
  (value (signal-dashboard-step dashboard)))

(defun tile-index (dashboard tile)
  "The value of TILE in tenths, which the reader depends on."
  (value (svref (signal-dashboard-indices dashboard) tile)))

(defun advance-signal-dashboard (dashboard step)
  "Bring DASHBOARD to STEP: the history first, and then the signal. Returns true if it was a new
step, and false if it was there already (a frame can be built more than once)."
  (when (advance-dashboard (signal-dashboard-model dashboard) step)
    (setf (value (signal-dashboard-step dashboard)) step)
    t))
