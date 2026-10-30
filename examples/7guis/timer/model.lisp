(defpackage #:almighty-bolt-7guis/timer/model
  (:use #:cl)
  (:export #:timer
           #:make-timer
           #:timer-duration
           #:timer-elapsed
           #:timer-running-p
           #:timer-fraction
           #:timer-advance
           #:timer-reset
           #:timer-with-duration
           #:format-seconds))

(in-package #:almighty-bolt-7guis/timer/model)

;;; The timer's logic: an elapsed time that grows up to a duration. Lowering the duration
;;; below the elapsed time stops it with the gauge full; raising it starts it again.

(defstruct (timer (:constructor make-timer (&key (duration 10d0) (elapsed 0d0)))
                  (:copier nil))
  (duration 10d0 :type double-float :read-only t)
  (elapsed 0d0 :type double-float :read-only t))

(defun timer-running-p (timer)
  "True while the elapsed time is short of the duration."
  (< (timer-elapsed timer) (timer-duration timer)))

(defun timer-fraction (timer)
  "How full the gauge is, from 0 to 1."
  (if (plusp (timer-duration timer))
      (min 1d0 (/ (timer-elapsed timer) (timer-duration timer)))
      1d0))

(defun timer-advance (timer seconds)
  "TIMER after SECONDS have passed, which only changes it while it runs, and never takes
it past its duration. Returns TIMER itself if nothing changed."
  (if (and (timer-running-p timer) (plusp seconds))
      (make-timer :duration (timer-duration timer)
                  :elapsed (min (timer-duration timer) (+ (timer-elapsed timer) seconds)))
      timer))

(defun timer-reset (timer)
  (if (zerop (timer-elapsed timer))
      timer
      (make-timer :duration (timer-duration timer) :elapsed 0d0)))

(defun timer-with-duration (timer duration)
  (if (= duration (timer-duration timer))
      timer
      (make-timer :duration (float duration 1d0) :elapsed (timer-elapsed timer))))

(defun format-seconds (seconds)
  "SECONDS to one decimal place, as the task shows the elapsed time: 3.4s."
  (let ((tenths (floor (+ (* seconds 10) 1/2))))
    (format nil "~d.~ds" (floor tenths 10) (mod tenths 10))))
