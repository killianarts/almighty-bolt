(defpackage #:almighty-bolt-7guis/t/acceptance/timer
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)))

(in-package #:almighty-bolt-7guis/t/acceptance/timer)

;;; The timer: a gauge that fills as the elapsed time grows to the duration, the elapsed
;;; time as text, a slider for the duration (it starts at 10 seconds), and Reset. The
;;; time here is a virtual clock, which the tests move. Controls: :gauge, :elapsed,
;;; :duration (the slider), :duration-label and :reset.

(d:define-acceptance the-timer-starts-at-nothing-with-ten-seconds-to-go :timer
  (assert-equal "0.0s" (d:text-in driver :elapsed))
  (assert-equal "10.0s" (d:text-in driver :duration-label)))

(d:define-acceptance time-passing-shows-in-the-elapsed-time :timer
  (d:advance-time driver 2.5d0)
  (assert-equal "2.5s" (d:text-in driver :elapsed))
  (d:advance-time driver 4d0)
  (assert-equal "6.5s" (d:text-in driver :elapsed)))

(d:define-acceptance the-timer-stops-at-the-duration-and-then-asks-for-no-more-frames :timer
  (d:advance-time driver 30d0)
  (assert-equal "10.0s" (d:text-in driver :elapsed))
  (d:frame-requested-p driver)                    ; what the frame asked for until now
  (d:redraw driver)
  (assert-false (d:frame-requested-p driver))
  (d:advance-time driver 5d0)
  (assert-equal "10.0s" (d:text-in driver :elapsed))
  (assert-false (d:frame-requested-p driver)))

(d:define-acceptance a-running-timer-keeps-asking-for-frames :timer
  (d:advance-time driver 1d0)
  (assert-true (d:frame-requested-p driver)))

(d:define-acceptance reset-starts-again-from-nothing :timer
  (d:advance-time driver 4d0)
  (d:click driver :reset)
  (assert-equal "0.0s" (d:text-in driver :elapsed))
  (d:advance-time driver 1.5d0)
  (assert-equal "1.5s" (d:text-in driver :elapsed))
  ;; And again after it has stopped.
  (d:advance-time driver 30d0)
  (assert-equal "10.0s" (d:text-in driver :elapsed))
  (d:click driver :reset)
  (d:advance-time driver 2d0)
  (assert-equal "2.0s" (d:text-in driver :elapsed)))

(d:define-acceptance a-longer-duration-lets-a-stopped-timer-run-again :timer
  (d:advance-time driver 30d0)
  (assert-equal "10.0s" (d:text-in driver :elapsed))
  (d:press-at driver :duration 0.95 0.5)
  (d:release-at driver :duration 0.95 0.5)
  (let ((duration (d:text-in driver :duration-label)))
    (assert-true (string/= "10.0s" duration))
    (d:advance-time driver 2d0)
    (assert-equal "12.0s" (d:text-in driver :elapsed))))

(d:define-acceptance a-shorter-duration-stops-the-timer-where-it-is :timer
  (d:advance-time driver 6d0)
  (d:press-at driver :duration 0.0 0.5)
  (d:release-at driver :duration 0.0 0.5)
  (assert-equal "1.0s" (d:text-in driver :duration-label))
  (d:advance-time driver 3d0)
  (assert-equal "6.0s" (d:text-in driver :elapsed)))
