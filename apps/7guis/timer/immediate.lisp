(defpackage #:almighty-bolt-7guis/timer/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/timer/model
                #:make-timer
                #:timer-elapsed
                #:timer-running-p
                #:timer-fraction
                #:timer-advance
                #:format-seconds)
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:label
                #:button
                #:padding
                #:column
                #:row
                #:progress
                #:slider)
  (:import-from #:almighty-bolt-variant-immediate/immediate
                #:make-immediate-variant
                #:now
                #:ask-for-frame)
  (:export #:make-timer-app))

(in-package #:almighty-bolt-7guis/timer/immediate)

;;; The timer, immediate: the elapsed time and the duration are variables, and the function
;;; that describes the screen first lets the time that has passed since it was last called go
;;; by, and asks for another frame if the timer is still running, which is what keeps frames
;;; coming.

(defun make-timer-app ()
  (let ((elapsed 0d0)
        (duration 10d0)
        (last-time nil))
    (flet ((timer ()
             (make-timer :duration duration :elapsed elapsed)))
      (make-immediate-variant
       (lambda ()
         (let ((now (now)))
           (cond ((timer-running-p (timer))
                  (when last-time
                    (setf elapsed (timer-elapsed (timer-advance (timer)
                                                                (/ (- now last-time) 1d9)))))
                  (setf last-time now))
                 (t (setf last-time nil))))
         (when (timer-running-p (timer))
           (ask-for-frame))
         (padding (:all 24)
           (column (:gap 14)
             (row (:gap 12 :cross-align :center)
               (label "Elapsed time:")
               (progress (timer-fraction (timer)) :id :gauge :width 240))
             (label (format-seconds elapsed) :id :elapsed)
             (row (:gap 12 :cross-align :center)
               (label "Duration:")
               (multiple-value-bind (new changed) (slider :duration duration 1 30 :width 240)
                 (when changed
                   (setf duration new)))
               (label (format-seconds duration) :id :duration-label))
             (when (button "Reset" :id :reset)
               (setf elapsed 0d0
                     last-time nil)))))))))

(register-app :immediate :timer #'make-timer-app :width 460 :height 250)
