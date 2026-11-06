(defpackage #:almighty-bolt-7guis/timer/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-7guis/timer/model)
                    (#:bolt #:almighty-bolt))
  (:export #:make-timer-app))

(in-package #:almighty-bolt-7guis/timer/app)

;;; The timer, immediate: the elapsed time and the duration are variables, and the function
;;; that describes the screen first lets the time that has passed since it was last called go
;;; by, and asks for another frame if the timer is still running, which is what keeps frames
;;; coming.

(defun make-timer-app ()
  (bolt:state-let ((elapsed 0d0)
                   (duration 10d0)
                   (last-time nil))
    (flet ((timer ()
             (model:make-timer :duration duration :elapsed elapsed)))
      (lambda ()
        (let ((now (bolt:now)))
          (cond ((model:timer-running-p (timer))
                 (when last-time
                   (setf elapsed (model:timer-elapsed (model:timer-advance (timer)
                                                                           (/ (- now last-time) 1d9)))))
                 (setf last-time now))
                (t (setf last-time nil))))
        (when (model:timer-running-p (timer))
          (bolt:ask-for-frame))
        (bolt:padding (:all 24)
          (bolt:column (:gap 14)
            (bolt:row (:gap 12 :cross-align :center)
              (bolt:label "Elapsed time:" :id :elapsed-caption)
              (bolt:progress (model:timer-fraction (timer))
                             :id :gauge :labelled-by :elapsed-caption :width 240))
            (bolt:label (model:format-seconds elapsed) :id :elapsed)
            (bolt:row (:gap 12 :cross-align :center)
              (bolt:label "Duration:" :id :duration-caption)
              (multiple-value-bind (new changed)
                  (bolt:slider duration 1 30 :id :duration :labelled-by :duration-caption :width 240)
                (when changed
                  (setf duration new)))
              (bolt:label (model:format-seconds duration) :id :duration-label))
            (when (bolt:button "Reset" :id :reset)
              (setf elapsed 0d0
                    last-time nil))))))))

(registry:register-app :timer 'make-timer-app :width 460 :height 250)
