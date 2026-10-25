(defpackage #:almighty-bolt-7guis/timer/reactive
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
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button
                #:padding
                #:column
                #:row
                #:progress
                #:slider
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant
                #:add-ticker)
  (:export #:make-timer-app))

(in-package #:almighty-bolt-7guis/timer/reactive)

;;; The timer, reactive: the elapsed time and the duration are signals. A ticker, run at the
;;; start of each frame, advances the elapsed time by what passed and says whether the timer
;;; still runs, which is what keeps frames coming. The gauge and the elapsed text are
;;; components of their own, so each tick describes those two again and not the whole screen.

(defun make-timer-app ()
  (let* ((elapsed (make-signal 0d0))
         (duration (make-signal 10d0))
         (last-time nil)
         (variant
           (make-reactive-variant
            (lambda ()
              (padding
               (column (row (label "Elapsed time:")
                            (component (lambda ()
                                         (progress :id :gauge :width 240
                                                   :fraction (timer-fraction
                                                              (make-timer :duration (value duration)
                                                                          :elapsed (value elapsed))))))
                            :gap 12 :cross-align :center)
                       (component (lambda ()
                                    (label (format-seconds (value elapsed)) :id :elapsed)))
                       (row (label "Duration:")
                            (slider :id :duration :minimum 1 :maximum 30 :width 240
                                    :value (value duration)
                                    :on-change (lambda (new) (setf (value duration) new)))
                            (label (format-seconds (value duration)) :id :duration-label)
                            :gap 12 :cross-align :center)
                       (button "Reset" :id :reset
                                       :on-click (lambda ()
                                                   (setf (value elapsed) 0d0
                                                         last-time nil)))
                       :gap 14)
               :all 24)))))
    (flet ((timer ()
             (make-timer :duration (value duration) :elapsed (value elapsed))))
      (add-ticker variant
                  (lambda (now)
                    (cond ((timer-running-p (timer))
                           (when last-time
                             (setf (value elapsed)
                                   (timer-elapsed (timer-advance (timer)
                                                                 (/ (- now last-time) 1d9)))))
                           (setf last-time now))
                          (t (setf last-time nil)))
                    ;; Frames are asked for only while the timer runs.
                    (timer-running-p (timer)))))
    variant))

(register-app :reactive :timer #'make-timer-app :width 460 :height 250)
