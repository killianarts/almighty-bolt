(defpackage #:almighty-bolt-7guis/timer/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/timer/model
                #:make-timer
                #:timer-duration
                #:timer-elapsed
                #:timer-running-p
                #:timer-fraction
                #:timer-advance
                #:timer-reset
                #:timer-with-duration
                #:format-seconds)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:button
                #:progress
                #:progress-fraction)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:slider)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant
                #:add-ticker)
  (:export #:make-timer-app))

(in-package #:almighty-bolt-7guis/timer/retained)

;;; The timer, retained: the model is a value held in a variable, the widgets are built
;;; once, and a ticker (run at the start of every frame, and keeping frames coming while it
;;; says the timer runs) advances the model by the time that passed and writes it into the
;;; gauge and the label. The slider and the button replace the model's value and write it
;;; into the widgets as well.

(defun set-label-text (label text)
  (unless (string= text (label-text label))
    (setf (label-text label) text)))

(defun make-timer-app ()
  (let ((timer (make-timer :duration 10d0))
        (last-time nil)
        (gauge nil) (elapsed nil) (duration nil) (slider nil) (reset nil))
    (labels ((show ()
               (setf (progress-fraction gauge) (timer-fraction timer))
               (set-label-text elapsed (format-seconds (timer-elapsed timer)))
               (set-label-text duration (format-seconds (timer-duration timer))))
             (change-duration (value)
               (setf timer (timer-with-duration timer value))
               (show))
             (start-again ()
               (setf timer (timer-reset timer)
                     last-time nil)
               (show))
             (tick (now)
               (cond ((timer-running-p timer)
                      (when last-time
                        (setf timer (timer-advance timer (/ (- now last-time) 1d9))))
                      (setf last-time now)
                      (show))
                     (t (setf last-time nil)))
               ;; Frames are asked for only while the timer runs.
               (timer-running-p timer)))
      (setf gauge (progress :id :gauge :width 240)
            elapsed (label "0.0s" :id :elapsed)
            duration (label "10.0s" :id :duration-label)
            slider (slider :id :duration :minimum 1 :maximum 30 :value 10 :width 240
                           :on-change #'change-duration)
            reset (button "Reset" :id :reset :on-click #'start-again))
      (let ((variant (make-retained-variant
                      (padding (:all 24)
                        (column (:gap 14)
                          (row (:gap 12 :cross-align :center)
                            (label "Elapsed time:") gauge)
                          elapsed
                          (row (:gap 12 :cross-align :center)
                            (label "Duration:") slider duration)
                          reset)))))
        (add-ticker variant #'tick)
        variant))))

(register-app :retained :timer 'make-timer-app :width 460 :height 250)
