(defpackage #:almighty-bolt-7guis/temperature/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/temperature/model
                #:celsius-text->fahrenheit-text
                #:fahrenheit-text->celsius-text)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button
                #:text-field
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/reactive)

;;; The temperature converter, reactive: the text of each field is a signal, and typing in
;;; one writes both. The fields show what the signals hold. Undo has a signal of the pairs of
;;; texts the fields held before each change.

(defun make-temperature ()
  (let ((celsius (make-signal ""))
        (fahrenheit (make-signal ""))
        (history (make-signal '())))
    (flet ((remember ()
             (setf (value history) (cons (cons (value celsius) (value fahrenheit))
                                         (value history)))))
      (make-reactive-variant
      (lambda ()
        (padding (row (text-field :id :celsius :width 100 :value (value celsius)
                                  :on-change (lambda (text)
                                               (remember)
                                               (setf (value celsius) text)
                                               (let ((other (celsius-text->fahrenheit-text text)))
                                                 (when other
                                                   (setf (value fahrenheit) other)))))
                      (label "Celsius =")
                      (text-field :id :fahrenheit :width 100 :value (value fahrenheit)
                                  :on-change (lambda (text)
                                               (remember)
                                               (setf (value fahrenheit) text)
                                               (let ((other (fahrenheit-text->celsius-text text)))
                                                 (when other
                                                   (setf (value celsius) other)))))
                      (label "Fahrenheit")
                      (button "Undo" :id :undo :enabled (and (value history) t)
                                     :on-click (lambda ()
                                                 (destructuring-bind (old-celsius . old-fahrenheit)
                                                     (first (value history))
                                                   (setf (value history) (rest (value history))
                                                         (value celsius) old-celsius
                                                         (value fahrenheit) old-fahrenheit))))
                      :gap 10 :cross-align :center)
                 :all 20))))))

(register-app :reactive :temperature #'make-temperature :width 520 :height 90)
