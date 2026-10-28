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
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:text-field)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/reactive)

;;; The temperature converter, reactive: the text of each field is a signal, and typing in
;;; one writes both. The fields show what the signals hold.

(defun make-temperature ()
  (let ((celsius (make-signal ""))
        (fahrenheit (make-signal "")))
    (make-reactive-variant
     (lambda ()
       (padding (:all 20)
         (row (:gap 10 :cross-align :center)
           (text-field :id :celsius :width 100 :value (value celsius)
                       :on-change (lambda (text)
                                    (setf (value celsius) text)
                                    (let ((other (celsius-text->fahrenheit-text text)))
                                      (when other
                                        (setf (value fahrenheit) other)))))
           (label "Celsius =")
           (text-field :id :fahrenheit :width 100 :value (value fahrenheit)
                       :on-change (lambda (text)
                                    (setf (value fahrenheit) text)
                                    (let ((other (fahrenheit-text->celsius-text text)))
                                      (when other
                                        (setf (value celsius) other)))))
           (label "Fahrenheit")))))))

(register-app :reactive :temperature 'make-temperature :width 520 :height 90)
