(defpackage #:almighty-bolt-7guis/temperature/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/temperature/model
                #:celsius-text->fahrenheit-text
                #:fahrenheit-text->celsius-text)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:text-field
                #:text-field-text)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/retained)

;;; The temperature converter, retained: two fields that are built once, each with a handler
;;; that writes into the other.

(defun make-temperature ()
  (let ((celsius nil)
        (fahrenheit nil))
    (setf celsius (text-field :id :celsius :width 100
                              :on-change (lambda (text)
                                           (let ((other (celsius-text->fahrenheit-text text)))
                                             (when other
                                               (setf (text-field-text fahrenheit) other)))))
          fahrenheit (text-field :id :fahrenheit :width 100
                                 :on-change (lambda (text)
                                              (let ((other (fahrenheit-text->celsius-text text)))
                                                (when other
                                                  (setf (text-field-text celsius) other))))))
    (make-retained-variant
     (padding (:all 20)
       (row (:gap 10 :cross-align :center)
         celsius (label "Celsius =") fahrenheit (label "Fahrenheit"))))))

(register-app :retained :temperature #'make-temperature :width 520 :height 90)
