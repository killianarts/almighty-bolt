(defpackage #:almighty-bolt-7guis/temperature/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/temperature/model
                #:celsius-text->fahrenheit-text
                #:fahrenheit-text->celsius-text)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:text-field
                #:padding
                #:row)
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/immediate)

;;; The temperature converter, immediate: the text of each field is a variable the field is
;;; given every frame, and a field that was typed in returns the new text, which is written to
;;; both.

(defun make-temperature ()
  (let ((celsius "")
        (fahrenheit ""))
    (lambda ()
      (padding (:all 20)
        (row (:gap 10 :cross-align :center)
          (multiple-value-bind (text changed) (text-field celsius :id :celsius :width 100)
            (when changed
              (setf celsius text)
              (let ((other (celsius-text->fahrenheit-text text)))
                (when other
                  (setf fahrenheit other)))))
          (label "Celsius =" :id :celsius-label)
          (multiple-value-bind (text changed) (text-field fahrenheit :id :fahrenheit :width 100)
            (when changed
              (setf fahrenheit text)
              (let ((other (fahrenheit-text->celsius-text text)))
                (when other
                  (setf celsius other)))))
          (label "Fahrenheit" :id :fahrenheit-label))))))

(register-app :immediate :temperature 'make-temperature :width 520 :height 90)
