(defpackage #:almighty-bolt-7guis/temperature/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/temperature/model
                #:celsius-text->fahrenheit-text
                #:fahrenheit-text->celsius-text)
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:label
                #:button
                #:text-field
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-immediate/immediate
                #:make-immediate-variant)
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/immediate)

;;; The temperature converter, immediate: the text of each field is a variable the field is
;;; given every frame, and a field that was typed in returns the new text, which is written to
;;; both. Undo has the pairs of texts the fields held before each change.

(defun make-temperature ()
  (let ((celsius "")
        (fahrenheit "")
        (history '()))
    (make-immediate-variant
     (lambda ()
       (padding (:all 20)
         (row (:gap 10 :cross-align :center)
           (multiple-value-bind (text changed) (text-field :celsius celsius :width 100)
             (when changed
               (push (cons celsius fahrenheit) history)
               (setf celsius text)
               (let ((other (celsius-text->fahrenheit-text text)))
                 (when other
                   (setf fahrenheit other)))))
           (label "Celsius =")
           (multiple-value-bind (text changed) (text-field :fahrenheit fahrenheit :width 100)
             (when changed
               (push (cons celsius fahrenheit) history)
               (setf fahrenheit text)
               (let ((other (fahrenheit-text->celsius-text text)))
                 (when other
                   (setf celsius other)))))
           (label "Fahrenheit")
           (when (button "Undo" :id :undo :enabled (and history t))
             (destructuring-bind (old-celsius . old-fahrenheit) (pop history)
               (setf celsius old-celsius
                     fahrenheit old-fahrenheit)))))))))

(register-app :immediate :temperature #'make-temperature :width 520 :height 90)
