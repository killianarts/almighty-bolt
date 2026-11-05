(defpackage #:almighty-bolt-7guis/temperature/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-7guis/temperature/model)
                    (#:bolt #:almighty-bolt))
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/app)

;;; The temperature converter, immediate: the text of each field is a variable the field is
;;; given every frame, and a field that was typed in returns the new text, which is written to
;;; both.

(defun make-temperature ()
  (bolt:state-let ((celsius "")
                   (fahrenheit ""))
    (lambda ()
      (bolt:padding (:all 20)
        (bolt:row (:gap 10 :cross-align :center)
          (multiple-value-bind (text changed)
                   (bolt:text-field celsius :id :celsius :labelled-by :celsius-label :width 100)
            (when changed
                   (setf celsius text)
                   (let ((other (model:celsius-text->fahrenheit-text text)))
                     (when other
                  (setf fahrenheit other)))))
          (bolt:label "Celsius =" :id :celsius-label)
          (multiple-value-bind (text changed)
                   (bolt:text-field fahrenheit :id :fahrenheit :labelled-by :fahrenheit-label :width 100)
            (when changed
                   (setf fahrenheit text)
                   (let ((other (model:fahrenheit-text->celsius-text text)))
                     (when other
                  (setf celsius other)))))
          (bolt:label "Fahrenheit" :id :fahrenheit-label))))))

(registry:register-app :temperature 'make-temperature :width 520 :height 90)
