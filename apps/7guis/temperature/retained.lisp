(defpackage #:almighty-bolt-7guis/temperature/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/temperature/model
                #:celsius-text->fahrenheit-text
                #:fahrenheit-text->celsius-text)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:button
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:text-field
                #:text-field-text)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-enabled)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-temperature))

(in-package #:almighty-bolt-7guis/temperature/retained)

;;; The temperature converter, retained: two fields that are built once, each with a handler
;;; that writes into the other. A field does not say what it held before a change, so the
;;; texts that both held after the last one are kept for Undo.

(defun make-temperature ()
  (let ((celsius nil)
        (fahrenheit nil)
        (undo nil)
        (history '())
        (seen-celsius "")
        (seen-fahrenheit ""))
    (flet ((changed ()
             ;; What the fields hold now is what the next change will have come from.
             (setf seen-celsius (text-field-text celsius)
                   seen-fahrenheit (text-field-text fahrenheit)
                   (node-enabled undo) (and history t))))
      (setf celsius (text-field :id :celsius :width 100
                                :on-change (lambda (text)
                                             (push (cons seen-celsius seen-fahrenheit) history)
                                             (let ((other (celsius-text->fahrenheit-text text)))
                                               (when other
                                                 (setf (text-field-text fahrenheit) other)))
                                             (changed)))
            fahrenheit (text-field :id :fahrenheit :width 100
                                   :on-change (lambda (text)
                                                (push (cons seen-celsius seen-fahrenheit) history)
                                                (let ((other (fahrenheit-text->celsius-text text)))
                                                  (when other
                                                    (setf (text-field-text celsius) other)))
                                                (changed)))
            undo (button "Undo" :id :undo
                                :on-click (lambda ()
                                            (destructuring-bind (old-celsius . old-fahrenheit)
                                                (pop history)
                                              (setf (text-field-text celsius) old-celsius
                                                    (text-field-text fahrenheit) old-fahrenheit)
                                              (changed)))))
      (changed))
    (make-retained-variant
     (padding (row celsius (label "Celsius =") fahrenheit (label "Fahrenheit") undo
                   :gap 10 :cross-align :center)
              :all 20))))

(register-app :retained :temperature #'make-temperature :width 520 :height 90)
