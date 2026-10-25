(defpackage #:almighty-bolt-7guis/counter/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:button
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-counter))

(in-package #:almighty-bolt-7guis/counter/retained)

;;; The Counter, retained: a tree of widgets built once, and a handler that changes one of
;;; them.

(defun make-counter ()
  (let* ((count 0)
         (number (label "0" :id :count :size 32))
         (root (padding (row number
                             (button "Count" :id :count-button
                                             :on-click (lambda ()
                                                         (incf count)
                                                         (setf (label-text number)
                                                               (princ-to-string count))))
                             :gap 16 :cross-align :center)
                        :all 24)))
    (make-retained-variant root)))

(register-app :retained :counter #'make-counter :width 360 :height 160)
