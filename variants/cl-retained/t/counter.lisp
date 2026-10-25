(defpackage #:almighty-bolt-variant-retained/t/counter
  (:use #:cl)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:button
                #:padding
                #:row)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-counter))

(in-package #:almighty-bolt-variant-retained/t/counter)

;;; The first of the 7GUIs tasks: a number and a button that adds one to it. It is
;;; the smallest app that needs state, an event and a change of layout (the number
;;; gets wider), so it is the first thing the retained variant has to run.

(defun make-counter ()
  "A retained variant showing the Counter. Returns it, the label that shows the
number and the button, as three values."
  (let* ((count 0)
         (number (label "0" :id :count :size 32))
         (button (button "Count" :id :count-button
                                 :on-click (lambda ()
                                             (incf count)
                                             (setf (label-text number)
                                                   (princ-to-string count)))))
         (root (padding (row number button :gap 16 :cross-align :center) :all 24)))
    (values (make-retained-variant root) number button)))
