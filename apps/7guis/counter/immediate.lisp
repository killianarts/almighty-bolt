(defpackage #:almighty-bolt-7guis/counter/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:button
                #:padding
                #:row)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant)
  (:export #:make-counter))

(in-package #:almighty-bolt-7guis/counter/immediate)

;;; The Counter, immediate: the count is a variable, and the function that describes the
;;; screen is called again for every frame. The button says whether it was clicked.

(defun make-counter ()
  (let ((count 0))
    (make-immediate-variant
     (lambda ()
       (padding (:all 24)
         (row (:gap 16 :cross-align :center)
           (label (princ-to-string count) :id :count :size 32)
           (when (button "Count" :id :count-button)
             (incf count))))))))

(register-app :immediate :counter 'make-counter :width 360 :height 160)
