(defpackage #:almighty-bolt-7guis/counter/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:bolt #:almighty-bolt))
  (:export #:make-counter))

(in-package #:almighty-bolt-7guis/counter/app)

;;; The Counter, immediate: the count is a variable, and the function that describes the
;;; screen is called again for every frame. The button says whether it was clicked.

(defun make-counter ()
  (bolt:state-let ((count 0))
    (lambda ()
      (bolt:padding (:all 24)
        (bolt:row (:gap 16 :cross-align :center)
          (bolt:label (princ-to-string count) :id :count :size 32)
          (when (bolt:button "Count" :id :count-button)
            (incf count)))))))

(registry:register-app :counter 'make-counter :width 360 :height 160)
