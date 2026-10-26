(defpackage #:almighty-bolt-7guis/counter/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value
                #:update)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:row
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-counter))

(in-package #:almighty-bolt-7guis/counter/reactive)

;;; The Counter, reactive: the count is a signal, the screen is described from it, and the
;;; number is a component of its own so that only it is described again when the count
;;; changes.

(defun make-counter ()
  (let ((count (make-signal 0)))
    (make-reactive-variant
     (lambda ()
       (padding (:all 24)
         (row (:gap 16 :cross-align :center)
           (component ()
             (label (princ-to-string (value count)) :id :count :size 32))
           (button "Count" :id :count-button
                           :on-click (lambda () (update count #'1+)))))))))

(register-app :reactive :counter #'make-counter :width 360 :height 160)
