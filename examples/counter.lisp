(defpackage #:almighty-bolt-examples/counter
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:export #:make-counter))

(in-package #:almighty-bolt-examples/counter)

;;; The smallest app. The count is state that survives a reload (STATE-LET); the function that
;;; MAKE-COUNTER returns describes the whole screen and is called again for every frame; the button
;;; says whether it was clicked, and the app does something about it where the button is described.
;;;
;;;   (bolt:open-app 'make-counter :title "Counter" :width 360 :height 160)
;;;
;;; Change a label here and compile the function, and the window changes with the count where it was.

(defun make-counter ()
  (bolt:state-let ((count 0))
    (lambda ()
      (bolt:padding (:all 24)
        (bolt:row (:gap 16 :cross-align :center)
          (bolt:label (princ-to-string count) :id :count :size 32)
          (when (bolt:button "Count" :id :count-button)
            (incf count)))))))
