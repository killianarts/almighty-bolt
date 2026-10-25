(defpackage #:almighty-bolt-7guis/t/acceptance/counter
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-7guis/t/acceptance/counter)

;;; The counter: a label showing a number that starts at 0, and a button that adds one.
;;; Controls: :count (the label) and :count-button.

(d:define-acceptance the-counter-starts-at-zero :counter
  (assert-equal "0" (d:text-in driver :count)))

(d:define-acceptance clicking-the-button-adds-one :counter
  (d:click driver :count-button)
  (assert-equal "1" (d:text-in driver :count))
  (d:click driver :count-button)
  (d:click driver :count-button)
  (assert-equal "3" (d:text-in driver :count)))

(d:define-acceptance the-number-can-be-wider-than-one-digit :counter
  (dotimes (i 12)
    (d:click driver :count-button))
  (assert-equal "12" (d:text-in driver :count))
  ;; The button is still there, and still works, after the number made room for itself.
  (d:click driver :count-button)
  (assert-equal "13" (d:text-in driver :count)))

(d:define-acceptance pressing-and-letting-go-elsewhere-does-not-count :counter
  (d:press-at driver :count-button 0.5 0.5)
  (d:drag-to driver :count-button 5.0 5.0)
  (d:release-at driver :count-button 5.0 5.0)
  (assert-equal "0" (d:text-in driver :count)))

(d:define-acceptance the-keyboard-can-press-the-button :counter
  (d:press-key driver #\Tab)
  (d:press-key driver #\Space)
  (d:press-key driver #\Return)
  (assert-equal "2" (d:text-in driver :count)))
