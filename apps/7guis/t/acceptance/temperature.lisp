(defpackage #:almighty-bolt-7guis/t/acceptance/temperature
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)))

(in-package #:almighty-bolt-7guis/t/acceptance/temperature)

;;; The temperature converter: two fields, Celsius and Fahrenheit. Typing a number in one
;;; shows the other; text that is not a number leaves the other as it was.
;;; Controls: :celsius and :fahrenheit.

(defun enter (driver id text)
  "Click the field ID and type TEXT."
  (d:click driver id)
  (d:type-text driver text))

(d:define-acceptance both-fields-start-empty :temperature
  (assert-equal "" (d:text-in driver :celsius))
  (assert-equal "" (d:text-in driver :fahrenheit)))

(d:define-acceptance typing-celsius-shows-fahrenheit :temperature
  (enter driver :celsius "100")
  (assert-equal "100" (d:text-in driver :celsius))
  (assert-equal "212" (d:text-in driver :fahrenheit)))

(d:define-acceptance typing-fahrenheit-shows-celsius :temperature
  (enter driver :fahrenheit "32")
  (assert-equal "0" (d:text-in driver :celsius))
  (assert-equal "32" (d:text-in driver :fahrenheit)))

(d:define-acceptance negative-and-fractional-numbers-convert :temperature
  (enter driver :celsius "-40")
  (assert-equal "-40" (d:text-in driver :fahrenheit))
  (d:click driver :fahrenheit)
  (d:press-key driver #\a :modifiers almighty-bolt-core:+modifier-super+)
  (d:type-text driver "98.6")
  (assert-equal "37" (d:text-in driver :celsius)))

(d:define-acceptance text-that-is-not-a-number-leaves-the-other-field-alone :temperature
  (enter driver :celsius "5")
  (assert-equal "41" (d:text-in driver :fahrenheit))
  (d:type-text driver "x")
  (assert-equal "5x" (d:text-in driver :celsius))
  (assert-equal "41" (d:text-in driver :fahrenheit))
  (d:press-key driver #\Backspace)
  (assert-equal "5" (d:text-in driver :celsius))
  (assert-equal "41" (d:text-in driver :fahrenheit)))

(d:define-acceptance deleting-the-text-keeps-the-last-number-in-the-other-field :temperature
  (enter driver :celsius "7")
  (d:press-key driver #\Backspace)
  (assert-equal "" (d:text-in driver :celsius))
  (assert-equal "44.6" (d:text-in driver :fahrenheit)))

(d:define-acceptance the-fields-keep-what-was-typed-while-the-other-is-edited :temperature
  (enter driver :celsius "10")
  (d:click driver :fahrenheit)
  (d:press-key driver #\a :modifiers almighty-bolt-core:+modifier-super+)
  (d:type-text driver "50")
  (assert-equal "10" (d:text-in driver :celsius))
  (d:type-text driver "1")
  (assert-equal "501" (d:text-in driver :fahrenheit))
  (assert-equal "260.56" (d:text-in driver :celsius)))

;;; --- E3 (notes/change-cost.org): Undo. The button is :undo; it takes back one change to a field,
;;; a key typed or a character deleted, and cannot be pressed with nothing to take back. ---

(d:define-acceptance undo-takes-back-the-last-change-to-a-field :temperature
  (enter driver :celsius "10")
  (assert-equal "50" (d:text-in driver :fahrenheit))
  (d:type-text driver "0")
  (assert-equal "100" (d:text-in driver :celsius))
  (assert-equal "212" (d:text-in driver :fahrenheit))
  (d:click driver :undo)
  (assert-equal "10" (d:text-in driver :celsius))
  (assert-equal "50" (d:text-in driver :fahrenheit)))

(d:define-acceptance undo-goes-back-a-step-at-a-time-to-the-start :temperature
  (enter driver :celsius "37")
  (d:click driver :undo)
  (assert-equal "3" (d:text-in driver :celsius))
  (assert-equal "37.4" (d:text-in driver :fahrenheit))
  (d:click driver :undo)
  (assert-equal "" (d:text-in driver :celsius))
  (assert-equal "" (d:text-in driver :fahrenheit))
  (d:click driver :undo)
  (assert-equal "" (d:text-in driver :celsius)))

(d:define-acceptance undo-restores-both-fields-when-the-other-one-was-edited :temperature
  (enter driver :celsius "5")
  (enter driver :fahrenheit "5")
  (assert-equal "-15" (d:text-in driver :celsius))
  (d:click driver :undo)
  (assert-equal "5" (d:text-in driver :celsius))
  (assert-equal "41" (d:text-in driver :fahrenheit)))

(d:define-acceptance undo-counts-text-that-is-not-a-number-as-a-change :temperature
  (enter driver :celsius "5")
  (d:type-text driver "x")
  (assert-equal "5x" (d:text-in driver :celsius))
  (d:click driver :undo)
  (assert-equal "5" (d:text-in driver :celsius))
  (assert-equal "41" (d:text-in driver :fahrenheit)))
