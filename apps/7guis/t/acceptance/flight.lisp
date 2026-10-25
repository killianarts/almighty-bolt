(defpackage #:almighty-bolt-7guis/t/acceptance/flight
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)))

(in-package #:almighty-bolt-7guis/t/acceptance/flight)

;;; The flight booker: a choice between a one-way and a return flight, a start date and a
;;; return date (dd.mm.yyyy; the return date can be used only for a return flight), and a
;;; Book button that can be pressed only when the dates make a flight, which then says
;;; what was booked. Controls: :kind (the choice, one-way first), :start, :return, :book
;;; and :message.

(defun replace-text (driver id text)
  "Click the field ID, select what it holds and type TEXT over it."
  (d:click driver id)
  (d:press-key driver #\a :modifiers almighty-bolt-core:+modifier-super+)
  (d:type-text driver text))

(defun choose-return-flight (driver)
  (d:click-at driver :kind 0.75 0.5))

(defun choose-one-way (driver)
  (d:click-at driver :kind 0.15 0.5))

(d:define-acceptance both-dates-start-the-same-and-the-flight-is-one-way :flight
  (assert-equal (d:text-in driver :start) (d:text-in driver :return))
  (assert-equal 10 (length (d:text-in driver :start)))
  (assert-equal "" (d:text-in driver :message)))

(d:define-acceptance booking-a-one-way-flight-says-so :flight
  (let ((date (d:text-in driver :start)))
    (d:click driver :book)
    (assert-equal (format nil "Booked a one-way flight on ~a." date)
                  (d:text-in driver :message))))

(d:define-acceptance the-return-date-cannot-be-used-for-a-one-way-flight :flight
  (let ((before (d:text-in driver :return)))
    (d:click driver :return)
    (d:type-text driver "9")
    (assert-equal before (d:text-in driver :return))))

(d:define-acceptance a-return-flight-has-a-return-date-to-edit :flight
  (choose-return-flight driver)
  (replace-text driver :start "01.05.2026")
  (replace-text driver :return "08.05.2026")
  (assert-equal "08.05.2026" (d:text-in driver :return))
  (d:click driver :book)
  (assert-equal "Booked a return flight, 01.05.2026 to 08.05.2026."
                (d:text-in driver :message)))

(d:define-acceptance a-return-before-the-start-cannot-be-booked :flight
  (choose-return-flight driver)
  (replace-text driver :start "10.05.2026")
  (replace-text driver :return "09.05.2026")
  (d:click driver :book)
  (assert-equal "" (d:text-in driver :message))
  (replace-text driver :return "10.05.2026")
  (d:click driver :book)
  (assert-true (search "return flight" (d:text-in driver :message))))

(d:define-acceptance an-invalid-start-date-cannot-be-booked :flight
  (replace-text driver :start "31.02.2026")
  (d:click driver :book)
  (assert-equal "" (d:text-in driver :message))
  (replace-text driver :start "28.02.2026")
  (d:click driver :book)
  (assert-true (search "28.02.2026" (d:text-in driver :message))))

(defun let-go (driver)
  "Press on nothing, which takes the keyboard from a field and leaves it as it looks."
  (d:click driver :message))

(d:define-acceptance an-invalid-date-looks-different-from-a-valid-one :flight
  (let ((valid (d:colors-in driver :start)))
    (replace-text driver :start "nonsense")
    (let-go driver)
    (assert-false (equal valid (d:colors-in driver :start)))
    (replace-text driver :start "28.02.2026")
    (let-go driver)
    (assert-equal valid (d:colors-in driver :start))))

(d:define-acceptance an-invalid-return-date-does-not-matter-for-a-one-way-flight :flight
  (choose-return-flight driver)
  (replace-text driver :return "nonsense")
  (choose-one-way driver)
  (d:click driver :book)
  (assert-true (search "one-way flight" (d:text-in driver :message))))

(d:define-acceptance changing-anything-clears-the-message :flight
  (d:click driver :book)
  (assert-true (plusp (length (d:text-in driver :message))))
  (replace-text driver :start "01.01.2027")
  (assert-equal "" (d:text-in driver :message)))
