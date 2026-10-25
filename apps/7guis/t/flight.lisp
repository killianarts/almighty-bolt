(defpackage #:almighty-bolt-7guis/t/flight
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-7guis/flight/model)))

(in-package #:almighty-bolt-7guis/t/flight)

(define-test dates-are-day-month-year-with-points ()
  (assert-equal '(2014 4 27) (m:parse-date "27.04.2014"))
  (assert-equal "27.04.2014" (m:format-date '(2014 4 27)))
  (assert-equal "01.01.0999" (m:format-date '(999 1 1))))

(define-test dates-must-exist ()
  (dolist (text '("" "27.4.2014" "2014-04-27" "32.01.2014" "00.01.2014" "31.04.2014"
                  "29.02.2013" "01.13.2014" "01.00.2014" "ab.cd.efgh" "27.04.20144"))
    (assert-false (m:parse-date text) text))
  (assert-equal '(2012 2 29) (m:parse-date "29.02.2012"))
  (assert-equal '(2000 2 29) (m:parse-date "29.02.2000"))
  (assert-false (m:parse-date "29.02.1900")))

(define-test dates-are-ordered ()
  (assert-true (m:date< '(2014 4 27) '(2014 4 28)))
  (assert-true (m:date< '(2013 12 31) '(2014 1 1)))
  (assert-false (m:date< '(2014 4 27) '(2014 4 27)))
  (assert-false (m:date< '(2014 5 1) '(2014 4 30))))

(define-test a-one-way-flight-needs-only-a-valid-start ()
  (assert-true (m:flight-valid-p :one-way "27.04.2014" "garbage"))
  (assert-false (m:flight-valid-p :one-way "garbage" "27.04.2014")))

(define-test a-return-flight-needs-two-dates-in-order ()
  (assert-true (m:flight-valid-p :return "27.04.2014" "28.04.2014"))
  (assert-true (m:flight-valid-p :return "27.04.2014" "27.04.2014"))
  (assert-false (m:flight-valid-p :return "27.04.2014" "26.04.2014"))
  (assert-false (m:flight-valid-p :return "27.04.2014" "nonsense"))
  (assert-false (m:flight-valid-p :return "nonsense" "28.04.2014")))

(define-test only-a-return-flight-has-a-return-date ()
  (assert-true (m:return-date-enabled-p :return))
  (assert-false (m:return-date-enabled-p :one-way)))

(define-test the-booking-says-what-was-booked ()
  (assert-equal "You have booked a one-way flight on 27.04.2014."
                (m:booking-message :one-way "27.04.2014" "ignored"))
  (assert-equal "You have booked a return flight on 27.04.2014, returning on 30.04.2014."
                (m:booking-message :return "27.04.2014" "30.04.2014")))
