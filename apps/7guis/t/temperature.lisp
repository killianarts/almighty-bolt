(defpackage #:almighty-bolt-7guis/t/temperature
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-7guis/temperature/model)))

(in-package #:almighty-bolt-7guis/t/temperature)

(define-test celsius-and-fahrenheit-convert-both-ways ()
  (assert-equal "32" (m:celsius-text->fahrenheit-text "0"))
  (assert-equal "212" (m:celsius-text->fahrenheit-text "100"))
  (assert-equal "-40" (m:celsius-text->fahrenheit-text "-40"))
  (assert-equal "98.6" (m:celsius-text->fahrenheit-text "37"))
  (assert-equal "0" (m:fahrenheit-text->celsius-text "32"))
  (assert-equal "100" (m:fahrenheit-text->celsius-text "212"))
  (assert-equal "37" (m:fahrenheit-text->celsius-text "98.6")))

(define-test a-text-that-is-not-a-number-changes-nothing ()
  (assert-false (m:celsius-text->fahrenheit-text ""))
  (assert-false (m:celsius-text->fahrenheit-text "-"))
  (assert-false (m:fahrenheit-text->celsius-text "hot")))

(define-test an-unfinished-number-is-ignored-until-it-is-finished ()
  ;; Typing -4 on the way to -40 converts at -4, and the sign alone converts nothing.
  (assert-equal "24.8" (m:celsius-text->fahrenheit-text "-4"))
  (assert-false (m:celsius-text->fahrenheit-text "-")))
