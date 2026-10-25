(defpackage #:almighty-bolt-7guis/t/numbers
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:n #:almighty-bolt-7guis/common/numbers)))

(in-package #:almighty-bolt-7guis/t/numbers)

(define-test numbers-are-signed-digits-with-an-optional-point ()
  (assert-eql 12d0 (n:parse-number "12"))
  (assert-eql -3.5d0 (n:parse-number "-3.5"))
  (assert-eql 0.5d0 (n:parse-number ".5"))
  (assert-eql 5d0 (n:parse-number "5."))
  (assert-eql 7d0 (n:parse-number "+7"))
  (assert-eql 42d0 (n:parse-number "  42 ")))

(define-test anything-else-is-not-a-number ()
  (dolist (text '("" "-" "." "+" "abc" "1e5" "1.2.3" "12a" "--1" "1 2"))
    (assert-false (n:parse-number text) text)))

(define-test numbers-are-written-with-at-most-two-decimals-and-no-trailing-zeros ()
  (assert-equal "98.6" (n:format-number 98.6d0))
  (assert-equal "100" (n:format-number 100d0))
  (assert-equal "-40" (n:format-number -40d0))
  (assert-equal "0" (n:format-number 0d0))
  (assert-equal "0" (n:format-number -0.001d0))
  (assert-equal "3.14" (n:format-number 3.14159d0))
  (assert-equal "-0.5" (n:format-number -0.5d0))
  (assert-equal "0.05" (n:format-number 0.05d0))
  (assert-equal "1234567" (n:format-number 1234567d0)))
