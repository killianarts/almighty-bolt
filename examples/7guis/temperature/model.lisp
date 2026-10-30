(defpackage #:almighty-bolt-7guis/temperature/model
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/numbers
                #:parse-number
                #:format-number)
  (:export #:celsius->fahrenheit
           #:fahrenheit->celsius
           #:celsius-text->fahrenheit-text
           #:fahrenheit-text->celsius-text))

(in-package #:almighty-bolt-7guis/temperature/model)

;;; The temperature converter's logic, which every variant's version shares: what a text
;;; means as a number, and what the other field shows when one of them is typed into. A
;;; text that is not a number changes nothing, which is the task's rule.

(defun celsius->fahrenheit (celsius)
  (+ 32 (* celsius 9/5)))

(defun fahrenheit->celsius (fahrenheit)
  (* (- fahrenheit 32) 5/9))

(defun celsius-text->fahrenheit-text (text)
  "What the Fahrenheit field shows when the Celsius field holds TEXT, or NIL if TEXT is
not a number and the other field is left as it is."
  (let ((number (parse-number text)))
    (and number (format-number (celsius->fahrenheit number)))))

(defun fahrenheit-text->celsius-text (text)
  "What the Celsius field shows when the Fahrenheit field holds TEXT, or NIL."
  (let ((number (parse-number text)))
    (and number (format-number (fahrenheit->celsius number)))))
