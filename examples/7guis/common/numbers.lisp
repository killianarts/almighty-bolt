(defpackage #:almighty-bolt-7guis/common/numbers
  (:use #:cl)
  (:export #:parse-number
           #:format-number))

(in-package #:almighty-bolt-7guis/common/numbers)

;;; What a text means as a number, and a number as text, for the tasks that take numbers
;;; in text fields (the temperature converter and the cells).

(defun parse-number (text)
  "The number TEXT spells, as a double-float, or NIL if it does not spell one. A number
is an optional sign, digits, and optionally a point and more digits (or a point and
digits alone). Spaces round it are ignored."
  (let* ((text (string-trim " " text))
         (end (length text))
         (i 0)
         (sign 1)
         (integer-part 0)
         (fraction 0)
         (scale 1)
         (digits 0))
    (when (and (< i end) (find (char text i) "+-"))
      (when (char= (char text i) #\-) (setf sign -1))
      (incf i))
    (loop :while (and (< i end) (digit-char-p (char text i)))
          :do (setf integer-part (+ (* 10 integer-part) (digit-char-p (char text i))))
              (incf digits)
              (incf i))
    (when (and (< i end) (char= (char text i) #\.))
      (incf i)
      (loop :while (and (< i end) (digit-char-p (char text i)))
            :do (setf fraction (+ (* 10 fraction) (digit-char-p (char text i)))
                      scale (* scale 10))
                (incf digits)
                (incf i)))
    (and (= i end) (plusp digits)
         (float (* sign (+ integer-part (/ fraction scale))) 1d0))))

(defun format-number (number)
  "NUMBER as text with at most two decimals and no trailing zeros: 98.6, 100, -40."
  (let* ((hundredths (round (* number 100)))
         (sign (if (minusp hundredths) "-" ""))
         (hundredths (abs hundredths)))
    (multiple-value-bind (whole fraction) (floor hundredths 100)
      (cond ((zerop hundredths) "0")
            ((zerop fraction) (format nil "~a~d" sign whole))
            ((zerop (mod fraction 10)) (format nil "~a~d.~d" sign whole (floor fraction 10)))
            (t (format nil "~a~d.~2,'0d" sign whole fraction))))))
