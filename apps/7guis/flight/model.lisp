(defpackage #:almighty-bolt-7guis/flight/model
  (:use #:cl)
  (:export #:parse-date
           #:format-date
           #:date<
           #:flight-valid-p
           #:date-text-valid-p
           #:return-date-enabled-p
           #:booking-message))

(in-package #:almighty-bolt-7guis/flight/model)

;;; The flight booker's logic: dates are written dd.mm.yyyy; a one-way flight needs one
;;; valid date and a return flight two, the second not before the first.

(defun leap-year-p (year)
  (and (zerop (mod year 4)) (or (plusp (mod year 100)) (zerop (mod year 400)))))

(defun days-in-month (year month)
  (case month
    ((4 6 9 11) 30)
    (2 (if (leap-year-p year) 29 28))
    (t 31)))

(defun parse-date (text)
  "The date TEXT spells as a list (year month day), or NIL. TEXT must be dd.mm.yyyy
with real numbers: two digits, a point, two digits, a point and four digits."
  (when (and (= (length text) 10)
             (char= (char text 2) #\.)
             (char= (char text 5) #\.)
             (every #'digit-char-p (subseq text 0 2))
             (every #'digit-char-p (subseq text 3 5))
             (every #'digit-char-p (subseq text 6 10)))
    (let ((day (parse-integer text :start 0 :end 2))
          (month (parse-integer text :start 3 :end 5))
          (year (parse-integer text :start 6 :end 10)))
      (when (and (<= 1 month 12) (plusp year) (<= 1 day (days-in-month year month)))
        (list year month day)))))

(defun format-date (date)
  (destructuring-bind (year month day) date
    (format nil "~2,'0d.~2,'0d.~4,'0d" day month year)))

(defun date< (a b)
  "True if date A, a list (year month day), is before date B."
  (loop :for x :in a
        :for y :in b
        :do (cond ((< x y) (return t))
                  ((> x y) (return nil)))))

(defun date-text-valid-p (text)
  (and (parse-date text) t))

(defun return-date-enabled-p (kind)
  "True if the return date field can be used for the kind of flight, :ONE-WAY or
:RETURN."
  (eq kind :return))

(defun flight-valid-p (kind start-text return-text)
  "True if the flight can be booked: a valid start date, and for a return flight a
valid return date that is not before it."
  (let ((start (parse-date start-text)))
    (and start
         (or (eq kind :one-way)
             (let ((return (parse-date return-text)))
               (and return (not (date< return start))))))))

(defun booking-message (kind start-text return-text)
  "What the task says when the flight is booked."
  (ecase kind
    (:one-way (format nil "Booked a one-way flight on ~a." start-text))
    (:return (format nil "Booked a return flight, ~a to ~a." start-text return-text))))
