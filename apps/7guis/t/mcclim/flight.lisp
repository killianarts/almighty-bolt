(defpackage #:almighty-bolt-7guis/t/mcclim/flight
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:f #:almighty-bolt-7guis/flight/mcclim)
                    (#:s #:almighty-bolt-7guis/t/mcclim/support)
                    (#:base #:almighty-bolt-stress/t/mcclim/support)
                    (#:clim #:clim)))

(in-package #:almighty-bolt-7guis/t/mcclim/flight)

;;; The flight booker in McCLIM, by the acceptance tests of the other variants
;;; (apps/7guis/t/acceptance/ flight.lisp): what is shown, what is offered, and what a typed date
;;; does to it. Where McCLIM does the test differently the name says how: a field that is not asked
;;; for is not grey, and Book that is not offered is not grey.

(defun form (frame)
  (clim:find-pane-named frame 'almighty-bolt-7guis/flight/mcclim::form))

(defun shown (frame)
  (base:pane-strings (form frame)))

(defun asked-for-p (frame prompt)
  (and (find-if (lambda (string) (search prompt string)) (shown frame)) t))

(defun ready (frame)
  (s:await (lambda () (s:button-offered-p (form frame) "Book")))
  frame)

(defmacro with-flight ((frame) &body body)
  `(base:with-running-frame (,frame #'f:make-flight-frame)
     (ready ,frame)
     ,@body))

(defun choose-kind (frame kind)
  (let ((option (first (s:gadgets-in (form frame) 'clim:option-pane))))
    (clim:value-changed-callback option (clim:gadget-client option) (clim:gadget-id option) kind)
    (sleep 0.5)))

(define-test both-dates-start-the-same-and-the-flight-is-one-way ()
  (with-flight (frame)
    (assert-equal :one-way (slot-value frame 'almighty-bolt-7guis/flight/mcclim::kind))
    (assert-equal "27.03.2026" (slot-value frame 'almighty-bolt-7guis/flight/mcclim::start))
    (assert-equal "27.03.2026" (slot-value frame 'almighty-bolt-7guis/flight/mcclim::end))))

(define-test booking-a-one-way-flight-says-so ()
  (with-flight (frame)
    (s:press-button (form frame) "Book")
    (assert-true (member "Booked a one-way flight on 27.03.2026." (shown frame) :test #'equal))))

(define-test the-return-date-is-not-asked-for-a-one-way-flight--differently ()
  (with-flight (frame)
    (assert-true (asked-for-p frame "Start"))
    (assert-false (asked-for-p frame "Return"))))

(define-test a-return-flight-asks-for-a-return-date ()
  (with-flight (frame)
    (choose-kind frame :return)
    (assert-true (asked-for-p frame "Return"))))

(define-test a-return-before-the-start-is-not-offered-for-booking--differently ()
  (with-flight (frame)
    (choose-kind frame :return)
    (s:type-into (form frame) "Return" "26.03.2026")
    (assert-false (s:button-offered-p (form frame) "Book"))
    (s:type-into (form frame) "Return" "28.03.2026")
    (assert-true (s:button-offered-p (form frame) "Book"))))

(define-test an-invalid-start-date-is-not-offered-for-booking--differently ()
  (with-flight (frame)
    (s:type-into (form frame) "Start" "30.02.2026")
    (assert-false (s:button-offered-p (form frame) "Book"))))

(define-test an-invalid-date-is-drawn-in-red-by-mcclim ()
  (with-flight (frame)
    (assert-false (eq clim:+red+ (s:query-ink (form frame) "Start")))
    (s:type-into (form frame) "Start" "30.02.2026")
    (assert-true (member (s:query-ink (form frame) "Start") (list clim:+red+ clim:+dark-red+)))))

(define-test an-invalid-return-date-does-not-matter-for-a-one-way-flight ()
  (with-flight (frame)
    (choose-kind frame :return)
    (s:type-into (form frame) "Return" "not a date")
    (assert-false (s:button-offered-p (form frame) "Book"))
    (choose-kind frame :one-way)
    (assert-true (s:button-offered-p (form frame) "Book"))))

(define-test changing-anything-clears-the-message ()
  (with-flight (frame)
    (s:press-button (form frame) "Book")
    (assert-true (member "Booked a one-way flight on 27.03.2026." (shown frame) :test #'equal))
    (s:type-into (form frame) "Start" "28.03.2026")
    (assert-false (find-if (lambda (string) (search "Booked" string)) (shown frame)))))
