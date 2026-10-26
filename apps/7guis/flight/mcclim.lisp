(defpackage #:almighty-bolt-7guis/flight/mcclim
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/flight/model
                #:parse-date
                #:return-date-enabled-p
                #:flight-valid-p
                #:booking-message)
  (:import-from #:clim
                #:define-application-frame
                #:make-application-frame
                #:run-frame-top-level
                #:define-presentation-type
                #:define-presentation-method
                #:presentation-typep
                #:accept
                #:textual-view
                #:read-token
                #:simple-parse-error
                #:accept-values-command-button
                #:+textual-dialog-view+)
  (:import-from #:clime
                #:accept-values-pane
                #:accept-values-pane-displayer)
  (:export #:make-flight-frame
           #:run-flight))

(in-package #:almighty-bolt-7guis/flight/mcclim)

;;; The flight booker, McCLIM: an ACCEPT-VALUES pane whose display function asks for the kind of
;;; flight and the dates, and keeps what it is given in the frame's slots. A date is a presentation
;;; type of this app, FLIGHT-DATE, a string that the model's PARSE-DATE reads: McCLIM makes a text
;;; field for it, draws the field's line in red when what is typed does not parse, and gives the
;;; display function the text as it is. There is no widget that is grey: what cannot be used is not
;;; asked for (the return date of a one-way flight) or not offered (Book).

(define-presentation-type flight-date () :inherit-from 'string)

(define-presentation-method presentation-typep (object (type flight-date))
  (and (stringp object) (parse-date object) t))

(define-presentation-method accept ((type flight-date) stream (view textual-view) &key)
  (let ((text (read-token stream)))
    (if (parse-date text)
        text
        (simple-parse-error "~a is not a date, which is dd.mm.yyyy." text))))

(define-application-frame flight-frame ()
  ((kind :initform :one-way)
   (start :initform "27.03.2026")
   (end :initform "27.03.2026")
   (message :initform "")
   (seen :initform nil))
  (:panes (form :accept-values
                :display-function '(accept-values-pane-displayer :displayer show-form
                                                                 :resynchronize-every-pass t)))
  (:layouts (default form))
  (:command-table (flight-frame :inherit-from (accept-values-pane)))
  (:default-initargs :width 460 :height 290))

(defmethod show-form ((frame flight-frame) stream)
  (with-slots (kind start end message seen) frame
    (setf kind (accept '(member :one-way :return) :prompt "Flight" :default kind :stream stream))
    (fresh-line stream)
    (setf start (accept 'flight-date :prompt "Start" :default start :stream stream
                                :view +textual-dialog-view+))
    (fresh-line stream)
    (when (return-date-enabled-p kind)
      (setf end (accept 'flight-date :prompt "Return" :default end :stream stream
                                    :view +textual-dialog-view+))
      (fresh-line stream))
    (unless (equal seen (list kind start end))
      (setf message ""
            seen (list kind start end)))
    (when (flight-valid-p kind start end)
      (accept-values-command-button (stream :resynchronize t) "Book"
        (setf message (booking-message kind start end))))
    (fresh-line stream)
    (write-string message stream)))

(defun make-flight-frame (&rest initargs)
  (apply #'make-application-frame 'flight-frame initargs))

(defun run-flight (&rest initargs)
  (run-frame-top-level (apply #'make-flight-frame initargs)))
