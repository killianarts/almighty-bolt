(defpackage #:almighty-bolt-7guis/flight/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/flight/model
                #:date-text-valid-p
                #:return-date-enabled-p
                #:flight-valid-p
                #:booking-message)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-enabled)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:column)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:button)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:text-field
                #:text-field-text
                #:text-field-invalid
                #:choice
                #:choice-selected)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-flight))

(in-package #:almighty-bolt-7guis/flight/retained)

;;; The flight booker, retained: widgets built once, and one function that looks at what
;;; they hold and sets what depends on it (which fields are red, whether the return date
;;; can be used, whether Book can be pressed). Every handler calls it.

(defparameter +first-date+ "27.03.2026")

(defun make-flight ()
  (let ((kind nil) (start nil) (return nil) (book nil) (message nil))
    (labels ((flight-kind ()
               (if (zerop (choice-selected kind)) :one-way :return))
             (refresh ()
               (let ((enabled (return-date-enabled-p (flight-kind))))
                 (setf (node-enabled return) enabled
                       (text-field-invalid start) (not (date-text-valid-p (text-field-text start)))
                       (text-field-invalid return) (and enabled
                                                        (not (date-text-valid-p
                                                              (text-field-text return))))
                       (node-enabled book) (and (flight-valid-p (flight-kind)
                                                                (text-field-text start)
                                                                (text-field-text return))
                                                t))))
             (changed (&rest ignore)
               (declare (ignore ignore))
               (setf (label-text message) "")
               (refresh))
             (book-it ()
               (setf (label-text message)
                     (booking-message (flight-kind) (text-field-text start)
                                      (text-field-text return)))))
      (setf kind (choice '("one-way flight" "return flight") :id :kind :on-change #'changed)
            start (text-field :id :start :text +first-date+ :width 160 :on-change #'changed)
            return (text-field :id :return :text +first-date+ :width 160 :on-change #'changed)
            book (button "Book" :id :book :on-click #'book-it)
            message (label "" :id :message))
      (refresh)
      (make-retained-variant
       (padding (:all 24)
         (column (:gap 12) kind start return book message))))))

(register-app :retained :flight 'make-flight :width 460 :height 290)
