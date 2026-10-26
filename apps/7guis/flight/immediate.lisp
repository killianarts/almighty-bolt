(defpackage #:almighty-bolt-7guis/flight/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/flight/model
                #:date-text-valid-p
                #:return-date-enabled-p
                #:return-date-invalid-p
                #:flight-valid-p
                #:booking-message)
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:label
                #:button
                #:padding
                #:column
                #:text-field
                #:choice)
  (:import-from #:almighty-bolt-variant-immediate/immediate
                #:make-immediate-variant)
  (:export #:make-flight))

(in-package #:almighty-bolt-7guis/flight/immediate)

;;; The flight booker, immediate: the kind of flight, the two dates and the message are
;;; variables, and which fields are red, whether the return date can be used and whether Book
;;; can be pressed are worked out where the widgets are called, from them.

(defparameter +first-date+ "27.03.2026")

(defun make-flight ()
  (let ((kind 0)
        (start +first-date+)
        (return +first-date+)
        (message ""))
    (make-immediate-variant
     (lambda ()
       (let* ((flight-kind (if (zerop kind) :one-way :return))
              (return-enabled (return-date-enabled-p flight-kind)))
         (padding (:all 24)
           (column (:gap 12)
             (multiple-value-bind (index changed)
                 (choice :kind '("one-way flight" "return flight") kind)
               (when changed
                 (setf kind index
                       message "")))
             (multiple-value-bind (text changed)
                 (text-field :start start :width 160 :invalid (not (date-text-valid-p start)))
               (when changed
                 (setf start text
                       message "")))
             (multiple-value-bind (text changed)
                 (text-field :return return :width 160 :enabled return-enabled
                                            :invalid (return-date-invalid-p flight-kind return))
               (when changed
                 (setf return text
                       message "")))
             (when (button "Book" :id :book
                                  :enabled (and (flight-valid-p flight-kind start return) t))
               (setf message (booking-message flight-kind start return)))
             (label message :id :message))))))))

(register-app :immediate :flight #'make-flight :width 460 :height 290)
