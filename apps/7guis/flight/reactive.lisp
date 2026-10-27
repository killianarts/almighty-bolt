(defpackage #:almighty-bolt-7guis/flight/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/flight/model
                #:date-text-valid-p
                #:return-date-enabled-p
                #:flight-valid-p
                #:booking-message)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value
                #:computed)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:column)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button
                #:text-field
                #:choice)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-flight))

(in-package #:almighty-bolt-7guis/flight/reactive)

;;; The flight booker, reactive: the kind of flight, the two dates and the message are
;;; signals; which fields are red, whether the return date can be used and whether Book can
;;; be pressed are worked out from them, where they are drawn, and are not state of their
;;; own.

(defparameter +first-date+ "27.03.2026")

(defun make-flight ()
  (let* ((kind (make-signal 0))
         (start (make-signal +first-date+))
         (return (make-signal +first-date+))
         (message (make-signal ""))
         (flight-kind (computed (if (zerop (value kind)) :one-way :return)))
         (bookable (computed (flight-valid-p (value flight-kind) (value start) (value return)))))
    (flet ((edit (signal)
             (lambda (text)
               (setf (value signal) text
                     (value message) ""))))
      (make-reactive-variant
       (lambda ()
         (let ((return-enabled (return-date-enabled-p (value flight-kind))))
           (padding (:all 24)
             (column (:gap 12)
               (choice '("one-way flight" "return flight") :id :kind :selected (value kind)
                                                           :on-change (lambda (index)
                                                                        (setf (value kind) index
                                                                              (value message) "")))
               (text-field :id :start :width 160 :value (value start)
                           :invalid (not (date-text-valid-p (value start)))
                           :on-change (edit start))
               (text-field :id :return :width 160 :value (value return)
                           :enabled return-enabled
                           :invalid (and return-enabled
                                         (not (date-text-valid-p (value return))))
                           :on-change (edit return))
               (button "Book" :id :book :enabled (and (value bookable) t)
                              :on-click (lambda ()
                                          (setf (value message)
                                                (booking-message (value flight-kind)
                                                                 (value start)
                                                                 (value return)))))
               (label (value message) :id :message)))))))))

(register-app :reactive :flight #'make-flight :width 460 :height 290)
