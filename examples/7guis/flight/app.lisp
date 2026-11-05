(defpackage #:almighty-bolt-7guis/flight/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-7guis/flight/model)
                    (#:bolt #:almighty-bolt))
  (:export #:make-flight))

(in-package #:almighty-bolt-7guis/flight/app)

;;; The flight booker, immediate: the kind of flight, the two dates and the message are
;;; variables, and which fields are red, whether the return date can be used and whether Book
;;; can be pressed are worked out where the widgets are called, from them.

(defparameter +first-date+ "27.03.2026")

(defun make-flight ()
  (bolt:state-let ((kind 0)
                   (start +first-date+)
                   (return +first-date+)
                   (message ""))
    (lambda ()
      (let* ((flight-kind (if (zerop kind) :one-way :return))
             (return-enabled (model:return-date-enabled-p flight-kind)))
        (bolt:padding (:all 24)
          (bolt:column (:gap 12)
            (multiple-value-bind (index changed)
                (bolt:choice '("one-way flight" "return flight") kind :id :kind)
              (when changed
                (setf kind index
                      message "")))
            (multiple-value-bind (text changed)
                (bolt:text-field start :id :start :width 160 :invalid (not (model:date-text-valid-p start)))
              (when changed
                (setf start text
                      message "")))
            (multiple-value-bind (text changed)
                (bolt:text-field return :id :return :width 160 :enabled return-enabled
                                                :invalid (and return-enabled
                                                              (not (model:date-text-valid-p return))))
              (when changed
                (setf return text
                      message "")))
            (when (bolt:button "Book" :id :book
                                      :enabled (and (model:flight-valid-p flight-kind start return) t))
              (setf message (model:booking-message flight-kind start return)))
            (bolt:label message :id :message)))))))

(registry:register-app :flight 'make-flight :width 460 :height 290)
