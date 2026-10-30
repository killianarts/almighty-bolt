(defpackage #:almighty-bolt-examples/todo
  (:use #:cl #:almighty-bolt)
  (:export #:make-todo))

(in-package #:almighty-bolt-examples/todo)

;;; A list of things to do: a field to write one in, a button to add it, a check box for each, and
;;; how many are left. The things are values that are replaced and never changed, and each check box
;;; is given the thing's own number as its id, so that the keyboard stays with a thing when the list
;;; changes under it.
;;;
;;;   (open-app 'make-todo :title "To do" :width 360 :height 320)

(defstruct (thing (:constructor make-thing (id text done)))
  (id 0 :read-only t)
  (text "" :read-only t)
  (done nil :read-only t))

(defun make-todo ()
  (kept-let ((things (list (make-thing 1 "Read the README" t)
                           (make-thing 2 "Open a window from the REPL" nil)
                           (make-thing 3 "Change a label and compile it" nil)))
             (next-id 4)
             (draft ""))
    (flet ((add ()
             (when (plusp (length draft))
               (setf things (append things (list (make-thing next-id draft nil))))
               (incf next-id)
               (setf draft "")))
           (toggle (thing done)
             (setf things (mapcar (lambda (each)
                                    (if (eq each thing)
                                        (make-thing (thing-id thing) (thing-text thing) done)
                                        each))
                                  things))))
      (lambda ()
        (padding (:all 20)
          (column (:gap 12 :cross-align :stretch)
            (row (:gap 10 :cross-align :center)
              (multiple-value-bind (text changed submitted) (text-field draft :id :draft :width 240)
                (when changed
                  (setf draft text))
                (when submitted
                  (add)))
              (when (button "Add" :id :add)
                (add)))
            (dolist (thing things)
              (multiple-value-bind (done changed)
                  (checkbox (thing-text thing) (thing-done thing) :id (thing-id thing))
                (when changed
                  (toggle thing done))))
            (label (format nil "~d left" (count nil things :key #'thing-done)) :id :left)))))))
