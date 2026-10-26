(defpackage #:almighty-bolt-7guis/crud/mcclim
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/crud/model
                #:make-entries
                #:entry-id
                #:entry-name
                #:entry-surname
                #:entry-display
                #:entries-add
                #:entries-update
                #:entries-remove
                #:entries-filtered)
  (:import-from #:clim
                #:define-application-frame
                #:make-application-frame
                #:run-frame-top-level
                #:define-presentation-type
                #:define-presentation-method
                #:define-presentation-to-command-translator
                #:present
                #:accept
                #:textual-view
                #:updating-output
                #:with-text-face
                #:*application-frame*
                #:accept-values-command-button
                #:+textual-dialog-view+
                #:vertically)
  (:import-from #:clime
                #:accept-values-pane
                #:accept-values-pane-displayer)
  (:export #:make-crud-frame
           #:run-crud))

(in-package #:almighty-bolt-7guis/crud/mcclim)

;;; CRUD, McCLIM: the entries are presented, each as an object of the type ENTRY, in a pane that
;;; redisplays only the rows that changed (UPDATING-OUTPUT, kept by the entry's id), and a click on
;;; one is a translator to the command COM-PICK. The filter and the two fields are in an
;;; ACCEPT-VALUES pane, which keeps what is typed in the frame's slots; Create, Update and Delete
;;; are its command buttons, and are not offered when they could not be used.

(define-presentation-type entry ())

(define-presentation-method present (entry (type entry) stream (view textual-view) &key)
  (write-string (entry-display entry) stream))

(define-application-frame crud-frame ()
  ((entries :initform (make-entries '("Hans" "Emil") '("Max" "Mustermann") '("Roman" "Tisch")))
   (filter :initform "")
   (picked :initform nil)
   (name :initform "")
   (surname :initform ""))
  (:panes (form :accept-values :display-time :command-loop
                :display-function '(accept-values-pane-displayer :displayer show-form
                                                                 :resynchronize-every-pass t))
          (listing :application :display-function 'show-entries :incremental-redisplay t
                             :scroll-bars :vertical :width 220 :height 150))
  (:layouts (default (vertically () form listing)))
  (:command-table (crud-frame :inherit-from (accept-values-pane)))
  (:default-initargs :width 640 :height 280))

(defun picked-entry (frame)
  "The entry that is picked and shows: a filter that hides it unpicks it."
  (with-slots (entries filter picked) frame
    (find picked (entries-filtered entries filter) :key #'entry-id)))

(defmethod show-form ((frame crud-frame) stream)
  (with-slots (entries filter picked name surname) frame
    (setf filter (accept 'string :prompt "Filter prefix" :default filter :stream stream
                                 :view +textual-dialog-view+))
    (fresh-line stream)
    ;; A field that is already on the screen keeps what is in it whatever default it is given, so a
    ;; field is a new one, with the new default, for each entry that is picked.
    (setf name (accept 'string :prompt "Name" :query-identifier (list "Name" picked) :default name
                               :stream stream :view +textual-dialog-view+))
    (fresh-line stream)
    (setf surname (accept 'string :prompt "Surname" :query-identifier (list "Surname" picked)
                                  :default surname :stream stream :view +textual-dialog-view+))
    (fresh-line stream)
    (when (plusp (+ (length name) (length surname)))
      (accept-values-command-button (stream :resynchronize t) "Create"
        (multiple-value-bind (new entry) (entries-add entries name surname)
          (setf entries new
                picked (entry-id entry)))))
    (when (picked-entry frame)
      (accept-values-command-button (stream :resynchronize t) "Update"
        (setf entries (entries-update entries picked name surname)))
      (accept-values-command-button (stream :resynchronize t) "Delete"
        (setf entries (entries-remove entries picked)
              picked nil
              name ""
              surname "")))))

(defun show-entries (frame stream)
  (with-slots (entries filter picked) frame
    (dolist (entry (entries-filtered entries filter))
      (updating-output (stream :unique-id (entry-id entry)
                               :cache-value (list entry (eql (entry-id entry) picked)))
        (if (eql (entry-id entry) picked)
            (with-text-face (stream :bold)
              (present entry 'entry :stream stream))
            (present entry 'entry :stream stream))
        (terpri stream)))))

(define-crud-frame-command (com-pick) ((entry 'entry))
  (with-slots (picked name surname) *application-frame*
    (setf picked (entry-id entry)
          name (entry-name entry)
          surname (entry-surname entry))))

(define-presentation-to-command-translator pick-entry (entry com-pick crud-frame :gesture :select)
    (object)
  (list object))

(defun make-crud-frame (&rest initargs)
  (apply #'make-application-frame 'crud-frame initargs))

(defun run-crud (&rest initargs)
  (run-frame-top-level (apply #'make-crud-frame initargs)))
