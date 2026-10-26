(defpackage #:almighty-bolt-7guis/crud/model
  (:use #:cl)
  (:export #:entry
           #:entry-id
           #:entry-name
           #:entry-surname
           #:entry-email
           #:entry-display
           #:entries
           #:make-entries
           #:entries-list
           #:entries-find
           #:entries-add
           #:entries-update
           #:entries-remove
           #:entries-filtered))

(in-package #:almighty-bolt-7guis/crud/model)

;;; The CRUD task's data: a list of people with names, surnames and emails, and the operations on
;;; it, all as values. What is selected and what is typed in the fields is the UI's state,
;;; which is what the variants differ in, and is not here.

(defstruct (entry (:constructor make-entry (id name surname email)) (:copier nil))
  (id 0 :type fixnum :read-only t)
  (name "" :type string :read-only t)
  (surname "" :type string :read-only t)
  (email "" :type string :read-only t))

(defun entry-display (entry)
  "How the list shows ENTRY: Surname, Name."
  (format nil "~a, ~a" (entry-surname entry) (entry-name entry)))

(defstruct (entries (:constructor %make-entries (list next-id)) (:copier nil))
  (list '() :type list :read-only t)
  (next-id 0 :type fixnum :read-only t))

(defun entries-add (entries name surname email)
  "ENTRIES with a new entry at the end, and the entry, as two values."
  (let ((entry (make-entry (entries-next-id entries) name surname email)))
    (values (%make-entries (append (entries-list entries) (list entry))
                           (1+ (entries-next-id entries)))
            entry)))

(defun make-entries (&rest pairs)
  "A list of entries from PAIRS, each (name surname)."
  (let ((entries (%make-entries '() 0)))
    (dolist (pair pairs entries)
      (setf entries (entries-add entries (first pair) (second pair) "")))))

(defun entries-find (entries id)
  (find id (entries-list entries) :key #'entry-id))

(defun entries-update (entries id name surname email)
  "ENTRIES with the entry ID given NAME, SURNAME and EMAIL, in the same place."
  (%make-entries (mapcar (lambda (entry)
                           (if (= (entry-id entry) id)
                               (make-entry id name surname email)
                               entry))
                         (entries-list entries))
                 (entries-next-id entries)))

(defun entries-remove (entries id)
  (%make-entries (remove id (entries-list entries) :key #'entry-id)
                 (entries-next-id entries)))

(defun entries-filtered (entries prefix)
  "The entries whose surname starts with PREFIX, ignoring case, in order. An empty
prefix keeps all of them."
  (if (zerop (length prefix))
      (entries-list entries)
      (remove-if-not (lambda (entry)
                       (let ((surname (entry-surname entry)))
                         (and (>= (length surname) (length prefix))
                              (string-equal prefix surname :end2 (length prefix)))))
                     (entries-list entries))))
