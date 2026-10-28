(defpackage #:almighty-bolt-7guis/crud/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:kept-let)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
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
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:label
                #:button
                #:padding
                #:column
                #:row
                #:text-field
                #:list-box)
  (:import-from #:almighty-bolt-variant-immediate/immediate
                #:make-immediate-variant)
  (:export #:make-crud))

(in-package #:almighty-bolt-7guis/crud/immediate)

;;; CRUD, immediate: the entries, the filter, the picked entry and the two fields are
;;; variables, kept when the app is made again. The list that shows is worked out at the top of
;;; every frame, and the buttons say whether they were pressed where they are described.

(defun make-crud ()
  (kept-let ((entries (make-entries '("Hans" "Emil") '("Max" "Mustermann") '("Roman" "Tisch")))
             (filter "")
             (picked nil)
             (name "")
             (surname ""))
    (make-immediate-variant
     (lambda ()
       (let* ((shown (entries-filtered entries filter))
              (picked-entry (find picked shown :key #'entry-id)))
         (padding (:all 20)
           (column (:gap 14)
             (row (:gap 10 :cross-align :center)
               (label "Filter prefix:")
               (multiple-value-bind (text changed) (text-field :filter filter :width 160)
                 (when changed
                   (setf filter text))))
             (row (:gap 20)
               (multiple-value-bind (index changed)
                   (list-box :list (mapcar #'entry-display shown)
                             (position picked shown :key #'entry-id) :width 220 :rows 6)
                 (when changed
                   (let ((entry (nth index shown)))
                     (setf picked (entry-id entry)
                           name (entry-name entry)
                           surname (entry-surname entry)))))
               (column (:gap 10)
                 (row (:gap 10 :cross-align :center)
                   (label "Name:")
                   (multiple-value-bind (text changed) (text-field :name name :width 160)
                     (when changed
                       (setf name text))))
                 (row (:gap 10 :cross-align :center)
                   (label "Surname:")
                   (multiple-value-bind (text changed) (text-field :surname surname :width 160)
                     (when changed
                       (setf surname text))))))
             (row (:gap 10)
               (when (button "Create" :id :create
                                      :enabled (plusp (+ (length name) (length surname))))
                 (multiple-value-bind (new entry) (entries-add entries name surname)
                   (setf entries new
                         picked (entry-id entry))))
               (when (button "Update" :id :update :enabled (and picked-entry t))
                 (setf entries (entries-update entries picked name surname)))
               (when (button "Delete" :id :delete :enabled (and picked-entry t))
                 (setf entries (entries-remove entries picked)
                       picked nil
                       name ""
                       surname ""))))))))))

(register-app :immediate :crud 'make-crud :width 640 :height 280)
