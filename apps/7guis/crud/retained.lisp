(defpackage #:almighty-bolt-7guis/crud/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/crud/model
                #:make-entries
                #:entry-id
                #:entry-name
                #:entry-surname
                #:entry-display
                #:entries-list
                #:entries-find
                #:entries-add
                #:entries-update
                #:entries-remove
                #:entries-filtered)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-enabled
                #:node-visible)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:button)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:text-field
                #:text-field-text
                #:checkbox
                #:checkbox-checked
                #:list-box
                #:list-box-items
                #:list-box-selected)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-crud))

(in-package #:almighty-bolt-7guis/crud/retained)

;;; CRUD, retained: the model is a value in a variable, and the widgets are built once.
;;; After every change one function rewrites the list from the model and the filter, and
;;; one decides which buttons can be pressed.

(defun make-crud ()
  (let ((entries (make-entries '("Hans" "Emil") '("Max" "Mustermann") '("Roman" "Tisch")))
        (selected nil)
        (filter nil) (filter-row nil) (filter-switch nil)
        (list nil) (name nil) (surname nil)
        (create nil) (update nil) (delete nil))
    (labels ((shown ()
               (entries-filtered entries (if (checkbox-checked filter-switch)
                                             (text-field-text filter)
                                             "")))
             (refresh-buttons ()
               (setf (node-enabled update) (and selected t)
                     (node-enabled delete) (and selected t)
                     (node-enabled create) (plusp (+ (length (text-field-text name))
                                                     (length (text-field-text surname))))))
             (refresh-list ()
               (let ((shown (shown)))
                 ;; A selection that the filter hides is dropped.
                 (unless (find selected shown :key #'entry-id)
                   (setf selected nil))
                 (setf (list-box-items list) (mapcar #'entry-display shown)
                       (list-box-selected list) (position selected shown :key #'entry-id))
                 (refresh-buttons)))
             (pick (index)
               (let ((entry (nth index (shown))))
                 (setf selected (entry-id entry)
                       (text-field-text name) (entry-name entry)
                       (text-field-text surname) (entry-surname entry))
                 (refresh-buttons)))
             (create-entry ()
               (multiple-value-bind (new entry)
                   (entries-add entries (text-field-text name) (text-field-text surname))
                 (setf entries new
                       selected (entry-id entry))
                 (refresh-list)))
             (update-entry ()
               (setf entries (entries-update entries selected (text-field-text name)
                                             (text-field-text surname)))
               (refresh-list))
             (delete-entry ()
               (setf entries (entries-remove entries selected)
                     selected nil
                     (text-field-text name) ""
                     (text-field-text surname) "")
               (refresh-list)))
      (setf filter (text-field :id :filter :width 160
                               :on-change (lambda (text) (declare (ignore text)) (refresh-list)))
            filter-row (row (:gap 10 :cross-align :center) (label "Filter prefix:") filter)
            filter-switch (checkbox "Filter" :id :filter-switch :checked t
                                             :on-change (lambda (on)
                                                          (setf (node-visible filter-row) on)
                                                          (refresh-list)))
            list (list-box :id :list :width 220 :rows 6 :on-select #'pick)
            name (text-field :id :name :width 160
                             :on-change (lambda (text) (declare (ignore text)) (refresh-buttons)))
            surname (text-field :id :surname :width 160
                                :on-change (lambda (text) (declare (ignore text)) (refresh-buttons)))
            create (button "Create" :id :create :on-click #'create-entry)
            update (button "Update" :id :update :on-click #'update-entry)
            delete (button "Delete" :id :delete :on-click #'delete-entry))
      (refresh-list)
      (make-retained-variant
       (padding (:all 20)
         (column (:gap 14)
           filter-switch
           filter-row
           (row (:gap 20)
             list
             (column (:gap 10)
               (row (:gap 10 :cross-align :center) (label "Name:") name)
               (row (:gap 10 :cross-align :center) (label "Surname:") surname)))
           (row (:gap 10) create update delete)))))))

(register-app :retained :crud #'make-crud :width 640 :height 320)
