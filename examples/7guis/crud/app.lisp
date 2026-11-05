(defpackage #:almighty-bolt-7guis/crud/app
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-7guis/crud/model))
  (:export #:make-crud))

(in-package #:almighty-bolt-7guis/crud/app)

;;; CRUD, immediate: the entries, the filter, the picked entry and the two fields are
;;; variables, kept when the app is made again. The list that shows is worked out at the top of
;;; every frame, and the buttons say whether they were pressed where they are described.

(defun make-crud ()
  (bolt:state-let ((entries (model:make-entries '("Hans" "Emil") '("Max" "Mustermann") '("Roman" "Tisch")))
                   (filter "")
                   (picked nil)
                   (name "")
                   (surname ""))
    (lambda ()
      (let* ((shown (model:entries-filtered entries filter))
             (picked-entry (find picked shown :key #'model:entry-id)))
        (bolt:padding (:all 20)
          (bolt:column (:gap 14)
            (bolt:row (:gap 10 :cross-align :center)
              (bolt:label "Filter prefix:" :id :filter-label)
              (multiple-value-bind (text changed)
                       (bolt:text-field filter :id :filter :labelled-by :filter-label :width 160)
                (when changed
                       (setf filter text))))
            (bolt:row (:gap 20)
              (multiple-value-bind (index changed)
                       (bolt:list-box (mapcar #'model:entry-display shown)
                                      (position picked shown :key #'model:entry-id) :id :list :width 220 :rows 6)
                (when changed
                       (let ((entry (nth index shown)))
                    (setf picked (model:entry-id entry)
                          name (model:entry-name entry)
                          surname (model:entry-surname entry)))))
              (bolt:column (:gap 10)
                (bolt:row (:gap 10 :cross-align :center)
                       (bolt:label "Name:" :id :name-label)
                       (multiple-value-bind (text changed)
                           (bolt:text-field name :id :name :labelled-by :name-label :width 160)
                    (when changed
                           (setf name text))))
                (bolt:row (:gap 10 :cross-align :center)
                       (bolt:label "Surname:" :id :surname-label)
                       (multiple-value-bind (text changed)
                           (bolt:text-field surname :id :surname :labelled-by :surname-label :width 160)
                    (when changed
                           (setf surname text))))))
            (bolt:row (:gap 10)
              (when (bolt:button "Create" :id :create
                                          :enabled (plusp (+ (length name) (length surname))))
                (multiple-value-bind (new entry) (model:entries-add entries name surname)
                       (setf entries new
                             picked (model:entry-id entry))))
              (when (bolt:button "Update" :id :update :enabled (and picked-entry t))
                (setf entries (model:entries-update entries picked name surname)))
              (when (bolt:button "Delete" :id :delete :enabled (and picked-entry t))
                (setf entries (model:entries-remove entries picked)
                      picked nil
                      name ""
                      surname "")))))))))

(registry:register-app :crud 'make-crud :width 640 :height 280)
