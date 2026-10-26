(defpackage #:almighty-bolt-7guis/crud/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/crud/model
                #:make-entries
                #:entry-id
                #:entry-name
                #:entry-surname
                #:entry-display
                #:entries-find
                #:entries-add
                #:entries-update
                #:entries-remove
                #:entries-filtered)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value
                #:computed)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button
                #:text-field
                #:list-box)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-crud))

(in-package #:almighty-bolt-7guis/crud/reactive)

;;; CRUD, reactive: the entries, the filter, the picked entry and the two fields are
;;; signals. The list that shows, and whether the picked entry is among them, are worked
;;; out from them, and the screen is described from those.

(defun make-crud ()
  (let* ((entries (make-signal (make-entries '("Hans" "Emil") '("Max" "Mustermann")
                                             '("Roman" "Tisch"))))
         (filter (make-signal ""))
         (picked (make-signal nil))
         (name (make-signal ""))
         (surname (make-signal ""))
         (shown (computed (entries-filtered (value entries) (value filter))))
         (picked-entry (computed (find (value picked) (value shown) :key #'entry-id))))
    (flet ((create ()
             (multiple-value-bind (new entry) (entries-add (value entries) (value name) (value surname))
               (setf (value entries) new
                     (value picked) (entry-id entry))))
           (update ()
             (setf (value entries) (entries-update (value entries) (value picked) (value name)
                                                   (value surname))))
           (delete-picked ()
             (setf (value entries) (entries-remove (value entries) (value picked))
                   (value picked) nil
                   (value name) ""
                   (value surname) ""))
           (pick (index)
             (let ((entry (nth index (value shown))))
               (setf (value picked) (entry-id entry)
                     (value name) (entry-name entry)
                     (value surname) (entry-surname entry)))))
      (make-reactive-variant
       (lambda ()
         (padding (:all 20)
           (column (:gap 14)
             (row (:gap 10 :cross-align :center)
               (label "Filter prefix:")
               (text-field :id :filter :width 160 :value (value filter)
                           :on-change (lambda (text) (setf (value filter) text))))
             (row (:gap 20)
               (list-box :id :list :width 220 :rows 6
                         :items (mapcar #'entry-display (value shown))
                         :selected (position (value picked) (value shown) :key #'entry-id)
                         :on-select #'pick)
               (column (:gap 10)
                 (row (:gap 10 :cross-align :center)
                   (label "Name:")
                   (text-field :id :name :width 160 :value (value name)
                               :on-change (lambda (text) (setf (value name) text))))
                 (row (:gap 10 :cross-align :center)
                   (label "Surname:")
                   (text-field :id :surname :width 160 :value (value surname)
                               :on-change (lambda (text)
                                            (setf (value surname) text))))))
             (row (:gap 10)
               (button "Create" :id :create :on-click #'create
                                :enabled (plusp (+ (length (value name))
                                                   (length (value surname)))))
               (button "Update" :id :update :on-click #'update
                                :enabled (and (value picked-entry) t))
               (button "Delete" :id :delete :on-click #'delete-picked
                                :enabled (and (value picked-entry) t))))))))))

(register-app :reactive :crud #'make-crud :width 640 :height 280)
