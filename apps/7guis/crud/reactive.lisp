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
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button
                #:padding
                #:column
                #:row
                #:text-field
                #:choice
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
         (filter-index (make-signal 0))
         (picked (make-signal nil))
         (name (make-signal ""))
         (surname (make-signal ""))
         (shown (computed (if (zerop (value filter-index))
                              (entries-filtered (value entries) (value filter))
                              (entries-list (value entries)))))
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
         (padding
          (column (choice '("filter on" "filter off") :id :filter-switch
                                                      :selected (value filter-index)
                                                      :on-change (lambda (index)
                                                                   (setf (value filter-index) index)))
                  (when (zerop (value filter-index))
                    (row (label "Filter prefix:")
                         (text-field :id :filter :width 160 :value (value filter)
                                     :on-change (lambda (text) (setf (value filter) text)))
                         :gap 10 :cross-align :center))
                  (row (list-box :id :list :width 220 :rows 6
                                 :items (mapcar #'entry-display (value shown))
                                 :selected (position (value picked) (value shown) :key #'entry-id)
                                 :on-select #'pick)
                       (column (row (label "Name:")
                                    (text-field :id :name :width 160 :value (value name)
                                                :on-change (lambda (text) (setf (value name) text)))
                                    :gap 10 :cross-align :center)
                               (row (label "Surname:")
                                    (text-field :id :surname :width 160 :value (value surname)
                                                :on-change (lambda (text)
                                                             (setf (value surname) text)))
                                    :gap 10 :cross-align :center)
                               :gap 10)
                       :gap 20)
                  (row (button "Create" :id :create :on-click #'create
                                        :enabled (plusp (+ (length (value name))
                                                           (length (value surname)))))
                       (button "Update" :id :update :on-click #'update
                                        :enabled (and (value picked-entry) t))
                       (button "Delete" :id :delete :on-click #'delete-picked
                                        :enabled (and (value picked-entry) t))
                       :gap 10)
                  :gap 14)
          :all 20))))))

(register-app :reactive :crud #'make-crud :width 640 :height 322)
