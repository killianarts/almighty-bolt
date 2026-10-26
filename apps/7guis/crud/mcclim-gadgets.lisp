(defpackage #:almighty-bolt-7guis/crud/mcclim-gadgets
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
                #:*application-frame*
                #:define-application-frame
                #:make-application-frame
                #:run-frame-top-level
                #:find-pane-named
                #:vertically
                #:horizontally
                #:labelling
                #:scrolling
                #:list-pane
                #:text-field
                #:push-button
                #:gadget-value
                #:activate-gadget
                #:deactivate-gadget)
  (:import-from #:clime
                #:list-pane-items)
  (:export #:make-crud-frame
           #:run-crud))

(in-package #:almighty-bolt-7guis/crud/mcclim-gadgets)

;;; CRUD, McCLIM again but with its gadgets: a list pane, three text fields and three buttons, each
;;; with a callback, and REFRESH, which puts the state into all of them after every change. The same
;;; library in the other architecture.

(define-application-frame crud-frame ()
  ((entries :initform (make-entries '("Hans" "Emil") '("Max" "Mustermann") '("Roman" "Tisch")))
   (picked :initform nil))
  (:panes (filter text-field :value "" :value-changed-callback 'on-filter)
          (entries-list list-pane :mode :exclusive :name-key 'entry-display :value-key 'entry-id
                                  :value-changed-callback 'on-pick :visible-items 6
                                  :items (coerce (entries-filtered
                                                  (slot-value *application-frame* 'entries) "")
                                                 'vector))
          (name text-field :value "" :value-changed-callback 'on-field)
          (surname text-field :value "" :value-changed-callback 'on-field)
          (create push-button :label "Create" :activate-callback 'on-create :active nil)
          (update push-button :label "Update" :activate-callback 'on-update :active nil)
          (delete push-button :label "Delete" :activate-callback 'on-delete :active nil))
  (:layouts (default (vertically (:spacing 12)
                       (labelling (:label "Filter prefix") filter)
                       (horizontally (:spacing 20)
                         (scrolling (:width 220 :height 150) entries-list)
                         (vertically (:spacing 10)
                           (labelling (:label "Name") name)
                           (labelling (:label "Surname") surname)))
                       (horizontally (:spacing 10) create update delete))))
  (:default-initargs :width 640 :height 280))

(defun pane (frame name)
  (find-pane-named frame name))

(defun shown (frame)
  (with-slots (entries) frame
    (entries-filtered entries (gadget-value (pane frame 'filter)))))

(defun refresh (frame)
  "Put the state into the list, the selection and the buttons. (McCLIM calls the callbacks of a text
field while the panes are being made, when the others are not there yet: then nothing is done.)"
  (when (every (lambda (name) (pane frame name))
               '(filter entries-list name surname create update delete))
    (with-slots (picked) frame
      (let ((shown (shown frame))
            (list (pane frame 'entries-list)))
        (setf (list-pane-items list) (coerce shown 'vector))
        (unless (find picked shown :key #'entry-id)
          (setf picked nil))
        (setf (gadget-value list) picked)
        (flet ((enable (name on)
                 (if on
                     (activate-gadget (pane frame name))
                     (deactivate-gadget (pane frame name)))))
          (enable 'create (plusp (+ (length (gadget-value (pane frame 'name)))
                                    (length (gadget-value (pane frame 'surname))))))
          (enable 'update picked)
          (enable 'delete picked))))))

(defun on-filter (gadget value)
  (declare (ignore value))
  (refresh (clim:pane-frame gadget)))

(defun on-field (gadget value)
  (declare (ignore value))
  (refresh (clim:pane-frame gadget)))

(defun on-pick (gadget id)
  (let ((frame (clim:pane-frame gadget)))
    (with-slots (entries picked) frame
      (setf picked id)
      (let ((entry (find id (entries-filtered entries "") :key #'entry-id)))
        (when entry
          (setf (gadget-value (pane frame 'name)) (entry-name entry)
                (gadget-value (pane frame 'surname)) (entry-surname entry))))
      (refresh frame))))

(defun on-create (button)
  (let ((frame (clim:pane-frame button)))
    (with-slots (entries picked) frame
      (multiple-value-bind (new entry)
          (entries-add entries (gadget-value (pane frame 'name))
                       (gadget-value (pane frame 'surname)))
        (setf entries new
              picked (entry-id entry))
        (refresh frame)))))

(defun on-update (button)
  (let ((frame (clim:pane-frame button)))
    (with-slots (entries picked) frame
      (setf entries (entries-update entries picked (gadget-value (pane frame 'name))
                                    (gadget-value (pane frame 'surname))))
      (refresh frame))))

(defun on-delete (button)
  (let ((frame (clim:pane-frame button)))
    (with-slots (entries picked) frame
      (setf entries (entries-remove entries picked)
            picked nil
            (gadget-value (pane frame 'name)) ""
            (gadget-value (pane frame 'surname)) "")
      (refresh frame))))

(defun make-crud-frame (&rest initargs)
  (apply #'make-application-frame 'crud-frame initargs))

(defun run-crud (&rest initargs)
  (run-frame-top-level (apply #'make-crud-frame initargs)))
