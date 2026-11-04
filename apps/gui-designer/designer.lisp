(defpackage #:almighty-bolt-gui-designer
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:import-from #:almighty-bolt-gui-designer/view
                #:make-node
                #:node-kind
                #:node-id
                #:node-children
                #:form-node
                #:node-form
                #:find-by-id
                #:parent-of
                #:all-ids
                #:fresh-id
                #:add-node
                #:remove-node
                #:with-option
                #:with-id)
  (:import-from #:almighty-bolt-gui-designer/catalogue
                #:+catalogue+
                #:entry-kind
                #:entry-title
                #:entry-options
                #:entry-container-p
                #:option-name
                #:option-type
                #:option-default
                #:find-entry
                #:node-option
                #:draw-node)
  (:export #:make-designer
           #:open-designer
           #:write-view
           #:read-view))

(in-package #:almighty-bolt-gui-designer)

;;; A GUI designer: the widgets it knows and an outline of the view on the left, the view being
;;; designed in the middle, drawn with the real widgets but not working (a press there selects what
;;; it was on, as a row of the outline does), and the selected widget's id and options on the right,
;;; to change. A widget from the left goes into the selected
;;; container, or after the selected widget. The view is saved as its form, in a file of its own.
;;; It uses the public package only.
;;;
;;;   (almighty-bolt-gui-designer:open-designer)

(defparameter +sample+
  '(:column :sign-in-form (:gap 12)
    (:label :heading (:text "Sign in" :size 22 :weight :bold))
    (:label :name-label (:text "Name"))
    (:text-field :name ())
    (:label :password-label (:text "Password"))
    (:text-field :password ())
    (:row :actions (:gap 8)
     (:button :sign-in (:text "Sign in"))
     (:button :forgot (:text "Forgot your password?"))))
  "The view the designer opens with, as a form (see FORM-NODE).")

(defparameter +empty+ '(:column :view (:gap 8))
  "A new view: an empty column.")

;;; --- the file ---

(defun write-view (view stream)
  "Write VIEW to STREAM as the file the designer saves: its form, readable in any package."
  (format stream ";;; A view made with the GUI designer of almighty-bolt: ~
                  (kind id (option value ...) what-is-in-it ...).~%")
  (with-standard-io-syntax
    (let ((*package* (find-package :keyword))
          (*print-case* :downcase)
          (*print-pretty* t)
          (*print-right-margin* 100))
      (prin1 (node-form view) stream)
      (terpri stream))))

(defun read-view (stream)
  "The view in STREAM, a file the designer saved. An error if it is not one."
  (let ((form (with-standard-io-syntax
                (let ((*read-eval* nil)
                      (*package* (find-package :keyword)))
                  (read stream)))))
    (labels ((check (form)
               (unless (and (consp form) (keywordp (first form)) (keywordp (second form))
                            (listp (third form)))
                 (error "~s is not a widget of a view." form))
               (find-entry (first form))
               (mapc #'check (cdddr form))))
      (check form))
    (form-node form)))

;;; --- pieces of the screen ---

(defun id (&rest parts)
  "A keyword made of PARTS."
  (intern (string-upcase (format nil "~{~a~^-~}" parts)) :keyword))

(defun show-value (value)
  (let ((*print-case* :downcase))
    (if (stringp value) value (prin1-to-string value))))

(defun id-text-p (text)
  "True if TEXT can be an id: letters, digits and dashes, starting with a letter."
  (and (plusp (length text))
       (alpha-char-p (char text 0))
       (every (lambda (c) (or (alphanumericp c) (char= c #\-))) text)))

(defun number-text (text)
  "The number TEXT says, or NIL."
  (let ((value (ignore-errors (with-standard-io-syntax
                                (let ((*read-eval* nil)) (read-from-string text))))))
    (and (realp value) value)))

(defun palette ()
  "The widgets that can be added. Returns the kind of the one pressed, or NIL."
  (let ((pressed nil))
    (bolt:column (:id :palette :gap 8 :cross-align :start)
      (bolt:label "Add" :id :palette-heading :size 17)
      (dolist (entry +catalogue+)
        (when (bolt:button (entry-title entry) :id (id "add" (entry-kind entry)))
          (setf pressed (entry-kind entry)))))
    pressed))

(defun outline-rows (view)
  "The widgets of VIEW in order, as (id . what the outline says of it): its kind and id, indented
by how deep it is."
  (let ((rows '()))
    (labels ((walk (node depth)
               (push (cons (node-id node)
                           (format nil "~a~a ~(~s~)" (make-string (* 3 depth) :initial-element #\Space)
                                   (entry-title (find-entry (node-kind node))) (node-id node)))
                     rows)
               (dolist (child (node-children node))
                 (walk child (1+ depth)))))
      (walk view 0))
    (nreverse rows)))

(defun outline (view selected)
  "The view's widgets as a list, the selected one chosen in it. Returns the id of the one chosen
there, on the frame it is, or NIL."
  (let ((rows (outline-rows view))
        (chosen nil))
    (bolt:column (:id :outline :gap 8)
      (bolt:label "Outline" :id :outline-heading :size 17)
      (multiple-value-bind (index moved)
          (bolt:list-box (mapcar #'cdr rows) (position selected rows :key #'car)
                         :id :outline-list :rows 12 :width 260
                         :labelled-by :outline-heading)
        (when (and moved index)
          (setf chosen (car (nth index rows))))))
    chosen))

(defun option-editor (node option drafts)
  "An editor of NODE's OPTION, of the option's type. Returns the new value if it was changed, and
true as a second value."
  (let* ((name (option-name option))
         (value (node-option node name))
         (editor (id "option" name))
         (key (cons (node-id node) name))
         (result nil)
         (changed nil))
    (bolt:row (:id (id "option" name "row") :gap 8 :cross-align :center)
      ;; A check box says its own name.
      (unless (eq (option-type option) :boolean)
        (bolt:label (string-downcase (string name)) :id (id "option" name "label")))
      (let ((type (option-type option)))
        (cond ((eq type :string)
               (multiple-value-bind (text edited)
                   (bolt:text-field value :id editor :width 200
                                          :labelled-by (id "option" name "label"))
                 (when edited (setf result text changed t))))
              ((eq type :number)
               (let ((shown (gethash key drafts (princ-to-string value))))
                 (multiple-value-bind (text edited)
                     (bolt:text-field shown :id editor :width 80 :invalid (null (number-text shown))
                                            :labelled-by (id "option" name "label"))
                   (when edited
                     (setf (gethash key drafts) text)
                     (let ((number (number-text text)))
                       (when number (setf result number changed t)))))))
              ((eq type :boolean)
               (multiple-value-bind (on moved)
                   (bolt:checkbox (string-downcase (string name)) value :id editor)
                 (when moved (setf result on changed t))))
              ((and (consp type) (eq (first type) :choice))
               (let ((choices (rest type)))
                 (multiple-value-bind (index moved)
                     (bolt:dropdown (mapcar #'show-value choices) (position value choices)
                                    :id editor :labelled-by (id "option" name "label"))
                   (when moved (setf result (nth index choices) changed t))))))))
    (values result changed)))

(defun selection (view selected drafts)
  "The selected node's kind, its id and its options, to change. Returns the view as the changes
leave it, and the selection (which an id changed changes)."
  (bolt:panel (:id :selection :all 14 :radius 0)
    (bolt:column (:gap 8 :min-width 300)
      (bolt:label "Selected" :id :selection-heading :size 17)
      (let ((node (and selected (find-by-id view selected))))
        (if (null node)
            (bolt:paragraph "Nothing. Press a widget in the view to select it."
                            :id :selected-nothing)
            (let ((entry (find-entry (node-kind node))))
              (bolt:label (format nil "~a ~(~s~)" (entry-title entry) (node-id node))
                          :id :selected-what :weight :bold)
              ;; The id: what the app's code will know it by, so one no other widget has. It is
              ;; changed when Return is pressed, and not as it is typed, which would make an id of
              ;; every start of the one wanted (and stop at one that another widget has).
              (bolt:row (:id :id-row :gap 8 :cross-align :center)
                (bolt:label "id" :id :id-label)
                (let* ((key (cons (node-id node) :id))
                       (shown (gethash key drafts (string-downcase (string (node-id node)))))
                       (wanted (and (id-text-p shown) (intern (string-upcase shown) :keyword)))
                       (free (and wanted (or (eq wanted (node-id node))
                                             (not (member wanted (all-ids view)))))))
                  (multiple-value-bind (text edited submitted)
                      (bolt:text-field shown :id :option-id :width 200 :invalid (not free)
                                             :labelled-by :id-label)
                    (when edited
                      (setf (gethash key drafts) text))
                    (when (and submitted free (not (eq wanted (node-id node))))
                      (remhash key drafts)
                      (setf view (with-id view (node-id node) wanted)
                            selected wanted
                            node (find-by-id view wanted)))))
                (bolt:label "Return renames" :id :id-hint :color :dim-text))
              (dolist (option (entry-options entry))
                (multiple-value-bind (value changed) (option-editor node option drafts)
                  (when changed
                    (setf view (with-option view (node-id node) (option-name option) value
                                            (option-default option))
                          node (find-by-id view (node-id node)))))))))))
    (values view selected))

(defun added (view selected kind)
  "VIEW with a new widget of KIND: at the end of the selected container, after the selected widget,
or at the end of the view if nothing is selected. Returns the view and the new widget's id."
  (let* ((new (make-node kind (fresh-id view kind)))
         (node (and selected (find-by-id view selected))))
    (values (cond ((null node)
                   (add-node view new :into (node-id view)))
                  ((entry-container-p (find-entry (node-kind node)))
                   (add-node view new :into selected))
                  (t (add-node view new :after selected)))
            (node-id new))))

(defun make-designer ()
  (bolt:state-let ((view (form-node +sample+))
                   (selected nil)
                   (drafts (make-hash-table :test 'equal))
                   (saving nil)
                   (opening nil)
                   (status ""))
    (lambda ()
      (flet ((select (id)
               (unless (eq id selected)
                 (clrhash drafts)
                 (setf selected id))))
        (bolt:column (:id :designer :fill t :cross-align :stretch)
          (bolt:panel (:id :toolbar :all 10 :radius 0)
            (bolt:row (:gap 8 :cross-align :center)
              (when (bolt:button "New" :id :new)
                (setf view (form-node +empty+) status "")
                (select nil))
              (when (bolt:button "Open…" :id :open)
                (setf opening t))
              (when (bolt:button "Save…" :id :save)
                (setf saving t))
              (when (bolt:button "Remove" :id :remove
                                          :enabled (and selected (not (eq selected (node-id view)))
                                                        t))
                (setf view (remove-node view selected))
                (select nil))
              (bolt:label status :id :status)))
          (let ((path (bolt:save-file-dialog :id :save-panel :show saving :types '("view")
                                             :name (format nil "~(~a~).view" (node-id view)))))
            (setf saving nil)
            (when path
              (with-open-file (out path :direction :output :if-exists :supersede)
                (write-view view out))
              (setf status (format nil "Saved to ~a." path))))
          (let ((path (bolt:open-file-dialog :id :open-panel :show opening :types '("view"))))
            (setf opening nil)
            (when path
              (handler-case
                  (with-open-file (in path)
                    (setf view (read-view in)
                          status (format nil "Opened ~a." path))
                    (select nil))
                (error (condition)
                  (setf status (format nil "Could not open ~a: ~a" path condition))))))
          (bolt:row (:id :panes :flex 1 :cross-align :stretch)
            (bolt:panel (:id :left :all 14 :radius 0)
              (bolt:column (:gap 20)
                (let ((kind (palette)))
                  (when kind
                    (multiple-value-bind (new id) (added view selected kind)
                      (setf view new)
                      (select id))))
                (let ((chosen (outline view selected)))
                  (when chosen
                    (select chosen)))))
            (bolt:panel (:id :canvas-back :color :background :radius 0 :flex 1 :all 32)
              (let ((pressed (bolt:design-surface (:id :canvas :selected selected)
                               (draw-node view))))
                (when pressed
                  (select pressed))))
            (multiple-value-bind (new now) (selection view selected drafts)
              (setf view new)
              (unless (eq now selected)
                (select now)))))))))

(defun open-designer ()
  "Open the GUI designer in a window, from the REPL."
  (bolt:open-app 'make-designer :title "GUI designer" :width 1180 :height 680))
