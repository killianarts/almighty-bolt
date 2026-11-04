(defpackage #:almighty-bolt-gui-designer
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:import-from #:almighty-bolt-gui-designer/view
                #:make-node
                #:node-kind
                #:node-id
                #:node-options
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
  (:import-from #:almighty-bolt/ui/catalogue
                #:catalogue
                #:entry-kind
                #:entry-title
                #:entry-options
                #:entry-container-p
                #:option-name
                #:option-type
                #:option-default
                #:find-entry
                #:check-view-form
                #:form-option
                #:draw-form)
  (:import-from #:almighty-bolt/ui/views
                #:set-view)
  (:export #:make-designer
           #:open-designer
           #:write-view
           #:read-view
           #:wiring-text))

(in-package #:almighty-bolt-gui-designer)

;;; A GUI designer: the widgets it knows and an outline of the view on the left, the view being
;;; designed in the middle, drawn with the real widgets but not working (a press there selects what
;;; it was on, as a row of the outline does), and the selected widget's id and options on the right,
;;; to change, with the start of the code that wires it below. A widget from the left goes into the
;;; selected container, or after the selected widget. The view is saved as a file of Lisp that
;;; defines it (DEFINE-VIEW), which an app loads before the code that draws it (VIEW), and is defined
;;; in the image as it is saved, so that an open window shows it at once. It uses the public package,
;;; and the library's catalogue of widgets, which it draws the view with as an app does.
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

(defun write-view (name view stream)
  "Write VIEW to STREAM as the file the designer saves: Lisp that defines it as the view NAME (a
symbol), in NAME's package, to be loaded before the code that draws it."
  (let ((package (symbol-package name)))
    (flet ((written (thing)
             (with-standard-io-syntax
               (let ((*package* package)
                     (*print-case* :downcase)
                     (*print-pretty* t)
                     (*print-right-margin* 98))
                 (prin1-to-string thing)))))
      (format stream ";;; A view made with the GUI designer of almighty-bolt. Load it before the code ~
                      that draws it,~%;;; (almighty-bolt:view ~a ...).~%~%(in-package #:~(~a~))~%~%"
              (written name) (package-name package))
      ;; The operator is written as the public package names it (its home is another package).
      (format stream "(almighty-bolt:define-view ~a~%  ~a)~%"
              (written name)
              (with-output-to-string (out)
                (loop :for character :across (written (node-form view))
                      :do (write-char character out)
                          (when (char= character #\Newline)
                            (write-string "  " out))))))))

(defun read-view (stream)
  "The view that STREAM, a file the designer saved, defines: its name (a symbol, of the package the
file is in) and the view, as two values. An error that says what is wrong if it defines none, or
one that cannot be drawn (CHECK-VIEW-FORM), or if it is in a package this Lisp does not have.
Nothing in the file is evaluated: IN-PACKAGE is followed, as LOAD would."
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package :cl-user)))
      (loop :for form = (read stream nil stream)
            :until (eq form stream)
            :do (cond ((and (consp form) (eq (first form) 'in-package))
                       (setf *package*
                             (or (find-package (second form))
                                 (error "The file is in the package ~a, which this Lisp does not ~
                                         have: load the app's system first."
                                        (string (second form))))))
                      ((and (consp form) (eq (first form) 'bolt:define-view))
                       (destructuring-bind (name view) (rest form)
                         (unless (and name (symbolp name))
                           (error "The view's name, ~s, is not a symbol." name))
                         (check-view-form view)
                         (return-from read-view (values name (form-node view)))))))
      (error "The file defines no view (DEFINE-VIEW)."))))

(defun wiring-text (name view)
  "The start of the code that draws the view called NAME (as written): a line for each widget, by
its kind and id, for what the app gives it."
  (with-output-to-string (out)
    (format out "(bolt:view ~a" name)
    (labels ((walk (node)
               (format out "~%  (~(~a ~s~))" (node-kind node) (node-id node))
               (mapc #'walk (node-children node))))
      (walk view))
    (format out ")")))

;;; --- pieces of the screen ---

(defun node-option (node name)
  "The value of NODE's option NAME: what it was given, or its kind's default."
  (form-option (list (node-kind node) (node-id node) (node-options node)) name))

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
    (bolt:column (:id :palette :gap 8 :cross-align :stretch)
      (bolt:label "Add" :id :palette-heading :size 17)
      (bolt:grid (:id :palette-grid :columns 2 :gap 6)
        (dolist (entry (catalogue))
          (when (bolt:button (entry-title entry) :id (id "add" (entry-kind entry)))
            (setf pressed (entry-kind entry))))))
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
              ((eq type :list)
               ;; One item to a line.
               ;; A draft while it is typed, so that a line just begun is not taken away.
               (multiple-value-bind (text edited)
                   (bolt:text-area (gethash key drafts (format nil "~{~a~^~%~}" value))
                                   :id editor :width 200 :rows 3
                                   :labelled-by (id "option" name "label"))
                 (when edited
                   (setf (gethash key drafts) text
                         result (remove "" (uiop:split-string text :separator '(#\Newline))
                                        :test #'string=)
                         changed t))))
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
leave it, the selection (which an id changed changes), and, if an option was changed, which: the
widget's id and the option's name, as a cons."
  (let ((edit nil))
  (bolt:padding (:id :selection :all 14)
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
                          edit (cons (node-id node) (option-name option))
                          node (find-by-id view (node-id node)))))))))))
    (values view selected edit)))

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
                   ;; What the view is called, which an app draws it by (the file's name, once
                   ;; saved), and the package that name is in (the app's), as written.
                   (name "sign-in")
                   (package "cl-user")
                   (selected nil)
                   (drafts (make-hash-table :test 'equal))
                   (saving nil)
                   (opening nil)
                   (status "")
                   ;; The views before this one, the last first, and those undone, the next first.
                   ;; A view is never changed, so each is the whole of what it was.
                   (undo '())
                   (redo '())
                   ;; What the last change was, if it was an option's: another change of the same
                   ;; option (typing a word) is one step with it.
                   (last-edit nil))
    (lambda ()
      (labels ((select (id)
                 (unless (eq id selected)
                   (clrhash drafts)
                   (setf selected id
                         ;; Coming back to an option later is another step.
                         last-edit nil)))
               (change (new &optional edit)
                 "Make NEW the view, as a step that Undo takes back."
                 (unless (eq new view)
                   (unless (and edit (equal edit last-edit))
                     (push view undo))
                   (setf redo '()
                         view new
                         last-edit edit)))
               (go-to (new)
                 "Show NEW, a view from the history, keeping the selection if it has the widget."
                 (setf view new
                       last-edit nil)
                 (clrhash drafts)
                 (unless (and selected (find-by-id view selected))
                   (select nil)))
               (step-back ()
                 (when undo
                   (push view redo)
                   (go-to (pop undo))))
               (step-forward ()
                 (when redo
                   (push view undo)
                   (go-to (pop redo)))))
        (bolt:column (:id :designer :fill t :cross-align :stretch)
          (bolt:panel (:id :toolbar :all 10 :radius 0)
            (bolt:row (:gap 8 :cross-align :center)
              (when (bolt:button "New" :id :new)
                (change (form-node +empty+))
                (setf status "" name "view")
                (select nil))
              (when (bolt:button "Open…" :id :open)
                (setf opening t))
              (when (bolt:button "Save…" :id :save)
                (setf saving t))
              (when (bolt:button "Remove" :id :remove
                                          :enabled (and selected (not (eq selected (node-id view)))
                                                        t))
                (change (remove-node view selected))
                (select nil))
              (when (bolt:button "Undo" :id :undo :enabled (and undo t))
                (step-back))
              (when (bolt:button "Redo" :id :redo :enabled (and redo t))
                (step-forward))
              (bolt:label "Package" :id :package-label)
              (multiple-value-bind (text edited)
                  (bolt:text-field package :id :package :width 180 :labelled-by :package-label
                                           :invalid (not (find-package (string-upcase package))))
                (when edited (setf package text)))
              (bolt:label status :id :status)))
          (let ((path (bolt:save-file-dialog :id :save-panel :show saving :types '("lisp")
                                             :name (format nil "~(~a~).lisp" name))))
            (setf saving nil)
            (when path
              ;; The view is called what the file is, in the package given.
              (let ((home (find-package (string-upcase package))))
                (if (null home)
                    (setf status (format nil "Not saved: this Lisp has no package called ~a." package))
                    (let ((symbol (intern (string-upcase (pathname-name path)) home)))
                      (setf name (string-downcase (pathname-name path)))
                      (with-open-file (out path :direction :output :if-exists :supersede)
                        (write-view symbol view out))
                      ;; Defined here too, so that a window that draws it shows it now; what the
                      ;; code that wires it no longer fits is said.
                      (let ((problems (handler-bind ((warning #'muffle-warning))
                                        (set-view symbol (node-form view)))))
                        (setf status (format nil "Saved to ~a~:[.~;; ~:*~{~a~^ ~}~]"
                                             path problems))))))))
          (let ((path (bolt:open-file-dialog :id :open-panel :show opening :types '("lisp"))))
            (setf opening nil)
            (when path
              (handler-case
                  (with-open-file (in path)
                    (multiple-value-bind (opened new) (read-view in)
                      (change new)
                      (setf name (string-downcase (symbol-name opened))
                            package (string-downcase (package-name (symbol-package opened)))
                            status (format nil "Opened ~a." path)))
                    (select nil))
                (error (condition)
                  (setf status (format nil "Could not open ~a: ~a" path condition))))))
          (bolt:row (:id :panes :flex 1 :cross-align :stretch)
            (bolt:panel (:id :left :all 14 :radius 0)
              (bolt:column (:gap 20)
                (let ((kind (palette)))
                  (when kind
                    (multiple-value-bind (new id) (added view selected kind)
                      (change new)
                      (select id))))
                (let ((chosen (outline view selected)))
                  (when chosen
                    (select chosen)))))
            (bolt:panel (:id :canvas-back :color :background :radius 0 :flex 1 :all 32)
              (let ((pressed (bolt:design-surface (:id :canvas :selected selected)
                               (draw-form (node-form view)))))
                (when pressed
                  (select pressed))))
            (let ((new view) (now selected) (edit nil))
              (bolt:panel (:id :right :radius 0)
               (bolt:column (:gap 0)
                (multiple-value-setq (new now edit) (selection view selected drafts))
                (bolt:padding (:id :wiring-pane :all 14)
                  (bolt:column (:gap 8)
                    (bolt:label "Wiring" :id :wiring-heading :size 17)
                    (bolt:paragraph "The start of the code that draws this view, to copy."
                                    :id :wiring-note :color :dim-text)
                    (bolt:text-area (wiring-text name view) :id :wiring :width 300 :rows 8
                                                            :labelled-by :wiring-heading)))))
              (change new edit)
              (unless (eq now selected)
                (select now))))
          ;; After everything, so that a text field with the keyboard undoes its own typing first.
          (when (bolt:shortcut #\z :command)
            (step-back))
          (when (bolt:shortcut #\z :command :shift)
            (step-forward)))))))

(defun open-designer ()
  "Open the GUI designer in a window, from the REPL."
  (bolt:open-app 'make-designer :title "GUI designer" :width 1180 :height 680))
