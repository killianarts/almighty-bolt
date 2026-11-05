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
                #:move-node
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
                #:set-view
                #:find-view
                #:view-file
                #:views-version
                #:view-names)
  (:export #:make-designer
           #:designer-parts
           #:sized-stage
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
;;; in the image as it is saved, so that an open window shows it at once. A view the image has is
;;; opened by its name (Open View), and is then live: every edit is made in the image as it is made,
;;; so an app that draws it, running, changes with it, and Save writes it to the file it came from. It uses the public package,
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

(defun view-title (name)
  "How the view NAME is listed: package::name, in lower case, whatever package is current."
  (format nil "~(~a::~a~)" (package-name (symbol-package name)) (symbol-name name)))

(defun view-only-file-p (path name)
  "True if the file PATH holds nothing but the definition of the view NAME (and the package it is
in): a file the designer wrote, which writing again loses nothing of. A file with code in it too is
not, and is edited where the code is."
  (handler-case
      (with-open-file (in path)
        (with-standard-io-syntax
          (let ((*read-eval* nil)
                (*package* (find-package :cl-user))
                (views 0))
            (loop :for form = (read in nil in)
                  :until (eq form in)
                  :do (cond ((and (consp form) (eq (first form) 'in-package))
                             (setf *package* (or (find-package (second form))
                                                 (return-from view-only-file-p nil))))
                            ((and (consp form) (eq (first form) 'bolt:define-view)
                                  (eq (second form) name))
                             (incf views))
                            (t (return-from view-only-file-p nil))))
            (= views 1))))
    (error () nil)))

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

;;; --- a stage, the size the person makes it ---

(defconstant +least-stage+ 80 "The least a stage is made wide or tall.")

(defun sized-stage (id width height draw drafts &key (scale 1))
  "DRAW, a function, on a stage WIDTH by HEIGHT points (BOLT:STAGE): what it draws is laid out at
that size whatever room the pane gives it, and cut off where the pane ends. Over it, its width and
height, each typed (and taken with Return) into a field; under its corner, a grip that drags its
size. DRAFTS is a hash table of what is typed in the fields, by their ids. With SCALE, the size is
in points of what is on the stage, which is shown SCALE times larger (an app in a pane at a zoom).
Returns the width and height as the person left them."
  (let ((w width) (h height))
    (flet ((size-field (field value label)
             (let ((shown (gethash field drafts (princ-to-string (round value)))))
               (multiple-value-bind (text edited submitted)
                   (bolt:text-field shown :id field :width 56 :labelled-by label
                                          :invalid (null (number-text shown)))
                 (when edited (setf (gethash field drafts) text))
                 (when submitted
                   (remhash field drafts)
                   (let ((number (number-text text)))
                     (when number (return-from size-field (max +least-stage+ number))))))
               value)))
      (bolt:column (:id id :gap 6)
        (bolt:row (:id :stage-size :gap 6 :cross-align :center)
          (bolt:label "Width" :id :stage-width-label :color :dim-text)
          (setf w (size-field :stage-width w :stage-width-label))
          (bolt:label "Height" :id :stage-height-label :color :dim-text)
          (setf h (size-field :stage-height h :stage-height-label)))
        (bolt:stage (:id :stage :width (* w scale) :height (* h scale))
          (funcall draw))
        ;; The grip, under the stage's corner; dragged, the stage follows it.
        (bolt:row (:id :stage-grip-row :width (* w scale))
          (bolt:spacer)
          (let ((events (bolt:canvas 14 14
                                     (lambda (list context width height)
                                       (declare (ignore context))
                                       (bolt:push-rounded-rect list 0 0 width height 3
                                                               (bolt:theme-color :dim-text)))
                                     :id :stage-grip :cursor :crosshair)))
            (dolist (event events)
              (destructuring-bind (kind &optional x y &rest more) event
                (declare (ignore more))
                (when (eq kind :drag)
                  ;; Where it is within the grip, which has moved with the size since: the
                  ;; middle of the grip is where the size is.
                  (setf w (max +least-stage+ (+ w (/ (- x 7) scale)))
                        h (max +least-stage+ (+ h (/ (- y 7) scale)))))))))))
    (values w h)))

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
        ;; Each is pressed to add it, or dragged to where it goes.
        (dolist (entry (catalogue))
          (when (bolt:drag-source (:id (id "add" (entry-kind entry)) :payload (entry-kind entry)
                                   :label (entry-title entry))
                  (bolt:panel (:id :tile :all 6 :radius 4)
                    (bolt:label (entry-title entry) :id :title)))
            (setf pressed (entry-kind entry)))))
      ;; The views defined, each to put inside this one.
      (let ((names (view-names)))
        (when names
          (bolt:label "Views" :id :views-heading :size 17)
          (bolt:grid (:id :views-grid :columns 2 :gap 6)
            (dolist (name names)
              (when (bolt:drag-source (:id (id "add-view" (symbol-name name))
                                       :payload (list :view name)
                                       :label (string-downcase (symbol-name name)))
                      (bolt:panel (:id :tile :all 6 :radius 4)
                        (bolt:label (string-downcase (symbol-name name)) :id :title)))
                (setf pressed (list :view name))))))))
    pressed))

(defun row-key (node-id)
  "The id of the outline's row of the widget NODE-ID: its own, but not the widget's, so that a row
and the widget in the view are not taken for each other."
  (id "row" (symbol-name node-id)))

(defun outline (view selected)
  "The view's widgets as a tree, each called by its kind and id, the selected one chosen in it.
Returns the id of the one chosen there, on the frame it is, or NIL."
  (let ((ids (make-hash-table))
        (chosen nil))
    (labels ((note (node)
               (setf (gethash (row-key (node-id node)) ids) (node-id node))
               (mapc #'note (node-children node))))
      (note view))
    (bolt:column (:id :outline :gap 8)
      (bolt:label "Outline" :id :outline-heading :size 17)
      (bolt:scroll (:id :outline-scroll :height 300)
        (multiple-value-bind (key changed)
            (bolt:tree (list view) :id :outline-tree :width 260 :children #'node-children
                                        :key (lambda (node) (row-key (node-id node)))
                                        :label (lambda (node)
                                                 (format nil "~a ~(~s~)"
                                                         (entry-title (find-entry (node-kind node)))
                                                         (node-id node)))
                                        :selected (and selected (row-key selected))
                                        :labelled-by :outline-heading)
          (when changed
            (setf chosen (gethash key ids))))))
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
              ((eq type :view)
               ;; The views defined, by name.
               (let* ((names (view-names))
                      (shown (mapcar (lambda (n) (string-downcase (symbol-name n))) names)))
                 (multiple-value-bind (index moved)
                     (bolt:dropdown (or shown '("No view yet")) (position value names)
                                    :id editor :labelled-by (id "option" name "label"))
                   (when (and moved names)
                     (setf result (nth index names) changed t)))))
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

(defun new-node (view what)
  "A node to add to VIEW for WHAT: a kind of widget, or (:view name) for that view inside it."
  (if (consp what)
      (make-node :view (fresh-id view :view) :options (list :name (second what)))
      (make-node what (fresh-id view what))))

(defun added (view selected kind)
  "VIEW with a new widget of KIND (or (:view name)): at the end of the selected container, after the
selected widget, or at the end of the view if nothing is selected. Returns the view and the new
widget's id."
  (let* ((new (new-node view kind))
         (node (and selected (find-by-id view selected))))
    (values (cond ((null node)
                   (add-node view new :into (node-id view)))
                  ((entry-container-p (find-entry (node-kind node)))
                   (add-node view new :into selected))
                  (t (add-node view new :after selected)))
            (node-id new))))

(defun designer-parts ()
  "The GUI designer's parts, each a function that draws one, all of one design and its history:
(values toolbar left canvas right keys open-view). LEFT is the widgets to add and the outline,
CANVAS the view drawn, RIGHT the selected widget's options and the wiring, KEYS the shortcuts
(Command-Z and Command-Shift-Z), described after the part that has the keyboard so that a text field
undoes its own typing first. OPEN-VIEW is not drawn: it is called with the name of a view the image
has, and the id of a widget in it, to open that view live with the widget selected. MAKE-DESIGNER
puts them in a window; the studio puts them in panes."
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
                   (last-edit nil)
                   ;; The view's name (a symbol) while it is live: each edit is made in the image
                   ;; as it is made. And the file it is saved to without asking, if there is one.
                   (live nil)
                   (file nil)
                   ;; How many times views had been defined when the live view was last looked at:
                   ;; one defined since, elsewhere (typed in Emacs), is taken up.
                   (seen-version 0)
                   ;; The size the view is designed at, which the person drags or types, as a
                   ;; window's: the view is laid out at it whatever the canvas's pane is.
                   (stage-w 480)
                   (stage-h 360)
                   (stage-drafts (make-hash-table)))
    (labels ((make-live ()
               "Make the view, as it is now, the live view in the image, and say what of the code
that wires it no longer fits."
               (when live
                 (let ((problems (handler-bind ((warning #'muffle-warning))
                                   (set-view live (node-form view)))))
                   (setf seen-version (views-version))
                   (setf status (if problems
                                    (format nil "~{~a~^ ~}" problems)
                                    (format nil "Live: ~(~a~) changes in the running app as it ~
                                                 is edited." live))))))
             (take-up-outside ()
               "If the live view was defined outside the designer since it last looked (typed in an
editor), make that the view, as a step that Undo takes back, without defining it again."
               (when (and live (/= seen-version (views-version)))
                 (setf seen-version (views-version))
                 (let ((form (find-view live)))
                   (when (and form (not (equal form (node-form view))))
                     (push view undo)
                     (setf redo '()
                           view (form-node form)
                           last-edit nil)
                     (clrhash drafts)
                     (unless (and selected (find-by-id view selected))
                       (setf selected nil))))))
             (select (id)
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
                       last-edit edit)
                 (make-live)))
             (go-to (new)
               "Show NEW, a view from the history, keeping the selection if it has the widget."
               (setf view new
                     last-edit nil)
               (make-live)
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
                 (go-to (pop redo))))
             (open-view (symbol &optional widget)
               "Open the view the image has called SYMBOL, live, with WIDGET (an id in it)
selected if given."
               (let ((form (find-view symbol)))
                 (when form
                   (setf live nil)
                   (change (form-node form))
                   (setf live symbol
                         seen-version (views-version)
                         file (view-file symbol)
                         name (string-downcase (symbol-name symbol))
                         package (string-downcase (package-name (symbol-package symbol)))
                         status (format nil "Live: ~(~a~) changes in the running app as it is ~
                                             edited." symbol))
                   (select (and widget (find-by-id view widget) widget)))))
             (save-to (path symbol)
               "Write the view to PATH as the view SYMBOL, and make it the live view."
               (with-open-file (out path :direction :output :if-exists :supersede)
                 (write-view symbol view out))
               (setf name (string-downcase (symbol-name symbol))
                     live symbol
                     file (pathname path))
               (let ((problems (handler-bind ((warning #'muffle-warning))
                                 (set-view symbol (node-form view) :file path))))
                 (setf status (format nil "Saved to ~a~:[.~;; ~:*~{~a~^ ~}~]" path problems)))))
      (values
        (lambda ()
          (take-up-outside)
          (bolt:column (:id :toolbar-part :cross-align :stretch)
            (let ((menu (bolt:app-menu "File" '(("New" :key #\n) ("Open…" :key #\o) :separator
                                                ("Save…" :key #\s)))))
             (bolt:panel (:id :toolbar :all 10 :radius 0)
               (bolt:row (:gap 8 :cross-align :center)
                 (when (or (bolt:button "New" :id :new) (equal menu "New"))
                   (setf live nil file nil)
                   (change (form-node +empty+))
                   (setf status "" name "view")
                   (select nil))
                 ;; The views the image has, each opened live.
                 (let* ((names (sort (view-names) #'string< :key #'view-title))
                        (chosen (bolt:menu-button "Open View"
                                                  (or (mapcar #'view-title names)
                                                      '("No views yet"))
                                                  :id :open-view)))
                   (when chosen
                     (let ((symbol (find chosen names :key #'view-title :test #'string=)))
                       (when symbol (open-view symbol)))))
                 (when (or (bolt:button "Open…" :id :open) (equal menu "Open…"))
                   (setf opening t))
                 (when (or (bolt:button "Save…" :id :save) (equal menu "Save…"))
                   ;; To its own file without asking, if that file is only the view.
                   (if (and live file (view-only-file-p file live))
                       (save-to file live)
                       (setf saving t)))
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
                 (bolt:label status :id :status))))
            ;; The dialogs, which float.
            (let ((path (bolt:save-file-dialog :id :save-panel :show saving :types '("lisp")
                                               :name (format nil "~(~a~).lisp" name))))
              (setf saving nil)
              (when path
                ;; The view is called what the file is, in the package given.
                (let ((home (find-package (string-upcase package))))
                  (if (null home)
                      (setf status (format nil "Not saved: this Lisp has no package called ~a." package))
                      ;; Defined here too, so that a window that draws it shows it now, and live
                      ;; from then on; what the code that wires it no longer fits is said.
                      (save-to path (intern (string-upcase (pathname-name path)) home))))))
            (let ((path (bolt:open-file-dialog :id :open-panel :show opening :types '("lisp"))))
              (setf opening nil)
              (when path
                (handler-case
                    (with-open-file (in path)
                      (multiple-value-bind (opened new) (read-view in)
                        (setf live nil)
                        (change new)
                        ;; Live if the image has the view: the app that draws it changes with it.
                        (setf live (and (find-view opened) opened)
                              file (pathname path))
                        (make-live)
                        (setf name (string-downcase (symbol-name opened))
                              package (string-downcase (package-name (symbol-package opened)))
                              status (format nil "Opened ~a." path)))
                      (select nil))
                  (error (condition)
                    (setf status (format nil "Could not open ~a: ~a" path condition))))))))
        (lambda ()
          (bolt:panel (:id :left :all 14 :radius 0)
            (bolt:column (:gap 20)
              (let ((kind (palette)))
                (when kind
                  (multiple-value-bind (new id) (added view selected kind)
                    (change new)
                    (select id))))
              (let ((chosen (outline view selected)))
                (when chosen
                  (select chosen))))))
        (lambda ()
          (bolt:panel (:id :canvas-back :color :background :radius 0 :flex 1 :all 24)
            (multiple-value-bind (w h)
                (sized-stage
                 :canvas-stage stage-w stage-h
                 (lambda ()
                   (multiple-value-bind (pressed drop)
                       (bolt:design-surface (:id :canvas :selected selected)
                         (draw-form (node-form view)))
                     (when pressed
                       (select pressed))
                     ;; A widget from the left, or one of the view's own, let go of where it goes.
                     (when drop
                       (destructuring-bind (payload into at) drop
                         (if (and (consp payload) (eq (first payload) :move))
                             (change (move-node view (second payload) into at))
                             (let ((new (new-node view payload)))
                               (change (add-node view new :into into :at at))
                               (select (node-id new))))))))
                 stage-drafts)
              (setf stage-w w stage-h h))))
        (lambda ()
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
        (lambda ()
          (when (bolt:shortcut #\z :command)
            (step-back))
          (when (bolt:shortcut #\z :command :shift)
            (step-forward)))
        ;; For a tool beside it (the studio's inspector): open a view of the image, live.
        (lambda (symbol &optional widget)
          (open-view symbol widget))))))

(defun make-designer ()
  "The GUI designer in a window of its own: its toolbar over its parts side by side."
  (multiple-value-bind (toolbar left canvas right keys) (designer-parts)
    (lambda ()
      (bolt:column (:id :designer :fill t :cross-align :stretch)
        (funcall toolbar)
        (bolt:row (:id :panes :flex 1 :cross-align :stretch)
          (funcall left)
          (funcall canvas)
          (funcall right))
        ;; After everything, so that a text field with the keyboard undoes its own typing first.
        (funcall keys)))))

(defun open-designer ()
  "Open the GUI designer in a window, from the REPL."
  (bolt:open-app 'make-designer :title "GUI designer" :width 1180 :height 680))
