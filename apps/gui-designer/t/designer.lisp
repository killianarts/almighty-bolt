(defpackage #:almighty-bolt-gui-designer/t/designer
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:designer #:almighty-bolt-gui-designer)
                    (#:catalogue #:almighty-bolt/ui/catalogue)
                    (#:views #:almighty-bolt/ui/views)))

(in-package #:almighty-bolt-gui-designer/t/designer)

;; A view defined, which the designer lists to put inside the one it designs.
(bolt:define-view t-nested
  (:column :nested (:gap 4)
   (:label :inner (:text "Inside"))))

;;; The GUI designer worked as a person works it, through the machine interface and the pointer, in
;;; a context with no window. The view it designs is drawn with the real widgets, so the machine
;;; interface finds them (inside the canvas); a press or typing on them is the designer's, not
;;; theirs.

(defstruct (screen (:constructor %make-screen (app context list)))
  app context list)

(defun draw (screen)
  (core:settle-frame (screen-app screen) (screen-context screen) (screen-list screen)))

(defun open-designer ()
  "The designer and a machine session on it."
  (let* ((instance (core:with-kept-state ((core:make-kept-state))
                    (app:make-app-instance 'designer:make-designer)))
         (screen (%make-screen instance (fake:make-fake-context :width 1100f0 :height 640f0)
                               (core:make-display-list :capacity 65536))))
    (draw screen)
    (values screen
            (m:make-session :app (lambda () (screen-app screen))
                            :deliver (lambda (event)
                                       (core:handle-event (screen-app screen) event)
                                       (draw screen))
                            :settle (lambda () (dotimes (i 4) (draw screen)))
                            :on-main #'funcall))))

(defun pointer (screen kind x y)
  "The pointer moving to X, Y, or its primary button going down or up there, and one frame: what the
event changed shows in it."
  (core:handle-event (screen-app screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button 0
                                              :buttons (if (eq kind :down) 1 0)
                                              :click-count (if (eq kind :move) 0 1)))
  (draw screen))

(defun color-at (screen x y)
  "The colour of the last rectangle drawn over the point X, Y."
  (let ((list (screen-list screen)) (color nil))
    (dotimes (index (core:display-list-count list) color)
      (let ((kind (core:display-list-kind list index)))
        (when (and (or (= kind core:+command-rect+) (= kind core:+command-rounded-rect+))
                   (<= (core:display-list-x list index) x)
                   (< x (+ (core:display-list-x list index) (core:display-list-width list index)))
                   (<= (core:display-list-y list index) y)
                   (< y (+ (core:display-list-y list index) (core:display-list-height list index))))
          (setf color (core:display-list-color list index)))))))

(defun shown-name (session id)
  (s:node-name (m:find-node session :id id)))

(defun shown-value (session id)
  (s:node-value (m:find-node session :id id)))

(defun in-view (session id)
  "The node of the widget ID of the view being designed, or NIL if the view has none."
  (handler-case (m:find-node session :id id :within :canvas)
    (error () nil)))

(defun top-of (session id)
  (second (s:node-bounds (m:find-node session :id id :within :canvas))))

(defun press-beside-heading (screen)
  "A press in the form, right of its heading: on the form and nothing in it."
  (multiple-value-bind (fx fy fw) (core:lookup-target (screen-app screen) :sign-in-form)
    (declare (ignore fy))
    (multiple-value-bind (hx hy hw) (core:lookup-target (screen-app screen) :heading)
      (assert-true (< (+ hx hw) (+ fx fw -10)))
      (pointer screen :move (+ fx fw -4) (+ hy 4))
      (pointer screen :down (+ fx fw -4) (+ hy 4))
      (pointer screen :up (+ fx fw -4) (+ hy 4)))))

(define-test the-view-is-drawn-with-the-real-widgets ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (let ((button (m:find-node session :id :sign-in :within :canvas))
          (field (m:find-node session :id :name :within :canvas)))
      (assert-eq :button (s:node-role button))
      (assert-equal "Sign in" (s:node-name button))
      (assert-eq :text-field (s:node-role field)))
    (assert-true (search "Nothing" (shown-name session :selected-nothing)))))

(define-test a-press-on-a-widget-of-the-view-selects-it ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :sign-in :within :canvas)
    (assert-equal "Button :sign-in" (shown-name session :selected-what))
    (assert-equal "Sign in" (shown-value session :option-text))
    (m:click session :id :heading :within :canvas)
    (assert-equal "Label :heading" (shown-name session :selected-what))
    (assert-equal ":bold" (shown-value session :option-weight))))

(define-test typing-on-a-text-field-of-the-view-selects-it-and-types-nothing ()
  ;; The press that would give the field the keyboard is the designer's; with no keyboard, the field
  ;; takes none of the keys.
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:type-text session "abc" :id :name :within :canvas)
    (assert-equal "" (s:node-value (m:find-node session :id :name :within :canvas)))
    (assert-equal "Text field :name" (shown-name session :selected-what))))

(define-test a-press-between-widgets-selects-the-container-they-are-in ()
  (multiple-value-bind (screen session) (open-designer)
    ;; Right of the heading, which is narrower than the form: in the form, on nothing in it.
    (press-beside-heading screen)
    (assert-equal "Column :sign-in-form" (shown-name session :selected-what))))

(define-test the-pointer-and-the-selection-are-outlined ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((dark (bolt:find-theme :dark)))
      (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :forgot)
        (declare (ignore w))
        (flet ((edge () (color-at screen (+ x 1) (+ y (/ h 2))))
               (corner () (color-at screen (- x 1) (- y 1))))
          (assert-false (eql (bolt:theme-setting dark :dim-text) (edge)))
          (pointer screen :move (+ x 10) (+ y (/ h 2)))
          (assert-eql (bolt:theme-setting dark :dim-text) (edge))
          ;; Pressed, it is marked as the selection in the frame of the press: corner brackets
          ;; just outside it, not a frame along its edges, and a tag over it that says it.
          (pointer screen :down (+ x 10) (+ y (/ h 2)))
          (assert-eql (bolt:theme-setting dark :ring) (corner))
          (assert-false (eql (bolt:theme-setting dark :ring) (edge)))
          (pointer screen :up (+ x 10) (+ y (/ h 2)))
          (assert-equal "Button :forgot" (shown-name session :selected-what))
          (assert-equal "Button :forgot · not wired" (shown-name session :selection-tag-text))
          ;; The tag is over the widget, not on it.
          (destructuring-bind (tx ty tw th)
              (s:node-bounds (m:find-node session :id :selection-tag-text))
            (declare (ignore tx tw))
            (assert-true (<= (+ ty th) y)))
          (pointer screen :leave 0 0)
          (assert-eql (bolt:theme-setting dark :ring) (corner)))))))

;;; --- changing the view ---

(define-test a-widget-added-goes-into-the-selected-container-or-after-the-selected-widget ()
  (multiple-value-bind (screen session) (open-designer)
    ;; With the form selected, at its end, below the buttons.
    (press-beside-heading screen)
    (m:click session :id :add-button :within :left)
    (assert-equal "Button :button-1" (shown-name session :selected-what))
    (assert-equal "Button" (s:node-name (in-view session :button-1)))
    (assert-true (> (top-of session :button-1) (top-of session :sign-in)))
    ;; With a label selected, just after it: between the heading and the label under it.
    (m:click session :id :heading :within :canvas)
    (m:click session :id :add-label :within :left)
    (assert-equal "Label :label-1" (shown-name session :selected-what))
    (assert-true (< (top-of session :heading) (top-of session :label-1)
                    (top-of session :name-label)))))

(define-test an-option-changed-changes-the-widget-in-the-view ()
  (multiple-value-bind (screen session) (open-designer)
    (m:click session :id :sign-in :within :canvas)
    (m:set-value session "Log in" :id :option-text)
    (assert-equal "Log in" (s:node-name (in-view session :sign-in)))
    (m:set-value session nil :id :option-enabled)
    (assert-true (s:node-property (in-view session :sign-in) :disabled))
    ;; A number: what is not one is shown as wrong and changes nothing. The form's gap, seen in how
    ;; far apart its first two widgets are.
    (press-beside-heading screen)
    (flet ((apart () (- (top-of session :name-label) (top-of session :heading))))
      (let ((before (apart)))
        (m:set-value session "wide" :id :option-gap)
        (assert-true (s:node-property (m:find-node session :id :option-gap) :invalid))
        (assert-equal before (apart))
        (m:set-value session "40" :id :option-gap)
        (assert-false (s:node-property (m:find-node session :id :option-gap) :invalid))
        (assert-equal (+ before 28) (apart))))))

(define-test an-id-can-be-changed-to-one-no-other-widget-has ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :sign-in :within :canvas)
    ;; Typed, it is not changed yet: Return changes it.
    (m:set-value session "submit" :id :option-id)
    (assert-equal "Button :sign-in" (shown-name session :selected-what))
    (m:press-key session :return)
    (assert-equal "Button :submit" (shown-name session :selected-what))
    (assert-true (in-view session :submit))
    (assert-false (in-view session :sign-in))
    ;; Another widget's id is shown as wrong, and Return leaves the widget its own.
    (m:set-value session "forgot" :id :option-id)
    (assert-true (s:node-property (m:find-node session :id :option-id) :invalid))
    (m:press-key session :return)
    (assert-equal "Button :submit" (shown-name session :selected-what))
    (assert-true (in-view session :forgot))))

(define-test remove-takes-the-selected-widget-out-of-the-view ()
  (multiple-value-bind (screen session) (open-designer)
    (m:click session :id :forgot :within :canvas)
    (m:click session :id :remove :within :toolbar)
    (assert-false (in-view session :forgot))
    (assert-true (in-view session :sign-in))
    (assert-true (search "Nothing" (shown-name session :selected-nothing)))
    ;; The view itself cannot be removed.
    (press-beside-heading screen)
    (assert-true (s:node-property (m:find-node session :id :remove :within :toolbar) :disabled))))

(define-test a-view-saved-opens-again-as-it-was ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((path (merge-pathnames (format nil "view-~d.lisp" (random 1000000000))
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (declare (ignore kind offered))
                     (assert-equal '("lisp") types)
                     (namestring path)))
             (m:click session :id :sign-in :within :canvas)
             (m:set-value session "Log in" :id :option-text)
             ;; The heading's weight back to the default: then the file does not say it.
             (m:click session :id :heading :within :canvas)
             (m:set-value session ":regular" :id :option-weight)
             (assert-equal ":regular" (shown-value session :option-weight))
             (m:click session :id :save :within :toolbar)
             (assert-true (search "Saved to" (shown-name session :status)))
             (let ((saved (uiop:read-file-string path)))
               ;; Written plainly: no package prefix on NIL.
               (assert-false (search "common-lisp:" saved) saved)
               (assert-true (search "(:text-field :name nil)" saved) saved)
               (assert-true (search "(:button :sign-in (:text \"Log in\"))" saved) saved)
               (assert-true (search "(:label :heading (:text \"Sign in\" :size 22))" saved) saved))
             (m:click session :id :new :within :toolbar)
             (assert-false (in-view session :sign-in))
             (m:click session :id :open :within :toolbar)
             (assert-true (search "Opened" (shown-name session :status)))
             (assert-equal "Log in" (s:node-name (in-view session :sign-in)))
             (assert-true (in-view session :password)))
        (when (probe-file path) (delete-file path))))))

(define-test a-file-that-is-not-a-view-is-not-opened ()
  ;; Each would open and then fail in every frame, or (two widgets with one id) select and change
  ;; the wrong one.
  ;; Each says what is wrong.
  (loop :for (text why) :in '(("(:column :v ())" "defines no view")
                              ("(almighty-bolt:define-view :x (:slab :nope ()))" "no widget of the kind :SLAB")
                              ("(almighty-bolt:define-view :x (:column :v () (:label :x (:size \"big\"))))" "of the option :SIZE")
                              ("(almighty-bolt:define-view :x (:column :v () (:label :x (:colour 3))))" "has no option :COLOUR")
                              ("(almighty-bolt:define-view :x (:column :v () (:button :x (:enabled maybe))))" "of the option :ENABLED")
                              ("(almighty-bolt:define-view :x (:column :v () (:label :x ()) (:row :r () (:label :x ()))))"
                               "Two widgets have the id :X")
                              ("(almighty-bolt:define-view :x (:column :v () (:label :x () (:label :y ()))))" "holds nothing"))
        :do
    (multiple-value-bind (screen session) (open-designer)
      (let ((path (merge-pathnames (format nil "not-~d.lisp" (random 1000000000))
                                   (uiop:temporary-directory))))
        (unwind-protect
             (progn
               (with-open-file (out path :direction :output)
                 (write-string text out))
               (setf (core:context-file-chooser (screen-context screen))
                     (lambda (kind types offered)
                       (declare (ignore kind types offered))
                       (namestring path)))
               (m:click session :id :open :within :toolbar)
               (assert-true (search "Could not open" (shown-name session :status)) text)
               (assert-true (search why (shown-name session :status)) why (shown-name session :status))
               (assert-true (in-view session :sign-in) text))
          (when (probe-file path) (delete-file path)))))))

(define-test a-view-with-an-option-set-to-false-opens-again-as-it-was ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((path (merge-pathnames (format nil "view-~d.lisp" (random 1000000000))
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (declare (ignore kind types offered))
                     (namestring path)))
             (m:click session :id :sign-in :within :canvas)
             (m:set-value session nil :id :option-enabled)
             (m:click session :id :save :within :toolbar)
             (assert-true (search "(:button :sign-in (:text \"Sign in\" :enabled nil))"
                                  (uiop:read-file-string path))
                          (uiop:read-file-string path))
             (m:click session :id :new :within :toolbar)
             (m:click session :id :open :within :toolbar)
             (assert-true (search "Opened" (shown-name session :status)))
             (assert-true (s:node-property (in-view session :sign-in) :disabled)))
        (when (probe-file path) (delete-file path))))))

(define-test the-outline-is-where-the-selected-widget-is-in-the-frame-it-changes-size ()
  ;; One key and one frame: a window draws no more until something happens, so the outline must be
  ;; round the button as that frame lays it out, not as the frame before did.
  (multiple-value-bind (screen session) (open-designer)
    (let ((ring (bolt:theme-setting (bolt:find-theme :dark) :ring)))
      (m:click session :id :forgot :within :canvas)
      (m:click session :id :option-text :within :selection)
      (multiple-value-bind (x0 y0 w0) (core:lookup-target (screen-app screen) :forgot)
        (declare (ignore x0 y0))
        (dolist (character (coerce " now" 'list))
          (core:handle-event (screen-app screen)
                             (core:make-key-event :timestamp 0 :kind :down :logical-key character
                                                  :modifiers 0))
          (draw screen))
        (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :forgot)
          (assert-true (> w w0))
          ;; The bracket at its bottom right corner, just outside it.
          (assert-eql ring (color-at screen (+ x w 1) (+ y h 1))))))))

;;; --- the outline ---

(defun outline-rows (session)
  "The rows of the outline, in order: each its words and how deep it is."
  (let ((rows '()))
    (labels ((walk (node)
               (when (eq (s:node-role node) :row)
                 (push (list (s:node-name node) (s:node-property node :level)) rows))
               (mapc #'walk (s:node-children node))))
      (walk (m:find-node session :id :outline-tree)))
    (nreverse rows)))

(define-test the-outline-is-a-tree-of-the-widgets-in-order-each-as-deep-as-it-is ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((rows (outline-rows session)))
      ;; Each kind abbreviated, and a container with how many widgets are in it.
      (assert-equal '("COL :sign-in-form · 6" 0) (first rows))
      (assert-equal '("LBL :heading" 1) (second rows))
      (assert-equal '("ROW :actions · 2" 1) (car (last rows 3)))
      (assert-equal '("BTN :forgot" 2) (car (last rows)))
      (assert-equal 9 (length rows)))
    ;; What is added is in it, where it went.
    (m:click session :id :heading :within :canvas)
    (m:click session :id :add-text-field :within :left)
    (assert-equal '("FLD :text-field-1" 1) (third (outline-rows session)))
    (assert-equal '("COL :sign-in-form · 7" 0) (first (outline-rows session)))
    ;; A container is closed and opened by the mark before it, and what is in it goes and comes.
    (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id :row-actions))
      (declare (ignore w))
      ;; The row is one deep: its mark is after one indent.
      (pointer screen :move (+ x 4 16 8) (+ y (/ h 2)))
      (pointer screen :down (+ x 4 16 8) (+ y (/ h 2)))
      (pointer screen :up (+ x 4 16 8) (+ y (/ h 2))))
    ;; The ten rows less the row's two buttons.
    (assert-equal 8 (length (outline-rows session)))
    (assert-equal "Row :actions" (shown-name session :selected-what))))

(define-test a-row-of-the-outline-selects-its-widget-and-the-view-selects-its-row ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    ;; The row of the row of buttons, which a press in the view could only find between them.
    (m:click session :id :row-actions :within :outline)
    (assert-equal "Row :actions" (shown-name session :selected-what))
    (m:click session :id :password :within :canvas)
    (assert-true (s:node-property (m:find-node session :id :row-password) :selected))
    (assert-false (s:node-property (m:find-node session :id :row-actions) :selected))))

;;; --- the catalogue ---

(define-test every-value-the-editors-offer-can-be-drawn ()
  ;; A choice the layout does not take would fail in every frame after it was chosen, and the window
  ;; with it (main-align once offered :stretch, which only cross-align takes). So every value of every
  ;; option of every kind is chosen, through its editor, in a view that is then drawn.
  (dolist (entry (catalogue:catalogue))
    (multiple-value-bind (screen session) (open-designer)
      (declare (ignore screen))
      (m:click session :id :new :within :toolbar)
      (m:click session :id (intern (format nil "ADD-~a" (catalogue:entry-kind entry)) :keyword)
                       :within :left)
      (dolist (option (catalogue:entry-options entry))
        (let ((editor (intern (format nil "OPTION-~a" (catalogue:option-name option)) :keyword))
              (type (catalogue:option-type option)))
          (dolist (value (cond ((eq type :string) '("" "Some words"))
                               ((eq type :list) (list (format nil "A~%B~%C")))
                               ((eq type :view) (list (string-downcase
                                                       (symbol-name (first (views:view-names))))))
                               ((eq type :number) '("0" "12"))
                               ((eq type :boolean) '(nil t))
                               (t (mapcar (lambda (choice) (string-downcase (prin1-to-string choice)))
                                          (rest type)))))
            (m:set-value session value :id editor)
            (assert-equal (format nil "~a ~(~s~)" (catalogue:entry-title entry)
                                  (intern (format nil "~a-1" (catalogue:entry-kind entry)) :keyword))
                          (shown-name session :selected-what)
                          (catalogue:entry-kind entry) (catalogue:option-name option) value)))))))

;;; --- undo ---

(define-test undo-takes-back-each-change-and-redo-puts-it-back ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (flet ((enabled-p (id)
             (not (s:node-property (m:find-node session :id id :within :toolbar) :disabled))))
      (assert-false (enabled-p :undo))
      ;; Three changes: a word typed into an option (one step, not one for each key), a widget
      ;; added, a widget removed.
      (m:click session :id :sign-in :within :canvas)
      (m:set-value session "Log in" :id :option-text)
      (m:click session :id :add-button :within :left)
      (m:click session :id :forgot :within :canvas)
      (m:click session :id :remove :within :toolbar)
      (assert-false (in-view session :forgot))
      (m:click session :id :undo :within :toolbar)
      (assert-true (in-view session :forgot))
      (m:click session :id :undo :within :toolbar)
      (assert-false (in-view session :button-1))
      (m:click session :id :undo :within :toolbar)
      (assert-equal "Sign in" (s:node-name (in-view session :sign-in)))
      (assert-false (enabled-p :undo))
      ;; And forward again.
      (m:click session :id :redo :within :toolbar)
      (assert-equal "Log in" (s:node-name (in-view session :sign-in)))
      (m:click session :id :redo :within :toolbar)
      (assert-true (in-view session :button-1))
      ;; A change after an undo is the way on: what was undone cannot be redone.
      (m:click session :id :heading :within :canvas)
      (m:set-value session "Welcome" :id :option-text)
      (assert-false (enabled-p :redo))
      (assert-true (in-view session :forgot)))))

(define-test command-g-wraps-the-selected-widget-in-a-column-where-it-was ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :sign-in :within :canvas)
    (m:press-key session :g :modifiers '(:super))
    ;; In the row, first, as the button was: a column with the button in it, selected.
    (assert-equal "Column :column-1" (shown-name session :selected-what))
    (let ((rows (outline-rows session)))
      (assert-equal '(("ROW :actions · 2" 1) ("COL :column-1 · 1" 2) ("BTN :sign-in" 3)
                      ("BTN :forgot" 2))
                    (last rows 4)))
    ;; One step, which Command-Z takes back.
    (m:press-key session :z :modifiers '(:super))
    (assert-equal '(("ROW :actions · 2" 1) ("BTN :sign-in" 2) ("BTN :forgot" 2))
                  (last (outline-rows session) 3))))

(define-test command-z-undoes-and-command-shift-z-redoes ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :forgot :within :canvas)
    (m:click session :id :remove :within :toolbar)
    (m:press-key session :z :modifiers '(:super))
    (assert-true (in-view session :forgot))
    (m:press-key session :z :modifiers '(:super :shift))
    (assert-false (in-view session :forgot))))

;;; --- the view in the app ---

(defun find-form (form id)
  "The widget called ID in the view FORM."
  (if (eq (second form) id)
      form
      (loop :for child :in (cdddr form) :thereis (find-form child id))))

(defun saving-to (screen name)
  "Make the save panel answer with a file called NAME.lisp; its path."
  (let ((path (merge-pathnames (format nil "~a.lisp" name) (uiop:temporary-directory))))
    (setf (core:context-file-chooser (screen-context screen))
          (lambda (kind types offered)
            (declare (ignore kind types offered))
            (namestring path)))
    path))

(define-test saving-defines-the-view-by-the-files-name-and-the-file-defines-it-again ()
  (multiple-value-bind (screen session) (open-designer)
    (let* ((name (format nil "login-~d" (random 1000000000)))
           (keyword (intern (string-upcase name) :cl-user))
           (path (saving-to screen name)))
      (unwind-protect
           (progn
             (m:click session :id :sign-in :within :canvas)
             (m:set-value session "Log in" :id :option-text)
             (m:click session :id :save :within :toolbar)
             ;; Defined here as it is saved, so a window that draws it shows it now.
             (assert-equal "Log in" (catalogue:form-option
                                     (find-form (views:find-view keyword) :sign-in) :text))
             ;; In the package given (CL-USER, until another is), with the name as the file's.
             (let ((saved (uiop:read-file-string path)))
               (assert-true (search "(in-package #:common-lisp-user)" saved) saved)
               (assert-true (search (format nil "(almighty-bolt:define-view ~a" name) saved) saved))
             ;; Loaded, the file defines it: as an app's system loads it.
             (remhash keyword views::*views*)
             (load path)
             (assert-true (views:find-view keyword)))
        (remhash keyword views::*views*)
        (when (probe-file path) (delete-file path))))))

(defvar *held-wiring* nil
  "Code that wires a view, held while a test needs it to be: a special variable, which the garbage
collector sees, where a local one that is not used again may not be.")

(define-test saving-a-view-that-no-longer-fits-its-wiring-says-so ()
  (multiple-value-bind (screen session) (open-designer)
    (let* ((name (format nil "wired-~d" (random 1000000000)))
           (keyword (intern (string-upcase name) :cl-user))
           (path (saving-to screen name)))
      (unwind-protect
           (progn
             ;; Code that wires the view's Forgot button, compiled and held, so it could still run.
             (let ((*held-wiring* (handler-bind ((warning #'muffle-warning))
                             (compile nil `(lambda () (bolt:view ,keyword
                                                        (button :forgot :on-press (print 1))))))))
               (m:click session :id :forgot :within :canvas)
               (m:click session :id :remove :within :toolbar)
               (m:click session :id :save :within :toolbar)
               (assert-true (search ":FORGOT is wired" (shown-name session :status))
                            (shown-name session :status))))
        (remhash keyword views::*views*)
        (when (probe-file path) (delete-file path))))))

(define-test the-start-of-the-wiring-names-every-widget-by-its-kind-and-id ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (let ((text (shown-value session :wiring)))
      (assert-true (search "(bolt:view sign-in" text) text)
      (assert-true (search "(button :forgot)" text) text)
      (assert-true (search "(text-field :password)" text) text))))

(define-test a-view-is-named-in-the-package-given ()
  (multiple-value-bind (screen session) (open-designer)
    (let* ((name (format nil "card-~d" (random 1000000000)))
           (path (saving-to screen name))
           (here (find-package :almighty-bolt-gui-designer/t/designer))
           (symbol (intern (string-upcase name) here)))
      (unwind-protect
           (progn
             ;; A package this Lisp does not have: shown as wrong, and the view is not saved.
             (m:set-value session "no-such-package" :id :package)
             (assert-true (s:node-property (m:find-node session :id :package) :invalid))
             (m:click session :id :save :within :toolbar)
             (assert-true (search "no package called" (shown-name session :status)))
             (assert-false (probe-file path))
             ;; The tests' own package: the name is a symbol of it, and the file is in it.
             (m:set-value session "almighty-bolt-gui-designer/t/designer" :id :package)
             (m:click session :id :save :within :toolbar)
             (assert-true (views:find-view symbol))
             (let ((saved (uiop:read-file-string path)))
               (assert-true (search "(in-package #:almighty-bolt-gui-designer/t/designer)" saved) saved)
               (assert-true (search (format nil "(almighty-bolt:define-view ~a~%" name) saved) saved))
             (assert-true (search (format nil "(bolt:view ~a" name) (shown-value session :wiring)))
             ;; Opened again, its name and package are the file's.
             (m:click session :id :new :within :toolbar)
             (m:set-value session "cl-user" :id :package)
             (m:click session :id :open :within :toolbar)
             (assert-equal "almighty-bolt-gui-designer/t/designer" (shown-value session :package)))
        (remhash symbol views::*views*)
        (when (probe-file path) (delete-file path))))))

(define-test a-view-in-a-package-this-lisp-does-not-have-is-not-opened ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((path (merge-pathnames (format nil "elsewhere-~d.lisp" (random 1000000000))
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output)
               (write-string "(in-package #:no-such-app) (almighty-bolt:define-view v (:column :v ()))"
                             out))
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (declare (ignore kind types offered))
                     (namestring path)))
             (m:click session :id :open :within :toolbar)
             (assert-true (search "NO-SUCH-APP, which this Lisp does not have"
                                  (shown-name session :status))
                          (shown-name session :status))
             (assert-false (find-package :no-such-app)))
        (when (probe-file path) (delete-file path))))))

(define-test the-file-menu-does-what-the-toolbar-does ()
  (multiple-value-bind (screen session) (open-designer)
    (flet ((choose (item)
             (core:handle-event (screen-app screen)
                                (core:make-menu-event :timestamp 0 :menu "File" :item item))
             (dotimes (i 3) (draw screen))))
      (assert-true (in-view session :sign-in))
      (choose "New")
      (assert-false (in-view session :sign-in))
      (let ((path (saving-to screen (format nil "menu-~d" (random 1000000000)))))
        (unwind-protect
             (progn (choose "Save…")
                    (assert-true (search "Saved to" (shown-name session :status))))
          (when (probe-file path) (delete-file path)))))))

;;; --- dragging ---

(defun middle-of (screen id)
  (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) id)
    (values (+ x (/ w 2)) (+ y (/ h 2)))))

(defun drag-to (screen x0 y0 x1 y1)
  "A press at X0, Y0, the pointer moved in steps to X1, Y1, and let go there: a frame for each."
  (pointer screen :move x0 y0)
  (pointer screen :down x0 y0)
  (loop :for step :from 1 :to 4
        :do (pointer screen :move (+ x0 (* (- x1 x0) (/ step 4))) (+ y0 (* (- y1 y0) (/ step 4)))))
  (pointer screen :up x1 y1)
  (dotimes (i 2) (draw screen)))

(defun left-of (session id) (first (s:node-bounds (m:find-node session :id id :within :canvas))))

(define-test a-widget-dragged-from-the-left-goes-where-it-is-let-go ()
  (multiple-value-bind (screen session) (open-designer)
    ;; Into the row of actions, between its two buttons.
    (multiple-value-bind (sx sy) (middle-of screen :add-button)
      (multiple-value-bind (ax ay aw ah) (core:lookup-target (screen-app screen) :sign-in)
        (let ((fx (core:lookup-target (screen-app screen) :forgot)))
          (drag-to screen sx sy (/ (+ ax aw fx) 2) (+ ay (/ ah 2))))))
    (assert-equal "Button :button-1" (shown-name session :selected-what))
    (assert-true (< (left-of session :sign-in) (left-of session :button-1)
                    (left-of session :forgot)))))

(define-test the-catalogue-is-in-groups-with-counts-and-searched ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (assert-equal "TEXT · 2" (shown-name session :group-text))
    (assert-equal "LAYOUT · 10" (shown-name session :group-layout))
    (assert-true (m:find-node session :id :add-button :within :group-input-grid))
    ;; Searched: what has the words in its name, in its group, counted.
    (m:set-value session "field" :id :palette-search)
    (assert-equal "INPUT · 1" (shown-name session :group-input))
    (assert-true (m:find-node session :id :add-text-field :within :left))
    (assert-false (ignore-errors (m:find-node session :id :add-button :within :left)))
    (assert-false (ignore-errors (m:find-node session :id :group-layout)))
    ;; Emptied, all of it again.
    (m:set-value session "" :id :palette-search)
    (assert-true (m:find-node session :id :add-button :within :left))))

(define-test what-is-dragged-from-the-left-says-the-id-it-will-have ()
  (multiple-value-bind (screen session) (open-designer)
    (m:click session :id :add-button :within :left)
    (multiple-value-bind (sx sy) (middle-of screen :add-button)
      (pointer screen :move sx sy)
      (pointer screen :down sx sy)
      (pointer screen :move (+ sx 40) (+ sy 40))
      ;; The view has a :button-1: this one will be :button-2.
      (assert-true (m:find-node session :name "Button :button-2"))
      (pointer screen :up (+ sx 40) (+ sy 40)))))

(define-test a-widget-of-the-view-dragged-moves-to-where-it-is-let-go ()
  (multiple-value-bind (screen session) (open-designer)
    ;; Forgot, from the row of actions to under the heading.
    (multiple-value-bind (fx fy) (middle-of screen :forgot)
      (multiple-value-bind (hx hy hw hh) (core:lookup-target (screen-app screen) :heading)
        (let ((ny (nth-value 1 (core:lookup-target (screen-app screen) :name-label))))
          (drag-to screen fx fy (+ hx (/ hw 2)) (/ (+ hy hh ny) 2)))))
    (assert-true (< (top-of session :heading) (top-of session :forgot)
                    (top-of session :name-label)))))

(define-test a-container-let-go-of-inside-itself-stays-where-it-was ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((before (top-of session :actions)))
      (multiple-value-bind (ax ay aw ah) (core:lookup-target (screen-app screen) :sign-in)
        (let ((fx (core:lookup-target (screen-app screen) :forgot)))
          (multiple-value-bind (sx sy) (middle-of screen :sign-in)
            ;; Pressed on the row itself (in the gap between its buttons), let go on its button.
            (drag-to screen (/ (+ ax aw fx) 2) (+ ay (/ ah 2)) sx sy))))
      (assert-equal before (top-of session :actions)))))

;;; --- views inside views ---

(define-test a-view-defined-is-listed-and-drawn-inside-the-view-being-designed ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :add-view-t-nested :within :left)
    (assert-equal "View :view-1" (shown-name session :selected-what))
    (assert-equal "t-nested" (shown-value session :option-name))
    ;; Its widgets, drawn inside, the outermost with the node's id.
    (assert-equal "Inside" (s:node-name (m:find-node session :id :inner :within :view-1)))))

;;; --- a view of the image, opened live ---

(defun view-file-with (name text &key code)
  "A file in the temporary directory that defines the view NAME (in CL-USER) with a label saying TEXT,
and, with CODE, a function too; loaded, as an app's system loads it. Its path."
  (let ((path (merge-pathnames (format nil "~(~a~).lisp" name) (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "(in-package #:common-lisp-user)~%~%(almighty-bolt:define-view ~(~a~)~%  ~
                   (:column :page (:gap 4) (:label :words (:text ~s))))~%"
              name text)
      (when code
        (format out "~%(defun ~(~a~)-words () ~s)~%" name text)))
    (load path)
    path))

(define-test a-view-of-the-image-is-opened-by-its-name-live-and-saved-to-its-own-file ()
  (multiple-value-bind (screen session) (open-designer)
    (let* ((name (intern (format nil "LIVE-~d" (random 1000000000)) :cl-user))
           (path (view-file-with name "Before"))
           (asked nil))
      (setf (core:context-file-chooser (screen-context screen))
            (lambda (kind types offered)
              (declare (ignore kind types offered))
              (setf asked t)
              nil))
      (unwind-protect
           (progn
             ;; The file it came from is known.
             (assert-equal (truename path) (truename (views:view-file name)))
             (m:click session :id :open-view)
             (m:click session :role :menu-item
                              :name (format nil "common-lisp-user::~(~a~)" name))
             (assert-equal "Before" (s:node-name (in-view session :words)))
             ;; An edit is made in the image as it is made: what draws the view shows it.
             (m:click session :id :words :within :canvas)
             (m:set-value session "After" :id :option-text)
             (assert-equal "After" (catalogue:form-option (find-form (views:find-view name) :words)
                                                          :text))
             (assert-true (search "Live" (shown-name session :status)) (shown-name session :status))
             ;; Undone, in the image too.
             (m:click session :id :undo :within :toolbar)
             (assert-equal "Before" (catalogue:form-option (find-form (views:find-view name) :words)
                                                           :text))
             (m:click session :id :redo :within :toolbar)
             ;; Saved to the file it came from, without asking where.
             (m:click session :id :save :within :toolbar)
             (assert-false asked)
             (assert-true (search "\"After\"" (uiop:read-file-string path))
                          (uiop:read-file-string path)))
        (remhash name views::*views*)
        (when (probe-file path) (delete-file path))))))

(define-test a-view-in-a-file-with-code-is-saved-elsewhere-and-the-file-is-left-as-it-is ()
  (multiple-value-bind (screen session) (open-designer)
    (let* ((name (intern (format nil "CODED-~d" (random 1000000000)) :cl-user))
           (path (view-file-with name "Before" :code t))
           (before (uiop:read-file-string path))
           (asked nil))
      (setf (core:context-file-chooser (screen-context screen))
            (lambda (kind types offered)
              (declare (ignore kind types offered))
              (setf asked t)
              nil))
      (unwind-protect
           (progn
             (m:click session :id :open-view)
             (m:click session :role :menu-item
                              :name (format nil "common-lisp-user::~(~a~)" name))
             (m:click session :id :words :within :canvas)
             (m:set-value session "After" :id :option-text)
             ;; Live in the image, but Save asks where: the file has code, which writing the view
             ;; there would lose.
             (assert-equal "After" (catalogue:form-option (find-form (views:find-view name) :words)
                                                          :text))
             (m:click session :id :save :within :toolbar)
             (dotimes (i 2) (draw screen))
             (assert-true asked)
             (assert-equal before (uiop:read-file-string path)))
        (remhash name views::*views*)
        (when (probe-file path) (delete-file path))))))
