(defpackage #:almighty-bolt/ui/inspector
  (:use #:cl)
  (:import-from #:almighty-bolt/app-instance
                #:request-frame)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
                #:box-kind
                #:box-theme
                #:box-enabled
                #:box-w
                #:box-h)
  (:import-from #:almighty-bolt/ui/ui
                #:ui-inspection
                #:make-inspection
                #:inspection-picking
                #:inspection-picked)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:theme-name
                #:find-theme
                #:theme-setting
                #:theme-form
                #:change-theme-setting
                #:+theme-settings+
                #:+theme-colors+
                #:color-hex)
  (:import-from #:almighty-bolt/ui/widgets
                #:with-theme
                #:column
                #:row
                #:panel
                #:scroll
                #:label
                #:paragraph
                #:button
                #:text-field
                #:shortcut)
  (:import-from #:almighty-bolt/ui/inspection
                #:box-by-key)
  (:import-from #:almighty-bolt/ui/immediate
                #:immediate-app-ui
                #:current-context)
  (:import-from #:almighty-bolt/ui/semantics
                #:box-facts)
  (:import-from #:almighty-bolt/ui/views
                #:view-widget
                #:wired-keys)
  (:export #:make-inspector))

(in-package #:almighty-bolt/ui/inspector)

;;; The inspector, in a window of its own beside the app's while the app is developed (Command-Option-I
;;; in the app's window opens and closes it; see app.lisp). Its Pick button makes the next press on
;;; the app pick the widget under the pointer instead of pressing it (inspection.lisp takes the press
;;; in the app's window, and outlines what was picked); the inspector says what the picked one is,
;;; changes its value as a hand would, and changes the theme it is drawn in. It is an app like any
;;; other, made by MAKE-INSPECTOR, and what it looks at is given to it: a function that returns the
;;; app's instance (which compiling code replaces; the ui, which holds the pick, goes to the new one),
;;; and one that does what it asks.
;;;
;;; It changes a widget as a person would, by the machine interface's actions (a click, SET-VALUE),
;;; whose events go to the app as a hand's do: the app stays in charge of what it holds. An action
;;; waits for the app's window between its steps (a dropdown's menu opens before its row is pressed),
;;; which the main thread, drawing the inspector, cannot do; so it is handed to ACT, which in a
;;; window does it on a thread of its own, and says what went wrong, if anything, with REPORT.
;;;
;;; It is drawn in the theme its window had when it was opened, so that changing that theme in it
;;; changes the app and not the inspector under the pointer.

(defstruct (inspector (:constructor make-inspector-state (app act edit-view)))
  ;; A function of no arguments that returns the app's instance, or NIL if its window has closed.
  (app nil :type function)
  ;; A function of an action (see MACHINE:PERFORM) and of REPORT, a function of what went wrong.
  (act nil :type function)
  ;; A function of a view's name and the id of a widget in it, which opens the view to be edited
  ;; with the widget selected (a designer beside the inspector); NIL if there is none.
  (edit-view nil)
  ;; What it asked of the app's window (made at the first frame, in the theme of then).
  (inspection nil)
  (theme nil)
  ;; What is known of the picked widget, and the key it was known for.
  (facts nil)
  ;; What the field holds, and the key of the box it was filled for; what went wrong with the last
  ;; thing asked for, if anything.
  (draft "" :type string)
  (draft-for 0 :type fixnum)
  (message nil)
  ;; The fields of the theme's part: which setting, and what to make it.
  (setting-draft "" :type string)
  (setting-value-draft "" :type string))

(defun make-inspector (target act &key edit-view)
  "The function that describes the inspector's window, for the app whose instance TARGET (a function
of no arguments) returns. ACT is called with an action that the inspector asks for (a property list
that MACHINE:PERFORM takes) and a function to call with what went wrong, if anything; it should do
the action after the frame, not in it. EDIT-VIEW, if given, is called with a view's name and the id
of a widget in it when Edit in the View is pressed for a widget a view drew."
  (let ((inspector (make-inspector-state target act edit-view)))
    (lambda () (describe-inspector inspector))))

;;; --- what it knows of the picked widget ---

(defun facts-of (ui box x y)
  "What the inspector says of BOX of UI, which is at X, Y in the window, as a property list."
  (append (list :kind (box-kind box) :id (box-id box) :enabled (box-enabled box)
                :theme (theme-name (box-theme box)) :x x :y y :w (box-w box) :h (box-h box))
          ;; The view that drew it, the widget of the view it is, and what of it the code gives.
          (multiple-value-bind (view widget) (view-widget box)
            (and view (list :view view :view-widget widget
                            :wired (wired-keys view widget))))
          (box-facts ui box x y)))

(defun shown (value)
  "VALUE as Lisp writes it, symbols in lower case."
  (let ((*print-case* :downcase))
    (prin1-to-string value)))

(defun fact-lines (facts)
  "The lines the inspector shows for FACTS."
  (destructuring-bind (&key kind id enabled theme x y w h role name (value nil value-p) state
                         view view-widget wired &allow-other-keys)
      facts
    (append (list (format nil "Kind: ~(~a~)" kind))
            (and view (list (format nil "View: ~a, its ~a" (shown view) (shown view-widget))
                            (if wired
                                (format nil "Given by the code: ~{~a~^, ~}"
                                        (mapcar #'shown wired))
                                "Given by the code: nothing; the view decides it all")))
            (and role (list (format nil "Role: ~(~a~)" role)))
            (list (if id (format nil "Id: ~a" (shown id)) "Id: none"))
            (and name (list (format nil "Name: ~a" name)))
            (and value-p (list (format nil "Value: ~a" (shown value))))
            (list (format nil "At: ~d, ~d" (round x) (round y))
                  (format nil "Size: ~d by ~d" (round w) (round h))
                  (if theme (format nil "Theme: ~a" (shown theme)) "Theme: one with no name")
                  (if enabled "Enabled" "Not enabled"))
            ;; Not whether the pointer is over it or it has the keyboard, which a person's pointer and
            ;; keyboard on the way to the inspector change.
            (loop :for (key value) :on state :by #'cddr
                  :unless (member key '(:hovered :focused))
                    :collect (format nil "~(~a~): ~a" key (shown value))))))

(defun draft-of (value)
  "VALUE as the field shows it to be changed: a number as a person writes it."
  (typecase value
    (string value)
    (real (format nil "~f" value))
    (t "")))

(defun number-in (string)
  "The number STRING is, or NIL."
  (let ((value (let ((*read-eval* nil))
                 (ignore-errors (read-from-string string)))))
    (and (realp value) value)))

;;; --- what it changes ---

(defun ask (inspector op &rest value)
  "Ask for OP to be done to the picked widget (with VALUE, if given), by its id, which every widget
that can be changed has. What went wrong is shown when ACT says so."
  (let ((context (current-context))
        (action (append (list :op op) (and value (list :value (first value)))
                        (list :id (getf (inspector-facts inspector) :id)))))
    (setf (inspector-message inspector) nil)
    (funcall (inspector-act inspector) action
             (lambda (message)
               (setf (inspector-message inspector) message)
               (request-frame context)))))

(defun describe-changes (inspector)
  "The controls for changing the picked widget, by its role."
  (let* ((facts (inspector-facts inspector))
         (role (getf facts :role))
         (state (getf facts :state)))
    (when (member role '(:button :menu-button :checkbox :switch :pop-up-button :toggle-button))
      (when (button "Press" :id :inspector-press)
        (ask inspector :click)))
    (when (member role '(:text-field :combo-box :text-area :slider))
      (row (:id :inspector-set-row :gap 8)
        (setf (inspector-draft inspector)
              (text-field (inspector-draft inspector) :id :inspector-draft :width 180))
        (when (button "Set" :id :inspector-set)
          (if (eq role :slider)
              (let ((number (number-in (inspector-draft inspector))))
                (if number
                    (ask inspector :set-value number)
                    (setf (inspector-message inspector)
                          (format nil "~s is not a number." (inspector-draft inspector)))))
              (ask inspector :set-value (inspector-draft inspector))))))
    ;; A button for each of the options of a choice or a dropdown, or the items of a list.
    (let ((options (or (getf state :options) (getf state :items))))
      (when (member role '(:radio-group :pop-up-button :list))
        (column (:id :inspector-options :gap 4)
          (loop :for option :in options
                :for i :from 0
                :do (when (button (format nil "Choose ~a" option) :id (list :inspector-option i))
                      ;; A dropdown is set by the words of its row, a choice or a list by the index.
                      (ask inspector :set-value (if (eq role :pop-up-button) option i)))))))))

(defun setting-named (words)
  "The setting of a theme (one of +THEME-SETTINGS+) that WORDS name, with or without a colon, or NIL."
  (let ((words (string-left-trim ":" (string-trim " " words))))
    (find words +theme-settings+ :key #'symbol-name :test #'string-equal)))

(defun setting-value (key words)
  "What WORDS, typed for the setting KEY, are: a colour as it is written, a font's name, or a
number. Signals an error that says what is wrong."
  (cond ((or (member key +theme-colors+) (eq key :font-name)) words)
        (t (let ((number (number-in words)))
             (unless number
               (error "~a takes a number, and ~s is not one." (shown key) words))
             (if (member key '(:font-size :radius)) (round number) number)))))

(defun theme-source (theme)
  "THEME's DEFINE-THEME form as it would be written in an app: by the library's name, a setting to a
line."
  (destructuring-bind (operator name (&key base) &rest settings) (theme-form theme)
    (declare (ignore operator))
    (with-output-to-string (out)
      (let ((*print-case* :downcase))
        (format out "(almighty-bolt:define-theme ~s (:base ~s)" name base)
        (loop :for (key value) :on settings :by #'cddr
              :do (format out "~%  ~s ~s" key value))
        (format out ")")))))

(defun describe-theme-change (inspector)
  "A setting of the theme the picked widget is drawn in changed, which defines the theme again, and
the theme written down."
  (let ((name (getf (inspector-facts inspector) :theme)))
    (label "Its theme" :id :inspector-theme-title :size 16)
    (if (null name)
        (paragraph "It is drawn in a theme with no name (one MAKE-THEME made), which cannot be defined again here."
                   :id :inspector-theme-unnamed)
        (let ((key (setting-named (inspector-setting-draft inspector))))
          (paragraph "A setting (accent, row-height, font-size, ...), and what to make it:"
                     :id :inspector-setting-hint)
          (row (:id :inspector-setting-row :gap 8)
            (setf (inspector-setting-draft inspector)
                  (text-field (inspector-setting-draft inspector) :id :inspector-setting :width 120))
            (setf (inspector-setting-value-draft inspector)
                  (text-field (inspector-setting-value-draft inspector) :id :inspector-setting-value
                                                                       :width 120)))
          (paragraph (if key
                         (format nil "~a is ~a now." (shown key)
                                 (let ((value (theme-setting (find-theme name) key)))
                                   (if (member key +theme-colors+)
                                       (color-hex value)
                                       (shown value))))
                         "No setting yet.")
                     :id :inspector-setting-now)
          (when (button "Change the theme" :id :inspector-change-theme)
            (setf (inspector-message inspector)
                  (if key
                      (handler-case
                          (progn (change-theme-setting
                                  name key (setting-value key (inspector-setting-value-draft inspector)))
                                 nil)
                        (error (condition) (princ-to-string condition)))
                      (format nil "A theme has no setting called ~s."
                              (inspector-setting-draft inspector)))))
          ;; Written down, to be copied into the app's code.
          (paragraph (theme-source (find-theme name)) :id :inspector-theme-form :selectable t)))))

;;; --- the window ---

(defun look-at-app (inspector ui)
  "Put the inspector's inspection in the app's UI (again, if the app's ui is new), and note what is
known of the picked widget from the app's last frame."
  (let ((inspection (inspector-inspection inspector)))
    (unless (eq (ui-inspection ui) inspection)
      (setf (ui-inspection ui) inspection))
    (let ((picked (inspection-picked inspection)))
      (setf (inspector-facts inspector)
            (multiple-value-bind (box x y) (if (zerop picked) nil (box-by-key ui picked))
              (and box (facts-of ui box x y))))
      ;; A new pick fills the field with its value, and forgets what went wrong before.
      (unless (= (inspector-draft-for inspector) picked)
        (setf (inspector-draft-for inspector) picked
              (inspector-draft inspector) (draft-of (getf (inspector-facts inspector) :value))
              (inspector-message inspector) nil)))))

(defun describe-inspector (inspector)
  "Describe the inspector's window."
  (unless (inspector-theme inspector)
    (setf (inspector-theme inspector) *theme*
          (inspector-inspection inspector) (make-inspection *theme*)))
  (let* ((inspection (inspector-inspection inspector))
         (instance (funcall (inspector-app inspector)))
         (ui (and instance (immediate-app-ui instance))))
    (when ui
      (look-at-app inspector ui)
      (when (and (inspection-picking inspection) (shortcut #\Escape))
        (setf (inspection-picking inspection) nil)))
    (with-theme ((inspector-theme inspector))
      (panel (:id :inspector :radius 0 :flex 1)
        (scroll (:id :inspector-scroll)
          (column (:id :inspector-column :gap 10)
            (label "Inspector" :id :inspector-title :size 18)
            (cond
              ((null ui)
               (paragraph "The app's window is closed." :id :inspector-hint))
              (t
               (when (button (if (inspection-picking inspection) "Stop picking" "Pick")
                             :id :inspector-pick)
                 (setf (inspection-picking inspection) (not (inspection-picking inspection))))
               (cond ((inspection-picking inspection)
                      (paragraph "Press a widget in the app's window to pick it. Escape stops."
                                 :id :inspector-hint))
                     ((zerop (inspection-picked inspection))
                      (paragraph "Press Pick, then a widget." :id :inspector-hint))
                     ((null (inspector-facts inspector))
                      (paragraph "What was picked is not on the screen now." :id :inspector-hint))
                     (t
                      (column (:id :inspector-facts :gap 4)
                        (loop :for line :in (fact-lines (inspector-facts inspector))
                              :for i :from 0
                              :do (paragraph line :id (list :inspector-fact i))))
                      (let ((facts (inspector-facts inspector)))
                        (when (and (getf facts :view) (inspector-edit-view inspector)
                                   (button "Edit in the View" :id :inspector-edit-view))
                          (funcall (inspector-edit-view inspector)
                                   (getf facts :view) (getf facts :view-widget))))
                      (describe-changes inspector)
                      ;; What went wrong with the last change, if anything, under what changes.
                      (when (inspector-message inspector)
                        (paragraph (inspector-message inspector) :id :inspector-message))
                      (describe-theme-change inspector)))))))))))
