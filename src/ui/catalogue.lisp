(defpackage #:almighty-bolt/ui/catalogue
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:button
                #:text-field
                #:column
                #:row)
  (:export #:entry
           #:entry-kind
           #:entry-title
           #:entry-options
           #:entry-wires
           #:entry-container-p
           #:option
           #:option-name
           #:option-type
           #:option-default
           #:+catalogue+
           #:find-entry
           #:option-value-p
           #:check-view-form
           #:form-option
           #:draw-form))

(in-package #:almighty-bolt/ui/catalogue)

;;; The widgets a view can be made of, and what each takes: its options, with what kind of thing
;;; each is (a designer shows an editor of that kind for it) and what it is when not given, and what
;;; an app's code can wire to it besides (WIRES: what a press does, the variable a field shows and
;;; sets, what goes inside a container). An entry draws a widget of its kind with the real widget, so
;;; a view in a designer looks as it will in the app. The option names are the widget's own (a
;;; label's :text is its text), so a view's form reads as the calls it stands for.
;;;
;;; A view's form is (kind id (option value ...) child-form ...), as
;;; (:row :actions (:gap 8) (:button :sign-in (:text "Sign in"))).

(defstruct (option (:constructor option (name type default)))
  "An option called NAME, of TYPE: :string, :number, :boolean or (:choice keyword ...)."
  (name :text :type keyword :read-only t)
  (type :string :read-only t)
  (default nil :read-only t))

(defstruct (entry (:constructor entry (kind title options wires draw &key container-p)))
  "The widget of KIND, called TITLE in a designer. WIRES are the keys an app's code can wire to it
besides its options. DRAW draws a widget of the kind: a function of its id, a function that draws
what is in it, a function of an option's name that is its value, and a function of a wire's key
that is what the app wired there (a function), or NIL."
  (kind :label :type keyword :read-only t)
  (title "" :type string :read-only t)
  (options '() :type list :read-only t)
  (wires '() :type list :read-only t)
  (draw nil :read-only t)
  (container-p nil :read-only t))

(defparameter +main-alignments+ '(:choice :start :center :end :space-between :space-around :space-evenly)
  "Where a column's or a row's children go along it, as the layout takes it.")

(defparameter +cross-alignments+ '(:choice :start :center :end :stretch)
  "Where they go across it.")

(defun call-wired (wired key &rest arguments)
  "Call what the app wired at KEY, if it wired anything there."
  (let ((function (funcall wired key)))
    (when function
      (apply function arguments))))

(defun draw-container (children wired)
  "What is in a container: what the view has there, then what the app wired as its :contents."
  (funcall children)
  (call-wired wired :contents))

(defparameter +catalogue+
  (list (entry :label "Label"
               (list (option :text :string "Label")
                     (option :size :number 15)
                     (option :weight '(:choice :regular :bold) :regular))
               '()
               (lambda (id children value wired)
                 (declare (ignore children wired))
                 (label (funcall value :text) :id id :size (funcall value :size)
                                              :weight (funcall value :weight))))
        (entry :button "Button"
               (list (option :text :string "Button")
                      (option :enabled :boolean t))
               '(:on-press)
               (lambda (id children value wired)
                 (declare (ignore children))
                 (when (button (funcall value :text) :id id :enabled (funcall value :enabled))
                   (call-wired wired :on-press))))
        (entry :text-field "Text field"
               (list (option :text :string "")
                     (option :width :number 180)
                     (option :enabled :boolean t))
               ;; :value is what the field shows (its :text), and the place typing sets.
               '(:value)
               (lambda (id children value wired)
                 (declare (ignore children))
                 (multiple-value-bind (text changed)
                     (text-field (funcall value :text) :id id :width (funcall value :width)
                                                       :enabled (funcall value :enabled))
                   (when changed
                     (call-wired wired :value text)))))
        (entry :column "Column"
               (list (option :gap :number 0)
                     (option :main-align +main-alignments+ :start)
                     (option :cross-align +cross-alignments+ :start))
               '(:contents)
               (lambda (id children value wired)
                 (column (:id id :gap (funcall value :gap)
                          :main-align (funcall value :main-align)
                          :cross-align (funcall value :cross-align))
                   (draw-container children wired)))
               :container-p t)
        (entry :row "Row"
               (list (option :gap :number 0)
                     (option :main-align +main-alignments+ :start)
                     (option :cross-align +cross-alignments+ :start))
               '(:contents)
               (lambda (id children value wired)
                 (row (:id id :gap (funcall value :gap)
                       :main-align (funcall value :main-align)
                       :cross-align (funcall value :cross-align))
                   (draw-container children wired)))
               :container-p t))
  "The kinds of widget a view can have, in the order a designer lists them.")

(defun find-entry (kind)
  (or (find kind +catalogue+ :key #'entry-kind)
      (error "There is no widget of the kind ~s; there are ~{~(~s~)~^, ~}." kind
             (mapcar #'entry-kind +catalogue+))))

(defun option-value-p (option value)
  "True if VALUE is one that OPTION can have, by its type."
  (let ((type (option-type option)))
    (cond ((eq type :string) (stringp value))
          ((eq type :number) (realp value))
          ((eq type :boolean) (member value '(t nil)))
          ((and (consp type) (eq (first type) :choice)) (member value (rest type)))
          (t nil))))

(defun check-view-form (form)
  "Signal an error that says what is wrong if FORM is not a view that can be drawn: a widget of a
kind the catalogue does not have, an option its kind does not have or a value that option cannot
have, an id that two widgets have, or something inside a widget that holds nothing. Returns FORM."
  (let ((ids '()))
    (labels ((check (form)
               (unless (and (consp form) (keywordp (first form)) (keywordp (second form))
                            (listp (third form)) (evenp (length (third form))))
                 (error "~s is not a widget of a view: (kind id (option value ...) what-is-in-it ...)."
                        form))
               (destructuring-bind (kind id options &rest children) form
                 (let ((entry (find-entry kind)))
                   (when (member id ids)
                     (error "Two widgets have the id ~s." id))
                   (push id ids)
                   (loop :for (name value) :on options :by #'cddr
                         :do (let ((option (find name (entry-options entry) :key #'option-name)))
                               (unless option
                                 (error "A ~(~a~) has no option ~s." kind name))
                               (unless (option-value-p option value)
                                 (error "~s is not a value of the option ~s of a ~(~a~)."
                                        value name kind))))
                   (when (and children (not (entry-container-p entry)))
                     (error "A ~(~a~) holds nothing, and ~s has something in it." kind id))
                   (mapc #'check children)))))
      (check form))
    form))

(defun form-option (form name)
  "The value of the option NAME of the widget FORM: what it was given, or its kind's default."
  (destructuring-bind (kind id options &rest children) form
    (declare (ignore id children))
    (let ((given (getf options name '%none)))
      (if (eq given '%none)
          (let ((option (find name (entry-options (find-entry kind)) :key #'option-name)))
            (unless option
              (error "A ~(~a~) has no option ~s." kind name))
            (option-default option))
          given))))

(defun draw-form (form &key wires id)
  "Draw the view FORM with the real widgets. WIRES is what an app wired to it, a list with one
element for each widget it wired, (id options wire ...): OPTIONS a property list of option values
that are used in place of the view's, then a wire's key and the function wired there. ID, if
given, is the id the view's outermost widget has in place of its own, so that one view can be
drawn more than once in one container."
  (labels ((draw (form outermost)
             (destructuring-bind (kind id* options &rest children) form
               (declare (ignore options))
               (let ((wire (and wires (assoc id* wires))))
                 (flet ((children ()
                          (dolist (child children)
                            (draw child nil)))
                        (value (name)
                          (let ((wired (getf (second wire) name '%none)))
                            (if (eq wired '%none)
                                (form-option form name)
                                wired)))
                        (wired (key)
                          (getf (cddr wire) key)))
                   (declare (dynamic-extent #'children #'value #'wired))
                   (funcall (entry-draw (find-entry kind))
                            (if (and outermost id) id id*)
                            #'children #'value #'wired))))))
    (draw form t)))
