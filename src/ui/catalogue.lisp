(defpackage #:almighty-bolt/ui/catalogue
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:paragraph
                #:button
                #:text-field
                #:text-area
                #:combo-box
                #:slider
                #:choice
                #:checkbox
                #:switch
                #:dropdown
                #:list-box
                #:progress
                #:spinner
                #:column
                #:row
                #:grid
                #:stack
                #:padding
                #:panel
                #:center
                #:scroll
                #:pressable)
  (:export #:entry
           #:catalogue
           #:define-widget-entry
           #:entry-value
           #:entry-kind
           #:entry-title
           #:entry-options
           #:entry-wires
           #:entry-container-p
           #:option
           #:option-name
           #:option-type
           #:option-default
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

;; Here when what follows is compiled, since an entry is put in the catalogue then too.
(eval-when (:compile-toplevel :load-toplevel :execute)
(defstruct (option (:constructor option (name type default)))
  "An option called NAME, of TYPE: :string, :number, :boolean, :list (of strings), :view (the name
of a view, or NIL) or (:choice keyword ...)."
  (name :text :type keyword :read-only t)
  (type :string :read-only t)
  (default nil :read-only t))

(defstruct (entry (:constructor make-entry (kind title options wires value draw container-p)))
  "The widget of KIND, called TITLE in a designer, with its OPTIONS. WIRES are the keys an app's code
can wire to it besides its options: :on-press and the like, :value if VALUE names the option that a
place wired as :value shows (and typing or moving sets), :contents for a container. DRAW draws a
widget of the kind: a function of its id, a function that draws what is in it, a function of an
option's name that is its value, and a function of a wire's key that is what the app wired there,
or NIL."
  (kind :label :type keyword :read-only t)
  (title "" :type string :read-only t)
  (options '() :type list :read-only t)
  (wires '() :type list :read-only t)
  (value nil :read-only t)
  (draw nil :read-only t)
  (container-p nil :read-only t))

(defvar *entries* '()
  "The entries of the catalogue, in the order they were first defined.")

(defun catalogue ()
  "The kinds of widget a view can have, each an ENTRY, in the order a designer lists them."
  *entries*)

(defun add-entry (entry)
  "Put ENTRY in the catalogue, in the place of one of its kind if there is one."
  (let ((place (position (entry-kind entry) *entries* :key #'entry-kind)))
    (if place
        (setf *entries* (append (subseq *entries* 0 place) (list entry) (subseq *entries* (1+ place))))
        (setf *entries* (append *entries* (list entry))))
    (entry-kind entry)))

(defun index-in (index items)
  "INDEX made a whole number that is a place in ITEMS (the first if it is not one)."
  (let ((i (round index)))
    (if (< -1 i (length items)) i 0)))

(defun call-wired (wired key &rest arguments)
  "Call what the app wired at KEY, if it wired anything there."
  (let ((function (funcall wired key)))
    (when function
      (apply function arguments)))))

(defmacro define-widget-entry (kind (&key title options wires value container) &body body)
  "Put the widget of KIND (a keyword) in the catalogue, which a designer lists and a view
(DEFINE-VIEW) can have, called TITLE. OPTIONS are what it is given, each (name type default) with
TYPE one of :string, :number, :boolean, :list (of strings) or (:choice keyword ...). WIRES are what
an app's code can give it besides its options (:on-press). VALUE, if given, names the option that a
place wired as :value shows, and that the widget sets by calling SET-VALUE. A CONTAINER holds what
the view puts in it, which BODY draws by calling (CONTENTS).

It is in the catalogue when it is compiled too, so that a view defined after it in the same file
can have it. BODY draws one, with the real widget, each frame: in it ID is its id, each option is a variable of
its name (in this package) holding its value, each wire a variable holding the function the app
wired there or NIL, and SET-VALUE the function that sets the place wired as :value, or NIL.

  (define-widget-entry :slider (:title \"Slider\"
                                :options ((:value :number 50) (:minimum :number 0)
                                          (:maximum :number 100))
                                :value :value)
    (multiple-value-bind (now moved) (slider value minimum maximum :id id)
      (when (and moved set-value) (funcall set-value now))))"
  (flet ((var (keyword) (intern (symbol-name keyword) *package*)))
    (let ((id (var :id)) (contents (var :contents)) (set-value (var :set-value))
          (children (gensym "CHILDREN")) (value-of (gensym "VALUE")) (wired (gensym "WIRED")))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
        (add-entry
        (make-entry ,kind ,title
                    (list ,@(loop :for (name type default) :in options
                                  :collect `(option ,name ',type ',default)))
                    ',(append wires (and value '(:value)) (and container '(:contents)))
                    ,value
                    (lambda (,id ,children ,value-of ,wired)
                      (declare (ignorable ,id ,children ,value-of ,wired))
                      (let (,@(loop :for (name) :in options
                                    :collect `(,(var name) (funcall ,value-of ,name)))
                            ,@(loop :for key :in wires
                                    :collect `(,(var key) (funcall ,wired ,key)))
                            ,@(and value `((,set-value (funcall ,wired :value)))))
                        (declare (ignorable ,@(mapcar (lambda (o) (var (first o))) options)
                                            ,@(mapcar #'var wires)
                                            ,@(and value (list set-value))))
                        (flet ((,contents ()
                                 (funcall ,children)
                                 (call-wired ,wired :contents)))
                          (declare (dynamic-extent #',contents) (ignorable #',contents))
                          ,@body)))
                    ,(and container t)))))))

(defparameter +main-alignments+ '(:choice :start :center :end :space-between :space-around :space-evenly)
  "Where a column's or a row's children go along it, as the layout takes it.")

(defparameter +cross-alignments+ '(:choice :start :center :end :stretch)
  "Where they go across it.")

;;; --- the library's widgets ---

(define-widget-entry :label (:title "Label"
                             :options ((:text :string "Label") (:size :number 15)
                                       (:weight (:choice :regular :bold) :regular)))
  (label text :id id :size size :weight weight))

(define-widget-entry :paragraph (:title "Paragraph"
                                 :options ((:text :string "Some words, broken into lines to fit.")
                                           (:size :number 15)))
  (paragraph text :id id :size size))

(define-widget-entry :button (:title "Button"
                              :options ((:text :string "Button") (:enabled :boolean t))
                              :wires (:on-press))
  (when (and (button text :id id :enabled enabled) on-press)
    (funcall on-press)))

(define-widget-entry :text-field (:title "Text field"
                                  :options ((:text :string "") (:width :number 180)
                                            (:invalid :boolean nil) (:enabled :boolean t))
                                  :value :text)
  (multiple-value-bind (now changed) (text-field text :id id :width width :invalid invalid
                                                      :enabled enabled)
    (when (and changed set-value) (funcall set-value now))))

(define-widget-entry :text-area (:title "Text area"
                                 :options ((:text :string "") (:width :number 320)
                                           (:rows :number 6) (:enabled :boolean t))
                                 :value :text)
  (multiple-value-bind (now changed) (text-area text :id id :width width :rows (max 1 (round rows))
                                                     :enabled enabled)
    (when (and changed set-value) (funcall set-value now))))

(define-widget-entry :combo-box (:title "Combo box"
                                 :options ((:text :string "") (:suggestions :list ("One" "Two"))
                                           (:width :number 180) (:enabled :boolean t))
                                 :value :text)
  (multiple-value-bind (now changed) (combo-box text suggestions :id id :width width
                                                                 :enabled enabled)
    (when (and changed set-value) (funcall set-value now))))

(define-widget-entry :slider (:title "Slider"
                              :options ((:value :number 50) (:minimum :number 0)
                                        (:maximum :number 100) (:width :number 200)
                                        (:enabled :boolean t))
                              :value :value)
  (multiple-value-bind (now moved) (slider value minimum (max maximum minimum) :id id :width width
                                                                               :enabled enabled)
    (when (and moved set-value) (funcall set-value now))))

(define-widget-entry :choice (:title "Choice"
                              :options ((:options :list ("One" "Two" "Three"))
                                        (:selected :number 0)
                                        (:direction (:choice :horizontal :vertical) :horizontal)
                                        (:style (:choice :radio :segmented) :radio)
                                        (:enabled :boolean t))
                              :value :selected)
  (multiple-value-bind (now moved)
      (choice (or options '("")) (index-in selected (or options '(""))) :id id
              :direction direction :style style :enabled enabled)
    (when (and moved set-value) (funcall set-value now))))

(define-widget-entry :checkbox (:title "Check box"
                                :options ((:label :string "Check box") (:checked :boolean nil)
                                          (:enabled :boolean t))
                                :value :checked)
  (multiple-value-bind (now changed) (checkbox label checked :id id :enabled enabled)
    (when (and changed set-value) (funcall set-value now))))

(define-widget-entry :switch (:title "Switch"
                              :options ((:label :string "Switch") (:on :boolean nil)
                                        (:enabled :boolean t))
                              :value :on)
  (multiple-value-bind (now changed) (switch label on :id id :enabled enabled)
    (when (and changed set-value) (funcall set-value now))))

(define-widget-entry :dropdown (:title "Dropdown"
                                :options ((:options :list ("One" "Two" "Three"))
                                          (:selected :number 0) (:width :number 160)
                                          (:enabled :boolean t))
                                :value :selected)
  (multiple-value-bind (now moved)
      (dropdown (or options '("")) (index-in selected (or options '(""))) :id id
                :width width :enabled enabled)
    (when (and moved set-value) (funcall set-value now))))

(define-widget-entry :list-box (:title "List box"
                                :options ((:items :list ("One" "Two" "Three"))
                                          (:selected :number 0) (:rows :number 6)
                                          (:width :number 200) (:enabled :boolean t))
                                :value :selected)
  (multiple-value-bind (now moved)
      (list-box items (and items (index-in selected items)) :id id
                :rows (max 1 (round rows)) :width width :enabled enabled)
    (when (and moved set-value) (funcall set-value now))))

(define-widget-entry :progress (:title "Progress"
                                :options ((:fraction :number 0.5) (:width :number 200)))
  (progress (max 0 (min 1 fraction)) :id id :width width))

(define-widget-entry :spinner (:title "Spinner" :options ((:size :number 18)))
  (spinner :id id :size size))

;;; Not the spacer: it takes the free room of its row or column, and a column whose height is not
;;; bounded (a view's, which is as tall as what is in it) has none, which layout says is an error.

;;; --- the library's containers ---

(define-widget-entry :column (:title "Column"
                              :options ((:gap :number 0) (:main-align (:choice :start :center :end :space-between :space-around :space-evenly) :start)
                                        (:cross-align (:choice :start :center :end :stretch) :start))
                              :container t)
  (column (:id id :gap gap :main-align main-align :cross-align cross-align)
    (contents)))

(define-widget-entry :row (:title "Row"
                           :options ((:gap :number 0) (:main-align (:choice :start :center :end :space-between :space-around :space-evenly) :start)
                                     (:cross-align (:choice :start :center :end :stretch) :start))
                           :container t)
  (row (:id id :gap gap :main-align main-align :cross-align cross-align)
    (contents)))

(define-widget-entry :grid (:title "Grid"
                            :options ((:columns :number 2) (:gap :number 8))
                            :container t)
  (grid (:id id :columns (max 1 (round columns)) :gap gap)
    (contents)))

(define-widget-entry :stack (:title "Stack" :container t)
  (stack (:id id)
    (contents)))

(define-widget-entry :padding (:title "Padding" :options ((:all :number 12)) :container t)
  (padding (:id id :all all)
    (contents)))

(define-widget-entry :panel (:title "Panel"
                             :options ((:all :number 12) (:radius :number 8))
                             :container t)
  (panel (:id id :all all :radius radius)
    (contents)))

(define-widget-entry :center (:title "Center" :container t)
  (center (:id id)
    (contents)))

(define-widget-entry :scroll (:title "Scroll" :options ((:height :number 200)) :container t)
  (scroll (:id id :height height)
    (contents)))

(define-widget-entry :pressable (:title "Pressable"
                                 :options ((:look (:choice :plain :button) :plain)
                                           (:enabled :boolean t))
                                 :wires (:on-press)
                                 :container t)
  (when (and (pressable (:id id :look look :enabled enabled) (contents)) on-press)
    (funcall on-press)))

(defun find-entry (kind)
  (or (find kind (catalogue) :key #'entry-kind)
      (error "There is no widget of the kind ~s; there are ~{~(~s~)~^, ~}." kind
             (mapcar #'entry-kind (catalogue)))))

(defun option-value-p (option value)
  "True if VALUE is one that OPTION can have, by its type."
  (let ((type (option-type option)))
    (cond ((eq type :string) (stringp value))
          ((eq type :number) (realp value))
          ((eq type :boolean) (member value '(t nil)))
          ((eq type :list) (and (listp value) (every #'stringp value)))
          ((eq type :view) (symbolp value))
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
