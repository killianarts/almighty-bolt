(defpackage #:almighty-bolt-gui-designer/catalogue
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:import-from #:almighty-bolt-gui-designer/view
                #:node-kind
                #:node-id
                #:node-options
                #:node-children)
  (:export #:entry
           #:entry-kind
           #:entry-title
           #:entry-options
           #:entry-container-p
           #:option
           #:option-name
           #:option-type
           #:option-default
           #:+catalogue+
           #:find-entry
           #:option-value-p
           #:node-option
           #:draw-node))

(in-package #:almighty-bolt-gui-designer/catalogue)

;;; The widgets a view can be made of, and what each takes: its options, with what kind of thing
;;; each is (the designer shows an editor of that kind for it) and what it is when not given. An
;;; entry also draws a node of its kind with the real widget, so that the design looks as the app
;;; will. The option names are the widget's own (a label's :text is its text), so the view's form
;;; reads as the calls it stands for.

(defstruct (option (:constructor option (name type default)))
  "An option called NAME, of TYPE: :string, :number, :boolean or (:choice keyword ...)."
  (name :text :type keyword :read-only t)
  (type :string :read-only t)
  (default nil :read-only t))

(defstruct (entry (:constructor entry (kind title options draw &key container-p)))
  "The widget of KIND, called TITLE in the designer. DRAW draws a node of the kind: a function of the
node, a function that draws its children, and the node's option values by name."
  (kind :label :type keyword :read-only t)
  (title "" :type string :read-only t)
  (options '() :type list :read-only t)
  (draw nil :read-only t)
  (container-p nil :read-only t))

(defparameter +alignments+ '(:choice :start :center :end :stretch))

(defparameter +catalogue+
  (list (entry :label "Label"
               (list (option :text :string "Label")
                     (option :size :number 15)
                     (option :weight '(:choice :regular :bold) :regular))
               (lambda (id children value)
                 (declare (ignore children))
                 (bolt:label (funcall value :text) :id id :size (funcall value :size)
                                                   :weight (funcall value :weight))))
        (entry :button "Button"
               (list (option :text :string "Button")
                     (option :enabled :boolean t))
               (lambda (id children value)
                 (declare (ignore children))
                 (bolt:button (funcall value :text) :id id :enabled (funcall value :enabled))))
        (entry :text-field "Text field"
               (list (option :text :string "")
                     (option :width :number 180)
                     (option :enabled :boolean t))
               (lambda (id children value)
                 (declare (ignore children))
                 (bolt:text-field (funcall value :text) :id id :width (funcall value :width)
                                                        :enabled (funcall value :enabled))))
        (entry :column "Column"
               (list (option :gap :number 0)
                     (option :main-align +alignments+ :start)
                     (option :cross-align +alignments+ :start))
               (lambda (id children value)
                 (bolt:column (:id id :gap (funcall value :gap)
                               :main-align (funcall value :main-align)
                               :cross-align (funcall value :cross-align))
                   (funcall children)))
               :container-p t)
        (entry :row "Row"
               (list (option :gap :number 0)
                     (option :main-align +alignments+ :start)
                     (option :cross-align +alignments+ :start))
               (lambda (id children value)
                 (bolt:row (:id id :gap (funcall value :gap)
                            :main-align (funcall value :main-align)
                            :cross-align (funcall value :cross-align))
                   (funcall children)))
               :container-p t))
  "The kinds of widget a view can have, in the order the designer lists them.")

(defun find-entry (kind)
  (or (find kind +catalogue+ :key #'entry-kind)
      (error "The catalogue has no widget of the kind ~s." kind)))

(defun option-value-p (option value)
  "True if VALUE is one that OPTION can have, by its type."
  (let ((type (option-type option)))
    (cond ((eq type :string) (stringp value))
          ((eq type :number) (realp value))
          ((eq type :boolean) (member value '(t nil)))
          ((and (consp type) (eq (first type) :choice)) (member value (rest type)))
          (t nil))))

(defun node-option (node name)
  "The value of NODE's option NAME: what the node was given, or the option's default."
  (let ((given (getf (node-options node) name '%none)))
    (if (eq given '%none)
        (let ((option (find name (entry-options (find-entry (node-kind node))) :key #'option-name)))
          (unless option
            (error "A ~(~a~) has no option ~s." (node-kind node) name))
          (option-default option))
        given)))

(defun draw-node (node)
  "Draw NODE, and what is in it, with the real widgets."
  (funcall (entry-draw (find-entry (node-kind node)))
           (node-id node)
           (lambda () (mapc #'draw-node (node-children node)))
           (lambda (name) (node-option node name))))
