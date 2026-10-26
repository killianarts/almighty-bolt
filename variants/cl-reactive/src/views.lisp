(defpackage #:almighty-bolt-variant-reactive/views
  (:use #:cl)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:split-arguments)
  (:export #:view
           #:view-type
           #:view-props
           #:view-children
           #:view-id
           #:view-flex
           #:view-prop
           #:label
           #:paragraph
           #:button
           #:padding
           #:panel
           #:center
           #:stack
           #:row
           #:column
           #:scrim
           #:progress
           #:text-field
           #:slider
           #:choice
           #:list-box
           #:canvas
           #:component))

(in-package #:almighty-bolt-variant-reactive/views)

;;; What an app returns from a component: a description of what should be on the screen, as
;;; values, with no widgets in it. The framework compares it with the description it had
;;; before and changes the widgets that differ (elements.lisp). Each constructor takes the
;;; children first and the options after, as the retained widgets do, so that an app reads
;;; the same way in the two variants; the difference is that these are made again whenever
;;; the signals they read change, and the widgets are not.
;;;
;;; Every view takes :ID (the name tests and replay find it by), :FLEX, :VISIBLE (true by
;;; default), :ENABLED (true by default) and :KEY (what tells two views of the same kind
;;; apart when a list of them changes order). A NIL among children is left out, so that a
;;; child can be put in with WHEN.

(defparameter +layouts+
  '((:label :text :size :color)
    (:paragraph :text :size :color)
    (:button :text :on-click)
    (:padding :left :top :right :bottom)
    (:panel :left :top :right :bottom :color :radius)
    (:center)
    (:stack)
    (:row :gap :main-align :cross-align :fill)
    (:column :gap :main-align :cross-align :fill)
    (:scrim :on-press)
    (:progress :fraction :width)
    (:text-field :value :width :on-change :on-submit :invalid)
    (:slider :value :minimum :maximum :width :on-change)
    (:choice :options :selected :on-change)
    (:list-box :items :selected :rows :width :on-select)
    (:canvas :width :height :data :paint :on-press :on-move :on-exit :on-drag :on-release :on-key
     :on-text :on-composition :on-wheel :focusable)
    (:component :thunk))
  "For each kind of view, the names of its properties, in the order they are in the vector of a
view's PROPS. A description is made again whenever what it reads changes, and the vector is a
third of the size of a property list (a canvas has fifteen).")

(defparameter +no-props+ (vector))

(defstruct (view (:constructor %make-view (type props children id extras)) (:copier nil))
  (type nil :read-only t)
  ;; What only this kind of view has, in a vector in the order of +LAYOUTS+, which nobody changes.
  (props +no-props+ :type simple-vector :read-only t)
  (children '() :type list :read-only t)
  (id nil :read-only t)
  ;; The flex, whether it is visible and enabled, and its key, which nearly every view has the
  ;; defaults for: then this is NIL, and otherwise a vector of the four. A description is made
  ;; again whenever what it reads changes, so each slot of it is paid for many times.
  (extras nil :type (or null simple-vector) :read-only t))

(declaim (inline view-flex view-visible view-enabled view-key))
(defun view-flex (view)
  (let ((extras (view-extras view))) (if extras (svref extras 0) 0)))
(defun view-visible (view)
  (let ((extras (view-extras view))) (if extras (svref extras 1) t)))
(defun view-enabled (view)
  (let ((extras (view-extras view))) (if extras (svref extras 2) t)))
(defun view-key (view)
  (let ((extras (view-extras view))) (and extras (svref extras 3))))

(defun view-prop (view key)
  "What VIEW says for KEY: one of the keys that every view has, or one that its kind has."
  (case key
    (:id (view-id view))
    (:flex (view-flex view))
    (:visible (view-visible view))
    (:enabled (view-enabled view))
    (:key (view-key view))
    (t (let ((index (position key (cdr (assoc (view-type view) +layouts+ :test #'eq))
                              :test #'eq)))
         (and index (svref (view-props view) index))))))

(declaim (inline make-view))
(defun make-view (type props children id flex visible enabled key)
  (%make-view type props (if (member nil children) (remove nil children) children) id
              (if (and (eql flex 0) (eq visible t) (eq enabled t) (null key))
                  nil
                  (vector flex visible enabled key))))

(defun label (text &key id (size 15) color (flex 0) (visible t) (enabled t) key)
  (make-view :label (vector text size color) nil id flex visible enabled key))

(defun paragraph (text &key id (size 15) color (flex 0) (visible t) (enabled t) key)
  (make-view :paragraph (vector text size color) nil id flex visible enabled key))

(defun button (text &key id on-click (flex 0) (visible t) (enabled t) key)
  (make-view :button (vector text on-click) nil id flex visible enabled key))

(defun padding (child &key id all left top right bottom (flex 0) (visible t) (enabled t) key)
  (make-view :padding (vector (or left all 0) (or top all 0) (or right all 0) (or bottom all 0))
             (list child) id flex visible enabled key))

(defun panel (child &key id all left top right bottom color (radius 8) (flex 0) (visible t)
                      (enabled t) key)
  (make-view :panel (vector (or left all 0) (or top all 0) (or right all 0) (or bottom all 0)
                            color radius)
             (list child) id flex visible enabled key))

(defun center (child &key id (flex 0) (visible t) (enabled t) key)
  (make-view :center +no-props+ (list child) id flex visible enabled key))

(defun stack (&rest arguments)
  (multiple-value-bind (children options) (split-arguments arguments)
    (destructuring-bind (&key id (flex 0) (visible t) (enabled t) key) options
      (make-view :stack +no-props+ children id flex visible enabled key))))

(defun flex-view (type arguments)
  (multiple-value-bind (children options) (split-arguments arguments)
    (destructuring-bind (&key id (flex 0) (visible t) (enabled t) key (gap 0)
                           (main-align :start) (cross-align :start) fill)
        options
      (make-view type (vector gap main-align cross-align fill)
                 children id flex visible enabled key))))

(defun row (&rest arguments)
  "Children side by side, then :GAP, :MAIN-ALIGN, :CROSS-ALIGN, :FILL and the common keys."
  (flex-view :row arguments))

(defun column (&rest arguments)
  (flex-view :column arguments))

(defun scrim (&key id on-press (flex 0) (visible t) (enabled t) key)
  (make-view :scrim (vector on-press) nil id flex visible enabled key))

(defun progress (&key id (fraction 0) (width 200) (flex 0) (visible t) (enabled t) key)
  (make-view :progress (vector fraction width) nil id flex visible enabled key))

(defun text-field (&key id (value "") (width 180) on-change on-submit invalid (flex 0)
                     (visible t) (enabled t) key)
  "A field that shows VALUE. It is controlled: what a person types goes to ON-CHANGE, and
the field shows VALUE again afterwards, so a change that ON-CHANGE does not make is undone."
  (make-view :text-field (vector value width on-change on-submit (and invalid t))
             nil id flex visible enabled key))

(defun slider (&key id (value 0) (minimum 0) (maximum 1) (width 200) on-change (flex 0)
                 (visible t) (enabled t) key)
  (make-view :slider (vector value minimum maximum width on-change)
             nil id flex visible enabled key))

(defun choice (options &key id (selected 0) on-change (flex 0) (visible t) (enabled t) key)
  (make-view :choice (vector options selected on-change)
             nil id flex visible enabled key))

(defun list-box (&key id items selected (rows 8) (width 200) on-select (flex 0) (visible t)
                   (enabled t) key)
  (make-view :list-box (vector items selected rows width on-select)
             nil id flex visible enabled key))

(defun canvas (&key id (width 300) (height 200) data paint on-press on-move on-exit on-drag
                 on-release on-key on-text on-composition on-wheel focusable (flex 0)
                 (visible t) (enabled t) key)
  "A canvas that PAINT draws from DATA: PAINT is called with DATA, the canvas, the context,
the display list and the area, and is used again for as long as DATA is the same object.
The handlers are called as the retained canvas calls them, without the canvas (ON-TEXT with the
text; ON-COMPOSITION with the kind, the text and the start and length of the selection)."
  (make-view :canvas (vector width height data paint on-press on-move on-exit on-drag on-release
                             on-key on-text on-composition on-wheel (and focusable t))
             nil id flex visible enabled key))

(defun component (thunk &key key)
  "A part of the screen that is described by calling THUNK, and described again, alone,
when a signal that THUNK read changes."
  (make-view :component (vector thunk) nil nil 0 t t key))
