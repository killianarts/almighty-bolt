(defpackage #:almighty-bolt-variant-reactive/views
  (:use #:cl)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:split-arguments)
  (:export #:view
           #:view-type
           #:view-props
           #:view-children
           #:view-prop
           #:label
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

(defstruct (view (:constructor %make-view (type props children)) (:copier nil))
  (type nil :read-only t)
  (props '() :type list :read-only t)
  (children '() :type list :read-only t))

(defun view-prop (view key)
  (getf (view-props view) key))

(defun make-view (type props &optional children)
  (%make-view type props (remove nil children)))

(defun commons (id flex visible enabled key)
  (list :id id :flex flex :visible visible :enabled enabled :key key))

(defun label (text &key id (size 15) color (flex 0) (visible t) (enabled t) key)
  (make-view :label (list* :text text :size size :color color
                           (commons id flex visible enabled key))))

(defun button (text &key id on-click (flex 0) (visible t) (enabled t) key)
  (make-view :button (list* :text text :on-click on-click
                            (commons id flex visible enabled key))))

(defun padding (child &key id all left top right bottom (flex 0) (visible t) (enabled t) key)
  (make-view :padding (list* :left (or left all 0) :top (or top all 0)
                             :right (or right all 0) :bottom (or bottom all 0)
                             (commons id flex visible enabled key))
             (list child)))

(defun panel (child &key id all left top right bottom color (radius 8) (flex 0) (visible t)
                      (enabled t) key)
  (make-view :panel (list* :left (or left all 0) :top (or top all 0)
                           :right (or right all 0) :bottom (or bottom all 0)
                           :color color :radius radius
                           (commons id flex visible enabled key))
             (list child)))

(defun center (child &key id (flex 0) (visible t) (enabled t) key)
  (make-view :center (commons id flex visible enabled key) (list child)))

(defun stack (&rest arguments)
  (multiple-value-bind (children options) (split-arguments arguments)
    (destructuring-bind (&key id (flex 0) (visible t) (enabled t) key) options
      (make-view :stack (commons id flex visible enabled key) children))))

(defun flex-view (type arguments)
  (multiple-value-bind (children options) (split-arguments arguments)
    (destructuring-bind (&key id (flex 0) (visible t) (enabled t) key (gap 0)
                           (main-align :start) (cross-align :start) fill)
        options
      (make-view type (list* :gap gap :main-align main-align :cross-align cross-align
                             :fill fill
                             (commons id flex visible enabled key))
                 children))))

(defun row (&rest arguments)
  "Children side by side, then :GAP, :MAIN-ALIGN, :CROSS-ALIGN, :FILL and the common keys."
  (flex-view :row arguments))

(defun column (&rest arguments)
  (flex-view :column arguments))

(defun scrim (&key id on-press (flex 0) (visible t) (enabled t) key)
  (make-view :scrim (list* :on-press on-press (commons id flex visible enabled key))))

(defun progress (&key id (fraction 0) (width 200) (flex 0) (visible t) (enabled t) key)
  (make-view :progress (list* :fraction fraction :width width
                              (commons id flex visible enabled key))))

(defun text-field (&key id (value "") (width 180) on-change on-submit invalid (flex 0)
                     (visible t) (enabled t) key)
  "A field that shows VALUE. It is controlled: what a person types goes to ON-CHANGE, and
the field shows VALUE again afterwards, so a change that ON-CHANGE does not make is undone."
  (make-view :text-field (list* :value value :width width :on-change on-change
                                :on-submit on-submit :invalid (and invalid t)
                                (commons id flex visible enabled key))))

(defun slider (&key id (value 0) (minimum 0) (maximum 1) (width 200) on-change (flex 0)
                 (visible t) (enabled t) key)
  (make-view :slider (list* :value value :minimum minimum :maximum maximum :width width
                            :on-change on-change
                            (commons id flex visible enabled key))))

(defun choice (options &key id (selected 0) on-change (flex 0) (visible t) (enabled t) key)
  (make-view :choice (list* :options options :selected selected :on-change on-change
                            (commons id flex visible enabled key))))

(defun list-box (&key id items selected (rows 8) (width 200) on-select (flex 0) (visible t)
                   (enabled t) key)
  (make-view :list-box (list* :items items :selected selected :rows rows :width width
                              :on-select on-select
                              (commons id flex visible enabled key))))

(defun canvas (&key id (width 300) (height 200) data paint on-press on-move on-exit on-drag
                 on-release on-key on-wheel focusable (flex 0) (visible t) (enabled t) key)
  "A canvas that PAINT draws from DATA: PAINT is called with DATA, the canvas, the context,
the display list and the area, and is used again for as long as DATA is the same object.
The handlers are called as the retained canvas calls them, without the canvas."
  (make-view :canvas (list* :width width :height height :data data :paint paint
                            :on-press on-press :on-move on-move :on-exit on-exit
                            :on-drag on-drag :on-release on-release :on-key on-key
                            :on-wheel on-wheel :focusable (and focusable t)
                            (commons id flex visible enabled key))))

(defun component (thunk &key key)
  "A part of the screen that is described by calling THUNK, and described again, alone,
when a signal that THUNK read changes."
  (make-view :component (list :thunk thunk :key key)))
