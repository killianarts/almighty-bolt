(defpackage #:almighty-bolt-variant-reactive/elements
  (:use #:cl)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-observer
                #:call-observing
                #:dispose-observer)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:view-type
                #:view-props
                #:view-children
                #:view-prop)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-id
                #:node-flex
                #:node-children
                #:node-visible
                #:node-enabled
                #:adopt
                #:touch)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:label-color
                #:button
                #:button-text
                #:padding
                #:panel
                #:center
                #:stack
                #:row
                #:column
                #:scrim
                #:progress
                #:progress-fraction)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:text-field
                #:text-field-text
                #:text-field-invalid
                #:slider
                #:slider-value
                #:choice
                #:choice-selected
                #:list-box
                #:list-box-items
                #:list-box-selected
                #:canvas
                #:canvas-invalidate)
  (:export #:element
           #:element-node
           #:element-live-p
           #:element-depth
           #:mount
           #:rebuild
           #:unmount))

(in-package #:almighty-bolt-variant-reactive/elements)

;;; The persistent tree. An ELEMENT is what a view became: it remembers the view it was last
;;; given, the retained node it made (a component has none of its own: it has one child,
;;; whose node is its node), and its child elements. Describing the screen again with the
;;; same shape changes the nodes that differ and keeps the rest, along with what they hold
;;; (a field's caret, a list's scroll); a different kind of view in the same place replaces
;;; the node. A COMPONENT element also owns an observer, which tells the variant, through
;;; SCHEDULE, that it must be described again when a signal it read changes.

(defstruct (element (:constructor %make-element (view parent owner schedule)) (:copier nil))
  (view nil)
  (parent nil)
  ;; The component that this element is part of, which is described again to change it.
  (owner nil)
  (schedule nil :type function :read-only t)
  (own-node nil)
  (children '() :type list)
  ;; For a component: the function that describes it and its observer.
  (thunk nil)
  (observer nil)
  ;; For a canvas: the app's functions and data, which the node's own functions call.
  (box nil)
  (live t :type boolean))

(defun element-live-p (element)
  (element-live element))

(defun element-depth (element)
  (loop :for e = (element-parent element) :then (element-parent e)
        :while e
        :count t))

(defun element-node (element)
  "The retained node ELEMENT shows: its own, or that of a component's one child."
  (or (element-own-node element)
      (let ((child (first (element-children element))))
        (and child (element-node child)))))

(defun kind (element)
  (view-type (element-view element)))

;;; --- making nodes ---

(defstruct (canvas-box (:conc-name box-))
  data paint on-press on-move on-exit on-drag on-release on-key on-wheel)

(defun resync (element)
  "Describe the component ELEMENT is part of again, which puts back what a field shows if
nothing the component reads changed."
  (when (element-owner element)
    (funcall (element-schedule element) (element-owner element))))

(defun handler (element key)
  "A function that calls the handler the element's view has now for KEY."
  (lambda (&rest arguments)
    (let ((function (view-prop (element-view element) key)))
      (when function
        (apply function arguments)))))

(defun controlled-handler (element key)
  "As HANDLER, and then describes the component again, so that a field shows what the app's
state says even if the handler did not change it."
  (lambda (&rest arguments)
    (let ((function (view-prop (element-view element) key)))
      (prog1 (when function (apply function arguments))
        (resync element)))))

(defun create-node (element view)
  (flet ((p (key) (view-prop view key)))
    (let* ((id (p :id))
           (flex (float (p :flex) 1f0))
           (node
             (ecase (view-type view)
               (:label (apply #'label (p :text) :id id :size (p :size) :flex flex
                              (and (p :color) (list :color (p :color)))))
               (:button (button (p :text) :id id :flex flex
                                          :on-click (handler element :on-click)))
               (:padding (padding (label "") :id id :flex flex
                                             :left (p :left) :top (p :top)
                                             :right (p :right) :bottom (p :bottom)))
               (:panel (apply #'panel (label "") :id id :flex flex
                              :left (p :left) :top (p :top) :right (p :right)
                              :bottom (p :bottom) :radius (p :radius)
                              (and (p :color) (list :color (p :color)))))
               (:center (center (label "") :id id :flex flex))
               (:stack (stack :id id :flex flex))
               (:row (row :id id :flex flex :gap (p :gap) :main-align (p :main-align)
                          :cross-align (p :cross-align) :fill (p :fill)))
               (:column (column :id id :flex flex :gap (p :gap) :main-align (p :main-align)
                                :cross-align (p :cross-align) :fill (p :fill)))
               (:scrim (scrim :id id :on-press (handler element :on-press)))
               (:progress (progress :id id :flex flex :fraction (p :fraction) :width (p :width)))
               (:text-field (text-field :id id :flex flex :text (p :value) :width (p :width)
                                        :invalid (p :invalid)
                                        :on-change (controlled-handler element :on-change)
                                        :on-submit (handler element :on-submit)))
               (:slider (slider :id id :flex flex :minimum (p :minimum) :maximum (p :maximum)
                                :value (p :value) :width (p :width)
                                :on-change (controlled-handler element :on-change)))
               (:choice (choice (p :options) :id id :flex flex :selected (p :selected)
                                             :on-change (controlled-handler element :on-change)))
               (:list-box (list-box :id id :flex flex :items (p :items) :rows (p :rows)
                                    :width (p :width) :selected (p :selected)
                                    :on-select (controlled-handler element :on-select)))
               (:canvas
                (let ((box (make-canvas-box)))
                  (setf (element-box element) box)
                  (canvas :id id :flex flex :width (p :width) :height (p :height)
                          :focusable (p :focusable)
                          :paint (lambda (canvas context list x y w h)
                                   (when (box-paint box)
                                     (funcall (box-paint box) (box-data box) canvas context list
                                              x y w h)))
                          :on-press (lambda (canvas x y button)
                                      (declare (ignore canvas))
                                      (when (box-on-press box)
                                        (funcall (box-on-press box) x y button)))
                          :on-move (lambda (canvas x y)
                                     (declare (ignore canvas))
                                     (when (box-on-move box) (funcall (box-on-move box) x y)))
                          :on-exit (lambda (canvas)
                                     (declare (ignore canvas))
                                     (when (box-on-exit box) (funcall (box-on-exit box))))
                          :on-drag (lambda (canvas x y)
                                     (declare (ignore canvas))
                                     (when (box-on-drag box) (funcall (box-on-drag box) x y)))
                          :on-release (lambda (canvas x y inside)
                                        (declare (ignore canvas))
                                        (when (box-on-release box)
                                          (funcall (box-on-release box) x y inside)))
                          :on-key (lambda (canvas event)
                                    (declare (ignore canvas))
                                    (and (box-on-key box) (funcall (box-on-key box) event)))
                          :on-wheel (lambda (canvas dx dy)
                                      (declare (ignore canvas))
                                      (and (box-on-wheel box)
                                           (funcall (box-on-wheel box) dx dy)))))))))
      (unless (p :visible)
        (setf (node-visible node) nil))
      (unless (p :enabled)
        (setf (node-enabled node) nil))
      (when (eq (view-type view) :canvas)
        (update-box (element-box element) view))
      node)))

(defun update-box (box view)
  "Give the canvas's functions the app's current ones. Returns true if what it draws from
changed."
  (let ((changed (or (not (eq (box-data box) (view-prop view :data)))
                     (not (eq (box-paint box) (view-prop view :paint))))))
    (setf (box-data box) (view-prop view :data)
          (box-paint box) (view-prop view :paint)
          (box-on-press box) (view-prop view :on-press)
          (box-on-move box) (view-prop view :on-move)
          (box-on-exit box) (view-prop view :on-exit)
          (box-on-drag box) (view-prop view :on-drag)
          (box-on-release box) (view-prop view :on-release)
          (box-on-key box) (view-prop view :on-key)
          (box-on-wheel box) (view-prop view :on-wheel))
    changed))

;;; --- changing nodes in place ---

(defparameter +structure+
  '((:label :size)
    (:padding :left :top :right :bottom)
    (:panel :left :top :right :bottom :color :radius)
    (:row :gap :main-align :cross-align :fill)
    (:column :gap :main-align :cross-align :fill)
    (:progress :width)
    (:text-field :width)
    (:slider :minimum :maximum :width)
    (:choice :options)
    (:list-box :rows :width)
    (:canvas :width :height :focusable))
  "For each kind of view, the properties that the node is made with and cannot be changed:
a view that differs in one of them is a different node.")

(defun same-structure-p (type old new)
  (every (lambda (key) (equal (getf old key) (getf new key)))
         (rest (assoc type +structure+))))

(defun update-node (element old-view new-view)
  "Change the node of ELEMENT from what OLD-VIEW made it to what NEW-VIEW says. Returns
false, having changed nothing, if it cannot be done (the node must be made again)."
  (let* ((node (element-own-node element))
         (type (view-type new-view))
         (old (view-props old-view))
         (new (view-props new-view)))
    (flet ((p (key) (getf new key)))
      (unless (same-structure-p type old new)
        (return-from update-node nil))
      (unless (eql (getf old :id) (p :id))
        (setf (node-id node) (p :id)))
      (unless (eql (getf old :flex) (p :flex))
        (setf (node-flex node) (float (p :flex) 1f0))
        (touch node t))
      (unless (eq (node-visible node) (and (p :visible) t))
        (setf (node-visible node) (and (p :visible) t)))
      (unless (eq (node-enabled node) (and (p :enabled) t))
        (setf (node-enabled node) (and (p :enabled) t)))
      ;; What a node shows is compared with the node, not with the last view, so that a
      ;; field the person changed is put back to what the state says.
      (ecase type
        (:label
         (unless (string= (label-text node) (p :text))
           (setf (label-text node) (p :text)))
         (when (and (p :color) (not (eql (p :color) (getf old :color))))
           (setf (label-color node) (p :color)))
         (when (and (null (p :color)) (getf old :color))
           (return-from update-node nil)))
        (:button
         (unless (string= (button-text node) (p :text))
           (setf (button-text node) (p :text))))
        ((:padding :panel :center :stack :row :column :scrim))
        (:progress
         (setf (progress-fraction node) (p :fraction)))
        (:text-field
         (unless (string= (text-field-text node) (p :value))
           (setf (text-field-text node) (p :value)))
         (setf (text-field-invalid node) (p :invalid)))
        (:slider
         (setf (slider-value node) (p :value)))
        (:choice
         (setf (choice-selected node) (p :selected)))
        (:list-box
         (unless (equal (coerce (list-box-items node) 'list) (coerce (p :items) 'list))
           (setf (list-box-items node) (p :items)))
         (unless (eql (list-box-selected node) (p :selected))
           (setf (list-box-selected node) (p :selected))))
        (:canvas
         (when (update-box (element-box element) new-view)
           (canvas-invalidate node)))))
    t))

;;; --- the tree ---

(defun same-kind-p (element view)
  (and (eq (kind element) (view-type view))
       (eql (view-prop (element-view element) :key) (view-prop view :key))))

(defparameter +containers+ '(:padding :panel :center :stack :row :column)
  "The kinds of view whose node holds the nodes of the view's children. The others (a
button, which has a label inside it) keep the children they were made with.")

(defun sync-node-children (element)
  "Make the node of ELEMENT hold the nodes of its children, if they are not the ones it has."
  (let ((node (element-own-node element)))
    (when (and node (member (kind element) +containers+))
      (let ((wanted (mapcar #'element-node (element-children element)))
            (have (coerce (node-children node) 'list)))
        (unless (and (= (length wanted) (length have)) (every #'eq wanted have))
          (adopt node wanted)
          (touch node t))))))

(defun mount (view parent owner schedule)
  "A new element, with its nodes, for VIEW inside PARENT. OWNER is the component it is part
of and SCHEDULE what to call with a component that must be described again."
  (let ((element (%make-element view parent owner schedule)))
    (cond
      ((eq (view-type view) :component)
       (setf (element-thunk element) (view-prop view :thunk))
       (setf (element-observer element)
             (make-observer (lambda () (funcall schedule element))))
       (let ((child-view (call-observing (element-observer element) (element-thunk element))))
         (unless child-view
           (error "A component must describe something, not NIL."))
         (setf (element-children element) (list (mount child-view element element schedule)))))
      (t
       (setf (element-own-node element) (create-node element view))
       (reconcile-children element (view-children view))))
    element))

(defun unmount (element)
  "Forget ELEMENT and everything under it, so that no signal tells them anything."
  (setf (element-live element) nil)
  (when (element-observer element)
    (dispose-observer (element-observer element)))
  (mapc #'unmount (element-children element))
  nil)

(defun replace-element (element view)
  (let ((new (mount view (element-parent element) (element-owner element)
                    (element-schedule element))))
    (unmount element)
    new))

(defun reconcile (element view)
  "Make ELEMENT show VIEW, changing as little as it can, and return the element that does:
ELEMENT, or a new one if what is there cannot be changed into it."
  (cond
    ((not (same-kind-p element view))
     (replace-element element view))
    ((eq (view-type view) :component)
     ;; A component given a new function may now describe something else.
     (setf (element-view element) view
           (element-thunk element) (view-prop view :thunk))
     (rebuild element)
     element)
    (t
     (let ((old (element-view element)))
       (setf (element-view element) view)
       (cond ((update-node element old view)
              (reconcile-children element (view-children view))
              element)
             (t (replace-element element view)))))))

(defun reconcile-children (parent views)
  "Make the children of PARENT show VIEWS: a child that has the kind and the key of a view
is changed to it, the others are made, and what is left over is dropped."
  (let ((old (element-children parent))
        (owner (if (eq (kind parent) :component) parent (element-owner parent)))
        (result '()))
    (dolist (view views)
      (let ((match (find-if (lambda (element) (same-kind-p element view)) old)))
        (when match
          (setf old (remove match old :count 1 :test #'eq)))
        (push (if match
                  (reconcile match view)
                  (mount view parent owner (element-schedule parent)))
              result)))
    (mapc #'unmount old)
    (setf (element-children parent) (nreverse result))
    (sync-node-children parent)
    parent))

(defun nearest-node-element (element)
  (loop :for e = (element-parent element) :then (element-parent e)
        :while e
        :when (element-own-node e)
          :return e))

(defun rebuild (component)
  "Describe COMPONENT again, and change what is on the screen to match."
  (let* ((view (call-observing (element-observer component) (element-thunk component)))
         (old (first (element-children component))))
    (unless view
      (error "A component must describe something, not NIL."))
    (let ((new (reconcile old view)))
      (unless (eq new old)
        (setf (element-children component) (list new))
        (let ((parent (nearest-node-element component)))
          (when parent
            (sync-node-children parent)))))
    component))
