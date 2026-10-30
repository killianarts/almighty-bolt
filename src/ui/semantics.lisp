(defpackage #:almighty-bolt/ui/semantics
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:accessibility-tree
                #:text-state-string
                #:text-state-caret
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
                #:box-kind
                #:box-children
                #:box-text
                #:box-enabled
                #:box-data
                #:box-n1
                #:box-n2
                #:box-n3
                #:box-aux
                #:box-labelled-by
                #:box-x
                #:box-y
                #:box-w
                #:box-h)
  (:import-from #:almighty-bolt/ui/ui
                #:ui-root
                #:ui-boxes
                #:ui-count
                #:ui-focus
                #:ui-hot)
  (:import-from #:almighty-bolt/ui/widgets
                #:field-state-state
                #:choice-state-lefts
                #:choice-state-widths
                #:list-state-top
                #:+row-height+)
  (:import-from #:almighty-bolt/ui/immediate
                #:immediate-variant
                #:immediate-variant-ui)
  (:export #:node-id
           #:node-role
           #:node-name
           #:node-value
           #:node-state
           #:node-bounds
           #:node-children
           #:node-property
           #:find-nodes
           #:walk-nodes))

(in-package #:almighty-bolt/ui/semantics)

;;; What the screen means, for a person who cannot see it and for a program that is not one: the
;;; tree of what was last presented, with for every thing its role, its name, its value, its state,
;;; where it is and what is inside it. It is made when it is asked for, from the boxes of the
;;; frame that was last built; a frame does nothing for it. The caller makes sure no frame is being
;;; built (the machine interface asks on the main thread, between frames).
;;;
;;; A node is a property list, so that it can be printed, compared and sent as it is:
;;;   (:id ID :role ROLE :name NAME :value VALUE :state (...) :bounds (X Y WIDTH HEIGHT)
;;;    :children (...))
;;; with ID, NAME, VALUE and the state keys left out when there is nothing to say. The bounds are in
;;; points from the top left of the window.

(defun node-id (node) (getf node :id))
(defun node-role (node) (getf node :role))
(defun node-name (node) (getf node :name))
(defun node-value (node) (getf node :value))
(defun node-state (node) (getf node :state))
(defun node-bounds (node) (getf node :bounds))
(defun node-children (node) (getf node :children))
(defun node-property (node key) (getf (getf node :state) key))

(defun role-of (box)
  (ecase (box-kind box)
    (:label :static-text)
    (:paragraph :paragraph)
    (:progress :progress-bar)
    (:button :button)
    (:text-field :text-field)
    (:slider :slider)
    (:choice :radio-group)
    (:checkbox :checkbox)
    (:list-box :list)
    (:canvas :canvas)
    (:scrim :overlay)
    ((:column :row :stack :padding :panel :center) :group)))

(defun find-box (ui id)
  "The box of the frame with the id ID, or NIL."
  (loop :for index :below (ui-count ui)
        :do (let ((box (aref (ui-boxes ui) index)))
              (when (and (box-id box) (equal (box-id box) id))
                (return box)))))

(defun name-of (ui box)
  "What the thing is called: its own words, or the words of the widget that its :labelled-by names."
  (case (box-kind box)
    ((:label :paragraph :button :checkbox) (box-text box))
    (t (let ((label (and (box-labelled-by box) (find-box ui (box-labelled-by box)))))
         (and label (box-text label))))))

(defun value-of (box)
  (case (box-kind box)
    (:text-field (box-text box))
    (:slider (let ((aux (box-aux box)))
               (and aux (aref aux 0))))
    (:progress (float (box-n1 box) 1d0))
    (:checkbox (= (box-n3 box) 1))
    (:choice (let ((options (car (box-data box))))
               (nth (box-n3 box) options)))
    (:list-box (let ((items (car (box-data box))) (selected (box-n2 box)))
                 (and (>= selected 0) (nth (round selected) items))))
    (t nil)))

(defun state-of (ui box x y)
  (let ((state '()))
    (flet ((note (key value) (setf state (append state (list key value)))))
      (unless (box-enabled box) (note :disabled t))
      (when (and (plusp (ui-focus ui)) (= (box-key box) (ui-focus ui))) (note :focused t))
      (when (and (plusp (ui-hot ui)) (= (box-key box) (ui-hot ui))) (note :hovered t))
      (case (box-kind box)
        (:text-field
         (let ((text (field-state-state (box-data box))))
           (when (= (box-n3 box) 1) (note :invalid t))
           (note :caret (text-state-caret text))
           (when (text-state-selection-p text)
             (note :selection (list (text-state-selection-start text)
                                    (text-state-selection-end text))))))
        (:slider
         (let ((aux (box-aux box)))
           (when aux
             (note :minimum (aref aux 1))
             (note :maximum (aref aux 2)))))
        (:checkbox (note :checked (= (box-n3 box) 1)))
        (:choice
         (note :selected (box-n3 box))
         (note :options (car (box-data box)))
         ;; Where each option is, so that it can be pressed.
         (let ((state (cdr (box-data box))))
           (note :option-bounds
                 (loop :for left :across (choice-state-lefts state)
                       :for width :across (choice-state-widths state)
                       :collect (list (+ x left) y width (box-h box))))))
        (:list-box
         (note :selected (let ((selected (box-n2 box)))
                           (and (>= selected 0) (round selected))))
         (note :items (car (box-data box)))
         ;; The rows that show: the first one's index, and where each is.
         (let* ((state (cdr (box-data box)))
                (top (list-state-top state))
                (rows (box-n3 box))
                (count (length (car (box-data box)))))
           (note :first-visible top)
           (note :item-bounds
                 (loop :for index :from top :below (min count (+ top rows))
                       :for row :from 0
                       :collect (list x (+ y 1 (* row +row-height+)) (box-w box)
                                      +row-height+)))))))
    state))

(defun node-of (ui box x y)
  (let* ((x (+ x (box-x box)))
         (y (+ y (box-y box)))
         (id (box-id box))
         (name (name-of ui box))
         (value (value-of box))
         (state (state-of ui box x y))
         (children (loop :for child :across (box-children box)
                         :collect (node-of ui child x y))))
    (append (and id (list :id id))
            (list :role (role-of box))
            (and name (list :name name))
            ;; A check box that is not checked has the value NIL, which is a value.
            (and (or value (eq (box-kind box) :checkbox)) (list :value value))
            (and state (list :state state))
            (list :bounds (list x y (box-w box) (box-h box)))
            (and children (list :children children)))))

(defmethod accessibility-tree ((variant immediate-variant))
  "The semantic tree of the frame last built, a node (see above) for the whole window, or NIL if
no frame has been built."
  (let* ((ui (immediate-variant-ui variant))
         (root (ui-root ui)))
    (and root (node-of ui root 0f0 0f0))))

(defun walk-nodes (function node)
  "Call FUNCTION on NODE and, depth first, on every node inside it."
  (funcall function node)
  (dolist (child (node-children node))
    (walk-nodes function child)))

(defun find-nodes (tree &key id role name)
  "The nodes of TREE with the given ID, ROLE and NAME (each only if given), in depth-first order."
  (let ((found '()))
    (walk-nodes (lambda (node)
                  (when (and (or (null id) (equal (node-id node) id))
                             (or (null role) (eq (node-role node) role))
                             (or (null name) (equal (node-name node) name)))
                    (push node found)))
                tree)
    (nreverse found)))
