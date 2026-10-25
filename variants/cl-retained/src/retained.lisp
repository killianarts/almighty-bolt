(defpackage #:almighty-bolt-variant-retained/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:handle-event
                #:hit-target
                #:lookup-target
                #:context-width
                #:context-height
                #:push-rect
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:key-event
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-shift+)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-id
                #:node-parent
                #:node-x
                #:node-y
                #:node-w
                #:node-h
                #:node-tree
                #:make-tree-state
                #:tree-state-layout-dirty
                #:tree-state-changed
                #:touch
                #:*paint-caching*
                #:layout-node
                #:paint-node
                #:node-focusable-p
                #:activate
                #:node-at
                #:interactive-at
                #:find-node
                #:node-origin
                #:map-nodes)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:+background+
                #:button-p
                #:button-state
                #:button-focused)
  (:export #:retained-variant
           #:make-retained-variant
           #:retained-variant-caching
           #:retained-variant-root
           #:retained-variant-hover
           #:retained-variant-pressed
           #:retained-variant-focus))

(in-package #:almighty-bolt-variant-retained/retained)

;;; The retained variant: a tree of nodes that lives between frames. A frame lays the
;;; tree out if something changed, and paints it: a node that can keep the commands it
;;; painted does so, and replays them, moved, until it changes; the rest are painted
;;; each frame by walking the tree. Events are handled as
;;; they arrive: the pointer is hit-tested against the last layout, buttons track
;;; hover and press, a click calls the button's handler, and Tab moves the focus.

(defclass retained-variant (variant)
  ((root :initarg :root :reader retained-variant-root)
   ;; Whether nodes keep what they paint. Off, every frame paints the whole tree.
   (caching :initarg :caching :initform t :reader retained-variant-caching)
   (laid-out-width :initform -1f0 :type single-float :accessor laid-out-width)
   (laid-out-height :initform -1f0 :type single-float :accessor laid-out-height)
   (hover :initform nil :accessor retained-variant-hover)
   (pressed :initform nil :accessor retained-variant-pressed)
   (focus :initform nil :accessor retained-variant-focus))
  (:documentation "A variant that keeps a tree of nodes."))

(defun make-retained-variant (root &key (caching t))
  "A variant that shows the tree under ROOT. The tree must not already have an owner.
With CACHING, nodes that can keep the commands they paint do, and paint again only
when they change."
  (setf (node-tree root) (make-tree-state))
  (make-instance 'retained-variant :root root :caching caching))

(defun state (variant)
  (node-tree (retained-variant-root variant)))

;;; --- a frame ---

(defmethod frame ((variant retained-variant) context list)
  (let* ((root (retained-variant-root variant))
         (state (state variant))
         (width (context-width context))
         (height (context-height context)))
    ;; Sizes are worked out when something that can change them did, or the window did.
    (when (or (tree-state-layout-dirty state)
              (/= width (laid-out-width variant))
              (/= height (laid-out-height variant)))
      (layout-node root context 0f0 width 0f0 height)
      (setf (node-x root) 0f0
            (node-y root) 0f0
            (laid-out-width variant) width
            (laid-out-height variant) height
            (tree-state-layout-dirty state) nil))
    (push-rect list 0 0 width height +background+)
    (let ((*paint-caching* (retained-variant-caching variant)))
      (paint-node root context list 0f0 0f0))
    ;; Building changed no state, so the frame is settled.
    nil))

;;; --- asking where things are ---

(defmethod hit-target ((variant retained-variant) x y)
  (let ((root (retained-variant-root variant)))
    (loop :for node = (node-at root x y) :then (node-parent node)
          :while node
          :when (node-id node)
            :return (node-id node))))

(defmethod lookup-target ((variant retained-variant) id)
  (let ((node (find-node (retained-variant-root variant) id)))
    (when node
      (multiple-value-bind (x y) (node-origin node)
        (values x y (node-w node) (node-h node))))))

;;; --- events ---

(defun refresh-button (variant button)
  "Set the look of BUTTON from where the pointer is and what is pressed."
  (let* ((hover (retained-variant-hover variant))
         (pressed (retained-variant-pressed variant))
         (new (cond ((and (eq pressed button) (eq hover button)) :pressed)
                    ((and (eq hover button) (null pressed)) :hover)
                    (t :normal))))
    (unless (eq new (button-state button))
      (setf (button-state button) new)
      (touch button nil))))

(defun refresh (variant node)
  (when (and node (button-p node))
    (refresh-button variant node)))

(defun set-hover (variant node)
  (let ((old (retained-variant-hover variant)))
    (unless (eq old node)
      (setf (retained-variant-hover variant) node)
      (refresh variant old)
      (refresh variant node))))

(defun press (variant node)
  (setf (retained-variant-pressed variant) node)
  (refresh variant node))

(defun release (variant target)
  "The primary button went up over TARGET (or over nothing, if NIL). A node that was
pressed is clicked if the pointer is still over it."
  (let ((pressed (retained-variant-pressed variant)))
    (when pressed
      (setf (retained-variant-pressed variant) nil)
      (refresh variant pressed)
      (when (eq pressed target)
        (activate pressed)))))

(defun handle-pointer (variant event)
  (let ((target (interactive-at (retained-variant-root variant)
                                (float (pointer-event-x event) 1f0)
                                (float (pointer-event-y event) 1f0))))
    (ecase (pointer-event-kind event)
      ((:move :enter) (set-hover variant target))
      (:leave (set-hover variant nil))
      (:down (when (and target (eql (pointer-event-button event) 0))
               (set-hover variant target)
               (press variant target)))
      (:up (when (eql (pointer-event-button event) 0)
             (release variant target))))))

;;; --- the keyboard focus ---

(defun set-focus (variant node)
  (let ((old (retained-variant-focus variant)))
    (unless (eq old node)
      (setf (retained-variant-focus variant) node)
      (when (and old (button-p old))
        (setf (button-focused old) nil)
        (touch old nil))
      (when (and node (button-p node))
        (setf (button-focused node) t)
        (touch node nil)))))

(defun next-focus (variant forward-p)
  "The node that Tab (or Shift-Tab, if FORWARD-P is false) moves the focus to:
the next focusable node in the order the tree is painted, wrapping round."
  (let ((current (retained-variant-focus variant))
        (first nil) (last nil) (before nil) (after nil) (seen nil))
    (map-nodes (lambda (node)
                 (when (node-focusable-p node)
                   (unless first (setf first node))
                   (setf last node)
                   (cond ((eq node current) (setf seen t))
                         (seen (unless after (setf after node)))
                         (t (setf before node)))))
               (retained-variant-root variant))
    (if forward-p
        (or after first)
        (or before last))))

(defun handle-key (variant event)
  (when (eq (key-event-kind event) :down)
    (let ((key (key-event-logical-key event))
          (focus (retained-variant-focus variant)))
      (cond ((eql key #\Tab)
             (set-focus variant
                        (next-focus variant
                                    (not (modifier-down-p (key-event-modifiers event)
                                                          +modifier-shift+)))))
            ((and focus (member key '(#\Space #\Return)))
             (activate focus))))))

(defmethod handle-event ((variant retained-variant) event)
  (let ((state (state variant)))
    (setf (tree-state-changed state) nil)
    (typecase event
      (pointer-event (handle-pointer variant event))
      (key-event (handle-key variant event)))
    ;; A frame is wanted only if something visible changed.
    (tree-state-changed state)))
