(defpackage #:almighty-bolt-variant-retained/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:handle-event
                #:hit-target
                #:lookup-target
                #:request-frame
                #:context-width
                #:context-height
                #:context-now-ns
                #:push-rect
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:wheel-event
                #:wheel-event-x
                #:wheel-event-y
                #:wheel-event-dx
                #:wheel-event-dy
                #:key-event
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-shift+)
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+background+)
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
                #:make-session
                #:session-context
                #:session-hover
                #:session-pressed
                #:session-focus
                #:session-tickers
                #:refresh-look
                #:focus-node
                #:touch
                #:*paint-caching*
                #:layout-node
                #:paint-node
                #:focusable-p
                #:node-focus-on-click-p
                #:pointer-press
                #:pointer-drag
                #:pointer-release
                #:pointer-wheel
                #:key-input
                #:node-at
                #:interactive-at
                #:find-node
                #:node-origin
                #:map-nodes)
  (:export #:retained-variant
           #:make-retained-variant
           #:retained-variant-caching
           #:retained-variant-root
           #:retained-variant-session
           #:retained-variant-hover
           #:retained-variant-pressed
           #:retained-variant-focus
           #:add-ticker))

(in-package #:almighty-bolt-variant-retained/retained)

;;; The retained variant: a tree of nodes that lives between frames. A frame runs the
;;; tickers (functions that change nodes as time passes), lays the tree out if something
;;; changed, and paints it: a node that can keep the commands it painted does so, and
;;; replays them, moved, until it changes; the rest are painted each frame by walking the
;;; tree. Events are handled as they arrive: the pointer is hit-tested against the last
;;; layout and what it does is up to the node it is over (buttons click, fields take the
;;; caret, sliders drag), keys go to the node with the focus, and Tab moves the focus.

(defclass retained-variant (variant)
  ((root :initarg :root :reader retained-variant-root)
   ;; Whether nodes keep what they paint. Off, every frame paints the whole tree.
   (caching :initarg :caching :initform t :reader retained-variant-caching)
   (laid-out-width :initform -1f0 :type single-float :accessor laid-out-width)
   (laid-out-height :initform -1f0 :type single-float :accessor laid-out-height)
   (session :initform (make-session) :reader retained-variant-session))
  (:documentation "A variant that keeps a tree of nodes."))

(defun make-retained-variant (root &key (caching t))
  "A variant that shows the tree under ROOT. The tree must not already have an owner.
With CACHING, nodes that can keep the commands they paint do, and paint again only
when they change."
  (setf (node-tree root) (make-tree-state))
  (make-instance 'retained-variant :root root :caching caching))

(defun retained-variant-hover (variant)
  (session-hover (retained-variant-session variant)))

(defun retained-variant-pressed (variant)
  (session-pressed (retained-variant-session variant)))

(defun retained-variant-focus (variant)
  (session-focus (retained-variant-session variant)))

(defun add-ticker (variant function)
  "Run FUNCTION at the start of every frame, with the time in nanoseconds. It may change
nodes. If it returns true the variant asks for another frame, so a ticker that returns
true while something is animating, and false when it stops, keeps frames coming for as long
as they are needed and no longer."
  (let ((session (retained-variant-session variant)))
    (setf (session-tickers session) (append (session-tickers session) (list function)))
    function))

(defun state (variant)
  (node-tree (retained-variant-root variant)))

;;; --- a frame ---

(defmethod frame ((variant retained-variant) context list)
  (let* ((root (retained-variant-root variant))
         (state (state variant))
         (session (retained-variant-session variant))
         (width (context-width context))
         (height (context-height context)))
    (setf (session-context session) context)
    (let ((again nil)
          (now (context-now-ns context)))
      (dolist (ticker (session-tickers session))
        (when (funcall ticker now)
          (setf again t)))
      (when again
        (request-frame context)))
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
    ;; Building changed no state the core must settle, so the frame is settled.
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

;;; --- the pointer ---

(defun set-hover (session node)
  (let ((old (session-hover session)))
    (unless (eq old node)
      (setf (session-hover session) node)
      (refresh-look session old)
      (refresh-look session node))))

(defun local-point (node x y)
  "The point X, Y of the window in NODE's own coordinates."
  (multiple-value-bind (origin-x origin-y) (node-origin node)
    (values (- x origin-x) (- y origin-y))))

(defun press (session node x y button)
  (multiple-value-bind (local-x local-y) (local-point node x y)
    (pointer-press node session local-x local-y button)))

(defun handle-pointer (variant event)
  (let* ((session (retained-variant-session variant))
         (x (float (pointer-event-x event) 1f0))
         (y (float (pointer-event-y event) 1f0))
         (target (interactive-at (retained-variant-root variant) x y))
         (button (pointer-event-button event)))
    (ecase (pointer-event-kind event)
      ((:move :enter)
       (set-hover session target)
       (let ((pressed (session-pressed session)))
         (when pressed
           (multiple-value-bind (local-x local-y) (local-point pressed x y)
             (pointer-drag pressed session local-x local-y)))))
      (:leave (set-hover session nil))
      (:down
       (cond
         ((not (eql button 0))
          (when target (press session target x y button)))
         (t
          (let ((focus (session-focus session)))
            (cond ((and target (node-focus-on-click-p target))
                   (focus-node session target))
                  ;; Pressing elsewhere takes the keyboard from a field.
                  ((and focus (node-focus-on-click-p focus))
                   (focus-node session nil))))
          (when target
            (set-hover session target)
            (setf (session-pressed session) target)
            (refresh-look session target)
            (press session target x y 0)))))
      (:up
       (when (eql button 0)
         (let ((pressed (session-pressed session)))
           (when pressed
             (setf (session-pressed session) nil)
             (refresh-look session pressed)
             (refresh-look session (session-hover session))
             (multiple-value-bind (local-x local-y) (local-point pressed x y)
               (pointer-release pressed session local-x local-y (eq pressed target))))))))))

(defun handle-wheel (variant event)
  (let ((session (retained-variant-session variant))
        (x (float (wheel-event-x event) 1f0))
        (y (float (wheel-event-y event) 1f0)))
    (loop :for node = (interactive-at (retained-variant-root variant) x y)
            :then (node-parent node)
          :while node
          :when (pointer-wheel node session
                               (float (wheel-event-dx event) 1f0)
                               (float (wheel-event-dy event) 1f0))
            :return t)))

;;; --- the keyboard focus ---

(defun next-focus (variant forward-p)
  "The node that Tab (or Shift-Tab, if FORWARD-P is false) moves the focus to:
the next focusable node in the order the tree is painted, wrapping round."
  (let ((current (retained-variant-focus variant))
        (first nil) (last nil) (before nil) (after nil) (seen nil))
    (map-nodes (lambda (node)
                 (when (focusable-p node)
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
    (let* ((session (retained-variant-session variant))
           (focus (session-focus session)))
      (unless (and focus (key-input focus session event))
        (when (eql (key-event-logical-key event) #\Tab)
          (focus-node session
                      (next-focus variant
                                  (not (modifier-down-p (key-event-modifiers event)
                                                        +modifier-shift+)))))))))

(defmethod handle-event ((variant retained-variant) event)
  (let ((state (state variant)))
    (setf (tree-state-changed state) nil)
    (typecase event
      (pointer-event (handle-pointer variant event))
      (wheel-event (handle-wheel variant event))
      (key-event (handle-key variant event)))
    ;; A frame is wanted only if something visible changed.
    (tree-state-changed state)))
