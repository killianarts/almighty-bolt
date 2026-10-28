(defpackage #:almighty-bolt-variant-retained/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:handle-event
                #:hit-target
                #:lookup-target
                #:inherit-state
                #:kept-in-use-p
                #:request-frame
                #:context-width
                #:context-height
                #:context-now-ns
                #:with-phase
                #:+phase-update+
                #:+phase-layout+
                #:+phase-paint+
                #:push-rect
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:pointer-event-click-count
                #:wheel-event
                #:wheel-event-x
                #:wheel-event-y
                #:make-axis-lock
                #:lock-wheel
                #:text-input-event
                #:text-input-event-text
                #:composition-event
                #:composition-event-kind
                #:composition-event-text
                #:composition-event-selection-start
                #:composition-event-selection-length
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
                #:node-children
                #:snapshot-node
                #:carry-mutated
                #:carry-interaction
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
                #:session-click-count
                #:session-focus
                #:session-tickers
                #:refresh-look
                #:focus-node
                #:touch
                #:*paint-caching*
                #:layout-node
                #:paint-node
                #:focusable-p
                #:node-autofocus-p
                #:node-focus-on-click-p
                #:pointer-press
                #:pointer-move
                #:pointer-exit
                #:pointer-drag
                #:pointer-release
                #:pointer-wheel
                #:key-input
                #:text-input
                #:composition-input
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
           #:inherit-retained-state
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
   (session :initform (make-session) :reader retained-variant-session)
   ;; True once the first frame has given the keyboard to a node that asked for it.
   (autofocus-done :initform nil :accessor autofocus-done)
   ;; Which way the trackpad gesture in progress is going, so that a swipe goes one way.
   (wheel-lock :initform (make-axis-lock) :reader retained-variant-wheel-lock)
   ;; A function that brings what the app worked out into line with what the old tree's state has
   ;; given the new one, called when the app is made again. See MAKE-RETAINED-VARIANT.
   (after-reload :initarg :after-reload :initform nil :reader retained-variant-after-reload)
   ;; True if the app that made this variant keeps state with KEPT-LET. The nodes hold what the app
   ;; reads back, so the state of the old nodes is only worth carrying if the app's own variables
   ;; are carried too; otherwise the new app is as if started, and what the nodes held is not
   ;; put back to disagree with the variables.
   (keeps-state :initform (kept-in-use-p) :reader retained-variant-keeps-state))
  (:documentation "A variant that keeps a tree of nodes."))

(defun make-retained-variant (root &key (caching t) after-reload)
  "A variant that shows the tree under ROOT. The tree must not already have an owner.
With CACHING, nodes that can keep the commands they paint do, and paint again only
when they change.

What is in the tree now is what it started as: when the app is made again (decision record 0031),
what the person and the app changed in the old tree since then is carried into the new one by id,
and the rest is as the new code made it, if the app keeps its own state with KEPT-LET (an app that
does not is started again). AFTER-RELOAD, a function of no arguments, is then called,
for an app that works things out from the nodes' state and from code that may have changed: the
function that brings the list, the buttons and the rest in line with the model."
  (setf (node-tree root) (make-tree-state))
  (map-nodes #'snapshot-node root)
  (make-instance 'retained-variant :root root :caching caching :after-reload after-reload))

(defun keyed-nodes (root)
  "A table of the nodes under ROOT, by what says that a node of a tree made again is the same one:
its id, or, for a node without, its place among the children of the node it is in."
  (let ((table (make-hash-table :test 'equal)))
    (labels ((walk (node key)
               (unless (nth-value 1 (gethash key table))
                 (setf (gethash key table) node))
               (loop :for child :across (node-children node)
                     :for index :from 0
                     :do (walk child (or (node-id child) (list :child key index))))))
      (walk root (or (node-id root) :root)))
    table))

(defun inherit-retained-state (new old &key mutated)
  "Carry the state of the tree of OLD, a retained variant, into the tree of NEW, which replaces it:
nodes are the same by id (KEYED-NODES) and type. With MUTATED, the slots of a node that were changed
since the tree was made go (CARRY-MUTATED), and with it not, only what the person did that is not
the app's (CARRY-INTERACTION). The keyboard goes to the node that matches the one that had it."
  (let ((old-nodes (keyed-nodes (retained-variant-root old)))
        (new-nodes (keyed-nodes (retained-variant-root new)))
        (focus (session-focus (retained-variant-session old))))
    (maphash (lambda (key node)
               (let ((match (gethash key old-nodes)))
                 (when (and match (eq (type-of match) (type-of node)))
                   (if mutated
                       (carry-mutated node match)
                       (carry-interaction node match)))))
             new-nodes)
    (when focus
      (let ((node (loop :for key :being :the :hash-keys :of old-nodes :using (hash-value value)
                        :when (eq value focus)
                          :return (gethash key new-nodes))))
        (when (and node (eq (type-of node) (type-of focus)) (focusable-p node))
          (focus-node (retained-variant-session new) node))))
    (setf (autofocus-done new) (autofocus-done old))
    (touch (retained-variant-root new) t)
    new))

(defmethod inherit-state ((new retained-variant) (old retained-variant))
  "What the person and the app did to the old tree is carried into the new one (see
MAKE-RETAINED-VARIANT), and then the app brings what it worked out into line. Nothing is carried to
an app that keeps no state of its own: its variables start again, and so does what its nodes show."
  (when (retained-variant-keeps-state new)
    (inherit-retained-state new old :mutated t)
    (when (retained-variant-after-reload new)
      (funcall (retained-variant-after-reload new)))))

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
    (unless (autofocus-done variant)
      (setf (autofocus-done variant) t)
      (unless (session-focus session)
        (block asking
          (map-nodes (lambda (node)
                       (when (and (node-autofocus-p node) (focusable-p node))
                         (focus-node session node)
                         (return-from asking)))
                     root))))
    (with-phase (context +phase-update+)
      (let ((again nil)
            (now (context-now-ns context)))
        (dolist (ticker (session-tickers session))
          (when (funcall ticker now)
            (setf again t)))
        (when again
          (request-frame context))))
    ;; Sizes are worked out when something that can change them did, or the window did.
    (with-phase (context +phase-layout+)
      (when (or (tree-state-layout-dirty state)
                (/= width (laid-out-width variant))
                (/= height (laid-out-height variant)))
        (layout-node root context 0f0 width 0f0 height)
        (setf (node-x root) 0f0
              (node-y root) 0f0
              (laid-out-width variant) width
              (laid-out-height variant) height
              (tree-state-layout-dirty state) nil)))
    (with-phase (context +phase-paint+)
      (push-rect list 0 0 width height +background+)
      (let ((*paint-caching* (retained-variant-caching variant)))
        (paint-node root context list 0f0 0f0)))
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
      (refresh-look session node)
      (when old
        (pointer-exit old session)))))

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
       (when target
         (multiple-value-bind (local-x local-y) (local-point target x y)
           (pointer-move target session local-x local-y)))
       (let ((pressed (session-pressed session)))
         (when pressed
           (multiple-value-bind (local-x local-y) (local-point pressed x y)
             (pointer-drag pressed session local-x local-y)))))
      (:leave (set-hover session nil))
      (:down
       (setf (session-click-count session) (max 1 (pointer-event-click-count event)))
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
    (multiple-value-bind (dx dy) (lock-wheel (retained-variant-wheel-lock variant) event)
      (loop :for node = (interactive-at (retained-variant-root variant) x y)
              :then (node-parent node)
            :while node
            :when (pointer-wheel node session (float dx 1f0) (float dy 1f0))
              :return t))))

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

(defun handle-text (variant event)
  (let ((focus (session-focus (retained-variant-session variant))))
    (when focus
      (typecase event
        (text-input-event
         (text-input focus (retained-variant-session variant) (text-input-event-text event)))
        (composition-event
         (composition-input focus (retained-variant-session variant)
                            (composition-event-kind event) (composition-event-text event)
                            (composition-event-selection-start event)
                            (composition-event-selection-length event)))))))

(defmethod handle-event ((variant retained-variant) event)
  (let ((state (state variant)))
    (setf (tree-state-changed state) nil)
    (typecase event
      (pointer-event (handle-pointer variant event))
      (wheel-event (handle-wheel variant event))
      (key-event (handle-key variant event))
      ((or text-input-event composition-event) (handle-text variant event)))
    ;; A frame is wanted only if something visible changed.
    (tree-state-changed state)))
