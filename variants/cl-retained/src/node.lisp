(defpackage #:almighty-bolt-variant-retained/node
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:make-display-list
                #:clear-display-list
                #:context-scale
                #:display-list-capacity
                #:display-list-full
                #:append-display-list)
  (:export #:node
           #:node-id
           #:node-parent
           #:node-children
           #:node-flex
           #:node-x
           #:node-y
           #:node-w
           #:node-h
           #:node-cacheable
           #:node-paint-dirty
           #:node-cache
           #:node-tree
           #:node-visible
           #:node-enabled
           #:node-hovered
           #:node-pressed
           #:node-focused
           #:session
           #:make-session
           #:session-context
           #:session-hover
           #:session-pressed
           #:session-focus
           #:session-tickers
           #:refresh-look
           #:focus-node
           #:interactive-p
           #:focusable-p
           #:pointer-press
           #:pointer-move
           #:pointer-exit
           #:pointer-drag
           #:pointer-release
           #:pointer-wheel
           #:key-input
           #:node-focus-on-click-p
           #:*paint-caching*
           #:invalidate-paint
           #:set-size
           #:set-place
           #:tree-state
           #:make-tree-state
           #:tree-state-layout-dirty
           #:tree-state-changed
           #:adopt
           #:root-of
           #:touch
           #:layout-node
           #:paint-node
           #:node-interactive-p
           #:node-focusable-p
           #:activate
           #:node-at
           #:interactive-at
           #:find-node
           #:node-origin
           #:map-nodes))

(in-package #:almighty-bolt-variant-retained/node)

;;; The retained variant keeps a tree of nodes between frames. A node knows its
;;; children and its parent, its size, and its place in its parent as the last layout
;;; left them. Hit testing reads those, so it answers for the last layout, as
;;; decision record 0009 asks, and a change to a node only marks the tree: sizes are
;;; worked out again in the next frame, never in the middle of an event.

(defstruct (node (:constructor nil))
  (id nil)
  (parent nil)
  (children #() :type simple-vector)
  ;; How much of the space left in a row or column this node takes, 0 for none.
  (flex 0f0 :type single-float)
  ;; Place in the parent, and size, in points, from the last layout.
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (w 0f0 :type single-float)
  (h 0f0 :type single-float)
  ;; Whether the node is shown, and whether it takes part in events. Set with
  ;; (SETF NODE-VISIBLE) and (SETF NODE-ENABLED), which mark the tree.
  (%visible t :type boolean)
  (%enabled t :type boolean)
  ;; What the pointer and the keyboard are doing to the node, set by the SESSION.
  (hovered nil :type boolean)
  (pressed nil :type boolean)
  (focused nil :type boolean)
  ;; A node that is CACHEABLE keeps the commands it painted, relative to its own top
  ;; left, in CACHE, and paints again only when PAINT-DIRTY. See PAINT-NODE.
  (cacheable nil :type boolean)
  (cache nil)
  (paint-dirty t :type boolean)
  ;; On the root of a tree that has an owner: what a change anywhere sets.
  (tree nil))

(defstruct (tree-state (:constructor make-tree-state ()))
  "What changed in a tree since someone last looked. LAYOUT-DIRTY means sizes must
be worked out again; CHANGED means anything visible changed, which is what an event
handler reports to the host so that it asks for a frame."
  (layout-dirty t :type boolean)
  (changed nil :type boolean))

(defun adopt (parent children)
  "Make CHILDREN, a list of nodes, the children of PARENT. Returns PARENT."
  (let ((vector (coerce children 'simple-vector)))
    (loop :for child :across vector
          :do (setf (node-parent child) parent))
    (setf (node-children parent) vector)
    parent))

(defun root-of (node)
  "The node at the top of the tree that NODE is in."
  (loop :while (node-parent node)
        :do (setf node (node-parent node)))
  node)

(defun invalidate-paint (node)
  "Note that what NODE paints changed: the node that keeps the commands for it, which
is NODE if it is cacheable and otherwise the nearest cacheable node above it, must
paint again."
  (loop :for n = node :then (node-parent n)
        :while n
        :when (node-cacheable n)
          :do (setf (node-paint-dirty n) t)
              (return)))

(defun set-size (node w h)
  "Set the size of NODE, which is what a layout does. A change means NODE paints
differently."
  (declare (type single-float w h))
  (unless (and (= w (node-w node)) (= h (node-h node)))
    (setf (node-w node) w
          (node-h node) h)
    (invalidate-paint node)))

(defun set-place (child x y)
  "Set the place of CHILD in its parent. A change means what paints the parent
paints differently, but not CHILD, whose commands are relative to itself."
  (declare (type single-float x y))
  (unless (and (= x (node-x child)) (= y (node-y child)))
    (setf (node-x child) x
          (node-y child) y)
    (when (node-parent child)
      (invalidate-paint (node-parent child)))))

(defun touch (node layout-p)
  "Note that NODE changed, so that what paints it paints again and the tree's owner
knows a frame is wanted, and, if LAYOUT-P, that sizes have to be worked out again. A
tree with no owner is left alone. Never lays anything out: that is the next frame's
work."
  (invalidate-paint node)
  (let ((state (node-tree (root-of node))))
    (when state
      (setf (tree-state-changed state) t)
      (when layout-p
        (setf (tree-state-layout-dirty state) t))))
  nil)

(declaim (inline node-visible node-enabled))
(defun node-visible (node) (node-%visible node))
(defun node-enabled (node) (node-%enabled node))

(defun (setf node-visible) (visible node)
  "Show or hide NODE. A hidden node takes no room, draws nothing and gets no events."
  (unless (eq visible (node-%visible node))
    (setf (node-%visible node) visible)
    (touch node t)
    (when (node-parent node)
      (touch (node-parent node) t)))
  visible)

(defun (setf node-enabled) (enabled node)
  "Enable or disable NODE. A disabled node is drawn dimmed and gets no events."
  (unless (eq enabled (node-%enabled node))
    (setf (node-%enabled node) enabled)
    (touch node nil))
  enabled)

;;; --- what a kind of node does ---

(defgeneric layout-node (node context min-w max-w min-h max-h)
  (:documentation "Work out the size of NODE within the constraints, and the places
of its children in it, store them in the nodes, and return the width and height."))

(defvar *paint-caching* t
  "True if cacheable nodes keep what they paint. Bound by the variant for a frame.")

(defgeneric paint-node (node context display-list x y)
  (:documentation "Append the drawing commands of NODE and its children to
DISPLAY-LIST, where X, Y is the position of NODE's top left in the window. A
cacheable node paints into its own cache, relative to itself, when it has changed
and otherwise replays the cache, moved to X, Y."))

;;; The text drawing puts each glyph on a whole device pixel, from its position in the
;;; window. A cache is painted at the top left of the node and moved, so it comes out
;;; on the same pixels as text painted where the node is only if the move is a whole
;;; number of pixels. A cacheable node is therefore painted at the nearest whole pixel,
;;; with or without a cache, which also puts the edges of its shapes on pixels.

(declaim (inline snap-to-pixel))
(defun snap-to-pixel (position scale)
  "POSITION, in points, at the nearest whole device pixel when there are SCALE pixels
to a point."
  (declare (type single-float position scale))
  (* (/ 1f0 scale)
     (round (the (single-float -1f6 1f6) (* scale position)))))

(defmethod layout-node :around ((node node) context min-w max-w min-h max-h)
  (if (node-visible node)
      (call-next-method)
      (progn (set-size node 0f0 0f0)
             (values 0f0 0f0))))

(defmethod paint-node :around ((node node) context list x y)
  (cond
    ((not (node-visible node))
     nil)
    ((not (node-cacheable node))
     (call-next-method))
    (t
     (let* ((scale (context-scale context))
            (x (snap-to-pixel x scale))
            (y (snap-to-pixel y scale)))
       (cond
         ((not *paint-caching*)
          (call-next-method node context list x y))
         (t
          (when (node-paint-dirty node)
            (let ((cache (or (node-cache node)
                             (setf (node-cache node) (make-display-list :capacity 64)))))
              ;; A cache that is too small is replaced by one twice the size, which
              ;; only happens as the tree is first painted.
              (loop
                (clear-display-list cache)
                (handler-case (progn (call-next-method node context cache 0f0 0f0)
                                     (return))
                  (display-list-full ()
                    (setf cache (make-display-list
                                 :capacity (* 2 (display-list-capacity cache)))
                          (node-cache node) cache))))
              (setf (node-paint-dirty node) nil)))
          (append-display-list list (node-cache node) x y)
          nil))))))

(defgeneric node-interactive-p (node)
  (:documentation "True if pointer events are for NODE and not for what is under it.")
  (:method ((node node)) nil))

(defgeneric node-focusable-p (node)
  (:documentation "True if NODE can have the keyboard focus.")
  (:method ((node node)) nil))

(defgeneric activate (node)
  (:documentation "What a click, or the space bar on the focused node, does.")
  (:method ((node node)) nil))

(defun interactive-p (node)
  "True if pointer events are for NODE now: it is shown, enabled and interactive."
  (and (node-visible node) (node-enabled node) (node-interactive-p node)))

(defun focusable-p (node)
  "True if the keyboard can be given to NODE now."
  (and (node-visible node) (node-enabled node) (node-focusable-p node)))

;;; What an interactive node does with the pointer and the keys. Coordinates are in the
;;; node's own, from its top left. The SESSION is what the variant knows of the pointer
;;; and the keyboard (see below), and gives a node the context for measuring text.

(defgeneric pointer-press (node session x y button)
  (:documentation "A button of the pointer went down on NODE at X, Y. BUTTON is 0 for
the primary button. While the primary button is down NODE is the one pointer events go
to, wherever the pointer goes.")
  (:method ((node node) session x y button)
    (declare (ignore session x y button))
    nil))

(defgeneric pointer-move (node session x y)
  (:documentation "The pointer moved to X, Y over NODE, the node it is over.")
  (:method ((node node) session x y)
    (declare (ignore session x y))
    nil))

(defgeneric pointer-exit (node session)
  (:documentation "The pointer is no longer over NODE.")
  (:method ((node node) session)
    (declare (ignore session))
    nil))

(defgeneric pointer-drag (node session x y)
  (:documentation "The pointer moved to X, Y with the primary button down, after it went
down on NODE; the point may be outside it.")
  (:method ((node node) session x y)
    (declare (ignore session x y))
    nil))

(defgeneric pointer-release (node session x y inside-p)
  (:documentation "The primary button went up after it went down on NODE. INSIDE-P is
true if the pointer is over NODE.")
  (:method ((node node) session x y inside-p)
    (declare (ignore session x y inside-p))
    nil))

(defgeneric pointer-wheel (node session dx dy)
  (:documentation "The wheel or trackpad scrolled DX across and DY down over NODE, in
points. Returns true if it used them.")
  (:method ((node node) session dx dy)
    (declare (ignore session dx dy))
    nil))

(defgeneric key-input (node session event)
  (:documentation "A key event for NODE, which has the keyboard focus. Returns true if
it used the key, so that the variant does not (Tab, for one, moves the focus).")
  (:method ((node node) session event)
    (declare (ignore session event))
    nil))

(defgeneric node-focus-on-click-p (node)
  (:documentation "True if pressing the pointer on NODE gives it the keyboard focus.")
  (:method ((node node)) nil))

;;; --- what the variant knows of the pointer and the keyboard ---

(defstruct (session (:constructor make-session ()))
  "The state of the pointer and the keyboard in one tree: the node the pointer is over
(HOVER), the node a press began on (PRESSED), the node with the keyboard (FOCUS), the
context of the last frame, and functions to run at the start of each frame (TICKERS)."
  (context nil)
  (hover nil)
  (pressed nil)
  (focus nil)
  (tickers '() :type list))

(defun refresh-look (session node)
  "Set what NODE shows of the pointer: hovered if the pointer is over it and nothing else
is pressed, and pressed if a press began on it."
  (when node
    (let ((hovered (and (eq (session-hover session) node)
                        (or (null (session-pressed session))
                            (eq (session-pressed session) node))))
          (pressed (eq (session-pressed session) node)))
      ;; Being pressed shows only while the pointer is over the node, so a press that
      ;; ends elsewhere changes nothing to look at.
      (let ((visible (or (not (eq hovered (node-hovered node)))
                         (and (not (eq pressed (node-pressed node)))
                              (or hovered (node-hovered node))))))
        (setf (node-hovered node) hovered
              (node-pressed node) pressed)
        (when visible
          (touch node nil))))))

(defun focus-node (session node)
  "Give the keyboard to NODE, or to nothing if NODE is NIL."
  (let ((old (session-focus session)))
    (unless (eq old node)
      (setf (session-focus session) node)
      (when old
        (setf (node-focused old) nil)
        (touch old nil))
      (when node
        (setf (node-focused node) t)
        (touch node nil)))))

;;; --- finding nodes ---

(defun node-at (node x y)
  "The deepest node under the point X, Y, which is given in NODE's own coordinates,
or NIL if the point is outside NODE. Later children are on top, so they are asked
first."
  (declare (type single-float x y))
  (when (and (node-visible node)
             (<= 0f0 x) (< x (node-w node)) (<= 0f0 y) (< y (node-h node)))
    (let ((children (node-children node)))
      (loop :for i :from (1- (length children)) :downto 0
            :do (let* ((child (svref children i))
                       (hit (node-at child (- x (node-x child)) (- y (node-y child)))))
                  (when hit
                    (return-from node-at hit))))
      node)))

(defun interactive-at (node x y)
  "The node that pointer events at the point X, Y (in NODE's own coordinates) are for: the
topmost interactive node whose area holds it, looking at later children first, and then
at a node only if none of its children is. A shown node that is on top and not
interactive does not hide an interactive one under it. NIL if there is none."
  (declare (type single-float x y))
  (when (and (node-visible node)
             (<= 0f0 x) (< x (node-w node)) (<= 0f0 y) (< y (node-h node)))
    (let ((children (node-children node)))
      (loop :for i :from (1- (length children)) :downto 0
            :do (let* ((child (svref children i))
                       (hit (interactive-at child (- x (node-x child)) (- y (node-y child)))))
                  (when hit
                    (return-from interactive-at hit))))
      (when (interactive-p node)
        node))))

(defun map-nodes (function root)
  "Call FUNCTION with ROOT and every node under it, parents before children."
  (funcall function root)
  (loop :for child :across (node-children root)
        :do (map-nodes function child))
  nil)

(defun find-node (root id)
  "The node under ROOT with the id ID, or NIL."
  (map-nodes (lambda (node)
               (when (eql (node-id node) id)
                 (return-from find-node node)))
             root)
  nil)

(defun node-origin (node)
  "Where NODE's top left is in the window, from the last layout, as two values."
  (let ((x 0f0) (y 0f0))
    (declare (type single-float x y))
    (loop :for n = node :then (node-parent n)
          :while n
          :do (incf x (node-x n))
              (incf y (node-y n)))
    (values x y)))
