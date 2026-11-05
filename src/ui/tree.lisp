(defpackage #:almighty-bolt/ui/tree
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:widgets #:almighty-bolt/ui/widgets))
  (:export #:tree
           #:+tree-indent+))

(in-package #:almighty-bolt/ui/tree)

;;; A tree view: items one under another, each indented by how deep it is, with a mark before those
;;; that have children that opens and closes them. A press on a row selects its item, and a press on
;;; the mark (or two on the row) opens or closes it. The tree takes the keyboard as one thing, as an
;;; outline on a Mac does, so Tab goes past its rows: Up and Down move the selection, Right opens
;;; what is selected (or goes into it, if it is open) and Left closes it (or goes out to its parent).
;;; What is closed is kept by the tree's id, by the items' keys; everything is open until it is
;;; closed. What is selected is the app's, given each frame and returned, as a list box's is.

(defconstant +tree-indent+ 16f0
  "How far in each level of the tree is, and how wide the mark that opens and closes an item.")

(defstruct (tree-state (:constructor make-tree-state ()))
  ;; The keys of the items that are closed.
  (closed (make-hash-table :test 'equal) :type hash-table :read-only t))

(defstruct (shown (:constructor shown (key parent depth parent-p open)))
  "A row of the tree as it is shown this frame: its item's KEY, its PARENT's key (or NIL), its DEPTH,
whether it has children, and whether they are shown."
  key parent depth parent-p open)

(defvar *row-font* nil "The font of the rows of the tree being described.")

(defun describe-row (ui tree state item key text depth parent-p open selected)
  "Describe the row of ITEM, and take a press on it. Returns what it was pressed for: :SELECT,
:TOGGLE (the mark, or a double press), or NIL."
  (declare (ignore item state))
  (let* ((row (ui:open-box ui :tree-row :id key))
         (row-key (box:box-key row))
         (pressed nil))
    (setf (box:box-interactive row) t
          ;; Its depth's indent, then the mark's room.
          (box:box-pad-left row) (+ 4f0 (* (1+ depth) +tree-indent+))
          (box:box-pad-right row) 6f0
          (box:box-pad-top row) 2f0
          (box:box-pad-bottom row) 2f0
          (box:box-n1 row) (float depth 1f0)
          ;; 1 if it has children, 2 if they are shown too; 0 if it has none.
          (box:box-n2 row) (cond ((not parent-p) 0f0) (open 2f0) (t 1f0))
          (box:box-n3 row) (if selected 1 0))
    (when (and (= (ui:ui-hot ui) row-key) (ui:ui-pressed ui))
      (ui:consume-press ui)
      (ui:focus-by-click ui (box:box-key tree))
      (setf (ui:ui-active ui) row-key)
      (let ((rect (ui:rect-of ui row-key)))
        (setf pressed
              (if (and parent-p
                       (or (>= (ui:ui-click-count ui) 2)
                           (and rect
                                (< (- (ui:ui-pointer-x ui) (ui:rect-x rect)) (box:box-pad-left row)))))
                  :toggle
                  :select))))
    (when (and (= (ui:ui-active ui) row-key) (ui:ui-released ui))
      (ui:consume-release ui)
      (setf (ui:ui-active ui) 0))
    (setf (box:box-look row) (widgets:look-of ui row-key t))
    (widgets:label text :id :text :font *row-font*)
    (ui:close-box ui)
    pressed))

(defun move-in (rows selected key)
  "What an arrow KEY makes selected among ROWS (the rows shown, in order), with SELECTED selected, and
what it opens or closes: (values new-selection key-to-open key-to-close)."
  (let* ((place (position selected rows :key #'shown-key :test #'equal))
         (row (and place (aref rows place)))
         (last (1- (length rows))))
    (cond ((null place)
           (values (and (plusp (length rows)) (shown-key (aref rows 0))) nil nil))
          ((eql key core:+key-up+) (values (shown-key (aref rows (max 0 (1- place)))) nil nil))
          ((eql key core:+key-down+) (values (shown-key (aref rows (min last (1+ place)))) nil nil))
          ((eql key core:+key-home+) (values (shown-key (aref rows 0)) nil nil))
          ((eql key core:+key-end+) (values (shown-key (aref rows last)) nil nil))
          ((eql key core:+key-right+)
           (cond ((and (shown-parent-p row) (not (shown-open row))) (values selected selected nil))
                 ((and (shown-parent-p row) (< place last)) (values (shown-key (aref rows (1+ place))) nil nil))
                 (t (values selected nil nil))))
          ((eql key core:+key-left+)
           (cond ((and (shown-parent-p row) (shown-open row)) (values selected nil selected))
                 ((shown-parent row) (values (shown-parent row) nil nil))
                 (t (values selected nil nil))))
          (t (values selected nil nil)))))

(defun tree (roots &key (id (widgets:required-id 'tree)) (children (constantly '()))
                          (label #'princ-to-string) (key #'identity) selected labelled-by tooltip
                          (flex 0) width height min-width min-height max-width max-height font)
  "A tree of ROOTS and what CHILDREN (a function of an item) gives for each, one under another, each
called by LABEL (a function of an item) and known by KEY (another, whose values are unique in the
tree and compared with EQUAL; also each row's id). SELECTED is the key of the item selected, or NIL.
Returns the key selected now and whether the person changed it this frame. What is closed is kept
by the tree's id; everything is open until it is closed. FONT is the rows' (see LABEL)."
  (let* ((ui ui:*ui*)
         (*row-font* font)
         (tree (ui:open-box ui :tree :id id :labelled-by labelled-by :tooltip tooltip))
         (tree-key (box:box-key tree))
         (state (ui:state-of ui tree-key #'make-tree-state))
         (closed (tree-state-closed state))
         (rows (make-array 16 :adjustable t :fill-pointer 0))
         (new selected))
    (widgets:set-size-limits tree width height min-width min-height max-width max-height)
    (setf (box:box-interactive tree) t
          (box:box-focusable tree) t
          (box:box-enabled tree) t
          (box:box-cross-align tree) :stretch
          (box:box-flex tree) (float flex 1f0))
    (when (and (= (ui:ui-hot ui) tree-key) (ui:ui-pressed ui))
      (ui:consume-press ui)
      (ui:focus-by-click ui tree-key))
    (labels ((walk (item parent depth)
               (let* ((k (funcall key item))
                      (kids (funcall children item))
                      (open (and kids (not (gethash k closed)))))
                 (vector-push-extend (shown k parent depth (and kids t) open) rows)
                 (case (describe-row ui tree state item k (funcall label item) depth (and kids t) open
                                     (equal k new))
                   (:select (setf new k))
                   (:toggle (setf new k)
                    (if open (setf (gethash k closed) t) (remhash k closed))
                    (setf (ui:ui-consumed ui) t)))
                 (when open
                   (dolist (kid kids)
                     (walk kid k (1+ depth)))))))
      (dolist (root roots)
        (walk root nil 0)))
    (when (= (ui:ui-focus ui) tree-key)
      (dolist (event (copy-list (ui:ui-keys ui)))
        (let ((k (core:key-event-logical-key event)))
          (when (member k (list core:+key-up+ core:+key-down+ core:+key-left+ core:+key-right+ core:+key-home+ core:+key-end+))
            (ui:consume-key ui event)
            (multiple-value-bind (now open shut) (move-in rows new k)
              (setf new now)
              (when open (remhash open closed) (setf (ui:ui-consumed ui) t))
              (when shut (setf (gethash shut closed) t) (setf (ui:ui-consumed ui) t)))))))
    ;; The selection changed after its row was drawn: build the frame again.
    (unless (equal new selected)
      (setf (ui:ui-consumed ui) t))
    (setf (box:box-look tree) (widgets:look-of ui tree-key t))
    (ui:close-box ui)
    (values new (not (equal new selected)))))
