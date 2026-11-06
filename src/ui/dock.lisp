(defpackage #:almighty-bolt/ui/dock
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:box #:almighty-bolt/ui/box)
                    (#:hash #:almighty-bolt/ui/hash)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:widgets #:almighty-bolt/ui/widgets)
                    (#:drag #:almighty-bolt/ui/drag)
                    (#:split #:almighty-bolt/ui/split))
  (:export #:dock
           #:dock-panes
           #:remove-pane
           #:place-pane
           #:pane-group
           #:show-pane))

(in-package #:almighty-bolt/ui/dock)

;;; Panes that the person arranges: groups of tabs, each showing one of its panes, side by side or one
;;; over the other with dividers between them. A pane is moved by dragging its tab: let go of on
;;; another group's tabs, it goes in that group; on a group's edge, it splits the group there, in a
;;; group of its own; on the middle of a group, it goes in it. A group left empty goes, and the
;;; divider with it. A pane is moved without the pointer too: its tab's menu (a secondary press, or
;;; Show Menu in VoiceOver) moves it to the next or the previous group, or splits its group with it,
;;; and Control-Shift-Right and -Left move the active pane to the next and the previous group.
;;;
;;; The layout is data, which the dock returns, so that an app can keep it (in STATE-LET, or a file):
;;;   (:tabs SELECTED KEY ...)                     a group of the panes KEY ..., SELECTED shown
;;;   (:split DIRECTION SIZE FIRST SECOND [KEEP])  two layouts, as SPLIT draws them: SIZE is the
;;;                                                first's, or with KEEP :second the second's
;;; A pane keeps what it holds (a caret, a scroll, which items of a tree are closed) when it is moved,
;;; because what is in it is keyed by the dock and the pane, not by where in the dock it is.

(defconstant +edge-fraction+ 0.25
  "How far into a group, as a fraction of its width or height, a drop is on its edge.")

;;; --- the layout ---

(defun dock-panes (layout)
  "The keys of the panes in LAYOUT, in order."
  (ecase (first layout)
    (:tabs (cddr layout))
    (:split (append (dock-panes (fourth layout)) (dock-panes (fifth layout))))))

(defun remove-pane (layout key)
  "LAYOUT without the pane KEY: a group it leaves empty goes, and the split it was in becomes the
other side. NIL if nothing is left."
  (ecase (first layout)
    (:tabs (let ((keys (remove key (cddr layout) :test #'equal)))
             (cond ((null keys) nil)
                   ((member (second layout) keys :test #'equal) (list* :tabs (second layout) keys))
                   (t (list* :tabs (first keys) keys)))))
    (:split (destructuring-bind (direction size first second &rest keep) (rest layout)
              (let ((first (remove-pane first key))
                    (second (remove-pane second key)))
                (cond ((and first second) (list* :split direction size first second keep))
                      (t (or first second))))))))

(defun place-pane (layout key anchor zone &optional (size 240))
  "LAYOUT with the pane KEY put by the group that has the pane ANCHOR: in it (ZONE :INTO), or
beside it in a group of its own (:LEFT, :RIGHT, :TOP, :BOTTOM), the new group SIZE points wide or
tall. KEY is taken from where it was first. LAYOUT as it was if ANCHOR is KEY, or not in it."
  (if (or (equal key anchor) (not (member anchor (dock-panes layout) :test #'equal)))
      layout
      (labels ((put (layout)
                 (ecase (first layout)
                   (:tabs
                    (if (member anchor (cddr layout) :test #'equal)
                        (let ((alone (list :tabs key key)))
                          (ecase zone
                            (:into (list* :tabs key (append (cddr layout) (list key))))
                            (:left (list :split :horizontal size alone layout))
                            (:right (list :split :horizontal size layout alone))
                            (:top (list :split :vertical size alone layout))
                            (:bottom (list :split :vertical size layout alone))))
                        layout))
                   (:split (destructuring-bind (direction size first second &rest keep)
                               (rest layout)
                             (list* :split direction size (put first) (put second) keep))))))
        (put (remove-pane layout key)))))

(defun groups (layout)
  "The keys of each group of LAYOUT, in order."
  (ecase (first layout)
    (:tabs (list (cddr layout)))
    (:split (append (groups (fourth layout)) (groups (fifth layout))))))

(defun group-move (layout key step)
  "Where the pane KEY goes to be in the group STEP after its own (1 the next, -1 the one before,
round from the last to the first), as a drop: (key anchor :into 0). NIL if there is one group."
  (let* ((groups (groups layout))
         (count (length groups))
         (index (position-if (lambda (keys) (member key keys :test #'equal)) groups)))
    (when (and index (> count 1))
      (list key (first (nth (mod (+ index step) count) groups)) :into 0))))

(defparameter *split-items*
  '(("Split Left" . :left) ("Split Right" . :right) ("Split Up" . :top) ("Split Down" . :bottom))
  "The items of a tab's menu that split its group with its pane, and the side the pane goes to.")

(defun tab-items (layout keys)
  "The items of the menu of a tab in the group of KEYS: to another group if there is one, and to a
side of its own if it is not alone in it."
  (append (and (> (length (groups layout)) 1)
               (list "Move to Next Group" "Move to Previous Group"))
          (and (> (length keys) 1)
               (mapcar #'car *split-items*))))

(defun pane-group (layout key)
  "The keys of the panes in the group of LAYOUT that has the pane KEY, or NIL."
  (find-if (lambda (keys) (member key keys :test #'equal)) (groups layout)))

(defun show-pane (layout key)
  "LAYOUT with the group that has the pane KEY showing it: for an app that brings a pane to the
front (the dock takes up a layout given that is not the one it was given before)."
  (ecase (first layout)
    (:tabs (if (member key (cddr layout) :test #'equal)
               (list* :tabs key (cddr layout))
               layout))
    (:split (destructuring-bind (direction size first second &rest keep) (rest layout)
              (list* :split direction size (show-pane first key) (show-pane second key) keep)))))

(defun with-panes (layout keys)
  "LAYOUT with only the panes in KEYS, and any of KEYS it did not have in its first group."
  (let* ((layout (reduce #'remove-pane (set-difference (and layout (dock-panes layout)) keys
                                                       :test #'equal)
                         :initial-value layout))
         (missing (remove-if (lambda (key) (and layout (member key (dock-panes layout) :test #'equal)))
                             keys)))
    (cond ((null missing) layout)
          ((null layout) (list* :tabs (first missing) missing))
          (t (labels ((add (layout)
                        (ecase (first layout)
                          (:tabs (append layout missing))
                          (:split (list* :split (second layout) (third layout)
                                         (add (fourth layout)) (nthcdr 4 layout))))))
               (add layout))))))

(defun with-size (layout path size)
  "LAYOUT with the split at PATH (a list of 0 and 1, from the top) SIZE points."
  (if (null path)
      (list* :split (second layout) size (nthcdr 3 layout))
      (destructuring-bind (direction old first second &rest keep) (rest layout)
        (if (zerop (first path))
            (list* :split direction old (with-size first (rest path) size) second keep)
            (list* :split direction old first (with-size second (rest path) size) keep)))))

(defun with-selected (layout path key)
  "LAYOUT with the group at PATH showing KEY."
  (if (null path)
      (list* :tabs key (cddr layout))
      (destructuring-bind (direction size first second &rest keep) (rest layout)
        (if (zerop (first path))
            (list* :split direction size (with-selected first (rest path) key) second keep)
            (list* :split direction size first (with-selected second (rest path) key) keep)))))

;;; --- drawing it ---

(defstruct (dock-state (:constructor make-dock-state ()))
  ;; The layout now, and the one the app gave last: one it gives that is not that is taken up.
  (layout nil)
  (given nil)
  ;; The pane last pressed in or that has the keyboard, whose shortcuts are heard; and the key of
  ;; each shown pane's box, to know where it was.
  (active nil)
  (pane-boxes (make-hash-table :test 'equal) :read-only t))

(defun path-id (prefix path)
  (intern (format nil "~a~{-~d~}" prefix path) :keyword))

(defun drop-zone (rect bar-rect x y)
  "Where in a group whose rectangle is RECT (its tabs' BAR-RECT) the point X, Y is, for a drop:
:INTO, an edge, or NIL if it is outside it."
  (when (and rect (<= (ui:rect-x rect) x (+ (ui:rect-x rect) (ui:rect-w rect)))
             (<= (ui:rect-y rect) y (+ (ui:rect-y rect) (ui:rect-h rect))))
    (if (and bar-rect (<= y (+ (ui:rect-y bar-rect) (ui:rect-h bar-rect))))
        :into
        (let ((fx (/ (- x (ui:rect-x rect)) (max 1f0 (ui:rect-w rect))))
              (fy (/ (- y (ui:rect-y rect)) (max 1f0 (ui:rect-h rect)))))
          (cond ((< fx +edge-fraction+) :left)
                ((> fx (- 1 +edge-fraction+)) :right)
                ((< fy +edge-fraction+) :top)
                ((> fy (- 1 +edge-fraction+)) :bottom)
                (t :into))))))

(defun zone-rect (rect zone)
  "The part of RECT that a drop in ZONE gives the pane: x, y, width, height."
  (let ((x (ui:rect-x rect)) (y (ui:rect-y rect)) (w (ui:rect-w rect)) (h (ui:rect-h rect)))
    (ecase zone
      (:into (values x y w h))
      (:left (values x y (/ w 2) h))
      (:right (values (+ x (/ w 2)) y (/ w 2) h))
      (:top (values x y w (/ h 2)))
      (:bottom (values x (+ y (/ h 2)) w (/ h 2))))))

(defun show-zone (owner rect zone)
  "Outline, in the ring's colour, where a pane let go of now would go."
  (multiple-value-bind (x y w h) (zone-rect rect zone)
    (let ((ui ui:*ui*))
      (multiple-value-bind (float open)
          (ui:open-float ui :outline owner :dock-zone
                         (ui:make-float-spec :placement :at :x (float x 1f0) :y (float y 1f0) :tool t))
        (setf (box:box-pref-w float) (float w 1f0)
              (box:box-pref-h float) (float h 1f0)
              (box:box-color float) (theme:theme-ring theme:*theme*))
        (ui:close-float ui open)))))

(defun open-pane-box (dock-key key)
  "Open the container a pane's contents are in, keyed by the dock and the pane wherever it is."
  ;; Called :pane, so that the pane's key names its tab alone.
  (let ((box (ui:open-box ui:*ui* :column :id :pane :rooted-at (hash:mix dock-key (hash:hash-of key)))))
    (setf (box:box-cross-align box) :stretch
          (box:box-fill box) t
          (box:box-flex box) 1f0)
    box))

(defvar *dock-min* 80
  "How small the panes of the dock being drawn may be made.")

(defvar *dock-state* nil
  "The state of the dock being drawn.")

(defun draw-pane (dock-key pane)
  "Draw PANE, (key title function), in its box: its shortcuts heard if it is the active pane, and
made the active one if the keyboard is in it."
  (let* ((ui ui:*ui*)
         (state *dock-state*)
         (key (first pane))
         (box (open-pane-box dock-key key))
         (first-box (ui:ui-count ui)))
    (setf (gethash key (dock-state-pane-boxes state)) (box:box-key box))
    (let ((widgets:*shortcuts-heard* (and widgets:*shortcuts-heard* (equal key (dock-state-active state)))))
      (funcall (third pane)))
    (let ((focus (ui:ui-focus ui)))
      (when (and (/= focus 0)
                 (loop :for index :from first-box :below (ui:ui-count ui)
                         :thereis (= (box:box-key (svref (ui:ui-boxes ui) index)) focus)))
        (setf (dock-state-active state) key)))
    (ui:close-box ui)))

(defun draw-group (dock-key layout node path panes)
  "Draw the group NODE at PATH: its tabs and the pane it shows. Returns the layout it makes of NODE
(another pane chosen), and what was let go of on it, if anything: (values node (key anchor zone
size))."
  (destructuring-bind (selected &rest keys) (rest node)
    (let* ((ui ui:*ui*)
           (new node)
           (drop nil)
           (group (ui:open-box ui :column :id (path-id "GROUP" path)))
           (group-key (box:box-key group))
           (bar-key 0))
      (setf (box:box-cross-align group) :stretch
            (box:box-fill group) t
            (box:box-flex group) 1f0)
      (let ((bar (ui:open-box ui :row :id :bar)))
        (setf bar-key (box:box-key bar))
        (dolist (key keys)
          (let* ((title (second (assoc key panes :test #'equal)))
                 (item (widgets:context-menu (:id (list :tab-menu key) :items (tab-items layout keys))
                         (when (drag:drag-source (:id key :payload (list :dock-pane key) :label title)
                                 (widgets:panel (:id :tab :left 10 :right 10 :top 4 :bottom 4 :radius 4
                                                 :color (if (equal key selected) :row-selected :panel))
                                   ;; On the chosen tab's fill, in the colour words on it take.
                                   (widgets:label title :id :title
                                                        :color (and (equal key selected) :button-text))))
                           ;; Chosen, its pane is shown, and its shortcuts heard.
                           (setf new (list* :tabs key keys)
                                 (dock-state-active *dock-state*) key)))))
            ;; Moved by its menu: as if it were let go of there.
            (when item
              (let ((side (cdr (assoc item *split-items* :test #'string=)))
                    (rect (ui:rect-of ui group-key)))
                (setf drop
                      (if side
                          (list key (find key keys :test-not #'equal) side
                                (if rect
                                    (/ (if (member side '(:left :right)) (ui:rect-w rect) (ui:rect-h rect))
                                       2)
                                    240))
                          (group-move layout key
                                      (if (string= item "Move to Next Group") 1 -1))))))))
        (ui:close-box ui))
      (widgets:stack (:id :body :flex 1)
        (let ((pane (assoc (second new) panes :test #'equal)))
          (when pane
            (draw-pane dock-key pane))))
      (ui:close-box ui)
      ;; Where a pane being dragged would go here, and taking it when it is let go of.
      (let ((payload (drag:dragging)))
        (when (and (consp payload) (eq (first payload) :dock-pane))
          (let* ((moved (second payload))
                 (rect (ui:rect-of ui group-key))
                 (zone (drop-zone rect (ui:rect-of ui bar-key) (ui:ui-pointer-x ui) (ui:ui-pointer-y ui)))
                 (anchor (or (find moved keys :test-not #'equal) moved)))
            ;; Into the group it is in already, or beside the group that is only it: nowhere new.
            (when (and zone
                       (not (and (eq zone :into) (member moved keys :test #'equal)))
                       (not (equal anchor moved)))
              (if (drag:take-drop)
                  (setf drop (list moved anchor zone
                                   (/ (if (member zone '(:left :right)) (ui:rect-w rect) (ui:rect-h rect))
                                      2)))
                  (show-zone group-key rect zone))))))
      (values new drop))))

(defun draw-node (dock-key layout node path panes)
  "Draw NODE of LAYOUT, at PATH. Returns the layout it makes of it, and a drop on it."
  (ecase (first node)
    (:tabs (draw-group dock-key layout node path panes))
    (:split
     (destructuring-bind (direction size first second &optional (keep :first)) (rest node)
       (let ((new-first first) (new-second second) (drop nil))
         (let ((now (split:split (:id (path-id "SPLIT" path) :direction direction :size size
                                  :min *dock-min* :keep keep :flex 1)
                      (multiple-value-bind (node there) (draw-node dock-key layout first (append path '(0))
                                                               panes)
                        (setf new-first node drop (or drop there)))
                      (multiple-value-bind (node there) (draw-node dock-key layout second (append path '(1))
                                                               panes)
                        (setf new-second node drop (or drop there))))))
           (values (list* :split direction now new-first new-second (nthcdr 5 node)) drop)))))))

(defun draw-dock (id layout flex width height panes &optional (min 80))
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :column :id (or id (widgets:required-id 'dock))))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-dock-state)))
    (widgets:set-size-limits box width height nil nil nil nil)
    (setf (box:box-cross-align box) :stretch
          (box:box-fill box) t
          (box:box-flex box) (float flex 1f0))
    ;; A layout the app gives that is not the one it gave before is taken up.
    (unless (equal layout (dock-state-given state))
      (setf (dock-state-layout state) layout
            (dock-state-given state) layout))
    (let ((old (with-panes (dock-state-layout state) (mapcar #'first panes)))
          (*dock-state* state)
          (*dock-min* min))
      ;; A press in a pane, where it was in the last frame, makes it the active one; until there is
      ;; one, the first pane is.
      (when (ui:ui-pressed ui)
        (maphash (lambda (pane box-key)
                   (let ((rect (ui:rect-of ui box-key)))
                     (when (and rect
                                (<= (ui:rect-x rect) (ui:ui-pointer-x ui) (+ (ui:rect-x rect) (ui:rect-w rect)))
                                (<= (ui:rect-y rect) (ui:ui-pointer-y ui) (+ (ui:rect-y rect) (ui:rect-h rect))))
                       (setf (dock-state-active state) pane))))
                 (dock-state-pane-boxes state)))
      (unless (member (dock-state-active state) (dock-panes old) :test #'equal)
        (setf (dock-state-active state) (first (dock-panes old))))
      (clrhash (dock-state-pane-boxes state))
      (multiple-value-bind (new drop) (draw-node key old old '() panes)
        ;; The active pane moved by the keyboard, if what is in the panes did not take the keys.
        (unless drop
          (let ((step (cond ((widgets:shortcut core:+key-right+ :control :shift) 1)
                            ((widgets:shortcut core:+key-left+ :control :shift) -1))))
            (when step
              (setf drop (group-move old (dock-state-active state) step)))))
        (when drop
          (destructuring-bind (moved anchor zone size) drop
            (setf new (place-pane new moved anchor zone size))))
        ;; What was drawn this frame was the layout before: draw it again as it is now.
        (unless (equal new old)
          (setf (ui:ui-consumed ui) t))
        (setf (dock-state-layout state) new)
        (ui:close-box ui)
        new))))

(defmacro dock ((&key id layout (flex 0) width height (min 80)) &body panes)
  "Panes the person arranges, each (KEY TITLE BODY...): KEY names it (a keyword), TITLE is its tab's
words, and BODY draws it when it is shown. LAYOUT, if given, is how they are arranged (see above);
by default all are tabs of one group. Returns the layout now, which the app gives back to keep it
(a LAYOUT other than the one given the frame before is taken up). A pane is moved by dragging its
tab: to another group's tabs or middle to go in it, to a group's edge to split it there. What is in
a pane keeps its state when the pane is moved. MIN is how small a group may be made, by its divider
or by the window. A split of the layout keeps the size of its first group, or with :second as its
last element the second's (a sidebar on the right), when the dock is made larger or smaller. The :id
is required: the layout is kept by it."
  `(draw-dock ,id ,layout ,flex ,width ,height
              (list ,@(loop :for (key title . body) :in panes
                            :collect `(list ,key ,title (lambda () ,@body))))
              ,min))
