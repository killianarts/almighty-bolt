(defpackage #:almighty-bolt/ui/drag
  (:use #:cl)
  (:local-nicknames (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:widgets #:almighty-bolt/ui/widgets))
  (:export #:drag-source
           #:drop-target
           #:dragging
           #:begin-drag
           #:follow-drag
           #:drag-ghost
           #:insertion-place
           #:show-insertion
           #:take-drop))

(in-package #:almighty-bolt/ui/drag)

;;; Dragging, in the immediate way: a source says what it carries once the pointer has moved far
;;; enough from where it was pressed (so a press and a release in place are still a click), a ghost
;;; under the pointer says what it is, and a target that the pointer is over when it is let go of
;;; takes it, where among its children it was let go. The release of a drag that is moving is the
;;; drop's and no widget's (BEGIN-FRAME-INPUT sees to that), so a target takes it whether it is
;;; described before the source or after.

(defconstant +drag-distance+ 4f0
  "How far, in points, the pointer moves from the press before a drag begins.")

(defun dragging ()
  "What is being dragged, once it is moving, or NIL."
  (let ((drag (ui:ui-drag ui:*ui*)))
    (and drag (ui:drag-moving drag) (ui:drag-payload drag))))

(defun begin-drag (key payload label)
  "A press on the box KEY may begin a drag of PAYLOAD, which the ghost calls LABEL."
  (let ((ui ui:*ui*))
    (setf (ui:ui-active ui) key
          (ui:ui-drag ui) (ui:make-drag key payload label (ui:ui-pointer-x ui) (ui:ui-pointer-y ui)))))

(defun follow-drag (key)
  "For the box KEY that a drag may come from: start it moving once the pointer has gone far enough,
and end it if the press is let go of in place. True if it was let go of in place over KEY (a
click)."
  (let* ((ui ui:*ui*)
         (drag (ui:ui-drag ui)))
    (when (and drag (eql (ui:drag-origin drag) key) (= (ui:ui-active ui) key))
      (when (and (not (ui:drag-moving drag))
                 (> (+ (abs (- (ui:ui-pointer-x ui) (ui:drag-x drag)))
                       (abs (- (ui:ui-pointer-y ui) (ui:drag-y drag))))
                    +drag-distance+))
        (setf (ui:drag-moving drag) t))
      (when (ui:ui-released ui)
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0
              (ui:ui-drag ui) nil)
        (= (ui:ui-hot ui) key)))))

(defun drag-ghost (owner)
  "The ghost under the pointer that says what is dragged from the box OWNER, while it moves."
  (let* ((ui ui:*ui*)
         (drag (ui:ui-drag ui)))
    (when (and drag (ui:drag-moving drag) (eql (ui:drag-origin drag) owner) (not (ui:drag-dropping drag)))
      (multiple-value-bind (float open)
          (ui:open-float ui :panel owner :drag-ghost
                         (ui:make-float-spec :placement :at :x (+ (ui:ui-pointer-x ui) 14f0)
                                             :y (+ (ui:ui-pointer-y ui) 10f0) :tool t))
        (setf (box:box-color float) (theme:theme-button theme:*theme*)
              (box:box-n1 float) 4f0
              (box:box-pad-left float) 8f0 (box:box-pad-right float) 8f0
              (box:box-pad-top float) 4f0 (box:box-pad-bottom float) 4f0)
        (widgets:label (princ-to-string (or (ui:drag-label drag) (ui:drag-payload drag))) :id :label :size 13
                       :color (theme:theme-button-text theme:*theme*))
        (ui:close-float ui open)))))

(defun insertion-place (container horizontal x y)
  "Where among CONTAINER's children (a box of this build) a drop at X, Y goes: its index, and where
the marker of it is in the window (x, y, width, height). Children whose places are not known (no id,
nothing to press) do not count, and HORIZONTAL is for a row."
  (let* ((ui ui:*ui*)
         (outer (ui:rect-of ui (box:box-key container)))
         (children (box:box-children container))
         (index 0)
         (edge nil))
    (loop :for child :across children
          :for i :from 0
          :do (let ((rect (ui:rect-of ui (box:box-key child))))
                (when rect
                  (let ((start (if horizontal (ui:rect-x rect) (ui:rect-y rect)))
                        (end (if horizontal
                                 (+ (ui:rect-x rect) (ui:rect-w rect))
                                 (+ (ui:rect-y rect) (ui:rect-h rect)))))
                    (if (< (if horizontal x y) (/ (+ start end) 2))
                        (progn (unless edge (setf edge start))
                               (return))
                        (setf index (1+ i)
                              edge end))))))
    ;; With nothing before the place, the marker is at the start of the container.
    (let ((edge (or edge (and outer (if horizontal (ui:rect-x outer) (ui:rect-y outer))) 0f0)))
      (if (null outer)
          (values index 0f0 0f0 0f0 0f0)
          (if horizontal
              (values index (- edge 1f0) (ui:rect-y outer) 2f0 (ui:rect-h outer))
              (values index (ui:rect-x outer) (- edge 1f0) (ui:rect-w outer) 2f0))))))

(defun show-insertion (owner x y w h)
  "The marker of where a drop goes: a bar of the theme's ring colour at X, Y, W by H in the window."
  (let ((ui ui:*ui*))
    (multiple-value-bind (float open)
        (ui:open-float ui :outline owner :insertion
                       (ui:make-float-spec :placement :at :x (float x 1f0) :y (float y 1f0) :tool t))
      (setf (box:box-pref-w float) (float w 1f0)
            (box:box-pref-h float) (float h 1f0)
            (box:box-color float) (theme:theme-ring theme:*theme*))
      (ui:close-float ui open))))

(defun take-drop ()
  "Take what is being dropped this frame: its payload, or NIL if nothing is."
  (let* ((ui ui:*ui*)
         (drag (ui:ui-drag ui)))
    (when (and drag (ui:drag-dropping drag))
      (setf (ui:ui-drag ui) nil
            (ui:ui-consumed ui) t)
      (ui:drag-payload drag))))

(defun begin-drag-source (id payload label)
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :drag-source :id (or id (widgets:required-id 'drag-source))))
         (key (box:box-key box)))
    (setf (box:box-interactive box) t)
    (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
      (ui:consume-press ui)
      (begin-drag key payload label))
    (values box (follow-drag key))))

(defun end-drag-source (box clicked)
  (ui:close-box ui:*ui*)
  (drag-ghost (box:box-key box))
  (and clicked :clicked))

(defmacro drag-source ((&key id payload label) &body body)
  "What BODY describes, one under another, which a press and a move of the pointer drags: PAYLOAD
is what is carried (to a DROP-TARGET or whatever takes it), and LABEL what the ghost under the
pointer says (PAYLOAD if not given). Returns :CLICKED on the frame it is pressed and let go of
without moving, as a button is, and otherwise NIL. The :id is required."
  (let ((box (gensym "BOX")) (clicked (gensym "CLICKED")))
    `(multiple-value-bind (,box ,clicked) (begin-drag-source ,id ,payload ,label)
       ,@body
       (end-drag-source ,box ,clicked))))

(defun begin-drop-target (id direction gap)
  (let ((box (ui:open-box ui:*ui* (if (eq direction :horizontal) :row :column)
                          :id (or id (widgets:required-id 'drop-target)))))
    (setf (box:box-gap box) (float gap 1f0))
    box))

(defun end-drop-target (box accepts)
  (let* ((ui ui:*ui*)
         (drag (ui:ui-drag ui))
         (rect (ui:rect-of ui (box:box-key box)))
         (x (ui:ui-pointer-x ui))
         (y (ui:ui-pointer-y ui)))
    (ui:close-box ui)
    (when (and drag (ui:drag-moving drag) rect
               (<= (ui:rect-x rect) x (+ (ui:rect-x rect) (ui:rect-w rect)))
               (<= (ui:rect-y rect) y (+ (ui:rect-y rect) (ui:rect-h rect)))
               (or (null accepts) (funcall accepts (ui:drag-payload drag))))
      (multiple-value-bind (index mx my mw mh)
          (insertion-place box (eq (box:box-kind box) :row) x y)
        (if (ui:drag-dropping drag)
            (values (take-drop) index)
            (progn (show-insertion (box:box-key box) mx my mw mh)
                   nil))))))

(defmacro drop-target ((&key id (direction :vertical) (gap 0) accepts) &body body)
  "What BODY describes, one under another (or side by side, with DIRECTION :HORIZONTAL), GAP
apart, onto which what is dragged can be dropped: while it is over it, a bar shows where among its
children it would go. Returns, on the frame it is dropped, what was dragged and the index it goes
at (the number of children before the place, of those that have an id), and otherwise NIL.
ACCEPTS, if given, is a function of what is dragged that says whether it can be dropped here. The
:id is required."
  (let ((box (gensym "BOX")))
    `(let ((,box (begin-drop-target ,id ,direction ,gap)))
       ,@body
       (end-drop-target ,box ,accepts))))
