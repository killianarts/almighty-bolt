(defpackage #:almighty-bolt/ui/drag
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-kind
                #:box-children
                #:box-interactive
                #:box-gap
                #:box-color
                #:box-n1
                #:box-pref-w
                #:box-pref-h
                #:box-pad-left
                #:box-pad-top
                #:box-pad-right
                #:box-pad-bottom)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:open-box
                #:close-box
                #:open-float
                #:close-float
                #:make-float-spec
                #:ui-hot
                #:ui-active
                #:ui-pressed
                #:ui-released
                #:ui-pointer-x
                #:ui-pointer-y
                #:ui-consumed
                #:ui-drag
                #:make-drag
                #:drag-origin
                #:drag-payload
                #:drag-label
                #:drag-x
                #:drag-y
                #:drag-moving
                #:drag-dropping
                #:consume-press
                #:consume-release
                #:rect-of
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:theme-ring
                #:theme-button
                #:theme-button-text)
  (:import-from #:almighty-bolt/ui/widgets
                #:required-id
                #:label)
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
  (let ((drag (ui-drag *ui*)))
    (and drag (drag-moving drag) (drag-payload drag))))

(defun begin-drag (key payload label)
  "A press on the box KEY may begin a drag of PAYLOAD, which the ghost calls LABEL."
  (let ((ui *ui*))
    (setf (ui-active ui) key
          (ui-drag ui) (make-drag key payload label (ui-pointer-x ui) (ui-pointer-y ui)))))

(defun follow-drag (key)
  "For the box KEY that a drag may come from: start it moving once the pointer has gone far enough,
and end it if the press is let go of in place. True if it was let go of in place over KEY (a
click)."
  (let* ((ui *ui*)
         (drag (ui-drag ui)))
    (when (and drag (eql (drag-origin drag) key) (= (ui-active ui) key))
      (when (and (not (drag-moving drag))
                 (> (+ (abs (- (ui-pointer-x ui) (drag-x drag)))
                       (abs (- (ui-pointer-y ui) (drag-y drag))))
                    +drag-distance+))
        (setf (drag-moving drag) t))
      (when (ui-released ui)
        (consume-release ui)
        (setf (ui-active ui) 0
              (ui-drag ui) nil)
        (= (ui-hot ui) key)))))

(defun drag-ghost (owner)
  "The ghost under the pointer that says what is dragged from the box OWNER, while it moves."
  (let* ((ui *ui*)
         (drag (ui-drag ui)))
    (when (and drag (drag-moving drag) (eql (drag-origin drag) owner) (not (drag-dropping drag)))
      (multiple-value-bind (float open)
          (open-float ui :panel owner :drag-ghost
                      (make-float-spec :placement :at :x (+ (ui-pointer-x ui) 14f0)
                                       :y (+ (ui-pointer-y ui) 10f0) :tool t))
        (setf (box-color float) (theme-button *theme*)
              (box-n1 float) 4f0
              (box-pad-left float) 8f0 (box-pad-right float) 8f0
              (box-pad-top float) 4f0 (box-pad-bottom float) 4f0)
        (label (princ-to-string (or (drag-label drag) (drag-payload drag))) :id :label :size 13
               :color (theme-button-text *theme*))
        (close-float ui open)))))

(defun insertion-place (container horizontal x y)
  "Where among CONTAINER's children (a box of this build) a drop at X, Y goes: its index, and where
the marker of it is in the window (x, y, width, height). Children whose places are not known (no id,
nothing to press) do not count, and HORIZONTAL is for a row."
  (let* ((ui *ui*)
         (outer (rect-of ui (box-key container)))
         (children (box-children container))
         (index 0)
         (edge nil))
    (loop :for child :across children
          :for i :from 0
          :do (let ((rect (rect-of ui (box-key child))))
                (when rect
                  (let ((start (if horizontal (rect-x rect) (rect-y rect)))
                        (end (if horizontal
                                 (+ (rect-x rect) (rect-w rect))
                                 (+ (rect-y rect) (rect-h rect)))))
                    (if (< (if horizontal x y) (/ (+ start end) 2))
                        (progn (unless edge (setf edge start))
                               (return))
                        (setf index (1+ i)
                              edge end))))))
    ;; With nothing before the place, the marker is at the start of the container.
    (let ((edge (or edge (and outer (if horizontal (rect-x outer) (rect-y outer))) 0f0)))
      (if (null outer)
          (values index 0f0 0f0 0f0 0f0)
          (if horizontal
              (values index (- edge 1f0) (rect-y outer) 2f0 (rect-h outer))
              (values index (rect-x outer) (- edge 1f0) (rect-w outer) 2f0))))))

(defun show-insertion (owner x y w h)
  "The marker of where a drop goes: a bar of the theme's ring colour at X, Y, W by H in the window."
  (let ((ui *ui*))
    (multiple-value-bind (float open)
        (open-float ui :outline owner :insertion
                    (make-float-spec :placement :at :x (float x 1f0) :y (float y 1f0) :tool t))
      (setf (box-pref-w float) (float w 1f0)
            (box-pref-h float) (float h 1f0)
            (box-color float) (theme-ring *theme*))
      (close-float ui open))))

(defun take-drop ()
  "Take what is being dropped this frame: its payload, or NIL if nothing is."
  (let* ((ui *ui*)
         (drag (ui-drag ui)))
    (when (and drag (drag-dropping drag))
      (setf (ui-drag ui) nil
            (ui-consumed ui) t)
      (drag-payload drag))))

(defun begin-drag-source (id payload label)
  (let* ((ui *ui*)
         (box (open-box ui :drag-source :id (or id (required-id 'drag-source))))
         (key (box-key box)))
    (setf (box-interactive box) t)
    (when (and (= (ui-hot ui) key) (ui-pressed ui))
      (consume-press ui)
      (begin-drag key payload label))
    (values box (follow-drag key))))

(defun end-drag-source (box clicked)
  (close-box *ui*)
  (drag-ghost (box-key box))
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
  (let ((box (open-box *ui* (if (eq direction :horizontal) :row :column)
                       :id (or id (required-id 'drop-target)))))
    (setf (box-gap box) (float gap 1f0))
    box))

(defun end-drop-target (box accepts)
  (let* ((ui *ui*)
         (drag (ui-drag ui))
         (rect (rect-of ui (box-key box)))
         (x (ui-pointer-x ui))
         (y (ui-pointer-y ui)))
    (close-box ui)
    (when (and drag (drag-moving drag) rect
               (<= (rect-x rect) x (+ (rect-x rect) (rect-w rect)))
               (<= (rect-y rect) y (+ (rect-y rect) (rect-h rect)))
               (or (null accepts) (funcall accepts (drag-payload drag))))
      (multiple-value-bind (index mx my mw mh)
          (insertion-place box (eq (box-kind box) :row) x y)
        (if (drag-dropping drag)
            (values (take-drop) index)
            (progn (show-insertion (box-key box) mx my mw mh)
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
