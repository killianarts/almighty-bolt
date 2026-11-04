(defpackage #:almighty-bolt/ui/design-surface
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
                #:box-kind
                #:box-parent
                #:box-flex
                #:box-color
                #:box-interactive
                #:box-moves)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:open-box
                #:close-box
                #:ui-boxes
                #:ui-count
                #:ui-hot
                #:ui-pressed
                #:ui-pointer-x
                #:ui-pointer-y
                #:ui-released
                #:ui-secondary-pressed
                #:ui-keys
                #:ui-inputs
                #:ui-wheel-dx
                #:ui-wheel-dy
                #:ui-inert
                #:consume-press
                #:open-float
                #:close-float
                #:make-float-spec
                #:rect-of
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:theme-ring
                #:theme-dim-text)
  (:import-from #:almighty-bolt/ui/drag
                #:begin-drag
                #:follow-drag
                #:drag-ghost
                #:insertion-place
                #:show-insertion
                #:take-drop
                #:dragging)
  (:import-from #:almighty-bolt/ui/widgets
                #:required-id)
  (:export #:design-surface))

(in-package #:almighty-bolt/ui/design-surface)

;;; A design surface shows widgets as they look, and none of them works: it is where a designer
;;; draws the view being designed. What is described in it is laid out and drawn as anywhere else,
;;; one under another, but the pointer finds only the surface: none of what is in it is among the
;;; frame's hits (see REMEMBER-GEOMETRY), so none of it is ever what the pointer is over, and nothing
;;; in it is hovered, pressed, given the keyboard (by a press or by Tab) or given a cursor. And
;;; while it is described, the frame's input is put aside (and put back after), so that what takes
;;; input whatever has the keyboard (a shortcut, a dialog's Escape, a press outside a popover) takes
;;; none there; a float opened in it (a menu, a dialog) is inert as the rest is, and a canvas in it
;;; that asks for the keyboard (:autofocus) is not given it. The surface takes the press
;;; itself and says which thing in it, by its id, the press was on; it outlines the thing with an id
;;; that the pointer is over, and the one that is SELECTED, as the inspector outlines what it picked.

(defun begin-design-surface (id flex)
  "Open the surface's box, and put the frame's input aside. Returns the box, the index of the first
box inside it, and the input put aside, for PUT-INPUT-BACK."
  (let* ((ui *ui*)
         (box (open-box ui :surface :id (or id (required-id 'design-surface))))
         (held (list (ui-pressed ui) (ui-released ui) (ui-secondary-pressed ui) (ui-keys ui)
                     (ui-inputs ui) (ui-wheel-dx ui) (ui-wheel-dy ui))))
    (setf (box-interactive box) t
          ;; Every move over it wants a frame, for the outline that follows the pointer.
          (box-moves box) t
          (box-flex box) (float flex 1f0)
          (ui-pressed ui) nil (ui-released ui) nil (ui-secondary-pressed ui) nil
          (ui-keys ui) '() (ui-inputs ui) '() (ui-wheel-dx ui) 0f0 (ui-wheel-dy ui) 0f0)
    (incf (ui-inert ui))
    (values box (ui-count ui) held)))

(defun put-input-back (held)
  "Give the frame back the input BEGIN-DESIGN-SURFACE put aside."
  (let ((ui *ui*))
    (destructuring-bind (pressed released secondary keys inputs dx dy) held
      (setf (ui-pressed ui) pressed (ui-released ui) released (ui-secondary-pressed ui) secondary
            (ui-keys ui) keys (ui-inputs ui) inputs (ui-wheel-dx ui) dx (ui-wheel-dy ui) dy))
    (setf (ui-inert ui) (max 0 (1- (ui-inert ui))))))

(defun inside-p (box surface)
  (loop :for parent = (box-parent box) :then (box-parent parent)
        :while parent
          :thereis (eq parent surface)))

(defun outline-over (ui surface id box color)
  "A frame of COLOR round BOX, called ID among SURFACE's outlines, laid out over BOX where it is in
this frame: a widget that has changed its size is outlined as it is now."
  (when box
    (multiple-value-bind (float open)
        (open-float ui :outline (box-key surface) id
                    (make-float-spec :anchor box :placement :over :tool t))
      (setf (box-color float) color)
      (close-float ui open))))

(defun thing-under (ui surface first x y)
  "The innermost box with an id inside SURFACE (its boxes are FIRST and after, this build) that was
under the point X, Y in the last frame, or NIL."
  (let ((boxes (ui-boxes ui))
        (found nil))
    ;; In the order they were described, a box comes before what is in it, and boxes beside each
    ;; other do not overlap: the last that holds the point is the innermost.
    (loop :for index :from first :below (ui-count ui)
          :for box = (svref boxes index)
          :do (when (and (box-id box) (inside-p box surface))
                (let ((rect (rect-of ui (box-key box))))
                  (when (and rect
                             (<= (rect-x rect) x) (< x (+ (rect-x rect) (rect-w rect)))
                             (<= (rect-y rect) y) (< y (+ (rect-y rect) (rect-h rect))))
                    (setf found box)))))
    found))

(defun thing-with-id (ui surface first id)
  (let ((boxes (ui-boxes ui)))
    (loop :for index :from first :below (ui-count ui)
          :for box = (svref boxes index)
            :thereis (and (equal (box-id box) id) (inside-p box surface) box))))

(defparameter +containers+ '(:column :row :grid :stack :padding :panel :center :scroll :pressable)
  "The kinds of box that hold others, which a drop on a surface goes into.")

(defun container-under (ui surface first x y)
  "The innermost box with an id inside SURFACE that holds others and was under X, Y in the last
frame, or the surface's first box if none was (the view's outermost), or NIL."
  (let ((boxes (ui-boxes ui))
        (found nil))
    (loop :for index :from first :below (ui-count ui)
          :for box = (svref boxes index)
          :do (when (and (box-id box) (member (box-kind box) +containers+) (inside-p box surface))
                (let ((rect (rect-of ui (box-key box))))
                  (when (and rect
                             (<= (rect-x rect) x) (< x (+ (rect-x rect) (rect-w rect)))
                             (<= (rect-y rect) y) (< y (+ (rect-y rect) (rect-h rect))))
                    (setf found box)))))
    (or found
        (let ((first-box (and (< first (ui-count ui)) (svref boxes first))))
          (and first-box (box-id first-box) (member (box-kind first-box) +containers+)
               first-box)))))

(defun end-design-surface (surface first selected)
  "Close SURFACE, take a press on it, and outline what the pointer is over and what is SELECTED.
Returns the id of the thing the press was on, on the frame of the press, or NIL; and what was
dropped on it this frame, as a list of what was dragged, the id of the container it goes into and
the index among that container's children, or NIL. A press on a thing and a move drags it, as
(:move id)."
  (let* ((ui *ui*)
         (key (box-key surface))
         (hot (= (ui-hot ui) key))
         (pressed nil)
         (drop nil))
    (close-box ui)
    (let ((under (and hot
                      (thing-under ui surface first (ui-pointer-x ui) (ui-pointer-y ui))))
          (chosen (and selected (thing-with-id ui surface first selected))))
      (when (and hot (ui-pressed ui))
        (consume-press ui)
        (setf pressed (and under (box-id under)))
        (when pressed
          (begin-drag key (list :move pressed) (format nil "~(~s~)" pressed))))
      (follow-drag key)
      ;; What is dragged over the surface goes into the container under the pointer, where it is.
      (when (and hot (dragging))
        (let ((container (container-under ui surface first (ui-pointer-x ui) (ui-pointer-y ui))))
          (when container
            (multiple-value-bind (index mx my mw mh)
                (insertion-place container (eq (box-kind container) :row)
                                 (ui-pointer-x ui) (ui-pointer-y ui))
              (let ((payload (take-drop)))
                (if payload
                    (setf drop (list payload (box-id container) index))
                    (show-insertion key mx my mw mh)))))))
      (drag-ghost key)
      ;; Where the pointer is over is found in the last frame's places, which is where the person
      ;; saw things; the outlines are put where things are now.
      (outline-over ui surface :hover (and under (not (eq under chosen)) under)
                    (theme-dim-text *theme*))
      (outline-over ui surface :selected chosen (theme-ring *theme*)))
    (values pressed drop)))

(defmacro design-surface ((&key id selected (flex 0)) &body body)
  "What BODY describes, one under another, drawn as it looks but with none of it working: the
pointer, a press and the keyboard do not reach it. A press on it returns the id of the innermost
thing with an id that the press was on, on the frame of the press (and NIL otherwise, or if the press
was on none). The thing whose id is SELECTED is outlined, and so is the thing the pointer is over.
For a designer, which draws the view being designed in one. The :id is required."
  (let ((box (gensym "SURFACE")) (first (gensym "FIRST")) (held (gensym "HELD")))
    `(multiple-value-bind (,box ,first ,held) (begin-design-surface ,id ,flex)
       (unwind-protect (progn ,@body)
         (put-input-back ,held))
       (end-design-surface ,box ,first ,selected))))
