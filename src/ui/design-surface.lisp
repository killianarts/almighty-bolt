(defpackage #:almighty-bolt/ui/design-surface
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
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
  (:import-from #:almighty-bolt/ui/widgets
                #:required-id)
  (:export #:design-surface))

(in-package #:almighty-bolt/ui/design-surface)

;;; A design surface shows widgets as they look, and none of them works: it is where a designer
;;; draws the view being designed. What is described in it is laid out and drawn as anywhere else,
;;; one under another, but the pointer finds only the surface: none of what is in it is among the
;;; frame's hits (see REMEMBER-GEOMETRY), so none of it is ever what the pointer is over, and nothing
;;; in it is hovered, pressed, given the keyboard (by a press or by Tab) or given a cursor. The surface takes the press
;;; itself and says which thing in it, by its id, the press was on; it outlines the thing with an id
;;; that the pointer is over, and the one that is SELECTED, as the inspector outlines what it picked.

(defun begin-design-surface (id flex)
  "Open the surface's box. Returns it and the index of the first box inside it."
  (let* ((ui *ui*)
         (box (open-box ui :surface :id (or id (required-id 'design-surface)))))
    (setf (box-interactive box) t
          ;; Every move over it wants a frame, for the outline that follows the pointer.
          (box-moves box) t
          (box-flex box) (float flex 1f0))
    (values box (ui-count ui))))

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

(defun end-design-surface (surface first selected)
  "Close SURFACE, take a press on it, and outline what the pointer is over and what is SELECTED.
Returns the id of the thing the press was on, on the frame of the press, or NIL."
  (let* ((ui *ui*)
         (hot (= (ui-hot ui) (box-key surface)))
         (pressed nil))
    (close-box ui)
    (let ((under (and hot
                      (thing-under ui surface first (ui-pointer-x ui) (ui-pointer-y ui))))
          (chosen (and selected (thing-with-id ui surface first selected))))
      (when (and hot (ui-pressed ui))
        (consume-press ui)
        (setf pressed (and under (box-id under))))
      ;; Where the pointer is over is found in the last frame's places, which is where the person
      ;; saw things; the outlines are put where things are now.
      (outline-over ui surface :hover (and under (not (eq under chosen)) under)
                    (theme-dim-text *theme*))
      (outline-over ui surface :selected chosen (theme-ring *theme*)))
    pressed))

(defmacro design-surface ((&key id selected (flex 0)) &body body)
  "What BODY describes, one under another, drawn as it looks but with none of it working: the
pointer, a press and the keyboard do not reach it. A press on it returns the id of the innermost
thing with an id that the press was on, on the frame of the press (and NIL otherwise, or if the press
was on none). The thing whose id is SELECTED is outlined, and so is the thing the pointer is over.
For a designer, which draws the view being designed in one. The :id is required."
  (let ((box (gensym "SURFACE")) (first (gensym "FIRST")))
    `(multiple-value-bind (,box ,first) (begin-design-surface ,id ,flex)
       ,@body
       (end-design-surface ,box ,first ,selected))))
