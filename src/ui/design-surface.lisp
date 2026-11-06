(defpackage #:almighty-bolt/ui/design-surface
  (:use #:cl)
  (:local-nicknames (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:drag #:almighty-bolt/ui/drag)
                    (#:widgets #:almighty-bolt/ui/widgets))
  (:export #:design-surface
           #:mark-selection))

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
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :surface :id (or id (widgets:required-id 'design-surface))))
         (held (list (ui:ui-pressed ui) (ui:ui-released ui) (ui:ui-secondary-pressed ui) (ui:ui-keys ui)
                     (ui:ui-inputs ui) (ui:ui-wheel-dx ui) (ui:ui-wheel-dy ui))))
    (setf (box:box-interactive box) t
          ;; Every move over it wants a frame, for the outline that follows the pointer.
          (box:box-moves box) t
          (box:box-flex box) (float flex 1f0)
          (ui:ui-pressed ui) nil (ui:ui-released ui) nil (ui:ui-secondary-pressed ui) nil
          (ui:ui-keys ui) '() (ui:ui-inputs ui) '() (ui:ui-wheel-dx ui) 0f0 (ui:ui-wheel-dy ui) 0f0)
    (incf (ui:ui-inert ui))
    (values box (ui:ui-count ui) held)))

(defun put-input-back (held)
  "Give the frame back the input BEGIN-DESIGN-SURFACE put aside."
  (let ((ui ui:*ui*))
    (destructuring-bind (pressed released secondary keys inputs dx dy) held
      (setf (ui:ui-pressed ui) pressed (ui:ui-released ui) released (ui:ui-secondary-pressed ui) secondary
            (ui:ui-keys ui) keys (ui:ui-inputs ui) inputs (ui:ui-wheel-dx ui) dx (ui:ui-wheel-dy ui) dy))
    (setf (ui:ui-inert ui) (max 0 (1- (ui:ui-inert ui))))))

(defun inside-p (box surface)
  (loop :for parent = (box:box-parent box) :then (box:box-parent parent)
        :while parent
          :thereis (eq parent surface)))

(defun outline-over (ui surface id box color)
  "A frame of COLOR round BOX, called ID among SURFACE's outlines, laid out over BOX where it is in
this frame: a widget that has changed its size is outlined as it is now."
  (when box
    (multiple-value-bind (float open)
        (ui:open-float ui :outline (box:box-key surface) id
                       (ui:make-float-spec :anchor box :placement :over :tool t))
      (setf (box:box-color float) color)
      (ui:close-float ui open))))

(defun mark-selection (owner color tag &key anchor x y w h)
  "What a designer marks the selected widget with, as floats of the box OWNER: corner brackets of
COLOR round it, and over its top left, if TAG is given, a tag that says it (the widget, its id,
whether it is wired). Round the box ANCHOR, where it is in this frame, or the place X, Y, W, H in
the window."
  (let ((ui ui:*ui*))
    (multiple-value-bind (float open)
        (ui:open-float ui :outline owner :selected
                       (if anchor
                           (ui:make-float-spec :anchor anchor :placement :over :tool t)
                           (ui:make-float-spec :placement :at :x (float x 1f0) :y (float y 1f0)
                                               :tool t)))
      (unless anchor
        (setf (box:box-pref-w float) (float w 1f0)
              (box:box-pref-h float) (float h 1f0)))
      (setf (box:box-color float) color
            (box:box-n1 float) 1f0)
      (ui:close-float ui open))
    (when tag
      (multiple-value-bind (float open)
          (ui:open-float ui :panel owner :selection-tag
                         (if anchor
                             (ui:make-float-spec :anchor anchor :placement :above :tool t)
                             (ui:make-float-spec :placement :at :x (- (float x 1f0) 2f0)
                                                 :y (max 0f0 (- (float y 1f0) 24f0)) :tool t)))
        (setf (box:box-color float) color
              (box:box-n1 float) 3f0
              (box:box-pad-left float) 6f0 (box:box-pad-right float) 6f0
              (box:box-pad-top float) 2f0 (box:box-pad-bottom float) 2f0)
        (widgets:label tag :id :selection-tag-text :size 11
                           :color (theme:theme-background theme:*theme*))
        (ui:close-float ui open)))))

(defun thing-under (ui surface first x y)
  "The innermost box with an id inside SURFACE (its boxes are FIRST and after, this build) that was
under the point X, Y in the last frame, or NIL."
  (let ((boxes (ui:ui-boxes ui))
        (found nil))
    ;; In the order they were described, a box comes before what is in it, and boxes beside each
    ;; other do not overlap: the last that holds the point is the innermost.
    (loop :for index :from first :below (ui:ui-count ui)
          :for box = (svref boxes index)
          :do (when (and (box:box-id box) (inside-p box surface))
                (let ((rect (ui:rect-of ui (box:box-key box))))
                  (when (and rect
                             (<= (ui:rect-x rect) x) (< x (+ (ui:rect-x rect) (ui:rect-w rect)))
                             (<= (ui:rect-y rect) y) (< y (+ (ui:rect-y rect) (ui:rect-h rect))))
                    (setf found box)))))
    found))

(defun thing-with-id (ui surface first id)
  (let ((boxes (ui:ui-boxes ui)))
    (loop :for index :from first :below (ui:ui-count ui)
          :for box = (svref boxes index)
            :thereis (and (equal (box:box-id box) id) (inside-p box surface) box))))

(defparameter +containers+ '(:column :row :grid :stack :padding :panel :center :scroll :pressable)
  "The kinds of box that hold others, which a drop on a surface goes into.")

(defun container-under (ui surface first x y)
  "The innermost box with an id inside SURFACE that holds others and was under X, Y in the last
frame, or the surface's first box if none was (the view's outermost), or NIL."
  (let ((boxes (ui:ui-boxes ui))
        (found nil))
    (loop :for index :from first :below (ui:ui-count ui)
          :for box = (svref boxes index)
          :do (when (and (box:box-id box) (member (box:box-kind box) +containers+) (inside-p box surface))
                (let ((rect (ui:rect-of ui (box:box-key box))))
                  (when (and rect
                             (<= (ui:rect-x rect) x) (< x (+ (ui:rect-x rect) (ui:rect-w rect)))
                             (<= (ui:rect-y rect) y) (< y (+ (ui:rect-y rect) (ui:rect-h rect))))
                    (setf found box)))))
    (or found
        (let ((first-box (and (< first (ui:ui-count ui)) (svref boxes first))))
          (and first-box (box:box-id first-box) (member (box:box-kind first-box) +containers+)
               first-box)))))

(defun end-design-surface (surface first selected tag)
  "Close SURFACE, take a press on it, and outline what the pointer is over and what is SELECTED.
Returns the id of the thing the press was on, on the frame of the press, or NIL; and what was
dropped on it this frame, as a list of what was dragged, the id of the container it goes into and
the index among that container's children, or NIL. A press on a thing and a move drags it, as
(:move id)."
  (let* ((ui ui:*ui*)
         (key (box:box-key surface))
         (hot (= (ui:ui-hot ui) key))
         (pressed nil)
         (drop nil))
    (ui:close-box ui)
    (let ((under (and hot
                      (thing-under ui surface first (ui:ui-pointer-x ui) (ui:ui-pointer-y ui))))
          (chosen (and selected (thing-with-id ui surface first selected))))
      (when (and hot (ui:ui-pressed ui))
        (ui:consume-press ui)
        (setf pressed (and under (box:box-id under)))
        (when pressed
          (drag:begin-drag key (list :move pressed) (format nil "~(~s~)" pressed))))
      (drag:follow-drag key)
      ;; What is dragged over the surface goes into the container under the pointer, where it is.
      (when (and hot (drag:dragging))
        (let ((container (container-under ui surface first (ui:ui-pointer-x ui) (ui:ui-pointer-y ui))))
          (when container
            (multiple-value-bind (index mx my mw mh)
                (drag:insertion-place container (eq (box:box-kind container) :row)
                                      (ui:ui-pointer-x ui) (ui:ui-pointer-y ui))
              (let ((payload (drag:take-drop)))
                (if payload
                    (setf drop (list payload (box:box-id container) index))
                    (drag:show-insertion key mx my mw mh)))))))
      (drag:drag-ghost key)
      ;; Where the pointer is over is found in the last frame's places, which is where the person
      ;; saw things; the outlines are put where things are now.
      (outline-over ui surface :hover (and under (not (eq under chosen)) under)
                    (theme:theme-dim-text theme:*theme*))
      (when chosen
        (mark-selection (box:box-key surface) (theme:theme-ring theme:*theme*) tag
                        :anchor chosen)))
    (values pressed drop)))

(defmacro design-surface ((&key id selected tag (flex 0)) &body body)
  "What BODY describes, one under another, drawn as it looks but with none of it working: the
pointer, a press and the keyboard do not reach it. A press on it returns the id of the innermost
thing with an id that the press was on, on the frame of the press (and NIL otherwise, or if the press
was on none). The thing whose id is SELECTED is marked, with corner brackets and, if TAG is given,
a tag over it that says TAG; the thing the pointer is over is outlined.
For a designer, which draws the view being designed in one. The :id is required."
  (let ((box (gensym "SURFACE")) (first (gensym "FIRST")) (held (gensym "HELD")))
    `(multiple-value-bind (,box ,first ,held) (begin-design-surface ,id ,flex)
       (unwind-protect (progn ,@body)
         (put-input-back ,held))
       (end-design-surface ,box ,first ,selected ,tag))))
