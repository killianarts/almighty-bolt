(defpackage #:almighty-bolt/ui/split
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:widgets #:almighty-bolt/ui/widgets))
  (:export #:split
           #:+divider-width+))

(in-package #:almighty-bolt/ui/split)

;;; Two panes side by side (or one over the other) with a divider between them that the pointer
;;; drags, and the keyboard moves once Tab has brought it there. The first pane is as wide as the
;;; split keeps for it, by its id, from one frame to the next; the second has the rest. Each pane is a
;;; column whose children are as wide as it is, and what does not fit in a pane is cut at its edge,
;;; as a scroll container cuts what is inside it, and is not pressed where it is not seen.

(defconstant +divider-width+ 6f0
  "How wide (or, between panes one over the other, how tall) the divider is: the line drawn in its
middle is thin, and the rest is there to be pressed.")

(defconstant +divider-step+ 16f0
  "How far an arrow key moves the divider.")

(defstruct (split-state (:constructor make-split-state ()))
  ;; How wide (or tall) the first pane is: NIL until the split is first described.
  (size nil :type (or null single-float))
  ;; Where in the divider it was pressed, so that it does not jump under the pointer.
  (grab 0f0 :type single-float)
  ;; The :size it was given last: one that is not this is taken up, as a size the app keeps.
  (given nil :type (or null single-float)))

(defun open-pane (ui id)
  (let ((pane (ui:open-box ui :pane :id id)))
    (setf (box:box-cross-align pane) :stretch)
    pane))

(defun begin-split (id direction size flex width height &optional least)
  "Open the split and its first pane. Returns the split, its state and the first pane."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :split :id (or id (widgets:required-id 'split))))
         (state (ui:state-of ui (box:box-key box) #'make-split-state)))
    (widgets:set-size-limits box width height nil nil nil nil)
    (setf (box:box-cross-align box) :stretch
          (box:box-fill box) t
          (box:box-n3 box) (if (eq direction :vertical) 1 0)
          ;; How small a pane may be made, which the layout keeps the pane that flexes.
          (box:box-n1 box) (float (or least 0) 1f0)
          (box:box-flex box) (float flex 1f0))
    ;; The size the split was first given, and a new one it is given after, wins over the one it
    ;; keeps; the same one given again does not, so a split given a constant is moved by its
    ;; divider, and one given back what it returned is the app's.
    (let ((size (float size 1f0)))
      (unless (eql size (split-state-given state))
        (setf (split-state-size state) size
              (split-state-given state) size)))
    (values box state (open-pane ui :first))))

(defun divide (box state pane direction least most keep)
  "Close the first pane PANE, draw the divider and take what moved it, give the pane KEEP says
(:first or :second) its size, and open the second pane. The size is the one the person chose; the
layout shows the pane smaller while there is not room for it (see LAYOUT-SPLIT), and it is as it was
when there is again."
  (let* ((ui ui:*ui*)
         (horizontal (not (eq direction :vertical)))
         (outer (ui:rect-of ui (box:box-key box)))
         (size (split-state-size state))
         (least (float least 1f0))
         (second-kept (eq keep :second))
         (moved nil))
    (ui:close-box ui)
    (let* ((divider (ui:open-box ui :splitter :id :divider))
           (key (box:box-key divider)))
      (flet ((along (x y) (if horizontal x y)))
        (setf (box:box-interactive divider) t
              (box:box-focusable divider) t)
        (if horizontal
            (setf (box:box-pref-w divider) +divider-width+)
            (setf (box:box-pref-h divider) +divider-width+))
        (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
          (ui:consume-press ui)
          (ui:focus-by-click ui key)
          (setf (ui:ui-active ui) key)
          (let ((rect (ui:rect-of ui key)))
            (setf (split-state-grab state)
                  (if rect
                      (- (along (ui:ui-pointer-x ui) (ui:ui-pointer-y ui)) (along (ui:rect-x rect) (ui:rect-y rect)))
                      0f0))))
        (when (= (ui:ui-active ui) key)
          (when (and (ui:ui-down ui) outer)
            (let ((first (- (along (ui:ui-pointer-x ui) (ui:ui-pointer-y ui))
                            (along (ui:rect-x outer) (ui:rect-y outer))
                            (split-state-grab state))))
              (setf moved t
                    size (if second-kept
                             (- (along (ui:rect-w outer) (ui:rect-h outer)) +divider-width+ first)
                             first))))
          (when (ui:ui-released ui)
            (ui:consume-release ui)
            (setf (ui:ui-active ui) 0)))
        (when (= (ui:ui-focus ui) key)
          (dolist (event (copy-list (ui:ui-keys ui)))
            (let* ((k (core:key-event-logical-key event))
                   (step (cond ((eql k (if horizontal core:+key-left+ core:+key-up+)) (- +divider-step+))
                               ((eql k (if horizontal core:+key-right+ core:+key-down+)) +divider-step+))))
              (when step
                (ui:consume-key ui event)
                (setf moved t)
                (if second-kept (decf size step) (incf size step))))))
        (ui:want-cursor ui key (if horizontal :resize-left-right :resize-up-down))
        ;; No less than LEAST, no more than MOST, and leaving the second pane LEAST too.
        ;; Within the person's limits. Moved by the person, within the room there is: they cannot
        ;; choose more than they see. Made smaller by the window, the size is kept, and the layout
        ;; shows the pane smaller until there is room again.
        (when most (setf size (min size (float most 1f0))))
        (when (and moved outer)
          (setf size (min size (- (along (ui:rect-w outer) (ui:rect-h outer)) +divider-width+ least))))
        (setf size (max least size))
        (setf (split-state-size state) size
              (box:box-look divider) (widgets:look-of ui key t)
              (box:box-n1 divider) size
              (box:box-n3 divider) (if horizontal 0 1))
        (unless second-kept
          (if horizontal
              (setf (box:box-min-w pane) size (box:box-max-w pane) size)
              (setf (box:box-min-h pane) size (box:box-max-h pane) size)))
        (ui:close-box ui)))
    (let ((second (open-pane ui :second)))
      (if second-kept
          (progn
            (setf (box:box-flex pane) 1f0)
            (if horizontal
                (setf (box:box-min-w second) size (box:box-max-w second) size)
                (setf (box:box-min-h second) size (box:box-max-h second) size)))
          (setf (box:box-flex second) 1f0)))
    size))

(defun end-split (size)
  (let ((ui ui:*ui*))
    (ui:close-box ui)
    (ui:close-box ui)
    size))

(defmacro split ((&key id (direction :horizontal) (size 240) (min 80) max (keep :first) (flex 0)
                   width height)
                 first second)
  "Two panes, FIRST and SECOND, side by side (or with DIRECTION :VERTICAL one over the other), with a
divider between them that the pointer drags and the arrow keys move. The first pane starts SIZE
points wide (or tall), and is moved by the divider from there; a SIZE other than the one given
the frame before is taken up, so an app that keeps the size gives back what is returned. It is
kept no smaller than MIN and no larger than MAX, and the other pane has the rest. With KEEP :SECOND,
SIZE is the second pane's, which keeps it when the split is made larger or smaller (a sidebar on the
right), and the first has the rest. While there is not room for the pane its size, it is shown
smaller, leaving the other MIN, and it is its size again when there is room; the size returned is
the one chosen. Each pane is a column, :FIRST and :SECOND by id, whose children are
as wide as it is; what does not fit is cut at its edge. Returns how wide (or tall) the first pane is.
FLEX, WIDTH and HEIGHT are the container's own. The :id is required: the size is kept by it."
  (let ((box (gensym "BOX")) (state (gensym "STATE")) (pane (gensym "PANE")) (now (gensym "SIZE")))
    `(multiple-value-bind (,box ,state ,pane)
         (begin-split ,id ,direction ,size ,flex ,width ,height ,min)
       ,first
       (let ((,now (divide ,box ,state ,pane ,direction ,min ,max ,keep)))
         ,second
         (end-split ,now)))))
