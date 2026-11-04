(defpackage #:almighty-bolt/ui/split
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:key-event-logical-key
                #:+key-left+
                #:+key-right+
                #:+key-up+
                #:+key-down+)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-interactive
                #:box-focusable
                #:box-pref-w
                #:box-pref-h
                #:box-cross-align
                #:box-fill
                #:box-flex
                #:box-look
                #:box-n1
                #:box-n3
                #:box-min-w
                #:box-max-w
                #:box-min-h
                #:box-max-h)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:open-box
                #:close-box
                #:state-of
                #:rect-of
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h
                #:ui-hot
                #:ui-active
                #:ui-focus
                #:ui-pressed
                #:ui-released
                #:ui-down
                #:ui-pointer-x
                #:ui-pointer-y
                #:ui-keys
                #:consume-press
                #:consume-release
                #:consume-key
                #:focus-by-click
                #:want-cursor)
  (:import-from #:almighty-bolt/ui/widgets
                #:required-id
                #:look-of
                #:set-size-limits)
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
  (let ((pane (open-box ui :pane :id id)))
    (setf (box-cross-align pane) :stretch)
    pane))

(defun begin-split (id direction size flex width height)
  "Open the split and its first pane. Returns the split, its state and the first pane."
  (let* ((ui *ui*)
         (box (open-box ui :split :id (or id (required-id 'split))))
         (state (state-of ui (box-key box) #'make-split-state)))
    (set-size-limits box width height nil nil nil nil)
    (setf (box-cross-align box) :stretch
          (box-fill box) t
          (box-n3 box) (if (eq direction :vertical) 1 0)
          (box-flex box) (float flex 1f0))
    ;; The size the split was first given, and a new one it is given after, wins over the one it
    ;; keeps; the same one given again does not, so a split given a constant is moved by its
    ;; divider, and one given back what it returned is the app's.
    (let ((size (float size 1f0)))
      (unless (eql size (split-state-given state))
        (setf (split-state-size state) size
              (split-state-given state) size)))
    (values box state (open-pane ui :first))))

(defun divide (box state pane direction least most)
  "Close the first pane PANE, draw the divider and take what moved it, give PANE its size, and open
the second pane."
  (let* ((ui *ui*)
         (horizontal (not (eq direction :vertical)))
         (outer (rect-of ui (box-key box)))
         (size (split-state-size state))
         (least (float least 1f0)))
    (close-box ui)
    (let* ((divider (open-box ui :splitter :id :divider))
           (key (box-key divider)))
      (flet ((along (x y) (if horizontal x y)))
        (setf (box-interactive divider) t
              (box-focusable divider) t)
        (if horizontal
            (setf (box-pref-w divider) +divider-width+)
            (setf (box-pref-h divider) +divider-width+))
        (when (and (= (ui-hot ui) key) (ui-pressed ui))
          (consume-press ui)
          (focus-by-click ui key)
          (setf (ui-active ui) key)
          (let ((rect (rect-of ui key)))
            (setf (split-state-grab state)
                  (if rect
                      (- (along (ui-pointer-x ui) (ui-pointer-y ui)) (along (rect-x rect) (rect-y rect)))
                      0f0))))
        (when (= (ui-active ui) key)
          (when (and (ui-down ui) outer)
            (setf size (- (along (ui-pointer-x ui) (ui-pointer-y ui))
                          (along (rect-x outer) (rect-y outer))
                          (split-state-grab state))))
          (when (ui-released ui)
            (consume-release ui)
            (setf (ui-active ui) 0)))
        (when (= (ui-focus ui) key)
          (dolist (event (copy-list (ui-keys ui)))
            (let* ((k (key-event-logical-key event))
                   (step (cond ((eql k (if horizontal +key-left+ +key-up+)) (- +divider-step+))
                               ((eql k (if horizontal +key-right+ +key-down+)) +divider-step+))))
              (when step
                (consume-key ui event)
                (incf size step)))))
        (want-cursor ui key (if horizontal :resize-left-right :resize-up-down))
        ;; No less than LEAST, no more than MOST, and leaving the second pane LEAST too.
        (let ((room (and outer (- (along (rect-w outer) (rect-h outer)) +divider-width+ least))))
          (when most (setf size (min size (float most 1f0))))
          (when room (setf size (min size room)))
          (setf size (max least size)))
        (setf (split-state-size state) size
              (box-look divider) (look-of ui key t)
              (box-n1 divider) size
              (box-n3 divider) (if horizontal 0 1))
        (if horizontal
            (setf (box-min-w pane) size (box-max-w pane) size)
            (setf (box-min-h pane) size (box-max-h pane) size))
        (close-box ui)))
    (let ((second (open-pane ui :second)))
      (setf (box-flex second) 1f0))
    size))

(defun end-split (size)
  (let ((ui *ui*))
    (close-box ui)
    (close-box ui)
    size))

(defmacro split ((&key id (direction :horizontal) (size 240) (min 80) max (flex 0) width height)
                 first second)
  "Two panes, FIRST and SECOND, side by side (or with DIRECTION :VERTICAL one over the other), with a
divider between them that the pointer drags and the arrow keys move. The first pane starts SIZE
points wide (or tall), and is moved by the divider from there; a SIZE other than the one given
the frame before is taken up, so an app that keeps the size gives back what is returned. It is
kept no smaller than MIN, no larger than MAX, and leaving the second at
least MIN; the second has the rest. Each pane is a column, :FIRST and :SECOND by id, whose children are
as wide as it is; what does not fit is cut at its edge. Returns how wide (or tall) the first pane is.
FLEX, WIDTH and HEIGHT are the container's own. The :id is required: the size is kept by it."
  (let ((box (gensym "BOX")) (state (gensym "STATE")) (pane (gensym "PANE")) (now (gensym "SIZE")))
    `(multiple-value-bind (,box ,state ,pane) (begin-split ,id ,direction ,size ,flex ,width ,height)
       ,first
       (let ((,now (divide ,box ,state ,pane ,direction ,min ,max)))
         ,second
         (end-split ,now)))))
