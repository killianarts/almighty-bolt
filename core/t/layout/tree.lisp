(defpackage #:almighty-bolt-core/t/layout/tree
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:box #:almighty-bolt-core/layout/box)
                    (#:flex #:almighty-bolt-core/layout/flex)
                    (#:c #:almighty-bolt-core/layout/constraints)))

(in-package #:almighty-bolt-core/t/layout/tree)

;;; A small retained tree laid out with the layout functions, the way a retained
;;; variant will use them: every node keeps its children, its size and its place in
;;; its parent, and the closures the layout functions are given are made at each call
;;; and kept on the stack. It is both the example of how the functions are called and
;;; the check that calling them this way allocates nothing.

(defstruct (node (:constructor make-node (kind &key (children #()) (want-w 0f0) (want-h 0f0)
                                                 (padding 0f0) (gap 0f0) (flex 0f0)
                                                 (cross :center) (fill nil))))
  (kind :leaf :type (member :leaf :pad :row :column))
  (children #() :type simple-vector)
  (want-w 0f0 :type single-float)
  (want-h 0f0 :type single-float)
  (padding 0f0 :type single-float)
  (gap 0f0 :type single-float)
  (flex 0f0 :type single-float)
  (cross :center :type keyword)
  (fill nil)
  ;; The result of layout: the node's size, and its place in its parent.
  (w 0f0 :type single-float)
  (h 0f0 :type single-float)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float))

(defun leaf (w h &key (flex 0f0)) (make-node :leaf :want-w w :want-h h :flex flex))
(defun pad (padding child) (make-node :pad :children (vector child) :padding padding))
(defun row (gap &rest children)
  (make-node :row :children (coerce children 'simple-vector) :gap gap))
(defun column (gap &rest children)
  (make-node :column :children (coerce children 'simple-vector) :gap gap))

(defun layout-node (node min-w max-w min-h max-h)
  "Lay out NODE and everything under it within the constraints, and return its size."
  (declare (type node node) (type single-float min-w max-w min-h max-h))
  (multiple-value-bind (w h)
      (ecase (node-kind node)
        (:leaf (c:constrain-size (node-want-w node) (node-want-h node)
                                 min-w max-w min-h max-h))
        (:pad (let ((child (svref (node-children node) 0))
                    (padding (node-padding node)))
                (flet ((measure (a b c d) (layout-node child a b c d)))
                  (declare (dynamic-extent #'measure))
                  (multiple-value-bind (w h x y)
                      (box:layout-padding min-w max-w min-h max-h #'measure
                                          :left padding :top padding
                                          :right padding :bottom padding)
                    (setf (node-x child) x
                          (node-y child) y)
                    (values w h)))))
        ((:row :column)
         (let ((children (node-children node)))
           (flet ((measure (i a b c d) (layout-node (svref children i) a b c d))
                  (place (i x y)
                    (let ((child (svref children i)))
                      (setf (node-x child) x
                            (node-y child) y)))
                  (flex-of (i) (values (node-flex (svref children i)) t)))
             (declare (dynamic-extent #'measure #'place #'flex-of))
             (flex:layout-flex (if (eq (node-kind node) :row) :horizontal :vertical)
                               min-w max-w min-h max-h (length children)
                               #'measure #'place
                               :flex-of #'flex-of
                               :gap (node-gap node)
                               :cross-align (node-cross node)
                               :main-size (if (node-fill node) :max :min))))))
    (setf (node-w node) w
          (node-h node) h)
    (values w h)))

(defun place-of (node)
  (list (node-x node) (node-y node)))

;;; --- a counter: a label and a button, in a row, with space around ---

(defun counter ()
  (let* ((label (leaf 60f0 20f0))
         (button (pad 4f0 (leaf 40f0 16f0)))
         (root (pad 8f0 (row 8f0 label button))))
    (values root label button)))

(define-test a-counter-lays-out-the-way-the-numbers-say ()
  (multiple-value-bind (root label button) (counter)
    (multiple-value-bind (w h) (layout-node root 0f0 400f0 0f0 300f0)
      ;; The button is its leaf and 4 on each side: 48 by 24. The row is the label,
      ;; the gap and the button wide, as tall as the button, and the label is
      ;; centred on it. The root is the row and 8 on each side.
      (assert-eql 132f0 w)
      (assert-eql 40f0 h)
      (let ((row (svref (node-children root) 0)))
        (assert-equal '(8f0 8f0) (place-of row))
        (assert-eql 116f0 (node-w row))
        (assert-eql 24f0 (node-h row)))
      (assert-equal '(0f0 2f0) (place-of label))
      (assert-equal '(68f0 0f0) (place-of button))
      (assert-equal '(4f0 4f0) (place-of (svref (node-children button) 0))))))

(define-test a-toolbar-gives-the-space-in-the-middle-to-the-spacer ()
  (let* ((left (leaf 40f0 20f0))
         (spacer (leaf 0f0 0f0 :flex 1f0))
         (right (leaf 40f0 20f0))
         (toolbar (make-node :row :children (vector left spacer right) :fill t)))
    (layout-node toolbar 0f0 200f0 0f0 100f0)
    (assert-eql 200f0 (node-w toolbar))
    (assert-eql 120f0 (node-w spacer))
    (assert-eql 160f0 (node-x right))))

;;; --- no allocation ---

(defun app ()
  "A tree of about forty nodes: a column of rows with padding, a few of which flex."
  (pad 8f0
       (apply #'column 6f0
              (loop :repeat 6
                    :collect (make-node :row :gap 4f0 :fill t
                                             :children (vector (pad 3f0 (leaf 50f0 14f0))
                                                               (leaf 0f0 20f0 :flex 1f0)
                                                               (pad 3f0 (leaf 30f0 14f0))))))))

(defun layout-many (root count)
  "Compiled with this file, because lisp-unit2 evaluates test bodies under a
debugging policy that allocates."
  (declare (type node root) (type fixnum count))
  (dotimes (i count)
    (layout-node root 0f0 400f0 0f0 300f0)))

(define-test laying-out-a-tree-allocates-nothing ()
  (let ((root (app)))
    (layout-many root 100)
    (let ((before (sb-ext:get-bytes-consed)))
      (layout-many root 100000)
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        ;; A region is about 64 KB, so a byte or two a layout would show as megabytes.
        (assert-true (< delta 300000) delta)))))

(define-test a-flex-of-any-size-allocates-nothing-once-the-buffer-has-grown ()
  (dolist (count '(300 10000))
    (let ((root (apply #'row 0f0 (loop :repeat count :collect (leaf 2f0 3f0)))))
      ;; The first layout of a flex larger than any before it grows the buffer.
      (layout-many root 1)
      (let ((before (sb-ext:get-bytes-consed)))
        (layout-many root 200)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          (assert-true (< delta 300000) count delta))))))

;;; --- the buffer ---

(defun geometry (node)
  "The size and place of NODE and of everything under it, as a list."
  (cons (list (node-w node) (node-h node) (node-x node) (node-y node))
        (loop :for child :across (node-children node)
              :append (geometry child))))

(defun nested ()
  "A row of a leaf, a row of five leaves that flex, and a leaf: the inner flex needs
more room in the buffer than the outer one has left."
  (make-node :row :fill t
                  :children (vector (leaf 10f0 4f0)
                                    (make-node :row :fill t :flex 1f0
                                                    :children (vector (leaf 3f0 5f0)
                                                                      (leaf 0f0 6f0 :flex 1f0)
                                                                      (leaf 0f0 7f0 :flex 2f0)
                                                                      (leaf 4f0 8f0)
                                                                      (leaf 0f0 9f0 :flex 1f0)))
                                    (leaf 10f0 4f0))))

(define-test a-flex-that-grows-the-buffer-inside-another-lays-out-the-same ()
  (let ((reference (nested))
        (small (nested)))
    (layout-node reference 0f0 100f0 0f0 50f0)
    ;; Eight floats hold the outer flex's six, so the inner one's ten do not fit.
    (flex:with-layout-scratch (:size 8)
      (layout-node small 0f0 100f0 0f0 50f0))
    (assert-equal (geometry reference) (geometry small))
    ;; The flex in the middle takes what the two leaves leave: 80 wide.
    (assert-eql 80f0 (node-w (svref (node-children reference) 1)))))

(define-test threads-that-bind-their-own-buffers-lay-out-together ()
  (let* ((reference (progn (layout-node (nested) 0f0 100f0 0f0 50f0)
                           (let ((tree (nested)))
                             (layout-node tree 0f0 100f0 0f0 50f0)
                             (geometry tree))))
         (threads (loop :repeat 4
                        :collect (sb-thread:make-thread
                                  (lambda ()
                                    (flex:with-layout-scratch (:size 8)
                                      (let ((tree (nested)))
                                        (loop :repeat 2000
                                              :always (progn
                                                        (layout-node tree 0f0 100f0 0f0 50f0)
                                                        (equal (geometry tree) reference))))))))))
    (assert-equal '(t t t t) (mapcar #'sb-thread:join-thread threads))))
