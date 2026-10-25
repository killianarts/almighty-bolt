(defpackage #:almighty-bolt-variant-retained/t/widgets
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:counter #:almighty-bolt-variant-retained/t/counter)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-retained/t/widgets)

;;; Layout and painting of the widgets, with a context whose text is 8 points a
;;; character, 10 above the baseline and 4 below. They run on any platform.

(defun lay-out (root &key (width 400f0) (height 300f0))
  (let ((context (fake:make-fake-context :width width :height height)))
    (n:layout-node root context 0f0 width 0f0 height)
    context))

(defun geometry (node)
  "x, y, width and height of NODE, as a list."
  (list (n:node-x node) (n:node-y node) (n:node-w node) (n:node-h node)))

;;; --- a label ---

(define-test a-label-is-as-large-as-its-text ()
  (let ((label (w:label "Count")))
    (lay-out label)
    (assert-equal '(0f0 0f0 40f0 14f0) (geometry label))))

(define-test a-label-is-held-to-its-constraints ()
  (let ((label (w:label "Count")))
    (n:layout-node label (fake:make-fake-context) 0f0 30f0 0f0 10f0)
    (assert-equal '(0f0 0f0 30f0 10f0) (geometry label))))

(define-test changing-a-labels-text-leaves-its-size-until-the-next-layout ()
  (let ((label (w:label "ab"))
        (root nil))
    (setf root (w:padding label :all 2))
    (setf (n:node-tree root) (n:make-tree-state))
    (lay-out root)
    (setf (n:tree-state-layout-dirty (n:node-tree root)) nil)
    (setf (w:label-text label) "abcd")
    (assert-eql 16f0 (n:node-w label))
    (assert-true (n:tree-state-layout-dirty (n:node-tree root)))
    (assert-true (n:tree-state-changed (n:node-tree root)))
    (lay-out root)
    (assert-eql 32f0 (n:node-w label))))

;;; --- padding ---

(define-test padding-puts-space-round-the-child ()
  (let* ((label (w:label "ab"))
         (padding (w:padding label :left 1 :top 2 :right 3 :bottom 4)))
    (lay-out padding)
    (assert-equal '(0f0 0f0 20f0 20f0) (geometry padding))
    (assert-equal '(1f0 2f0 16f0 14f0) (geometry label))))

;;; --- a button ---

(define-test a-button-is-its-text-and-space ()
  (let ((button (w:button "Count")))
    (lay-out button)
    ;; 40 by 14 of text, and 14 either side and 7 above and below.
    (assert-equal '(0f0 0f0 68f0 28f0) (geometry button))
    (assert-equal '(14f0 7f0 40f0 14f0) (geometry (w:button-label button)))))

(define-test changing-a-buttons-text-changes-its-label ()
  (let ((button (w:button "ab")))
    (setf (w:button-text button) "abcd")
    (assert-equal "abcd" (w:button-text button))
    (lay-out button)
    (assert-eql 60f0 (n:node-w button))))

;;; --- rows and columns ---

(define-test a-row-puts-children-side-by-side ()
  (let* ((a (w:label "ab"))
         (b (w:label "abcd"))
         (row (w:row a b :gap 4)))
    (lay-out row)
    (assert-equal '(0f0 0f0 16f0 14f0) (geometry a))
    (assert-equal '(20f0 0f0 32f0 14f0) (geometry b))
    (assert-equal '(0f0 0f0 52f0 14f0) (geometry row))))

(define-test a-column-puts-children-one-under-another ()
  (let* ((a (w:label "ab"))
         (b (w:label "abcd"))
         (column (w:column a b :gap 6 :cross-align :end)))
    (lay-out column)
    (assert-equal '(16f0 0f0 16f0 14f0) (geometry a))
    (assert-equal '(0f0 20f0 32f0 14f0) (geometry b))
    (assert-equal '(0f0 0f0 32f0 34f0) (geometry column))))

(define-test a-child-with-a-flex-factor-takes-what-is-left ()
  (let* ((left (w:label "ab"))
         (middle (w:label "" :flex 1))
         (right (w:label "ab"))
         (row (w:row left middle right :fill t)))
    (lay-out row :width 100f0)
    (assert-equal '(0f0 0f0 100f0 14f0) (geometry row))
    (assert-eql 68f0 (n:node-w middle))
    (assert-eql 84f0 (n:node-x right))))

(define-test the-options-of-a-row-follow-its-children ()
  (let ((row (w:row :gap 3 :id :empty)))
    (assert-eql :empty (n:node-id row))
    (assert-eql 0 (length (n:node-children row)))
    (lay-out row)
    (assert-equal '(0f0 0f0 0f0 0f0) (geometry row))))

;;; --- the counter ---

(defun counter-tree ()
  "The Counter laid out: returns its root, number label and button."
  (multiple-value-bind (variant number button) (counter:make-counter)
    (let ((root (retained:retained-variant-root variant)))
      (lay-out root)
      (values root number button variant))))

(define-test the-counter-lays-out-the-way-the-numbers-say ()
  (multiple-value-bind (root number button) (counter-tree)
    ;; The button is 68 by 28 and the number 8 by 14, 16 apart, centred on the
    ;; row's height of 28; the row is 24 in from each edge.
    (assert-equal '(0f0 0f0 140f0 76f0) (geometry root))
    (let ((row (svref (n:node-children root) 0)))
      (assert-equal '(24f0 24f0 92f0 28f0) (geometry row)))
    (assert-equal '(0f0 7f0 8f0 14f0) (geometry number))
    (assert-equal '(24f0 0f0 68f0 28f0) (geometry button))
    (assert-equal '(48f0 24f0) (multiple-value-list (n:node-origin button)))))

(define-test the-counter-paints-its-widgets-in-order ()
  (multiple-value-bind (root number button) (counter-tree)
    (declare (ignore number button))
    (let ((list (core:make-display-list :capacity 64))
          (context (fake:make-fake-context)))
      (n:paint-node root context list 0f0 0f0)
      ;; The number's one glyph, the button's rounded rectangle and its five glyphs.
      (assert-eql 7 (core:display-list-count list))
      (assert-eql core:+command-glyph+ (core:display-list-kind list 0))
      (assert-eql (float (char-code #\0) 1f0) (core:display-list-parameter list 0))
      (assert-equal '(24f0 31f0) (list (core:display-list-x list 0) (core:display-list-y list 0)))
      (assert-eql core:+command-rounded-rect+ (core:display-list-kind list 1))
      (assert-equal '(48f0 24f0 68f0 28f0)
                    (list (core:display-list-x list 1) (core:display-list-y list 1)
                          (core:display-list-width list 1) (core:display-list-height list 1)))
      (assert-eql (float (char-code #\C) 1f0) (core:display-list-parameter list 2))
      (assert-equal '(62f0 31f0) (list (core:display-list-x list 2) (core:display-list-y list 2))))))

(define-test finding-nodes-by-id-and-by-point ()
  (multiple-value-bind (root number button) (counter-tree)
    (assert-true (eq button (n:find-node root :count-button)))
    (assert-true (eq number (n:find-node root :count)))
    (assert-false (n:find-node root :missing))
    ;; The text of the button is inside it, and a click there is the button's.
    (assert-true (eq (w:button-label button) (n:node-at root 70f0 40f0)))
    (assert-true (eq button (n:interactive-at root 70f0 40f0)))
    (assert-false (n:interactive-at root 28f0 40f0))
    (assert-false (n:node-at root 500f0 40f0))))
