(defpackage #:almighty-bolt-variant-retained/t/builder
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:b #:almighty-bolt-variant-retained/builder)
                    (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:fake #:almighty-bolt-variant-support/fake-context)))

(in-package #:almighty-bolt-variant-retained/t/builder)

;;; The containers written with the options first and the children as the body, against the
;;; functions they expand into, with a context whose text is 8 points a character.

(defun lay-out (root)
  (let ((context (fake:make-fake-context :width 400f0 :height 300f0)))
    (n:layout-node root context 0f0 400f0 0f0 300f0)
    root))

(defun geometry (node)
  "The place and size of NODE and of every node under it, in order."
  (cons (list (n:node-x node) (n:node-y node) (n:node-w node) (n:node-h node))
        (loop :for child :across (n:node-children node)
              :append (geometry child))))

(defun texts (node)
  (map 'list #'w:label-text (n:node-children node)))

(define-test the-children-are-the-values-of-the-forms-in-order-and-each-is-evaluated-once ()
  (let ((seen '()))
    (flet ((child (name)
             (push name seen)
             name))
      (assert-equal '(:a :b :c) (b:children-of (child :a) (child :b) (child :c)))
      (assert-equal '(:c :b :a) seen))))

(define-test nothing-is-a-list-with-no-children-and-a-nil-is-left-out ()
  (assert-equal '() (b:children-of))
  (assert-equal '(:a :c) (b:children-of :a nil :c))
  (assert-equal '(:a :b) (b:children-of :a (when nil :x) :b)))

(define-test a-list-is-put-in-as-its-members-and-a-nil-in-it-is-left-out ()
  (assert-equal '(:a :b :c :d) (b:children-of :a (list :b nil :c) :d))
  (assert-equal '(:a :b :c) (b:children-of (list :a) () (list :b :c))))

(define-test a-form-that-only-does-something-puts-nothing-in ()
  (let ((count 0))
    (assert-equal '(:a) (b:children-of (dotimes (i 3) (incf count)) :a))
    (assert-eql 3 count)))

(define-test a-column-is-the-same-as-the-column-function ()
  (let ((macro (lay-out (b:column (:gap 10 :cross-align :center)
                          (w:label "one") (w:label "wider") (w:label "3"))))
        (function (lay-out (w:column (w:label "one") (w:label "wider") (w:label "3")
                                     :gap 10 :cross-align :center))))
    (assert-equal (geometry function) (geometry macro))
    (assert-equal '("one" "wider" "3") (texts macro))))

(define-test a-row-takes-its-id-and-flex-and-puts-in-the-nodes-that-were-made-before ()
  (let* ((first (w:label "x"))
         (second (w:label "y"))
         (row (b:row (:id :line :flex 2) first second)))
    (assert-eq :line (n:node-id row))
    (assert-eql 2f0 (n:node-flex row))
    (assert-eq first (aref (n:node-children row) 0))
    (assert-eq second (aref (n:node-children row) 1))
    (assert-eq row (n:node-parent first))))

(define-test a-computed-row-is-a-loop-that-collects ()
  (let ((column (b:column (:gap 4)
                  (w:label "head")
                  (loop :for i :below 3
                        :collect (b:row (:gap 2) (w:label (princ-to-string i))))
                  (when nil (w:label "never")))))
    (assert-eql 4 (length (n:node-children column)))
    (assert-equal '("head")
                  (list (w:label-text (aref (n:node-children column) 0))))))

(define-test a-stack-puts-its-children-on-top-of-each-other ()
  (let ((macro (lay-out (b:stack (:id :layers) (w:label "back") (w:label "front"))))
        (function (lay-out (w:stack (w:label "back") (w:label "front") :id :layers))))
    (assert-equal (geometry function) (geometry macro))
    (assert-eq :layers (n:node-id macro))))

(define-test padding-panel-and-center-are-the-same-as-their-functions ()
  (assert-equal (geometry (lay-out (w:padding (w:label "a") :all 10 :left 3)))
                (geometry (lay-out (b:padding (:all 10 :left 3) (w:label "a")))))
  (assert-equal (geometry (lay-out (w:panel (w:label "a") :all 6 :radius 4)))
                (geometry (lay-out (b:panel (:all 6 :radius 4) (w:label "a")))))
  (assert-equal (geometry (lay-out (w:center (w:label "a"))))
                (geometry (lay-out (b:center () (w:label "a"))))))

(define-test a-container-of-one-child-wants-exactly-one-form ()
  (assert-error 'error (macroexpand-1 '(b:padding (:all 1))))
  (assert-error 'error (macroexpand-1 '(b:padding (:all 1) a b)))
  (assert-error 'error (macroexpand-1 '(b:panel () a b)))
  (assert-error 'error (macroexpand-1 '(b:center () a b)))
  (assert-true (macroexpand-1 '(b:padding (:all 1) a))))
