(defpackage #:almighty-bolt-variant-retained/builder
  (:use #:cl)
  (:local-nicknames (#:w #:almighty-bolt-variant-retained/widgets))
  (:export #:children-of
           #:single-child
           #:row
           #:column
           #:stack
           #:padding
           #:panel
           #:center))

(in-package #:almighty-bolt-variant-retained/builder)

;;; The containers of the retained variant written the way the immediate variant writes them: the
;;; options in a list first, and the children as the body, so that a tree reads as its code is
;;; nested, and a computed row is a form in the body:
;;;
;;;   (column (:gap 8)
;;;     (label "Dashboard")
;;;     (loop :for r :below 6 :collect (row (:gap 8) (tile r))))
;;;
;;; The functions of WIDGETS stay (these expand into them), so the two can be mixed. The difference
;;; from the immediate variant is how a child gets in. There a widget that is called in the body puts
;;; itself in; here a node is already made when it is named (an app builds its widgets first and
;;; keeps them), so what a form of the body is worth is its value: a node is a child, a list of them
;;; is spliced in, and NIL is left out (a child that is only sometimes there is (when ...)). A form
;;; that only does something, such as a DOTIMES, puts nothing in; a loop that makes children is a
;;; LOOP with :COLLECT.

(declaim (inline add-children))
(defun add-children (value children)
  "CHILDREN, a list in reverse order, with VALUE put on it: not if it is NIL, and each of its
members if it is a list."
  (cond ((null value) children)
        ((listp value) (dolist (child value children)
                         (when child
                           (push child children))))
        (t (cons value children))))

(defmacro children-of (&rest forms)
  "The list of the children that FORMS give, in order: each value that is a node, and the nodes in
each value that is a list. Every form is evaluated once, in order."
  (if (null forms)
      '()
      (let ((children (gensym "CHILDREN")))
        `(let ((,children '()))
           ,@(loop :for form :in forms
                   :collect `(setf ,children (add-children ,form ,children)))
           (nreverse ,children)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun single-child (container body)
    "The one form of BODY, which a container of a single child has."
    (unless (and body (null (rest body)))
      (error "~(~a~) takes one child form, and here are ~d." container (length body)))
    (first body)))

(defmacro row ((&rest options) &body body)
  "The children side by side. OPTIONS are :GAP, :MAIN-ALIGN, :CROSS-ALIGN, :FILL, :ID and :FLEX."
  `(w:flex-box-of :horizontal (children-of ,@body) ,@options))

(defmacro column ((&rest options) &body body)
  "The children one under another. OPTIONS are those of ROW."
  `(w:flex-box-of :vertical (children-of ,@body) ,@options))

(defmacro stack ((&rest options) &body body)
  "The children on top of each other, the later on top. OPTIONS are :ID and :FLEX."
  `(w:stack-of (children-of ,@body) ,@options))

(defmacro padding ((&rest options) &body body)
  "Space round the one child. OPTIONS are :ALL, :LEFT, :TOP, :RIGHT, :BOTTOM, :ID and :FLEX."
  `(w:padding ,(single-child 'padding body) ,@options))

(defmacro panel ((&rest options) &body body)
  "The one child on a rounded background. OPTIONS are those of PADDING, and :COLOR and :RADIUS."
  `(w:panel ,(single-child 'panel body) ,@options))

(defmacro center ((&rest options) &body body)
  "The one child in the middle of the room there is. OPTIONS are :ID and :FLEX."
  `(w:center ,(single-child 'center body) ,@options))
