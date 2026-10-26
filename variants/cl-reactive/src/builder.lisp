(defpackage #:almighty-bolt-variant-reactive/builder
  (:use #:cl)
  (:local-nicknames (#:v #:almighty-bolt-variant-reactive/views))
  (:import-from #:almighty-bolt-variant-retained/builder
                #:children-of
                #:single-child)
  (:export #:children-of
           #:row
           #:column
           #:stack
           #:padding
           #:panel
           #:center
           #:component))

(in-package #:almighty-bolt-variant-reactive/builder)

;;; The views of the reactive variant written as the retained builder writes widgets, and as the
;;; immediate variant writes its containers: the options in a list first, and the children as the
;;; body (see the retained builder for what a form of the body is worth). The functions of VIEWS
;;; stay, and these expand into them, handing over the children as one list, so a description that
;;; is made again whenever a signal changes copies nothing it did not before.

(defmacro row ((&rest options) &body body)
  "The children side by side. OPTIONS are :GAP, :MAIN-ALIGN, :CROSS-ALIGN, :FILL, :ID, :FLEX,
:VISIBLE, :ENABLED and :KEY."
  `(v:flex-view-of :row (children-of ,@body) ,@options))

(defmacro column ((&rest options) &body body)
  "The children one under another. OPTIONS are those of ROW."
  `(v:flex-view-of :column (children-of ,@body) ,@options))

(defmacro stack ((&rest options) &body body)
  "The children on top of each other, the later on top. OPTIONS are the common ones."
  `(v:stack-of (children-of ,@body) ,@options))

(defmacro padding ((&rest options) &body body)
  "Space round the one child. OPTIONS are :ALL, :LEFT, :TOP, :RIGHT, :BOTTOM and the common ones."
  `(v:padding ,(single-child 'padding body) ,@options))

(defmacro panel ((&rest options) &body body)
  "The one child on a rounded background. OPTIONS are those of PADDING, and :COLOR and :RADIUS."
  `(v:panel ,(single-child 'panel body) ,@options))

(defmacro center ((&rest options) &body body)
  "The one child in the middle of the room there is. OPTIONS are the common ones."
  `(v:center ,(single-child 'center body) ,@options))

(defmacro component ((&rest options) &body body)
  "A part of the screen that BODY describes, and describes again, alone, when a signal that it
read changes. OPTIONS are :KEY."
  `(v:component (lambda () ,@body) ,@options))
