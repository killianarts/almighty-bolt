(defpackage #:almighty-bolt-gui-designer/view
  (:use #:cl)
  (:export #:node
           #:make-node
           #:node-kind
           #:node-id
           #:node-options
           #:node-children
           #:form-node
           #:find-by-id))

(in-package #:almighty-bolt-gui-designer/view)

;;; A view being designed, as data: a tree of nodes, each a widget of some kind (a label, a button,
;;; a column) with its id, the options it was given and, for a container, what is in it. A node is
;;; never changed; an edit makes a new tree, which shares what it did not change with the old one, so
;;; the old tree is still there to go back to.
;;;
;;; The id is what the app's code will wire its logic to, so the designer keeps it unique in the view
;;; and keeps a node's selection by it (the key of the node's box changes when the tree is edited).

(defstruct (node (:constructor make-node (kind id &key options children)))
  "A widget of KIND (a keyword the catalogue knows), called ID, with OPTIONS (a property list of what
it was given, by the names of the widget's own options) and CHILDREN (a list of nodes)."
  (kind :label :type keyword :read-only t)
  (id :node :type keyword :read-only t)
  (options '() :type list :read-only t)
  (children '() :type list :read-only t))

(defun form-node (form)
  "The node that FORM writes: (kind id (option value ...) child-form ...), as
(:button :sign-in (:text \"Sign in\"))."
  (destructuring-bind (kind id &optional options &rest children) form
    (make-node kind id :options options :children (mapcar #'form-node children))))

(defun find-by-id (node id)
  "The node called ID in the tree NODE, or NIL."
  (if (eq (node-id node) id)
      node
      (loop :for child :in (node-children node)
              :thereis (find-by-id child id))))
