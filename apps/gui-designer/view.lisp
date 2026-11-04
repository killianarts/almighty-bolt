(defpackage #:almighty-bolt-gui-designer/view
  (:use #:cl)
  (:export #:node
           #:make-node
           #:node-kind
           #:node-id
           #:node-options
           #:node-children
           #:form-node
           #:node-form
           #:find-by-id
           #:parent-of
           #:all-ids
           #:fresh-id
           #:replace-node
           #:add-node
           #:remove-node
           #:with-option
           #:with-id))

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

(defun node-form (node)
  "The form that writes NODE (see FORM-NODE)."
  (list* (node-kind node) (node-id node) (node-options node)
         (mapcar #'node-form (node-children node))))

(defun parent-of (tree id)
  "The node in TREE that has the node called ID among its children, or NIL."
  (if (find id (node-children tree) :key #'node-id)
      tree
      (loop :for child :in (node-children tree)
              :thereis (parent-of child id))))

(defun all-ids (tree)
  (cons (node-id tree) (mapcan #'all-ids (node-children tree))))

(defun fresh-id (tree kind)
  "An id for a new node of KIND that no node of TREE has: the kind and the first number free, as
:button-1."
  (let ((ids (all-ids tree)))
    (loop :for n :from 1
          :for id = (intern (format nil "~:@(~a-~d~)" kind n) :keyword)
          :unless (member id ids) :return id)))

(defun replace-node (tree id function)
  "TREE with the node called ID made what FUNCTION makes of it: a node, or NIL to take it out. What
is not on the way to it is shared with TREE."
  (labels ((walk (node)
             (if (eq (node-id node) id)
                 (funcall function node)
                 (let ((children (mapcan (lambda (child)
                                           (let ((new (walk child)))
                                             (and new (list new))))
                                         (node-children node))))
                   (if (and (= (length children) (length (node-children node)))
                            (every #'eq children (node-children node)))
                       node
                       (copy-with-children node children))))))
    (walk tree)))

(defun copy-with-children (node children)
  (make-node (node-kind node) (node-id node) :options (node-options node) :children children))

(defun add-node (tree node &key into after)
  "TREE with NODE put at the end of the node called INTO, or just after the node called AFTER."
  (cond (into
         (replace-node tree into (lambda (container)
                                   (copy-with-children container
                                                       (append (node-children container)
                                                               (list node))))))
        (t
         (let ((parent (parent-of tree after)))
           (replace-node tree (node-id parent)
                         (lambda (container)
                           (copy-with-children
                            container
                            (loop :for child :in (node-children container)
                                  :collect child
                                  :when (eq (node-id child) after) :collect node))))))))

(defun remove-node (tree id)
  "TREE without the node called ID, and what is in it."
  (replace-node tree id (constantly nil)))

(defun with-option (tree id name value default)
  "TREE with the node called ID given VALUE for its option NAME; at DEFAULT, the option is left out,
so that the form says only what was changed."
  (replace-node tree id
                (lambda (node)
                  (let ((options (loop :for (key old) :on (node-options node) :by #'cddr
                                       :unless (eq key name) :append (list key old))))
                    (make-node (node-kind node) id
                               :options (if (equal value default)
                                            options
                                            (append options (list name value)))
                               :children (node-children node))))))

(defun with-id (tree id new)
  "TREE with the node called ID called NEW."
  (replace-node tree id
                (lambda (node)
                  (make-node (node-kind node) new :options (node-options node)
                                                  :children (node-children node)))))
