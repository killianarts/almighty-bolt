(defpackage #:almighty-bolt/kept
  (:use #:cl)
  (:export #:state-let
           #:kept-state
           #:make-kept-state
           #:call-with-kept-state
           #:with-kept-state
           #:reset-kept-state
           #:kept-state-names
           #:kept-value
           #:kept-in-use-p))

(in-package #:almighty-bolt/kept)

;;; State that is still there when the app is made again. An app's own
;;; state is in variables that its constructor closes over, and a new call of the constructor makes
;;; new ones. STATE-LET is LET for them: the window keeps a store of cells, by the name of the
;;; variable, and when the app is made again a name that is in the store gets the cell it had.

(defvar *kept-state* nil
  "The store that STATE-LET uses, or NIL: a STATE-LET then makes new cells, as LET does.")

(defstruct (cell (:constructor make-cell (value form)) (:copier nil))
  value
  ;; The form the value was made from. If the form is not the same when the app is made again,
  ;; it was edited, and the cell is made again.
  form)

(defstruct (kept-state (:constructor make-kept-state ()) (:copier nil))
  "What an app keeps between the times it is made: its cells, by name and by how many of that name
the app had made before (an app may bind a name twice)."
  (cells (make-hash-table :test 'equal) :read-only t)
  ;; How many times each name has been bound in the pass that is being made.
  (seen (make-hash-table :test 'eq) :read-only t)
  ;; The cells that the pass made new. They go into the store when it ends without an error, so
  ;; that a pass that fails does not take the state of the app that is still running.
  (pending '()))

(defun call-with-kept-state (state function)
  "Call FUNCTION, which makes an app, with STATE as the store of its STATE-LETs. A STATE of NIL
is no store. The cells that FUNCTION makes are kept if it returns, and forgotten if it does not."
  (cond ((null state) (funcall function))
        (t
         (clrhash (kept-state-seen state))
         (setf (kept-state-pending state) '())
         (let ((*kept-state* state))
           (multiple-value-prog1 (funcall function)
             (loop :for (key . cell) :in (kept-state-pending state)
                   :do (setf (gethash key (kept-state-cells state)) cell))
             (setf (kept-state-pending state) '()))))))

(defmacro with-kept-state ((state) &body body)
  "Run BODY, which makes an app, with STATE as the store of its STATE-LETs (CALL-WITH-KEPT-STATE)."
  `(call-with-kept-state ,state (lambda () ,@body)))

(defun reset-kept-state (state)
  "Forget everything STATE keeps, so that the next time the app is made it starts from its
initial values."
  (clrhash (kept-state-cells state))
  nil)

(defun kept-state-names (state)
  "The names that STATE keeps cells for, each as many times as it has cells."
  (sort (loop :for (name . nil) :being :the :hash-keys :of (kept-state-cells state)
              :collect name)
        #'string< :key #'symbol-name))

(defun kept-value (state name &optional (occurrence 1))
  "The value of the variable NAME that STATE keeps (the OCCURRENCE-th of that name, for an app that
binds one twice), and true if there is one; for looking at what an app keeps from the REPL."
  (let ((cell (gethash (cons name occurrence) (kept-state-cells state))))
    (if cell
        (values (cell-value cell) t)
        (values nil nil))))

(defun kept-in-use-p ()
  "True if a STATE-LET has run in the app that is being made, with a store: the app keeps state, and
what it shows can be carried to the next time it is made without showing what its variables do not
hold. A variant asks as it is made."
  (and *kept-state* (plusp (hash-table-count (kept-state-seen *kept-state*)))))

(defun kept-cell (name form thunk)
  "The cell for the variable NAME, whose initial value is made by calling THUNK and which was
written as FORM: the one that the store has, if it was made from the same FORM; and otherwise
a new one."
  (let ((state *kept-state*))
    (if (null state)
        (make-cell (funcall thunk) form)
        (let* ((key (cons name (incf (gethash name (kept-state-seen state) 0))))
               (cell (gethash key (kept-state-cells state))))
          (if (and cell (equal (cell-form cell) form))
              cell
              (let ((new (make-cell (funcall thunk) form)))
                (push (cons key new) (kept-state-pending state))
                new))))))

(defmacro state-let (bindings &body body)
  "LET for the state of an app that is made again when its code changes. Each of BINDINGS is
(NAME INITIAL-FORM), as in LET. NAME is a place, so SETF and closures work as they do with LET.
When the app is made again with the same store, a NAME that has a cell in it gets that cell, with
the value it had, and INITIAL-FORM is not run; if INITIAL-FORM is not the form that the cell was
made from (it was edited), the cell is made again. Without a store this is LET. Put in it only
state: what is worked out from the state is made again from the new code, so keep it out."
  (let* ((bindings (mapcar (lambda (binding)
                             (if (consp binding) binding (list binding nil)))
                           bindings))
         (cells (mapcar (lambda (binding)
                          (declare (ignore binding))
                          (gensym "CELL"))
                        bindings)))
    `(let ,(loop :for (name initial) :in bindings
                 :for cell :in cells
                 :collect `(,cell (kept-cell ',name ',initial (lambda () ,initial))))
       (declare (ignorable ,@cells) (type cell ,@cells))
       (symbol-macrolet ,(loop :for (name) :in bindings
                               :for cell :in cells
                               :collect `(,name (cell-value ,cell)))
         ,@body))))
