(defpackage #:almighty-bolt/ui/views
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/widgets
                #:label)
  (:import-from #:almighty-bolt/ui/catalogue
                #:define-widget-entry
                #:find-entry
                #:entry-kind
                #:entry-options
                #:entry-wires
                #:option-name
                #:catalogue
                #:entry-value
                #:check-view-form
                #:draw-form)
  (:export #:define-view
           #:view
           #:set-view
           #:find-view
           #:view-names
           #:wiring-problems
           #:*view-defined-hook*))

(in-package #:almighty-bolt/ui/views)

;;; A view is a piece of a window that a designer made: a form of widgets, kept by its name (a
;;; symbol, of the app's package, so that two apps' or libraries' views do not take each other's
;;; names), which DEFINE-VIEW gives. An app draws it with VIEW and wires its own
;;; code to it by the ids the designer gave: what a widget shows (its options, by their names), what
;;; a press does (:on-press), the variable a field shows and sets (:value), what goes in a container
;;; (:contents). The design and the code are each whole without the other, and the ids are what they
;;; share.
;;;
;;; The wiring is checked against the design when the app's code is compiled (VIEW is a macro, and
;;; the view is known then if its file was loaded first), when a view is defined again (what each
;;; VIEW whose code is still there wires is noted, and checked against the new design), and when it
;;; is drawn. A view defined again is drawn in every window at once, without the code that wires it
;;; being compiled again: VIEW finds the view by its name when it draws.

(defvar *views* (make-hash-table)
  "The views defined, each a form, by name.")

(defun make-weak-table ()
  "A table whose entries go when nothing else holds their key (where the Lisp can say so)."
  #+sbcl (make-hash-table :test 'eq :weakness :key :synchronized t)
  #-sbcl (make-hash-table :test 'eq))

(defun collect-garbage ()
  "Let the Lisp find what nothing holds, so that the weak tables forget it."
  #+sbcl (sb-ext:gc :full t))

(defvar *wirings* (make-weak-table)
  "What each VIEW form that is loaded wires: its spec, a list of (id kind key ...) (the keys being
the options and wires it gives), to the name of the view it wires. A spec is a constant of the code
that VIEW was compiled into, and the table holds it weakly: when that code is gone (compiled
again, and the old code no longer held), so is its entry.")

(defvar *checked* (make-weak-table)
  "For each spec, the form it was last found to fit, so that a view is checked again only when it
has changed. Weak, as *WIRINGS* is.")

(defvar *views-drawn* '()
  "The names of the views being drawn, the innermost first, so that a view inside itself is said
and not drawn for ever.")

(defvar *view-defined-hook* nil
  "A function called with no arguments after a view has been defined, or NIL: the host sets it so
that an open window shows a view changed in a designer.")

(defun find-view (name)
  "The form of the view called NAME, or NIL."
  (values (gethash name *views*)))

(defun view-names ()
  (loop :for name :being :the :hash-keys :of *views* :collect name))

(defun spec-problems (spec form)
  "What does not fit in the wiring SPEC and the view FORM, as a list of sentences."
  (let ((problems '()))
    (labels ((find-id (form id)
               (if (eq (second form) id)
                   form
                   (loop :for child :in (cdddr form)
                           :thereis (find-id child id)))))
      (dolist (clause spec)
        (destructuring-bind (id kind &rest keys) clause
          (declare (ignore keys))
          (let ((found (find-id form id)))
            (cond ((null found)
                   (push (format nil "~s is wired, and the view has no widget with that id." id)
                         problems))
                  ((not (eq (first found) kind))
                   (push (format nil "~s is wired as a ~(~a~), and in the view it is a ~(~a~)."
                                 id kind (first found))
                         problems)))))))
    (nreverse problems)))

(defun wiring-problems (name)
  "What does not fit between the view called NAME and the code that wires it: a list of
sentences, empty if all is well. Only code that can still run is asked: if something does not
fit, the garbage is collected first and the question asked again, so that the wiring of code
compiled again since (and no longer held) is not counted."
  (let ((form (find-view name)))
    (flet ((problems ()
             (remove-duplicates (loop :for spec :being :the :hash-keys :of *wirings*
                                        :using (hash-value view)
                                      :when (eq view name)
                                        :append (spec-problems spec form))
                                :test #'string=)))
      (and form
           (and (problems)
                (progn (collect-garbage) (problems)))))))

(define-condition view-wiring-mismatch (style-warning)
  ((name :initarg :name :reader view-wiring-mismatch-name)
   (problem :initarg :problem :reader view-wiring-mismatch-problem))
  (:report (lambda (condition stream)
             (format stream "The view ~s is wired by code that does not fit it: ~a"
                     (view-wiring-mismatch-name condition) (view-wiring-mismatch-problem condition))))
  (:documentation "A view defined that the code wiring it, as loaded, does not fit. A style warning,
so that compiling a view's file does not fail for it: drawing the view checks the code that runs,
and is an error if it does not fit."))

(defun set-view (name form)
  "Make FORM, checked (CHECK-VIEW-FORM), the view called NAME, and show it in every window. Returns
what does not fit between it and the code that wires it (WIRING-PROBLEMS), and signals a style
warning (VIEW-WIRING-MISMATCH) of each."
  (check-type name (and symbol (not null)))
  (check-view-form form)
  (setf (gethash name *views*) form)
  (when *view-defined-hook* (funcall *view-defined-hook*))
  (let ((problems (wiring-problems name)))
    (dolist (problem problems)
      (warn 'view-wiring-mismatch :name name :problem problem))
    problems))

(defmacro define-view (name form)
  "Define the view NAME (a symbol, as a function's name is) as FORM: (kind id (option value ...) what-is-in-it ...), as
a designer writes it. An error when compiled if FORM cannot be drawn (CHECK-VIEW-FORM). It is
defined when compiled too, so that the code that wires it, compiled after, is checked against it."
  (check-type name (and symbol (not null)))
  (check-view-form form)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-view ',name ',form)))

(define-condition view-not-defined (style-warning)
  ((name :initarg :name :reader view-not-defined-name))
  (:report (lambda (condition stream)
             (format stream "No view is called ~s when this is compiled, so its wiring is checked ~
                             only when it is drawn. Load the file that defines it (DEFINE-VIEW) ~
                             first."
                     (view-not-defined-name condition))))
  (:documentation "A VIEW form compiled before the view it draws is defined."))

(defun note-wiring (name spec)
  "Note that a VIEW form wires SPEC to the view NAME; called when that form is loaded."
  (setf (gethash spec *wirings*) name)
  spec)

(defun draw-view (name id spec wires)
  (let ((form (or (find-view name)
                  (error "No view is called ~s. Load the file that defines it (DEFINE-VIEW) before ~
                          the code that draws it." name))))
    (unless (eq (gethash spec *checked*) form)
      (let ((problems (spec-problems spec form)))
        (when problems
          (error "The view ~s and the code that wires it do not fit: ~{~a~^ ~}" name problems)))
      (setf (gethash spec *checked*) form))
    (let ((*views-drawn* (cons name *views-drawn*)))
      (draw-form form :wires wires :id id))))

;; A view inside another: the view of that name, drawn with the node's id for its outermost.
(define-widget-entry :view (:title "View" :options ((:name :view nil)))
  (let ((form (and name (find-view name))))
    (cond ((null name) (label "A view: choose which" :id id))
          ((null form) (label (format nil "No view is called ~(~s~)" name) :id id))
          ((member name *views-drawn*) (label (format nil "~(~s~) is inside itself" name) :id id))
          (t (let ((*views-drawn* (cons name *views-drawn*)))
               (draw-form form :id id))))))

(defun clause-entry (clause)
  "The catalogue's entry for the kind CLAUSE names: (button :sign-in ...) is a :button."
  (unless (and (consp clause) (symbolp (first clause)) (keywordp (second clause)))
    (error "~s is not wiring: (kind id key value ...), as (button :sign-in :on-press (go))." clause))
  (let ((kind (intern (symbol-name (first clause)) :keyword)))
    (or (find kind (catalogue) :key #'entry-kind)
        (error "~s: there is no widget of the kind ~(~a~); there are ~{~(~a~)~^, ~}." clause kind
               (mapcar #'entry-kind (catalogue))))))

(defmacro view (name-and-id &body clauses)
  "Draw the view NAME, wired to the code by CLAUSES. NAME-AND-ID is the view's name (a symbol), or
(name :id id) to give its outermost widget ID in place of its own, so that it can be in one
container more than once. Each clause is a widget of the view, by its kind and id, and what the
code gives it: (button :sign-in :enabled (plusp (length name)) :on-press (sign-in name)). An option
is given as the widget's own (its value is worked out each frame); :on-press is what a press of a
button does, :value is a place that a widget that holds a value (a text field, a slider, a check
box, a list) shows and that the person sets, :contents is what is
drawn in a column or row after what the view has there. What is not wired is as the view has it.

Checked when compiled: a kind there is no widget of, an option or a wire that kind does not take,
and, if the view is defined then, an id it does not have or a widget of another kind. A view not
defined then is a style warning, and the wiring is checked when the view is drawn."
  (destructuring-bind (name &key id) (if (listp name-and-id) name-and-id (list name-and-id))
    (check-type name (and symbol (not null)))
    (let ((spec '()) (wires '()) (functions '()))
      (dolist (clause clauses)
        (let* ((entry (clause-entry clause))
               (kind (entry-kind entry))
               (wire-id (second clause))
               (options '())
               (wired '()))
          (when (find wire-id spec :key #'first)
            (error "~s is wired twice in one VIEW." wire-id))
          (loop :for (key value) :on (cddr clause) :by #'cddr
                ;; :value is the place a widget that holds a value shows and sets, even where it
                ;; also names one of its options (a slider's).
                :do (cond ((and (find key (entry-options entry) :key #'option-name)
                                (not (and (eq key :value) (entry-value entry))))
                           (setf options (list* value key options)))
                          ((not (member key (entry-wires entry)))
                           (error "~s: a ~(~a~) takes ~{~(~s~)~^, ~}, and not ~s." clause kind
                                  (append (mapcar #'option-name (entry-options entry))
                                          (entry-wires entry))
                                  key))
                          ((eq key :value)
                           ;; What the field shows, and a function that sets the place.
                           (let ((function (gensym "SET")) (new (gensym "NEW")))
                             (setf options (list* value (entry-value entry) options))
                             (push `(,function (,new) (setf ,value ,new)) functions)
                             (setf wired (list* `(function ,function) key wired))))
                          (t
                           (let ((function (gensym (symbol-name key))))
                             (push `(,function () ,value) functions)
                             (setf wired (list* `(function ,function) key wired))))))
          (push (list* wire-id kind (loop :for (key) :on (cddr clause) :by #'cddr :collect key))
                spec)
          (push `(list* ,wire-id (list ,@(reverse options)) (list ,@(reverse wired))) wires)))
      (setf spec (nreverse spec))
      ;; Against the view as it is when the code is compiled.
      (let ((form (find-view name)))
        (if form
            (let ((problems (spec-problems spec form)))
              (when problems
                (error "The view ~s and this wiring do not fit: ~{~a~^ ~}" name problems)))
            (warn 'view-not-defined :name name)))
      (let ((list (gensym "WIRES")))
        `(flet ,(reverse functions)
           (declare (dynamic-extent ,@(mapcar (lambda (f) `(function ,(first f))) functions)))
           (let ((,list (list ,@(reverse wires))))
             (declare (dynamic-extent ,list))
             (draw-view ',name ,id (load-time-value (note-wiring ',name ',spec)) ,list)
             nil))))))
