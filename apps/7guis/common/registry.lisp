(defpackage #:almighty-bolt-7guis/common/registry
  (:use #:cl)
  (:import-from #:almighty-bolt/app
                #:make-app-instance)
  (:export #:app
           #:app-variant
           #:app-task
           #:app-constructor
           #:app-width
           #:app-height
           #:app-title
           #:register-app
           #:find-app
           #:make-app-object
           #:all-apps
           #:+tasks+
           #:+variants+))

(in-package #:almighty-bolt-7guis/common/registry)

;;; Which apps exist: a task in a variant. Each file for a task registers a constructor, a
;;; function of no arguments that returns the function that describes the screen, so that the
;;; launcher and the acceptance tests can run any of them by name. The constructor is a name, so
;;; that a constructor compiled again is the one that is called.

(defparameter +tasks+ '(:counter :temperature :flight :timer :crud :circles :cells :layout :dialog
                        :dashboard :rows :editor))
(defparameter +variants+ '(:immediate))

(defstruct (app (:constructor make-app (variant task constructor width height title)))
  (variant nil :read-only t)
  (task nil :read-only t)
  (constructor nil :type (or function symbol) :read-only t)
  (width 400 :read-only t)
  (height 300 :read-only t)
  (title "" :read-only t))

(defvar *apps* '())

(defun register-app (variant task constructor &key (width 400) (height 300) title)
  "Record that VARIANT has an app for TASK, made by calling CONSTRUCTOR, which runs in a
window WIDTH by HEIGHT points. CONSTRUCTOR is the name of a function, not the function: a name
is looked up when the app is made, so an app whose function was compiled again is made from the
new one, which is what hot reload needs (decision record 0030). #'name would keep the old."
  (check-type variant (member :immediate))
  (check-type task (member :counter :temperature :flight :timer :crud :circles :cells :layout
                           :dialog :dashboard :rows :editor))
  (setf *apps* (remove-if (lambda (app) (and (eq (app-variant app) variant) (eq (app-task app) task)))
                          *apps*))
  (push (make-app variant task constructor width height
                  (or title (format nil "~(~a~) (~(~a~))" task variant)))
        *apps*)
  task)

(defun find-app (variant task)
  (find-if (lambda (app) (and (eq (app-variant app) variant) (eq (app-task app) task))) *apps*))

(defun make-app-object (app)
  "A new instance of APP, for the host to run: the function that its constructor makes, which
describes the screen, in what the host runs. The constructor is a name, so one compiled again is
the one that is called."
  (make-app-instance (app-constructor app)))

(defun all-apps ()
  (reverse *apps*))
