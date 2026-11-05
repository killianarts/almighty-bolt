(defpackage #:almighty-bolt-7guis/common/registry
  (:use #:cl)
  (:local-nicknames (#:app #:almighty-bolt/app))
  (:export #:app
           #:app-task
           #:app-constructor
           #:app-width
           #:app-height
           #:app-title
           #:register-app
           #:find-app
           #:make-app-object
           #:all-apps
           #:+tasks+))

(in-package #:almighty-bolt-7guis/common/registry)

;;; Which apps exist: one for each task. Each task's app.lisp registers a constructor, a
;;; function of no arguments that returns the function that describes the screen, so that the
;;; launcher and the acceptance tests can run any of them by name. The constructor is a name, so
;;; that a constructor compiled again is the one that is called.

(defparameter +tasks+ '(:counter :temperature :flight :timer :crud :circles :cells :layout :dialog
                        :dashboard :rows :editor))
(defstruct (app (:constructor make-app (task constructor width height title)))
  (task nil :read-only t)
  (constructor nil :type (or function symbol) :read-only t)
  (width 400 :read-only t)
  (height 300 :read-only t)
  (title "" :read-only t))

(defvar *apps* '())

(defun register-app (task constructor &key (width 400) (height 300) title)
  "Record that the app for TASK is made by calling CONSTRUCTOR, and runs in a
window WIDTH by HEIGHT points. CONSTRUCTOR is the name of a function, not the function: a name
is looked up when the app is made, so an app whose function was compiled again is made from the
new one, which is what hot reload needs. #'name would keep the old."
  (check-type task (member :counter :temperature :flight :timer :crud :circles :cells :layout
                           :dialog :dashboard :rows :editor))
  (setf *apps* (remove task *apps* :key #'app-task))
  (push (make-app task constructor width height (or title (string-downcase task)))
        *apps*)
  task)

(defun find-app (task)
  (find task *apps* :key #'app-task))

(defun make-app-object (app)
  "A new instance of APP, for the host to run: the function that its constructor makes, which
describes the screen, in what the host runs. The constructor is a name, so one compiled again is
the one that is called."
  (app:make-app-instance (app-constructor app)))

(defun all-apps ()
  (reverse *apps*))
