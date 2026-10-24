;;;; task-board.lisp — McCLIM presentations + incremental redisplay demo
;;;;
;;;; Load and run:
;;;;   (load "task-board.lisp")
;;;;   (task-board:run)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "mcclim" :silent t))

(defpackage #:task-board
  (:use #:clim #:clim-lisp)
  (:export #:run))
(in-package #:task-board)

;;; Model. Every CLOS class is automatically a presentation type, and
;;; subclasses are presentation subtypes.

(defclass task ()
  ((title :initarg :title :accessor title)
   (done  :initarg :done  :initform nil :accessor done-p)))

(defclass urgent-task (task) ())

;;; How a task looks on screen.

(define-presentation-method present
    (task (type task) stream (view textual-view) &key)
  (format stream "[~:[ ~;x~]] ~A" (done-p task) (title task)))

;;; Urgent tasks reuse the parent's drawing, but in bold red.
(define-presentation-method present
    (task (type urgent-task) stream (view textual-view) &key)
  (with-drawing-options (stream :ink +red+)
    (with-text-face (stream :bold)
      (call-next-method))))

;;; Frame

(define-application-frame task-board ()
  ((tasks :accessor tasks
          :initform (list (make-instance 'task :title "Write docs")
                          (make-instance 'urgent-task :title "Fix prod bug")
                          (make-instance 'task :title "Lunch"))))
  (:panes
   ;; With :INCREMENTAL-REDISPLAY T, CLIM wraps the display function in a
   ;; top-level UPDATING-OUTPUT and, after each command, redraws only the
   ;; records whose cache values changed instead of clearing the pane.
   (board :application
          :display-function 'display-board
          :incremental-redisplay t)
   (interactor :interactor))
  (:layouts
   (default (vertically ()
              (:fill board)
              (1/4 interactor)))))

(defun task-state (task)
  "Everything that affects how TASK is drawn. If this is EQUAL to the value
cached on the last pass, the row's output record is reused as is."
  (list (title task) (done-p task) (class-of task)))

(defun display-board (frame pane)
  (dolist (task (tasks frame))
    ;; :UNIQUE-ID matches this row to its record from the previous pass.
    ;; The task object itself works, since CHANGE-CLASS keeps its identity.
    ;; :CACHE-VALUE decides whether the body runs again or the old output
    ;; is kept (and just moved, if rows above it were added or removed).
    (updating-output (pane :unique-id task
                           :id-test #'eq
                           :cache-value (task-state task)
                           :cache-test #'equal)
      ;; PRESENT draws the object and remembers which object sits at this
      ;; spot on screen, and its type (TASK or URGENT-TASK).
      (present task (presentation-type-of task) :stream pane)
      (terpri pane))))

;;; Commands. Arguments are typed with presentation types.

(define-task-board-command (com-toggle-task :name t) ((task 'task))
  (setf (done-p task) (not (done-p task))))

(define-task-board-command (com-delete-task :name t) ((task 'task))
  (setf (tasks *application-frame*)
        (remove task (tasks *application-frame*))))

(define-task-board-command (com-escalate-task :name t) ((task 'task))
  (change-class task 'urgent-task))

(define-task-board-command (com-downgrade-task :name t) ((task 'urgent-task))
  (change-class task 'task))

(define-task-board-command (com-add-task :name t) ((title 'string))
  (setf (tasks *application-frame*)
        (append (tasks *application-frame*)
                (list (make-instance 'task :title title)))))

;;; Translators: clicking a presentation runs a command.

;; Left-click any task, plain or urgent, to toggle it.
(define-presentation-to-command-translator toggle-task
    (task com-toggle-task task-board
     :gesture :select
     :documentation "Toggle done")
    (object)
  (list object))

;; Offered only for tasks that aren't already urgent.
(define-presentation-to-command-translator escalate-task
    (task com-escalate-task task-board
     :gesture nil                       ; right-click menu only
     :tester ((object) (not (typep object 'urgent-task)))
     :documentation "Escalate")
    (object)
  (list object))

;; Defined on URGENT-TASK, so it never applies to plain tasks.
(define-presentation-to-command-translator downgrade-task
    (urgent-task com-downgrade-task task-board
     :gesture nil
     :documentation "Downgrade")
    (object)
  (list object))

(define-presentation-to-command-translator delete-task
    (task com-delete-task task-board
     :gesture nil
     :documentation "Delete")
    (object)
  (list object))

(defun run ()
  (run-frame-top-level (make-application-frame 'task-board)))
