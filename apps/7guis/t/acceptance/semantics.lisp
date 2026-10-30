(defpackage #:almighty-bolt-7guis/t/acceptance/semantics
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:core #:almighty-bolt/internals)
                    (#:s #:almighty-bolt/ui/semantics)))

(in-package #:almighty-bolt-7guis/t/acceptance/semantics)

;;; What the screen means, read back as a program would: the tree of roles, names, values and states
;;; of the frame last built, from apps that were driven by events. Events in, the tree out; and the
;;; bounds in it are checked against the pointer, which is the only way to know that they are where
;;; the widget really is.

(defun tree (driver)
  (core:accessibility-tree (d:driver-variant driver)))

(defun the-node (driver &rest criteria)
  (let ((found (apply #'s:find-nodes (tree driver) criteria)))
    (assert-eql 1 (length found) criteria)
    (first found)))

(defun middle (node)
  (destructuring-bind (x y width height) (s:node-bounds node)
    (values (+ x (/ width 2)) (+ y (/ height 2)))))

(defun click-node (driver node)
  "Press and release the pointer in the middle of NODE's bounds, as a program that has only the tree
would."
  (multiple-value-bind (x y) (middle node)
    (d:click-at-point driver x y)))

(define-test the-crud-has-the-parts-of-a-form-with-the-names-the-person-reads ()
  (let ((driver (d:start :immediate :crud)))
    (assert-eq :group (s:node-role (tree driver)))
    (dolist (name '("Filter prefix:" "Name:" "Surname:"))
      (let ((field (the-node driver :role :text-field :name name)))
        (assert-equal "" (s:node-value field) name)))
    (dolist (name '("Create" "Update" "Delete"))
      (assert-eq :button (s:node-role (the-node driver :role :button :name name)) name))
    (let ((list (the-node driver :role :list)))
      (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman")
                    (getf (s:node-state list) :items))
      (assert-false (s:node-value list)))))

(define-test what-is-not-possible-yet-is-disabled-and-what-the-person-did-is-in-the-state ()
  (let ((driver (d:start :immediate :crud)))
    (assert-true (s:node-property (the-node driver :role :button :name "Update") :disabled))
    (assert-true (s:node-property (the-node driver :role :button :name "Delete") :disabled))
    (d:click driver :name)
    (d:type-text driver "Anna")
    (let ((field (the-node driver :role :text-field :name "Name:")))
      (assert-equal "Anna" (s:node-value field))
      (assert-true (s:node-property field :focused))
      (assert-eql 4 (s:node-property field :caret)))
    (assert-false (s:node-property (the-node driver :role :text-field :name "Surname:") :focused))
    (d:click-at driver :list 0.3 0.05)
    (assert-equal "Emil, Hans" (s:node-value (the-node driver :role :list)))
    (assert-false (s:node-property (the-node driver :role :button :name "Update") :disabled))))

(define-test the-bounds-in-the-tree-are-where-the-widget-is ()
  (let ((driver (d:start :immediate :crud)))
    ;; A press in the middle of the field's bounds gives it the keyboard, and the next keys go in it.
    (click-node driver (the-node driver :role :text-field :name "Surname:"))
    (d:type-text driver "Q")
    (assert-equal "Q" (s:node-value (the-node driver :role :text-field :name "Surname:")))
    (assert-equal "" (s:node-value (the-node driver :role :text-field :name "Name:")))
    ;; And in the middle of a button's bounds is the button.
    (click-node driver (the-node driver :role :button :name "Create"))
    (assert-equal 4 (length (getf (s:node-state (the-node driver :role :list)) :items)))))

(define-test every-node-is-inside-its-parent-and-inside-the-window ()
  (dolist (task '(:crud :temperature :timer :flight :circles :cells :layout :dialog))
    (let* ((driver (d:start :immediate task))
           (tree (tree driver)))
      (destructuring-bind (x y width height) (s:node-bounds tree)
        (declare (ignore x y))
        (s:walk-nodes (lambda (node)
                        (destructuring-bind (nx ny nw nh) (s:node-bounds node)
                          (assert-true (and (>= nx -0.01) (>= ny -0.01)
                                            (<= (+ nx nw) (+ width 0.01))
                                            (<= (+ ny nh) (+ height 0.01)))
                                       task (s:node-id node) (s:node-bounds node))))
                      tree)))))

(define-test the-temperature-fields-are-named-by-the-labels-beside-them ()
  (let ((driver (d:start :immediate :temperature)))
    (d:click driver :celsius)
    (d:type-text driver "100")
    (assert-equal "100" (s:node-value (the-node driver :role :text-field :name "Celsius =")))
    (assert-equal "212" (s:node-value (the-node driver :role :text-field :name "Fahrenheit")))))

(define-test the-timer-has-a-slider-with-its-range-and-a-bar-that-fills ()
  (let ((driver (d:start :immediate :timer)))
    (let ((slider (the-node driver :role :slider :name "Duration:")))
      (assert-eql 10d0 (s:node-value slider))
      (assert-eql 1d0 (s:node-property slider :minimum))
      (assert-eql 30d0 (s:node-property slider :maximum)))
    (assert-eql 0d0 (s:node-value (the-node driver :role :progress-bar :name "Elapsed time:")))
    (d:advance-time driver 5d0)
    (assert-true (< 0.4d0 (s:node-value (the-node driver :role :progress-bar)) 0.6d0))))

(define-test the-flight-booker-has-a-choice-with-its-options-and-the-one-chosen ()
  (let ((driver (d:start :immediate :flight)))
    (let ((choice (the-node driver :role :radio-group)))
      (assert-equal "one-way flight" (s:node-value choice))
      (assert-equal '("one-way flight" "return flight") (s:node-property choice :options)))
    (d:click-at driver :kind 0.75 0.5)
    (assert-equal "return flight" (s:node-value (the-node driver :role :radio-group)))))
