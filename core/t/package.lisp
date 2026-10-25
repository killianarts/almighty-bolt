(defpackage #:almighty-bolt-core/t/package
  (:use #:cl #:lisp-unit2))

(in-package #:almighty-bolt-core/t/package)

;;; The umbrella package must offer exactly what the portable modules export, as
;;; the very same symbols. A symbol added to a module and forgotten in
;;; src/package.lisp shows up here.

(defparameter +modules+
  '(:almighty-bolt-core/clock
    :almighty-bolt-core/ring
    :almighty-bolt-core/event
    :almighty-bolt-core/text-edit
    :almighty-bolt-core/display-list
    :almighty-bolt-core/variant
    :almighty-bolt-core/settle
    :almighty-bolt-core/layout/constraints
    :almighty-bolt-core/layout/box
    :almighty-bolt-core/layout/flex))

(defun external-symbols (package)
  (let ((symbols '()))
    (do-external-symbols (symbol package symbols)
      (push symbol symbols))))

(define-test the-umbrella-re-exports-every-symbol-of-the-portable-modules ()
  (dolist (module +modules+)
    (dolist (symbol (external-symbols module))
      (multiple-value-bind (found status) (find-symbol (symbol-name symbol) :almighty-bolt-core)
        (assert-true (eq symbol found) symbol)
        (assert-eql :external status symbol)))))

(define-test the-umbrella-exports-nothing-else ()
  (let ((from-modules (loop :for module :in +modules+
                            :append (external-symbols module))))
    (dolist (symbol (external-symbols :almighty-bolt-core))
      (assert-true (member symbol from-modules) symbol))))

(define-test the-umbrella-package-is-usable ()
  (assert-true (plusp (almighty-bolt-core:now-ns)))
  (assert-eql 0 (almighty-bolt-core:display-list-count (almighty-bolt-core:make-display-list :capacity 2))))
