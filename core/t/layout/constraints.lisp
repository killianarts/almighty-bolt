(defpackage #:almighty-bolt-core/t/layout/constraints
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:c #:almighty-bolt-core/layout/constraints)))

(in-package #:almighty-bolt-core/t/layout/constraints)

;;; These run with float traps on, as the test runner leaves them. A host masks
;;; them, so a NaN or an infinity that a trap would catch here would go unnoticed in
;;; a window.

(defun constraints (&rest values)
  "A list of the four values of a constraints function, for comparing."
  values)

(define-test constrain-brings-a-size-within-its-limits ()
  (assert-eql 10f0 (c:constrain 5f0 10f0 20f0))
  (assert-eql 15f0 (c:constrain 15f0 10f0 20f0))
  (assert-eql 20f0 (c:constrain 25f0 10f0 20f0))
  (assert-eql 20f0 (c:constrain c:+unbounded+ 10f0 20f0))
  (assert-eql 7f0 (c:constrain 7f0 0f0 c:+unbounded+)))

(define-test constrain-size-does-both-axes ()
  (assert-equal '(10f0 50f0) (multiple-value-list (c:constrain-size 5f0 80f0 10f0 20f0 0f0 50f0))))

(define-test only-the-largest-float-is-unbounded ()
  (assert-true (c:unbounded-p c:+unbounded+))
  (assert-false (c:unbounded-p 1f30))
  (assert-false (c:unbounded-p 0f0)))

(define-test constraints-are-valid-when-ordered-and-not-negative ()
  (assert-true (c:valid-constraints-p 0f0 10f0 0f0 10f0))
  (assert-true (c:valid-constraints-p 10f0 10f0 0f0 c:+unbounded+))
  (assert-false (c:valid-constraints-p 5f0 3f0 0f0 1f0))
  (assert-false (c:valid-constraints-p 0f0 1f0 5f0 3f0))
  (assert-false (c:valid-constraints-p -1f0 1f0 0f0 1f0)))

(define-test loosening-takes-the-minimums-away ()
  (assert-equal (constraints 0f0 100f0 0f0 50f0)
                (multiple-value-list (c:loosen-constraints 20f0 100f0 10f0 50f0))))

(define-test tightening-forces-a-size-within-the-limits ()
  (assert-equal (constraints 30f0 30f0 0f0 50f0)
                (multiple-value-list (c:tighten-constraints 0f0 100f0 0f0 50f0 30f0 nil)))
  (assert-equal (constraints 100f0 100f0 20f0 20f0)
                (multiple-value-list (c:tighten-constraints 0f0 100f0 0f0 50f0 200f0 20f0)))
  (assert-equal (constraints 10f0 100f0 5f0 50f0)
                (multiple-value-list (c:tighten-constraints 10f0 100f0 5f0 50f0 nil nil))))

(define-test deflating-takes-the-space-from-both-ends ()
  (assert-equal (constraints 6f0 96f0 3f0 48f0)
                (multiple-value-list (c:deflate-constraints 10f0 100f0 5f0 50f0 4f0 2f0))))

(define-test deflating-never-goes-below-zero-or-below-the-minimum ()
  (assert-equal (constraints 0f0 0f0 0f0 0f0)
                (multiple-value-list (c:deflate-constraints 50f0 60f0 5f0 6f0 70f0 10f0)))
  (assert-equal (constraints 30f0 35f0 0f0 0f0)
                (multiple-value-list (c:deflate-constraints 50f0 55f0 5f0 6f0 20f0 10f0))))

(define-test deflating-leaves-an-unbounded-maximum-unbounded ()
  (assert-equal (constraints 0f0 c:+unbounded+ 0f0 c:+unbounded+)
                (multiple-value-list
                 (c:deflate-constraints 0f0 c:+unbounded+ 0f0 c:+unbounded+ 10f0 10f0))))
