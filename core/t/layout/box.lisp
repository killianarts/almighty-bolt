(defpackage #:almighty-bolt-core/t/layout/box
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:box #:almighty-bolt-core/layout/box)
                    (#:c #:almighty-bolt-core/layout/constraints)))

(in-package #:almighty-bolt-core/t/layout/box)

(defvar *seen* nil
  "The constraints the last leaf was measured with: min-w, max-w, min-h, max-h.")

(defun leaf (width height)
  "A child that wants WIDTH by HEIGHT, as near as the constraints allow."
  (lambda (min-w max-w min-h max-h)
    (setf *seen* (list min-w max-w min-h max-h))
    (values (c:constrain width min-w max-w) (c:constrain height min-h max-h))))

;;; --- padding ---

(define-test padding-adds-space-around-the-child-and-shrinks-its-room ()
  (assert-equal '(32f0 15f0 5f0 2f0)
                (multiple-value-list
                 (box:layout-padding 0f0 100f0 0f0 50f0 (leaf 20f0 10f0)
                                     :left 5f0 :top 2f0 :right 7f0 :bottom 3f0)))
  (assert-equal '(0f0 88f0 0f0 45f0) *seen*))

(define-test padding-with-tight-constraints-fills-them ()
  (assert-equal '(100f0 50f0 6f0 6f0)
                (multiple-value-list
                 (box:layout-padding 100f0 100f0 50f0 50f0 (leaf 1f0 1f0)
                                     :left 6f0 :top 6f0 :right 6f0 :bottom 6f0)))
  (assert-equal '(88f0 88f0 38f0 38f0) *seen*))

(define-test padding-without-a-child-is-just-the-space ()
  (assert-equal '(10f0 0f0 5f0 0f0)
                (multiple-value-list
                 (box:layout-padding 0f0 100f0 0f0 100f0 nil :left 5f0 :right 5f0))))

(define-test padding-passes-an-unbounded-maximum-on ()
  (assert-equal '(30f0 60f0 5f0 5f0)
                (multiple-value-list
                 (box:layout-padding 0f0 c:+unbounded+ 0f0 c:+unbounded+
                                     (leaf 20f0 50f0)
                                     :left 5f0 :top 5f0 :right 5f0 :bottom 5f0)))
  (assert-equal (list 0f0 c:+unbounded+ 0f0 c:+unbounded+) *seen*))

(define-test padding-larger-than-the-room-leaves-the-child-nothing ()
  (assert-equal '(10f0 0f0 8f0 0f0)
                (multiple-value-list
                 (box:layout-padding 0f0 10f0 0f0 100f0 (leaf 30f0 0f0)
                                     :left 8f0 :right 8f0)))
  (assert-equal '(0f0 0f0 0f0 100f0) *seen*))

;;; --- sized ---

(define-test sized-forces-the-child-on-the-axes-it-is-given ()
  (assert-equal '(40f0 5f0 0f0 0f0)
                (multiple-value-list
                 (box:layout-sized 0f0 100f0 0f0 50f0 (leaf 5f0 5f0) :width 40f0)))
  (assert-equal '(40f0 40f0 0f0 50f0) *seen*))

(define-test sized-stays-within-the-constraints ()
  (assert-equal '(100f0 50f0 0f0 0f0)
                (multiple-value-list
                 (box:layout-sized 0f0 100f0 0f0 50f0 (leaf 5f0 5f0)
                                   :width 200f0 :height 80f0))))

(define-test sized-without-a-child-is-the-size-asked-for ()
  (assert-equal '(30f0 20f0 0f0 0f0)
                (multiple-value-list
                 (box:layout-sized 0f0 100f0 0f0 50f0 nil :width 30f0 :height 20f0)))
  (assert-equal '(0f0 0f0 0f0 0f0)
                (multiple-value-list (box:layout-sized 0f0 100f0 0f0 50f0 nil))))

;;; --- align ---

(define-test align-centres-a-child-in-all-the-room ()
  (assert-equal '(100f0 50f0 40f0 20f0)
                (multiple-value-list (box:layout-align 0f0 100f0 0f0 50f0 (leaf 20f0 10f0))))
  (assert-equal '(0f0 100f0 0f0 50f0) *seen*))

(define-test align-loosens-the-constraints-for-the-child ()
  (box:layout-align 50f0 100f0 20f0 50f0 (leaf 20f0 10f0))
  (assert-equal '(0f0 100f0 0f0 50f0) *seen*))

(define-test align-takes-the-childs-size-on-an-unbounded-axis ()
  (assert-equal '(20f0 50f0 0f0 20f0)
                (multiple-value-list
                 (box:layout-align 0f0 c:+unbounded+ 0f0 50f0 (leaf 20f0 10f0)))))

(define-test align-puts-the-child-at-either-edge ()
  (assert-equal '(100f0 50f0 0f0 0f0)
                (multiple-value-list
                 (box:layout-align 0f0 100f0 0f0 50f0 (leaf 20f0 10f0)
                                   :align-x -1f0 :align-y -1f0)))
  (assert-equal '(100f0 50f0 80f0 40f0)
                (multiple-value-list
                 (box:layout-align 0f0 100f0 0f0 50f0 (leaf 20f0 10f0)
                                   :align-x 1f0 :align-y 1f0))))

(define-test align-with-a-factor-sizes-the-box-from-the-child ()
  (assert-equal '(40f0 50f0 10f0 20f0)
                (multiple-value-list
                 (box:layout-align 0f0 100f0 0f0 50f0 (leaf 20f0 10f0) :width-factor 2f0))))

(define-test align-without-a-child-is-all-the-room-or-nothing ()
  ;; With nothing to place, the box is the whole room, and where the nothing goes is
  ;; the middle of it.
  (assert-equal '(100f0 50f0 50f0 25f0)
                (multiple-value-list (box:layout-align 0f0 100f0 0f0 50f0 nil)))
  (assert-equal '(0f0 50f0 0f0 25f0)
                (multiple-value-list (box:layout-align 0f0 c:+unbounded+ 0f0 50f0 nil))))
