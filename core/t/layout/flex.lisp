(defpackage #:almighty-bolt-core/t/layout/flex
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:flex #:almighty-bolt-core/layout/flex)
                    (#:c #:almighty-bolt-core/layout/constraints)))

(in-package #:almighty-bolt-core/t/layout/flex)

;;; These run with float traps on, as the test runner leaves them (see the note in
;;; the constraints tests).

(defun flexes (&rest specs)
  "A FLEX-OF function from a list of (factor tight) pairs, one for each child."
  (lambda (i)
    (destructuring-bind (factor tight) (nth i specs)
      (values (float factor 1f0) tight))))

(defun lay-out (axis sizes constraints &rest options)
  "Lay out children that want SIZES, a list of (width height), within CONSTRAINTS, a
list of min-w, max-w, min-h and max-h. Returns the flex's width and height, the
placements as a list of (x y) in child order, and the calls to measure as a list of
(i min-w max-w min-h max-h) in the order they were made."
  (destructuring-bind (min-w max-w min-h max-h) constraints
    (let ((calls '())
          (placements (make-array (length sizes) :initial-element nil)))
      (multiple-value-bind (width height)
          (apply #'flex:layout-flex axis min-w max-w min-h max-h (length sizes)
                 (lambda (i a b c d)
                   (push (list i a b c d) calls)
                   (destructuring-bind (want-w want-h) (nth i sizes)
                     (values (c:constrain want-w a b) (c:constrain want-h c d))))
                 (lambda (i x y)
                   (setf (aref placements i) (list x y)))
                 options)
        (values width height (coerce placements 'list) (reverse calls))))))

(defparameter +row-sizes+ '((10f0 5f0) (20f0 8f0) (30f0 6f0)))

(defun xs (placements)
  (mapcar #'first placements))

(defun ys (placements)
  (mapcar #'second placements))

;;; --- a row of children that do not flex ---

(define-test a-row-places-children-one-after-another ()
  (multiple-value-bind (w h placements)
      (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0) :main-size :min)
    (assert-eql 60f0 w)
    (assert-eql 8f0 h)
    (assert-equal '((0f0 0f0) (10f0 0f0) (30f0 0f0)) placements)))

(define-test a-row-fills-its-main-axis-unless-told-to-be-small ()
  (multiple-value-bind (w h) (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0))
    (assert-eql 100f0 w)
    (assert-eql 8f0 h)))

(define-test children-that-do-not-flex-are-measured-with-all-the-room-on-the-main-axis ()
  (multiple-value-bind (w h placements calls)
      (lay-out :horizontal '((10f0 5f0)) '(0f0 100f0 0f0 50f0))
    (declare (ignore w h placements))
    (assert-equal (list (list 0 0f0 c:+unbounded+ 0f0 50f0)) calls)))

(define-test the-main-alignment-shares-out-what-is-left ()
  (flet ((xs-for (alignment)
           (xs (nth-value 2 (lay-out :horizontal +row-sizes+ '(0f0 90f0 0f0 50f0)
                                     :main-align alignment)))))
    (assert-equal '(0f0 10f0 30f0) (xs-for :start))
    (assert-equal '(30f0 40f0 60f0) (xs-for :end))
    (assert-equal '(15f0 25f0 45f0) (xs-for :center))
    (assert-equal '(0f0 25f0 60f0) (xs-for :space-between))
    (assert-equal '(5f0 25f0 55f0) (xs-for :space-around))
    (assert-equal '(7.5f0 25f0 52.5f0) (xs-for :space-evenly))))

(define-test a-gap-goes-between-children-and-counts-in-the-size ()
  (multiple-value-bind (w h placements)
      (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0) :main-size :min :gap 5f0)
    (declare (ignore h))
    (assert-eql 70f0 w)
    (assert-equal '(0f0 15f0 40f0) (xs placements))))

(define-test the-cross-alignment-places-children-across-the-tallest ()
  (flet ((ys-for (alignment)
           (ys (nth-value 2 (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0)
                                     :main-size :min :cross-align alignment)))))
    (assert-equal '(0f0 0f0 0f0) (ys-for :start))
    (assert-equal '(3f0 0f0 2f0) (ys-for :end))
    (assert-equal '(1.5f0 0f0 1f0) (ys-for :center))))

(define-test stretching-makes-children-as-tall-as-the-cross-axis-allows ()
  (multiple-value-bind (w h placements calls)
      (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0)
               :main-size :min :cross-align :stretch)
    (declare (ignore w))
    (assert-eql 50f0 h)
    (assert-equal '(0f0 0f0 0f0) (ys placements))
    ;; Every child was told to be as tall as the cross axis allows.
    (assert-equal '(50f0 50f0 50f0) (mapcar (lambda (call) (nth 3 call)) calls))
    (assert-equal '(50f0 50f0 50f0) (mapcar (lambda (call) (nth 4 call)) calls))))

;;; --- children that flex ---

(define-test children-that-flex-share-what-is-left-by-their-factors ()
  (multiple-value-bind (w h placements calls)
      (lay-out :horizontal '((10f0 5f0) (0f0 5f0) (0f0 5f0)) '(0f0 100f0 0f0 50f0)
               :flex-of (flexes '(0 nil) '(1 t) '(2 t)))
    (declare (ignore h))
    (assert-eql 100f0 w)
    (assert-equal '(0f0 10f0 40f0) (xs placements))
    ;; The child that does not flex is measured first, then the others in order,
    ;; each with its share as both minimum and maximum.
    (assert-equal (list (list 0 0f0 c:+unbounded+ 0f0 50f0)
                        (list 1 30f0 30f0 0f0 50f0)
                        (list 2 60f0 60f0 0f0 50f0))
                  calls)))

(define-test a-loose-child-may-take-less-than-its-share ()
  (multiple-value-bind (w h placements calls)
      (lay-out :horizontal '((10f0 5f0) (5f0 5f0)) '(0f0 100f0 0f0 50f0)
               :flex-of (flexes '(0 nil) '(1 nil)))
    (declare (ignore h))
    (assert-eql 100f0 w)
    (assert-equal '(0f0 10f0) (xs placements))
    (assert-equal (list 1 0f0 90f0 0f0 50f0) (second calls))))

(define-test the-gaps-come-out-of-the-space-that-flexes ()
  (multiple-value-bind (w h placements)
      (lay-out :horizontal '((10f0 5f0) (0f0 5f0) (0f0 5f0)) '(0f0 100f0 0f0 50f0)
               :gap 5f0 :flex-of (flexes '(0 nil) '(1 t) '(1 t)))
    (declare (ignore h))
    (assert-eql 100f0 w)
    (assert-equal '(0f0 15f0 60f0) (xs placements))))

(define-test no-space-is-left-for-flexing-when-the-others-take-it-all ()
  (multiple-value-bind (w h placements calls)
      (lay-out :horizontal '((120f0 5f0) (0f0 5f0)) '(0f0 100f0 0f0 50f0)
               :flex-of (flexes '(0 nil) '(1 t)))
    (declare (ignore h))
    (assert-eql 100f0 w)
    (assert-equal '(0f0 120f0) (xs placements))
    (assert-equal (list 1 0f0 0f0 0f0 50f0) (second calls))))

(define-test a-flex-with-no-upper-limit-cannot-share-space ()
  (assert-error 'flex:unbounded-flex
      (lay-out :horizontal '((10f0 5f0) (0f0 5f0)) (list 0f0 c:+unbounded+ 0f0 50f0)
               :flex-of (flexes '(0 nil) '(1 t))))
  (assert-eql :vertical
              (handler-case (lay-out :vertical '((0f0 5f0)) (list 0f0 50f0 0f0 c:+unbounded+)
                                     :flex-of (flexes '(1 t)))
                (flex:unbounded-flex (condition) (flex:unbounded-flex-axis condition)))))

(define-test an-unbounded-main-axis-is-as-large-as-the-children ()
  (multiple-value-bind (w h placements)
      (lay-out :horizontal +row-sizes+ (list 0f0 c:+unbounded+ 0f0 50f0))
    (declare (ignore h))
    (assert-eql 60f0 w)
    (assert-equal '(0f0 10f0 30f0) (xs placements))))

;;; --- a column ---

(define-test a-column-is-a-row-turned-on-its-side ()
  (multiple-value-bind (w h placements calls)
      (lay-out :vertical '((5f0 10f0) (8f0 20f0) (6f0 30f0)) '(0f0 50f0 0f0 100f0)
               :main-size :min :cross-align :end)
    (assert-eql 8f0 w)
    (assert-eql 60f0 h)
    (assert-equal '((3f0 0f0) (0f0 10f0) (2f0 30f0)) placements)
    ;; Constraints go to the child as min-w, max-w, min-h, max-h whatever the axis.
    (assert-equal (list 0 0f0 50f0 0f0 c:+unbounded+) (first calls))))

(define-test a-column-shares-space-down-the-page ()
  (multiple-value-bind (w h placements calls)
      (lay-out :vertical '((5f0 10f0) (5f0 0f0)) '(0f0 50f0 0f0 100f0)
               :flex-of (flexes '(0 nil) '(1 t)))
    (assert-eql 5f0 w)
    (assert-eql 100f0 h)
    (assert-equal '(10f0) (last (ys placements)))
    (assert-equal (list 1 0f0 50f0 90f0 90f0) (second calls))))

;;; --- edges ---

(define-test no-children-is-the-smallest-the-constraints-allow ()
  (multiple-value-bind (w h placements) (lay-out :horizontal '() '(10f0 100f0 20f0 50f0) :main-size :min)
    (assert-eql 10f0 w)
    (assert-eql 20f0 h)
    (assert-equal '() placements))
  (assert-eql 100f0 (lay-out :horizontal '() '(10f0 100f0 20f0 50f0))))

(define-test children-wider-than-the-room-overflow-without-being-moved-back ()
  (multiple-value-bind (w h placements)
      (lay-out :horizontal '((60f0 5f0) (60f0 5f0)) '(0f0 100f0 0f0 50f0) :main-align :center)
    (declare (ignore h))
    (assert-eql 100f0 w)
    (assert-equal '(0f0 60f0) (xs placements))))

(define-test an-option-that-is-not-one-of-the-choices-is-an-error ()
  (assert-error 'error (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0) :main-align :bogus))
  (assert-error 'error (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0) :cross-align :bogus))
  (assert-error 'error (lay-out :horizontal +row-sizes+ '(0f0 100f0 0f0 50f0) :main-size :bogus)))

(define-test a-flex-of-many-children-lays-out-like-any-other ()
  (let ((sizes (loop :repeat 300 :collect '(2f0 3f0))))
    (multiple-value-bind (w h placements)
        (lay-out :horizontal sizes '(0f0 1000f0 0f0 50f0) :main-size :min)
      (assert-eql 600f0 w)
      (assert-eql 3f0 h)
      (assert-eql 598f0 (first (car (last placements)))))))

;;; --- properties ---

(defvar *seed* 12345)

(defun random-below (n)
  "A number from 0 below N from a generator that does not change between runs."
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) 2147483648))
  (mod (ash *seed* -8) n))

(defun random-size (limit)
  "A multiple of a quarter, from 0 to LIMIT, so that sums and halves are exact."
  (* 0.25f0 (random-below (1+ (* 4 limit)))))

(defun random-constraint (unbounded-chance)
  (let* ((minimum (if (zerop (random-below 2)) 0f0 (random-size 40)))
         (maximum (if (zerop (random-below unbounded-chance))
                      c:+unbounded+
                      (+ minimum (random-size 100)))))
    (values minimum maximum)))

(defun random-choice (&rest choices)
  (nth (random-below (length choices)) choices))

(define-test whatever-the-constraints-a-flex-stays-inside-them-and-measures-each-child-once ()
  (setf *seed* 12345)
  (dotimes (case-number 3000)
    (let* ((axis (random-choice :horizontal :vertical))
           (count (random-below 9))
           (sizes (loop :repeat count :collect (list (random-size 80) (random-size 80))))
           (specs (loop :repeat count
                        :collect (list (random-choice 0 0 1 2) (zerop (random-below 2)))))
           (calls '())
           (placed (make-array count :initial-element 0))
           (min-w 0f0) (max-w 0f0) (min-h 0f0) (max-h 0f0))
      (setf (values min-w max-w) (random-constraint 4)
            (values min-h max-h) (random-constraint 4))
      (let ((unbounded-main (c:unbounded-p (if (eq axis :horizontal) max-w max-h)))
            (flexing (some (lambda (spec) (plusp (first spec))) specs)))
        (flet ((run ()
                 (flex:layout-flex axis min-w max-w min-h max-h count
                                   (lambda (i a b c d)
                                     (push (list i a b c d) calls)
                                     (destructuring-bind (want-w want-h) (nth i sizes)
                                       (values (c:constrain want-w a b) (c:constrain want-h c d))))
                                   (lambda (i x y)
                                     (declare (ignore x y))
                                     (incf (aref placed i)))
                                   :flex-of (apply #'flexes specs)
                                   :main-align (random-choice :start :end :center :space-between
                                                              :space-around :space-evenly)
                                   :cross-align (random-choice :start :end :center :stretch)
                                   :main-size (random-choice :max :min)
                                   :gap (random-size 10))))
          (if (and unbounded-main flexing)
              (assert-error 'flex:unbounded-flex (run))
              (multiple-value-bind (w h) (run)
                (let ((context (list case-number axis min-w max-w min-h max-h)))
                  (assert-true (<= min-w w max-w) context)
                  (assert-true (<= min-h h max-h) context)
                  (assert-equal (loop :for i :below count :collect i)
                                (sort (mapcar #'first calls) #'<) context)
                  (assert-equal (loop :repeat count :collect 1) (coerce placed 'list) context)
                  (dolist (call calls)
                    (assert-true (apply #'c:valid-constraints-p (rest call))
                                 call context))))))))))
