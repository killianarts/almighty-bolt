(defpackage #:almighty-bolt/layout/constraints
  (:use #:cl)
  (:export #:+unbounded+
           #:unbounded-p
           #:valid-constraints-p
           #:constrain
           #:constrain-size
           #:loosen-constraints
           #:tighten-constraints
           #:deflate-constraints))

(in-package #:almighty-bolt/layout/constraints)

;;; Box constraints, the arguments of Flutter-style layout: constraints go down to a
;;; child and a size comes back up. A
;;; constraint is a minimum and a maximum for the width and the same for the height,
;;; four single-floats in points, always passed as four arguments and returned as
;;; four values. There is no constraints structure, because making one for every
;;; child on every frame would put allocation in code that runs for every frame,
;;; and single-floats are immediate objects in SBCL, so these functions allocate
;;; nothing.
;;;
;;; An unbounded maximum is MOST-POSITIVE-SINGLE-FLOAT and not an infinity, so that
;;; this code is portable and nothing can reach a NaN: (- inf inf) and (* 0 inf)
;;; trap or give one. Anything that does arithmetic on a maximum asks UNBOUNDED-P first.

(defconstant +unbounded+ most-positive-single-float
  "The maximum of a constraint that has no upper limit.")

(declaim (inline unbounded-p constrain constrain-size))

(defun unbounded-p (maximum)
  "True if MAXIMUM is the limit of a constraint with no upper limit."
  (declare (type single-float maximum))
  (>= maximum +unbounded+))

(defun valid-constraints-p (min-w max-w min-h max-h)
  "True if the minimums are not negative and no minimum is above its maximum."
  (declare (type single-float min-w max-w min-h max-h))
  (and (<= 0f0 min-w max-w) (<= 0f0 min-h max-h)))

(defun constrain (size minimum maximum)
  "SIZE brought within MINIMUM and MAXIMUM."
  (declare (type single-float size minimum maximum))
  (max minimum (min size maximum)))

(defun constrain-size (width height min-w max-w min-h max-h)
  "WIDTH and HEIGHT, each brought within its constraints, as two values."
  (declare (type single-float width height min-w max-w min-h max-h))
  (values (constrain width min-w max-w) (constrain height min-h max-h)))

(defun loosen-constraints (min-w max-w min-h max-h)
  "The same maximums with the minimums taken away, as four values: a child may now
be as small as it likes."
  (declare (type single-float min-w max-w min-h max-h)
           (ignore min-w min-h))
  (values 0f0 max-w 0f0 max-h))

(defun tighten-constraints (min-w max-w min-h max-h width height)
  "Constraints that force WIDTH and HEIGHT, as four values. Each is a single-float,
brought within the constraints it is given, or NIL to leave that axis as it was."
  (declare (type single-float min-w max-w min-h max-h)
           (type (or null single-float) width height))
  (multiple-value-bind (new-min-w new-max-w)
      (if width
          (let ((w (constrain width min-w max-w))) (values w w))
          (values min-w max-w))
    (multiple-value-bind (new-min-h new-max-h)
        (if height
            (let ((h (constrain height min-h max-h))) (values h h))
            (values min-h max-h))
      (values new-min-w new-max-w new-min-h new-max-h))))

(defun deflate-constraints (min-w max-w min-h max-h dx dy)
  "The constraints a child gets when DX of the width and DY of the height are taken
by something else, as four values. The minimums go down but not below zero, the
maximums go down but not below the minimums, and an unbounded maximum stays so."
  (declare (type single-float min-w max-w min-h max-h dx dy))
  (let ((new-min-w (max 0f0 (- min-w dx)))
        (new-min-h (max 0f0 (- min-h dy))))
    (values new-min-w
            (if (unbounded-p max-w) max-w (max new-min-w (- max-w dx)))
            new-min-h
            (if (unbounded-p max-h) max-h (max new-min-h (- max-h dy))))))
