(defpackage #:almighty-bolt/layout/box
  (:use #:cl)
  (:import-from #:almighty-bolt/layout/constraints
                #:unbounded-p
                #:constrain
                #:loosen-constraints
                #:tighten-constraints
                #:deflate-constraints)
  (:export #:layout-padding
           #:layout-sized
           #:layout-align))

(in-package #:almighty-bolt/layout/box)

;;; Layout for one child (decision record 0010). Each function takes the
;;; constraints it was given, a MEASURE function for its child, and options, and
;;; returns four values: its own width and height, and the x and y of the child
;;; inside it. MEASURE takes the minimum and maximum width and height the child may
;;; have and returns the child's width and height. It is called once, and a NIL
;;; MEASURE means there is no child. The functions know nothing about what the child
;;; is: a variant measures a node of its own, or a text run, or another layout.

(defun layout-padding (min-w max-w min-h max-h measure
                       &key (left 0f0) (top 0f0) (right 0f0) (bottom 0f0))
  "A child with LEFT, TOP, RIGHT and BOTTOM of space around it. The child gets the
constraints less the space, and the box is as large as the child and the space."
  (declare (type single-float min-w max-w min-h max-h left top right bottom)
           (type (or null function) measure))
  (let ((dx (+ left right))
        (dy (+ top bottom)))
    (if measure
        (multiple-value-bind (child-min-w child-max-w child-min-h child-max-h)
            (deflate-constraints min-w max-w min-h max-h dx dy)
          (multiple-value-bind (child-w child-h)
              (funcall measure child-min-w child-max-w child-min-h child-max-h)
            (declare (type single-float child-w child-h))
            (values (constrain (+ child-w dx) min-w max-w)
                    (constrain (+ child-h dy) min-h max-h)
                    left top)))
        (values (constrain dx min-w max-w) (constrain dy min-h max-h) left top))))

(defun layout-sized (min-w max-w min-h max-h measure &key width height)
  "A child forced to WIDTH and HEIGHT, as far as the constraints allow. Either may
be NIL, which leaves that axis to the constraints. The child is at the corner."
  (declare (type single-float min-w max-w min-h max-h)
           (type (or null single-float) width height)
           (type (or null function) measure))
  (multiple-value-bind (child-min-w child-max-w child-min-h child-max-h)
      (tighten-constraints min-w max-w min-h max-h width height)
    (if measure
        (multiple-value-bind (child-w child-h)
            (funcall measure child-min-w child-max-w child-min-h child-max-h)
          (declare (type single-float child-w child-h))
          (values (constrain child-w min-w max-w) (constrain child-h min-h max-h) 0f0 0f0))
        (values child-min-w child-min-h 0f0 0f0))))

(defun layout-align (min-w max-w min-h max-h measure
                     &key (align-x 0f0) (align-y 0f0) width-factor height-factor)
  "A child placed inside a box that is as large as the constraints allow, or as
small as the child if an axis has no upper limit or its factor is given. ALIGN-X and
ALIGN-Y run from -1 (left, top) to 1 (right, bottom); the default 0 centres. A
factor makes the box that multiple of the child's size on its axis."
  (declare (type single-float min-w max-w min-h max-h align-x align-y)
           (type (or null single-float) width-factor height-factor)
           (type (or null function) measure))
  (let ((shrink-w (or width-factor (unbounded-p max-w)))
        (shrink-h (or height-factor (unbounded-p max-h))))
    (multiple-value-bind (child-w child-h)
        (if measure
            (multiple-value-bind (child-min-w child-max-w child-min-h child-max-h)
                (loosen-constraints min-w max-w min-h max-h)
              (funcall measure child-min-w child-max-w child-min-h child-max-h))
            (values 0f0 0f0))
      (declare (type single-float child-w child-h))
      (let ((width (constrain (if shrink-w (* child-w (or width-factor 1f0)) max-w)
                              min-w max-w))
            (height (constrain (if shrink-h (* child-h (or height-factor 1f0)) max-h)
                               min-h max-h)))
        (values width height
                (* (- width child-w) 0.5f0 (+ 1f0 align-x))
                (* (- height child-h) 0.5f0 (+ 1f0 align-y)))))))
