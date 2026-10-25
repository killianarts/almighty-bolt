(defpackage #:almighty-bolt-core/layout/flex
  (:use #:cl)
  (:import-from #:almighty-bolt-core/layout/constraints
                #:+unbounded+
                #:unbounded-p
                #:constrain-size)
  (:export #:layout-flex
           #:with-layout-scratch
           #:unbounded-flex
           #:unbounded-flex-axis))

(in-package #:almighty-bolt-core/layout/flex)

;;; A row or a column of children (decision record 0010), after Flutter's
;;; RenderFlex. Children that do not flex are measured first, with all the room on
;;; the main axis. What is left is shared among the children that flex, in
;;; proportion to their flex factors. Then the children are placed one after
;;; another along the main axis, with the space that remains divided by the main
;;; alignment, and across it by the cross alignment.
;;;
;;; The function knows nothing of its children. MEASURE and PLACE are called with a
;;; child's index, so a variant keeps its children wherever it likes.
;;;
;;; Between measuring and placing, a flex has to keep the size of each child, and it
;;; keeps them in a buffer that it reuses from one call to the next. The buffer is a
;;; stack of single-floats, two for each child of a flex that is being laid out, so a
;;; flex inside a child's MEASURE uses the part above its parent's. It grows to the
;;; largest flex it has served, once, and after that nothing allocates, whatever
;;; the number of children. It does not depend on the compiler putting anything on
;;; the stack, which SBCL does not do for vectors of floats on platforms with a
;;; precise stack (decision record 0010). It is shared by every flex in a thread, so
;;; two threads that lay out together must each bind their own, with
;;; WITH-LAYOUT-SCRATCH.

(define-condition unbounded-flex (error)
  ((axis :initarg :axis :reader unbounded-flex-axis))
  (:report (lambda (condition stream)
             (format stream "A child of a ~(~a~) flex has a flex factor, but its main ~
                             axis has no upper limit, so there is no space to share. ~
                             Give the flex a bounded size or take the factor away."
                     (unbounded-flex-axis condition)))))

(defvar *scratch* (make-array 1024 :element-type 'single-float)
  "The buffer in which flexes keep their children's sizes.")

(defvar *scratch-top* 0
  "How much of *SCRATCH* the flexes being laid out now are using.")

(declaim (type (simple-array single-float (*)) *scratch*)
         (type fixnum *scratch-top*))

(defmacro with-layout-scratch ((&key (size 1024)) &body body)
  "Run BODY with a buffer of its own for the flexes it lays out, for a thread that
lays out while another does. SIZE is how many floats it starts with, two for each
child of the largest flex; the buffer grows when a flex needs more."
  `(let ((*scratch* (make-array ,size :element-type 'single-float))
         (*scratch-top* 0))
     ,@body))

(defun layout-flex (axis min-w max-w min-h max-h count measure place
                    &key flex-of (main-align :start) (cross-align :start)
                      (main-size :max) (gap 0f0))
  "Lay out COUNT children along AXIS (:horizontal for a row, :vertical for a
column) and return the width and height of the whole as two values.

MEASURE is called as (MEASURE i min-w max-w min-h max-h), once for each child, and
returns the child's width and height. The children that do not flex are measured
first, in order, and then those that do. PLACE is called as (PLACE i x y), once for
each child, in order, after all are measured, with the child's corner in the flex.

FLEX-OF, if given, is called as (FLEX-OF i) and returns a flex factor, zero for a
child that does not flex, and a second value, true if the child must fill the space
it is given and false if it may take less. It may be called twice for a child.
A flex factor with no upper limit on the main axis signals UNBOUNDED-FLEX.

MAIN-ALIGN is :start, :end, :center, :space-between, :space-around or
:space-evenly. CROSS-ALIGN is :start, :end, :center or :stretch (which makes each
child as large as the cross axis allows, if it has an upper limit). MAIN-SIZE is
:max, to fill the main axis if it has an upper limit, or :min, to be as large as the
children and the gaps. GAP is the space between children.

The first flex larger than any before it allocates a larger buffer for the sizes of
its children, and no flex allocates after that (see WITH-LAYOUT-SCRATCH)."
  (declare (type (member :horizontal :vertical) axis)
           (type single-float min-w max-w min-h max-h gap)
           (type (integer 0 #.(floor array-dimension-limit 2)) count)
           (type function measure place)
           (type (or null function) flex-of))
  (let* ((horizontal (eq axis :horizontal))
         (max-main (if horizontal max-w max-h))
         (max-cross (if horizontal max-h max-w))
         (bounded-main (not (unbounded-p max-main)))
         (fill (ecase main-size (:max t) (:min nil)))
         (child-min-cross (if (and (eq cross-align :stretch)
                                   (not (unbounded-p max-cross)))
                              max-cross
                              0f0)))
    (declare (type single-float max-main max-cross child-min-cross))
    (flet ((run (scratch base)
             (declare (type (simple-array single-float (*)) scratch)
                      (type fixnum base))
             (let ((allocated (* gap (float (max 0 (1- count)) 1f0)))
                   (total-flex 0f0)
                   (cross-extent 0f0))
               (declare (type single-float allocated total-flex cross-extent))
               (flet ((measure-child (i child-min-main child-max-main)
                        ;; The child's main and cross sizes go into the scratch space.
                        (declare (type single-float child-min-main child-max-main))
                        (multiple-value-bind (w h)
                            (if horizontal
                                (funcall measure i child-min-main child-max-main
                                         child-min-cross max-cross)
                                (funcall measure i child-min-cross max-cross
                                         child-min-main child-max-main))
                          (declare (type single-float w h))
                          (let ((main (if horizontal w h))
                                (cross (if horizontal h w)))
                            (setf (aref scratch (+ base (* 2 i))) main
                                  (aref scratch (+ base (* 2 i) 1)) cross)
                            main))))
                 ;; Children that do not flex take what they need.
                 (dotimes (i count)
                   (let ((factor (if flex-of (the single-float (funcall (the function flex-of) i)) 0f0)))
                     (if (> factor 0f0)
                         (progn
                           (unless bounded-main
                             (error 'unbounded-flex :axis axis))
                           (incf total-flex factor))
                         (incf allocated (measure-child i 0f0 +unbounded+)))))
                 ;; Those that flex share what is left.
                 (when (> total-flex 0f0)
                   (let ((per-flex (/ (max 0f0 (- max-main allocated)) total-flex)))
                     (declare (type single-float per-flex))
                     (dotimes (i count)
                       (multiple-value-bind (factor tight) (funcall (the function flex-of) i)
                         (declare (type single-float factor))
                         (when (> factor 0f0)
                           (let ((extent (* per-flex factor)))
                             (incf allocated
                                   (measure-child i (if tight extent 0f0) extent))))))))
                 (dotimes (i count)
                   (setf cross-extent
                         (max cross-extent (aref scratch (+ base (* 2 i) 1)))))
                 (multiple-value-bind (width height)
                     (let ((main (if (and fill bounded-main) max-main allocated)))
                       (if horizontal
                           (constrain-size main cross-extent min-w max-w min-h max-h)
                           (constrain-size cross-extent main min-w max-w min-h max-h)))
                   (declare (type single-float width height))
                   (let* ((main-extent (if horizontal width height))
                          (cross-size (if horizontal height width))
                          (remaining (max 0f0 (- main-extent allocated)))
                          (n (float count 1f0)))
                     (declare (type single-float main-extent cross-size remaining n))
                     (multiple-value-bind (position between)
                         (ecase main-align
                           (:start (values 0f0 0f0))
                           (:end (values remaining 0f0))
                           (:center (values (* 0.5f0 remaining) 0f0))
                           (:space-between
                            (values 0f0 (if (> count 1) (/ remaining (- n 1f0)) 0f0)))
                           (:space-around
                            (let ((between (if (> count 0) (/ remaining n) 0f0)))
                              (values (* 0.5f0 between) between)))
                           (:space-evenly
                            (let ((between (/ remaining (+ n 1f0))))
                              (values between between))))
                       (declare (type single-float position between))
                       (dotimes (i count)
                         (let* ((main (aref scratch (+ base (* 2 i))))
                                (cross (aref scratch (+ base (* 2 i) 1)))
                                (offset (ecase cross-align
                                          ((:start :stretch) 0f0)
                                          (:end (- cross-size cross))
                                          (:center (* 0.5f0 (- cross-size cross))))))
                           (declare (type single-float main cross offset))
                           (if horizontal
                               (funcall place i position offset)
                               (funcall place i offset position))
                           (incf position (+ main gap between))))))
                   (values width height))))))
      ;; This flex's sizes go above those of the flexes around it. A larger buffer
      ;; replaces the old one for what is laid out from here on; the flexes that
      ;; are waiting keep the old one, which holds their sizes, and their place in
      ;; it is a number, which is the same in the new one.
      (let* ((base *scratch-top*)
             (top (+ base (* 2 count))))
        (declare (type fixnum base top))
        (when (> top (length *scratch*))
          (setf *scratch* (make-array (max top (* 2 (length *scratch*)))
                                      :element-type 'single-float)))
        (let ((scratch *scratch*)
              (*scratch-top* top))
          (run scratch base))))))
