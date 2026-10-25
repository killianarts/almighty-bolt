(defpackage #:almighty-bolt-core/wheel-lock
  (:use #:cl)
  (:import-from #:almighty-bolt-core/event
                #:event-timestamp
                #:wheel-event-dx
                #:wheel-event-dy
                #:wheel-event-precise-p
                #:wheel-event-lines
                #:+points-per-line+)
  (:export #:axis-lock
           #:make-axis-lock
           #:lock-wheel))

(in-package #:almighty-bolt-core/wheel-lock)

;;; A finger on a trackpad never moves along one axis only: a swipe meant to go across
;;; carries a fifth of its length down, and a swipe meant to go down drifts a little
;;; sideways. A view that scrolls both ways would move diagonally. Like the platform's own
;;; scroll views, a gesture is locked to the axis it mostly moves along: the first points of
;;; a gesture are added up for each axis, the larger is chosen as soon as it has 4 points,
;;; and what the other axis gave, before and after, is dropped. A gesture is the events that
;;; follow each other without a pause of 150 ms, the momentum after a flick included. A
;;; wheel's events (in lines) have one axis each and are not touched.

(defconstant +gesture-gap-ns+ 150000000
  "A pause this long between wheel events ends a gesture.")

(defconstant +decision-points+ 4d0
  "How far, in points, a gesture must have gone along an axis for the axis to be locked.")

(defstruct (axis-lock (:constructor make-axis-lock ()))
  "What is known of the gesture in progress."
  (started nil)
  (last-ns 0 :type (unsigned-byte 64))
  (axis nil :type (member nil :x :y))
  (sum-x 0d0 :type double-float)
  (sum-y 0d0 :type double-float))

(defun lock-wheel (lock event)
  "The scroll of the wheel EVENT in lines, as two values, across and down, with a trackpad's
(precise, in points) movement kept to the axis its gesture is locked to, and the other
given as zero. LOCK remembers the gesture between calls and belongs to one variant."
  (if (not (wheel-event-precise-p event))
      (wheel-event-lines event)
      (let ((ns (event-timestamp event))
            (dx (wheel-event-dx event))
            (dy (wheel-event-dy event)))
        (when (or (not (axis-lock-started lock))
                  (> (abs (- ns (axis-lock-last-ns lock))) +gesture-gap-ns+))
          (setf (axis-lock-axis lock) nil
                (axis-lock-sum-x lock) 0d0
                (axis-lock-sum-y lock) 0d0))
        (setf (axis-lock-started lock) t
              (axis-lock-last-ns lock) ns)
        (unless (axis-lock-axis lock)
          (incf (axis-lock-sum-x lock) (abs dx))
          (incf (axis-lock-sum-y lock) (abs dy))
          (when (>= (max (axis-lock-sum-x lock) (axis-lock-sum-y lock)) +decision-points+)
            (setf (axis-lock-axis lock)
                  (if (> (axis-lock-sum-x lock) (axis-lock-sum-y lock)) :x :y))))
        ;; Until the axis is locked, the one that has gone further so far is the one used.
        (let ((axis (or (axis-lock-axis lock)
                        (if (> (axis-lock-sum-x lock) (axis-lock-sum-y lock)) :x :y))))
          (if (eq axis :x)
              (values (/ dx +points-per-line+) 0d0)
              (values 0d0 (/ dy +points-per-line+)))))))
