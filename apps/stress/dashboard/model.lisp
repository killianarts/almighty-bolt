(defpackage #:almighty-bolt-stress/dashboard/model
  (:use #:cl)
  (:export #:+columns+
           #:+rows+
           #:+tiles+
           #:+samples+
           #:+series+
           #:+frame-ns+
           #:step-at
           #:value-index
           #:index-text
           #:tile-name
           #:tile-value-id
           #:tile-bar-id
           #:dashboard
           #:make-dashboard
           #:dashboard-step
           #:dashboard-indices
           #:dashboard-head
           #:dashboard-history
           #:advance-dashboard
           #:value-text
           #:tile-fraction))

(in-package #:almighty-bolt-stress/dashboard/model)

;;; The animated dashboard's data (the second stress app): 36 tiles in a grid, each with a name, a
;;; value that is a number of tenths from 0 to 99.9, and a bar, and a chart of the last 240 steps of
;;; three series. Every frame is a new step, so every tile changes in every frame, which is the
;;; point: what a variant spends on a screen where everything changes.
;;;
;;; What every variant shares is done once and allocates nothing in a step: the value of a tile at a
;;; step is a function of the two (a sine, rounded to a tenth), the text of a value is a string in a
;;; table made when this file is loaded, and the history is an array that is written in place. So
;;; the allocation that a variant shows in a frame is its own, and not the model's.

(defconstant +columns+ 6)
(defconstant +rows+ 6)
(defconstant +tiles+ (* +columns+ +rows+))
(defconstant +samples+ 240 "How many steps the chart shows.")
(defconstant +series+ 3)
(defconstant +frame-ns+ 16666667 "A step is a frame at 60 a second.")

(declaim (inline step-at))
(defun step-at (now-ns)
  "The step that the time NOW-NS, in nanoseconds, is in."
  (declare (type (unsigned-byte 62) now-ns))
  (values (floor now-ns +frame-ns+)))

(declaim (inline value-index))
(defun value-index (tile step)
  "The value of TILE at STEP in tenths, 0 to 999."
  (declare (type (integer 0 #.(1- +tiles+)) tile) (type (integer 0) step))
  (let* ((phase (+ (* 0.011f0 (float (1+ tile) 1f0) (float (mod step 6000) 1f0))
                   (* 0.7f0 (float tile 1f0))))
         (value (+ 500f0 (* 450f0 (sin phase)))))
    (max 0 (min 999 (round value)))))

(defparameter +texts+
  (let ((table (make-array 1000)))
    (dotimes (i 1000 table)
      (setf (svref table i) (format nil "~d.~d" (floor i 10) (mod i 10)))))
  "The text of every value from 0.0 to 99.9.")

(defun index-text (index)
  (svref +texts+ index))

(defparameter +names+
  (let ((table (make-array +tiles+)))
    (dotimes (i +tiles+ table)
      (setf (svref table i)
            (format nil "~a ~d" (nth (mod i 6) '("cpu" "mem" "disk" "net" "temp" "fan")) (floor i 6))))))

(defun tile-name (tile)
  (svref +names+ tile))

(defparameter +value-ids+
  (let ((table (make-array +tiles+)))
    (dotimes (i +tiles+ table)
      (setf (svref table i) (intern (format nil "TILE-~d-VALUE" i) :keyword)))))

(defparameter +bar-ids+
  (let ((table (make-array +tiles+)))
    (dotimes (i +tiles+ table)
      (setf (svref table i) (intern (format nil "TILE-~d-BAR" i) :keyword)))))

(defun tile-value-id (tile) (svref +value-ids+ tile))
(defun tile-bar-id (tile) (svref +bar-ids+ tile))

(defstruct (dashboard (:constructor make-dashboard ()))
  "The values at the current step, and the history the chart shows. HEAD is where the next sample
of the history goes, which is also the oldest."
  (step -1 :type fixnum)
  (indices (make-array +tiles+ :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (history (make-array (* +series+ +samples+) :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (*)))
  (head 0 :type fixnum))

(defun advance-dashboard (dashboard step)
  "Bring DASHBOARD to STEP. Returns true if that was a new step, and false if it was there already
(a frame can be built more than once). Does not allocate."
  (declare (type dashboard dashboard) (type (integer 0) step))
  (unless (= step (dashboard-step dashboard))
    (let ((indices (dashboard-indices dashboard))
          (history (dashboard-history dashboard))
          (head (dashboard-head dashboard))
          (sum-0 0) (sum-1 0) (sum-2 0))
      (declare (type fixnum sum-0 sum-1 sum-2))
      (dotimes (tile +tiles+)
        (let ((index (value-index tile step)))
          (setf (aref indices tile) index)
          (case (mod tile +series+)
            (0 (incf sum-0 index))
            (1 (incf sum-1 index))
            (t (incf sum-2 index)))))
      ;; Each series is the mean of the twelve tiles in it, as a fraction of the most a value is.
      (flet ((put (series sum)
               (setf (aref history (+ (* series +samples+) head))
                     (/ (float sum 1f0) (* (/ +tiles+ +series+) 1000f0)))))
        (put 0 sum-0)
        (put 1 sum-1)
        (put 2 sum-2))
      (setf (dashboard-head dashboard) (mod (1+ head) +samples+)
            (dashboard-step dashboard) step))
    t))

(declaim (inline value-text tile-fraction))
(defun value-text (dashboard tile)
  "The text of TILE's value now."
  (svref +texts+ (aref (dashboard-indices dashboard) tile)))

(defun tile-fraction (dashboard tile)
  "How full TILE's bar is now, 0 to 1."
  (/ (float (aref (dashboard-indices dashboard) tile) 1f0) 999f0))
