(defpackage #:almighty-bolt/stats
  (:use #:cl)
  (:export #:percentile
           #:median
           #:summarize))

(in-package #:almighty-bolt/stats)

;;; The few statistics the benchmark protocol asks for: a median and percentiles of the
;;; samples of a run, and a summary of them. Samples are real numbers in a sequence.

(defun percentile (samples fraction)
  "The value that FRACTION (0 to 1) of SAMPLES do not exceed: the nearest-rank percentile, so
the answer is always one of the samples. NIL for no samples."
  (let ((sorted (sort (coerce samples 'vector) #'<)))
    (when (plusp (length sorted))
      (aref sorted (min (1- (length sorted))
                        (max 0 (1- (ceiling (* fraction (length sorted))))))))))

(defun median (samples)
  "The middle of SAMPLES: the mean of the two middle ones when there are an even number."
  (let* ((sorted (sort (coerce samples 'vector) #'<))
         (n (length sorted)))
    (cond ((zerop n) nil)
          ((oddp n) (aref sorted (floor n 2)))
          (t (/ (+ (aref sorted (1- (floor n 2))) (aref sorted (floor n 2))) 2)))))

(defun summarize (samples)
  "A plist of the count, minimum, median, 95th and 99th percentiles, maximum and mean of
SAMPLES, or NIL if there are none."
  (let ((n (length samples)))
    (when (plusp n)
      (list :n n
            :min (reduce #'min samples)
            :median (median samples)
            :p95 (percentile samples 0.95)
            :p99 (percentile samples 0.99)
            :max (reduce #'max samples)
            :mean (/ (reduce #'+ samples) n)))))
