(defpackage #:almighty-bolt-core/ring
  (:use #:cl)
  (:import-from #:almighty-bolt-core/clock
                #:now-ns)
  (:export #:frame-ring
           #:make-frame-ring
           #:frame-ring-capacity
           #:frame-ring-count
           #:frame-ring-total
           #:clear-frame-ring
           #:record-frame
           #:with-recorded-frame
           #:frame-start-ns
           #:frame-end-ns
           #:frame-duration-ns
           #:frame-bytes-consed-before
           #:frame-bytes-consed-after
           #:frame-bytes-consed
           #:map-frames))

(in-package #:almighty-bolt-core/ring)

;;; A ring of per-frame records, preallocated so that recording a frame does
;;; not allocate and so does not disturb what it measures. Each record is
;;; four unsigned 64-bit words: start time, end time, bytes consed before and
;;; bytes consed after. When the ring is full the oldest record is overwritten.
;;;
;;; SB-EXT:GET-BYTES-CONSED only advances when an allocation region is closed
;;; (tens of kilobytes at a time), so one frame's consing reads as zero or as a
;;; whole region. Compare totals over many frames, not single frames.

(deftype u64 () '(unsigned-byte 64))

(defconstant +words-per-record+ 4)

(defstruct (frame-ring (:constructor %make-frame-ring (capacity data)))
  (capacity 1 :type (integer 1 #.(floor array-dimension-limit +words-per-record+))
              :read-only t)
  (data (make-array 0 :element-type 'u64) :type (simple-array u64 (*))
                                          :read-only t)
  (head 0 :type fixnum)
  (count 0 :type fixnum)
  (total 0 :type fixnum))

(defun make-frame-ring (&key (capacity 4096))
  "A ring that holds the last CAPACITY frame records."
  (check-type capacity (integer 1 #.(floor array-dimension-limit +words-per-record+)))
  (%make-frame-ring capacity
                    (make-array (* capacity +words-per-record+)
                                :element-type 'u64
                                :initial-element 0)))

(defun clear-frame-ring (ring)
  "Forget every record in RING. The storage is kept."
  (setf (frame-ring-head ring) 0
        (frame-ring-count ring) 0
        (frame-ring-total ring) 0)
  ring)

(declaim (inline record-frame))
(defun record-frame (ring start-ns end-ns consed-before consed-after)
  "Append a record to RING, overwriting the oldest if it is full. Does not allocate."
  (declare (type frame-ring ring)
           (type u64 start-ns end-ns consed-before consed-after))
  (let* ((capacity (frame-ring-capacity ring))
         (head (frame-ring-head ring))
         (base (* head +words-per-record+))
         (data (frame-ring-data ring)))
    (setf (aref data base) start-ns
          (aref data (+ base 1)) end-ns
          (aref data (+ base 2)) consed-before
          (aref data (+ base 3)) consed-after)
    (setf (frame-ring-head ring) (if (= (1+ head) capacity) 0 (1+ head)))
    (when (< (frame-ring-count ring) capacity)
      (incf (frame-ring-count ring)))
    (incf (frame-ring-total ring))
    nil))

(defmacro with-recorded-frame ((ring) &body body)
  "Run BODY, then record its start and end times and the bytes consed while it
ran in RING. Returns the values of BODY."
  (let ((r (gensym "RING"))
        (start (gensym "START"))
        (consed (gensym "CONSED")))
    `(let* ((,r ,ring)
            (,start (now-ns))
            (,consed (the u64 (sb-ext:get-bytes-consed))))
       (multiple-value-prog1 (progn ,@body)
         (record-frame ,r ,start (now-ns) ,consed
                       (the u64 (sb-ext:get-bytes-consed)))))))

(defun record-word (ring index offset)
  "The word at OFFSET of the INDEXth record, counting from the oldest."
  (declare (type frame-ring ring) (type fixnum index offset))
  (unless (< -1 index (frame-ring-count ring))
    (error "No record ~d in a ring holding ~d." index (frame-ring-count ring)))
  (let* ((capacity (frame-ring-capacity ring))
         (oldest (mod (- (frame-ring-head ring) (frame-ring-count ring)) capacity))
         (slot (mod (+ oldest index) capacity)))
    (aref (frame-ring-data ring) (+ (* slot +words-per-record+) offset))))

(defun frame-start-ns (ring index)
  "Start time of the INDEXth record, counting from the oldest."
  (record-word ring index 0))

(defun frame-end-ns (ring index)
  "End time of the INDEXth record, counting from the oldest."
  (record-word ring index 1))

(defun frame-bytes-consed-before (ring index)
  (record-word ring index 2))

(defun frame-bytes-consed-after (ring index)
  (record-word ring index 3))

(defun frame-duration-ns (ring index)
  "Nanoseconds the INDEXth frame took."
  (- (frame-end-ns ring index) (frame-start-ns ring index)))

(defun frame-bytes-consed (ring index)
  "Bytes consed during the INDEXth frame (see the note on granularity above)."
  (- (frame-bytes-consed-after ring index) (frame-bytes-consed-before ring index)))

(defun map-frames (function ring)
  "Call FUNCTION with the start time, end time, bytes consed before and bytes
consed after of each record, oldest first."
  (dotimes (i (frame-ring-count ring))
    (funcall function
             (record-word ring i 0)
             (record-word ring i 1)
             (record-word ring i 2)
             (record-word ring i 3)))
  nil)
