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
           #:frame-build-ns
           #:frame-drawable-wait-ns
           #:frame-encode-ns
           #:frame-builds
           #:frame-capped-p
           #:map-frames))

(in-package #:almighty-bolt-core/ring)

;;; A ring of per-frame records, preallocated so that recording a frame does
;;; not allocate and so does not disturb what it measures. Each record is
;;; eight unsigned 64-bit words: start time, end time, bytes consed before and
;;; after, and four more that hold the phases of the frame (decision record 0009):
;;; the time spent building, the time spent waiting for the display (the previous
;;; frame's GPU work, then the drawable), the time spent encoding and committing,
;;; and the number of builds with a flag for whether the settle cap was hit (they
;;; share a word). A recorder that does not time phases leaves them zero. When the
;;; ring is full the oldest record is overwritten.
;;;
;;; SB-EXT:GET-BYTES-CONSED only advances when an allocation region is closed
;;; (tens of kilobytes at a time), so one frame's consing reads as zero or as a
;;; whole region. Compare totals over many frames, not single frames.

(deftype u64 () '(unsigned-byte 64))

(defconstant +words-per-record+ 8)

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

(deftype u32 () '(unsigned-byte 32))

(defconstant +capped-bit+ 32
  "The bit of the last word that says the settle cap was hit.")

(declaim (inline record-frame))
(defun record-frame (ring start-ns end-ns consed-before consed-after
                     &optional (build-ns 0) (drawable-wait-ns 0) (encode-ns 0)
                       (builds 0) capped)
  "Append a record to RING, overwriting the oldest if it is full. Does not allocate.
BUILD-NS, DRAWABLE-WAIT-NS and ENCODE-NS are the time of the frame's phases, BUILDS
how many times the variant built it, and CAPPED whether the settle cap was hit."
  (declare (type frame-ring ring)
           (type u64 start-ns end-ns consed-before consed-after
                 build-ns drawable-wait-ns encode-ns)
           (type u32 builds))
  (let* ((capacity (frame-ring-capacity ring))
         (head (frame-ring-head ring))
         (base (* head +words-per-record+))
         (data (frame-ring-data ring)))
    (setf (aref data base) start-ns
          (aref data (+ base 1)) end-ns
          (aref data (+ base 2)) consed-before
          (aref data (+ base 3)) consed-after
          (aref data (+ base 4)) build-ns
          (aref data (+ base 5)) drawable-wait-ns
          (aref data (+ base 6)) encode-ns
          (aref data (+ base 7)) (if capped
                                     (logior builds (ash 1 +capped-bit+))
                                     builds))
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

(defun frame-build-ns (ring index)
  "Nanoseconds the INDEXth frame spent building (the variant's FRAME, as many times
as it ran), or zero if the recorder did not time it."
  (record-word ring index 4))

(defun frame-drawable-wait-ns (ring index)
  "Nanoseconds the INDEXth frame spent waiting for the display: for the previous
frame's GPU work to finish, and then for its drawable."
  (record-word ring index 5))

(defun frame-encode-ns (ring index)
  "Nanoseconds the INDEXth frame spent encoding, committing and presenting."
  (record-word ring index 6))

(defun frame-builds (ring index)
  "How many times the variant built the INDEXth frame (zero if not recorded)."
  (ldb (byte 32 0) (record-word ring index 7)))

(defun frame-capped-p (ring index)
  "True if the INDEXth frame hit the settle cap."
  (logbitp +capped-bit+ (record-word ring index 7)))

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
