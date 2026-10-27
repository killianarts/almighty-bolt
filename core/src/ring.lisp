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
           #:record-frame-details
           #:frame-gc-ns
           #:frame-first-build-ns
           #:frame-update-ns
           #:frame-layout-ns
           #:frame-paint-ns
           #:frame-gpu-ns
           #:frame-all-builds-ns
           #:frame-events-ns
           #:frame-events
           #:map-frames))

(in-package #:almighty-bolt-core/ring)

;;; A ring of per-frame records, preallocated so that recording a frame does
;;; not allocate and so does not disturb what it measures. Each record is
;;; eight unsigned 64-bit words: start time, end time, bytes consed before and
;;; after, and four more that hold the phases of the frame (decision record 0009):
;;; the time spent building, the time spent waiting for the display (the previous
;;; frame's GPU work, then the drawable), the time spent encoding and committing,
;;; and the number of builds with a flag for whether the settle cap was hit (they
;;; share a word). A recorder that does not time phases leaves them zero. Eight more words,
;;; filled in by RECORD-FRAME-DETAILS, hold what the instrumentation of Phase 3 adds: the time
;;; spent in garbage collection, the time of the first build (the rest of the build time is
;;; the cost of settling), the time the variant spent in its update, layout and paint phases,
;;; the GPU's time for the frame before, the time of all the builds (the host's build time also
;;; has what the settle loop itself costs), and the events: the time the variant spent handling
;;; the events delivered since the frame before and how many there were (they share a word, the
;;; time in its low 40 bits, 18 minutes, and the count above). When the ring is full the oldest
;;; record is overwritten.
;;;
;;; SB-EXT:GET-BYTES-CONSED only advances when an allocation region is closed
;;; (tens of kilobytes at a time), so one frame's consing reads as zero or as a
;;; whole region. Compare totals over many frames, not single frames.

(deftype u64 () '(unsigned-byte 64))

(defconstant +words-per-record+ 16)
(defconstant +events-ns-bits+ 40)
(defconstant +events-ns-mask+ (1- (ash 1 +events-ns-bits+)))
(defconstant +events-mask+ (1- (ash 1 (- 64 +events-ns-bits+))))

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
    ;; What an older record in this place held is gone, details and all.
    (loop :for word :from 8 :below +words-per-record+
          :do (setf (aref data (+ base word)) 0))
    (setf (frame-ring-head ring) (if (= (1+ head) capacity) 0 (1+ head)))
    (when (< (frame-ring-count ring) capacity)
      (incf (frame-ring-count ring)))
    (incf (frame-ring-total ring))
    nil))

(declaim (inline record-frame-details))
(defun record-frame-details (ring &key (gc-ns 0) (first-build-ns 0) (update-ns 0) (layout-ns 0)
                                    (paint-ns 0) (gpu-ns 0) (all-builds-ns 0) (events-ns 0)
                                    (events 0))
  "Set the words after the first eight of the record that RECORD-FRAME wrote last. EVENTS-NS and
EVENTS are the time the variant spent handling the events delivered since the frame before, and
how many there were; a time past 18 minutes or a count past 16 million is kept as that. Does
not allocate."
  (declare (type frame-ring ring)
           (type u64 gc-ns first-build-ns update-ns layout-ns paint-ns gpu-ns all-builds-ns
                 events-ns events))
  (let* ((capacity (frame-ring-capacity ring))
         (head (frame-ring-head ring))
         (last (if (zerop head) (1- capacity) (1- head)))
         (base (* last +words-per-record+))
         (data (frame-ring-data ring)))
    (setf (aref data (+ base 8)) gc-ns
          (aref data (+ base 9)) first-build-ns
          (aref data (+ base 10)) update-ns
          (aref data (+ base 11)) layout-ns
          (aref data (+ base 12)) paint-ns
          (aref data (+ base 13)) gpu-ns
          (aref data (+ base 14)) all-builds-ns
          (aref data (+ base 15)) (logior (min events-ns +events-ns-mask+)
                                          (ash (min events +events-mask+) +events-ns-bits+)))
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

(defun frame-gc-ns (ring index)
  "Nanoseconds of garbage collection that ended during the INDEXth frame."
  (record-word ring index 8))

(defun frame-first-build-ns (ring index)
  "Nanoseconds the first build of the INDEXth frame took. What the frame spent building beyond
that is the cost of settling."
  (record-word ring index 9))

(defun frame-update-ns (ring index)
  "Nanoseconds the variant spent in its update phase in the INDEXth frame."
  (record-word ring index 10))

(defun frame-layout-ns (ring index)
  "Nanoseconds the variant spent working out sizes and places in the INDEXth frame."
  (record-word ring index 11))

(defun frame-paint-ns (ring index)
  "Nanoseconds the variant spent making the display list in the INDEXth frame."
  (record-word ring index 12))

(defun frame-gpu-ns (ring index)
  "Nanoseconds the GPU took for the frame before the INDEXth, whose work the frame waited for."
  (record-word ring index 13))

(defun frame-all-builds-ns (ring index)
  "Nanoseconds the INDEXth frame spent in the variant's FRAME, in all its builds. What it spent
beyond the first build is the cost of settling."
  (record-word ring index 14))

(defun frame-events-ns (ring index)
  "Nanoseconds the variant spent handling the events delivered between the frame before the
INDEXth and it (zero if the instrumentation was off)."
  (ldb (byte +events-ns-bits+ 0) (record-word ring index 15)))

(defun frame-events (ring index)
  "How many events were delivered between the frame before the INDEXth and it (zero if the
instrumentation was off)."
  (ldb (byte (- 64 +events-ns-bits+) +events-ns-bits+) (record-word ring index 15)))

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
