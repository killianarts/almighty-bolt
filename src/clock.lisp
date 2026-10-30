(defpackage #:almighty-bolt/clock
  (:use #:cl)
  (:import-from #:cffi
                #:foreign-funcall)
  (:export #:nanoseconds
           #:now-ns
           #:*time-source*
           #:real-time-source
           #:with-time-source
           #:virtual-clock
           #:make-virtual-clock
           #:virtual-clock-ns
           #:virtual-clock-source
           #:advance-virtual-clock))

(in-package #:almighty-bolt/clock)

;;; All time in the core goes through NOW-NS, so replay can substitute a
;;; virtual clock by rebinding *TIME-SOURCE*. Nothing else in the core calls
;;; the system clock.

(deftype nanoseconds ()
  "Nanoseconds since an arbitrary start. Fits a fixnum for about 146 years."
  '(unsigned-byte 62))

(defconstant +clock-uptime-raw+ 8
  "CLOCK_UPTIME_RAW from <time.h> on macOS: monotonic, unaffected by clock
adjustments, paused while the machine sleeps. It shares its base with
mach_absolute_time, which NSEvent timestamps also use.")

(defun real-time-source ()
  "Nanoseconds on the operating system's monotonic clock."
  (the nanoseconds
       (foreign-funcall "clock_gettime_nsec_np"
                        :int +clock-uptime-raw+
                        :unsigned-long-long)))

(defvar *time-source* 'real-time-source
  "A function of no arguments (or a symbol naming one) that returns the current
time in nanoseconds. Rebind it with WITH-TIME-SOURCE; never call the system
clock directly.")

(declaim (inline now-ns))
(defun now-ns ()
  "The current time in nanoseconds, from the core clock."
  (the nanoseconds (funcall *time-source*)))

(defmacro with-time-source ((source) &body body)
  "Run BODY with SOURCE as the core clock."
  `(let ((*time-source* ,source))
     ,@body))

;;; A virtual clock only moves when told to. Replay advances it from the
;;; script's timeline.

(defstruct (virtual-clock (:constructor make-virtual-clock (&optional (ns 0))))
  (ns 0 :type nanoseconds))

(defun virtual-clock-source (clock)
  "A time source that reads CLOCK."
  (lambda () (virtual-clock-ns clock)))

(defun advance-virtual-clock (clock delta-ns)
  "Move CLOCK forward by DELTA-NS nanoseconds and return the new time."
  (check-type delta-ns (unsigned-byte 62))
  (incf (virtual-clock-ns clock) delta-ns))
