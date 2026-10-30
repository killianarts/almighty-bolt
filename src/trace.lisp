(defpackage #:almighty-bolt/trace
  (:use #:cl)
  (:import-from #:almighty-bolt/ring
                #:frame-ring-count
                #:frame-start-ns
                #:frame-duration-ns
                #:frame-bytes-consed
                #:frame-build-ns
                #:frame-drawable-wait-ns
                #:frame-encode-ns
                #:frame-builds
                #:frame-capped-p
                #:frame-gc-ns
                #:frame-first-build-ns
                #:frame-all-builds-ns
                #:frame-update-ns
                #:frame-layout-ns
                #:frame-paint-ns
                #:frame-gpu-ns)
  (:export #:write-trace))

(in-package #:almighty-bolt/trace)

;;; The frame ring as Chrome trace event JSON, to look at individual frames in Perfetto
;;; (ui.perfetto.dev) or chrome://tracing. A frame is a slice with the slices of its parts
;;; inside it: the build, the wait for the display and the encoding, one after another, and
;;; in the build the variant's update, layout and paint. The ring holds how long each part
;;; took and not when it began, so the parts are placed one after another from where the frame
;;; or the build began; the order inside a build is the order a variant does them in, but
;;; the gaps between them (a layout that is skipped, say) do not show.

(defun microseconds (ns)
  (/ ns 1000.0d0))

(defun write-slice (stream first name start-ns duration-ns &optional args)
  "One complete event. ARGS is a list of (name value) pairs; a value that is a number is written
as one, and anything else as a string."
  (unless first (write-string "," stream))
  (format stream "~&{\"name\":\"~a\",\"ph\":\"X\",\"ts\":~,3f,\"dur\":~,3f,\"pid\":1,\"tid\":1"
          name (microseconds start-ns) (microseconds duration-ns))
  (when args
    (write-string ",\"args\":{" stream)
    (loop :for ((key value) . more) :on args
          :do (if (numberp value)
                  (format stream "\"~a\":~a" key (if (floatp value) (format nil "~,3f" value) value))
                  (format stream "\"~a\":~s" key (princ-to-string value)))
              (when more (write-string "," stream)))
    (write-string "}" stream))
  (write-string "}" stream))

(defun write-trace (ring stream &key (name "almighty-bolt"))
  "Write the records of RING to STREAM as a Chrome trace. Returns the number of frames."
  (write-string "{\"traceEvents\":[" stream)
  (format stream "~&{\"name\":\"process_name\",\"ph\":\"M\",\"pid\":1,\"args\":{\"name\":~s}}"
          name)
  (dotimes (i (frame-ring-count ring))
    (let* ((start (frame-start-ns ring i))
           (build (frame-build-ns ring i))
           (wait (frame-drawable-wait-ns ring i))
           (encode (frame-encode-ns ring i))
           (first-build (frame-first-build-ns ring i))
           (all-builds (frame-all-builds-ns ring i)))
      (write-slice stream nil "frame" start (frame-duration-ns ring i)
                   (list (list "builds" (frame-builds ring i))
                         (list "capped" (if (frame-capped-p ring i) "yes" "no"))
                         (list "bytes_consed" (frame-bytes-consed ring i))
                         (list "gc_us" (microseconds (frame-gc-ns ring i)))
                         (list "gpu_us_of_frame_before" (microseconds (frame-gpu-ns ring i)))))
      (when (plusp build)
        (write-slice stream nil "build" start build
                     (list (list "first_build_us" (microseconds first-build))
                           (list "extra_builds_us"
                                 (microseconds (if (plusp first-build)
                                                   (max 0 (- all-builds first-build))
                                                   0)))))
        (let ((at start))
          (loop :for (part duration) :in (list (list "update" (frame-update-ns ring i))
                                               (list "layout" (frame-layout-ns ring i))
                                               (list "paint" (frame-paint-ns ring i)))
                :when (plusp duration)
                  :do (write-slice stream nil part at duration)
                      (incf at duration))))
      (when (plusp wait)
        (write-slice stream nil "wait for the display" (+ start build) wait))
      (when (plusp encode)
        (write-slice stream nil "encode and present" (+ start build wait) encode))))
  (format stream "~&]}~%")
  (frame-ring-count ring))
