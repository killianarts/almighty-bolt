(defpackage #:almighty-bolt-bench-null/measure
  (:use #:cl)
  (:import-from #:almighty-bolt-bench-null/null
                #:make-null-variant
                #:null-variant-events-seen
                #:null-variant-animate-frames)
  (:import-from #:almighty-bolt/internals
                #:now-ns
                #:request-frame
                #:frame-ring-total
                #:frame-duration-ns
                #:frame-build-ns
                #:frame-drawable-wait-ns
                #:frame-encode-ns
                #:frame-builds
                #:frame-capped-p
                #:frame-bytes-consed
                #:display-list-count)
  (:import-from #:almighty-bolt/platform/macos/host
                #:run-variant
                #:host-variant
                #:host-context
                #:host-ring
                #:host-renderer
                #:host-display-list
                #:close-host-window)
  (:import-from #:almighty-bolt/render/metal/renderer
                #:wait-for-last-frame
                #:last-frame-gpu-ns)
  (:export #:main
           #:report))

(in-package #:almighty-bolt-bench-null/measure)

;;; The null variant in a window: what a frame costs when the variant does nothing,
;;; which is what the core costs. Run it from a terminal of your own and leave the
;;; window alone, so that the stillness check means something.
;;;
;;; The frame's time is reported in parts, because the parts behave differently:
;;; the build (here, copying the list) and the encoding are the core's own work, and
;;; the wait is the display and the GPU making us wait, and it ranges
;;; from 84 us to 5 ms between runs of identical code.

(defun draws (host)
  (frame-ring-total (host-ring host)))

(defun wait-until (predicate &key (timeout-ns 1000000000))
  "Poll PREDICATE every half millisecond. Returns true if it became true in time."
  (let ((deadline (+ (now-ns) timeout-ns)))
    (loop
      (when (funcall predicate) (return t))
      (when (> (now-ns) deadline) (return nil))
      (sleep 0.0005))))

(defun microseconds (ns)
  (/ ns 1000.0))

(defun summarize (host start end)
  "Statistics over the ring's records START up to END: the mean and maximum of each
part of the frame in nanoseconds, how many frames waited over a millisecond, how
many took more than one build or hit the cap, and the bytes consed. The ring has
not wrapped, so a record's index is its order."
  (let ((ring (host-ring host))
        (count (- end start))
        (sums (make-array 4 :initial-element 0))
        (maxes (make-array 4 :initial-element 0))
        (slow 0) (rebuilt 0) (capped 0) (consed 0))
    (loop :for i :from start :below end
          :do (let ((parts (list (frame-build-ns ring i)
                                 (frame-drawable-wait-ns ring i)
                                 (frame-encode-ns ring i)
                                 (frame-duration-ns ring i))))
                (loop :for part :in parts
                      :for k :from 0
                      :do (incf (aref sums k) part)
                          (setf (aref maxes k) (max (aref maxes k) part)))
                (when (> (second parts) 1000000) (incf slow))
                (when (/= (frame-builds ring i) 1) (incf rebuilt))
                (when (frame-capped-p ring i) (incf capped))
                (incf consed (frame-bytes-consed ring i))))
    (values (map 'list (lambda (sum) (/ sum (max 1 count))) sums)
            (coerce maxes 'list)
            slow rebuilt capped consed)))

(defun line (label format-control &rest arguments)
  (format t "~&~38a ~?~%" label format-control arguments))

(defun report (host frames animation-frames)
  "Draw FRAMES frames one at a time, leave the window alone for two seconds, ask for
ANIMATION-FRAMES frames from inside the draws, and print what it saw."
  (let* ((context (host-context host))
         (variant (host-variant host))
         (begin (draws host))
         (latencies '())
         (timeouts 0))
    ;; Frames, one at a time, each timed from the request to the draw.
    (dotimes (i frames)
      (let ((target (1+ (draws host)))
            (asked (now-ns)))
        (request-frame context)
        (if (wait-until (lambda () (>= (draws host) target)))
            (push (- (now-ns) asked) latencies)
            (incf timeouts))))
    (let ((finish (draws host)))
      ;; Nothing is asked for: nothing must be drawn.
      (sleep 2)
      (let ((still (- (draws host) finish))
            (animation-before (draws host))
            (animation-consed-before (sb-ext:get-bytes-consed))
            (animation-start (now-ns)))
        ;; Frames asked for from inside the draws: the variant requests the next
        ;; frame while a frame is being drawn, a request the host keeps until the
        ;; frame is presented.
        (setf (null-variant-animate-frames variant) animation-frames)
        (request-frame context)
        (let* ((honoured (wait-until (lambda ()
                                       (>= (draws host)
                                           (+ animation-before animation-frames 1)))
                                     :timeout-ns 5000000000))
               (animation-ns (- (now-ns) animation-start))
               (animation-consed (- (sb-ext:get-bytes-consed) animation-consed-before))
               (animation-draws (- (draws host) animation-before)))
          (wait-for-last-frame (host-renderer host))
          (multiple-value-bind (means maxes slow rebuilt capped consed)
              (summarize host begin finish)
            (format t "~&~%--- The null variant: the core's own cost ---~%")
            (line "frames requested:" "~d (drawn ~d, timeouts ~d)"
                  frames (- finish begin) timeouts)
            (line "request-to-draw latency:" "mean ~,2f ms, max ~,2f ms"
                  (/ (reduce #'+ latencies) (max 1 (length latencies)) 1000000.0)
                  (/ (reduce #'max latencies :initial-value 0) 1000000.0))
            (line "draws in 2 s of stillness:" "~d" still)
            (loop :for name :in '("build (copy the list):" "wait (drawable, GPU):"
                                  "encode, commit, present:" "the whole frame:")
                  :for mean :in means
                  :for max :in maxes
                  :do (line name "mean ~,1f us, max ~,1f us" (microseconds mean)
                            (microseconds max)))
            (line "the core's own cost (build + encode):" "mean ~,1f us"
                  (microseconds (+ (first means) (third means))))
            (line "frames that waited over 1 ms:" "~d of ~d" slow (- finish begin))
            (line "frames built more than once, capped:" "~d, ~d" rebuilt capped)
            (line "bytes consed in the draw path:" "~:d (granular: the counter moves ~
                                                   in regions of about 64 KB)"
                  consed)
            (line "commands in the list:" "~d" (display-list-count (host-display-list host)))
            (line "GPU time of the last frame:" "~,1f us"
                  (microseconds (last-frame-gpu-ns (host-renderer host))))
            (line "frames asked for from inside draws:"
                  "~a: drew ~d of ~d in ~,2f s (~,1f ms a frame)"
                  (if honoured "honoured" "NOT HONOURED")
                  animation-draws (1+ animation-frames) (/ animation-ns 1d9)
                  (/ animation-ns 1000000.0 (max 1 animation-draws)))
            (line "bytes consed while animating:" "~:d (the whole process, so the driver counts too)"
                  animation-consed)
            (line "events the variant saw:" "~d (move the mouse over the window to see this rise)"
                  (null-variant-events-seen variant))))))
    (finish-output)))

(defun main (&key (frames 300) (animation-frames 100))
  "Open a window, run the null variant in it, print the report and quit."
  (run-variant (make-null-variant)
               :title "The null variant"
               :width 800 :height 600
               :driver (lambda (host)
                         (sleep 0.5)            ; let the window appear and settle
                         (report host frames animation-frames)
                         (close-host-window host))))
