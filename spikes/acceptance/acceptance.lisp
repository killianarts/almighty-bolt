(defpackage #:almighty-bolt-spikes/acceptance
  (:use #:cl)
  (:local-nicknames (#:clock #:almighty-bolt-core/clock)
                    (#:ring #:almighty-bolt-core/ring))
  (:export #:counters
           #:make-counters
           #:counters-draws
           #:counters-reshapes
           #:counters-width
           #:counters-height
           #:input-log
           #:make-input-log
           #:input-log-mouse-down
           #:input-log-mouse-moved
           #:input-log-mouse-up
           #:input-log-key-down
           #:rig-ring
           #:rig-counters
           #:rig-input
           #:request-redraw
           #:resize-content
           #:synthesize-input
           #:close-window
           #:run-acceptance))

(in-package #:almighty-bolt-spikes/acceptance)

;;; The acceptance script for Spikes A and B. A backend (cl-nextstep, or our own
;;; bindings) supplies a RIG: a window with a paused Metal view that draws only
;;; when asked, and the methods below. The script then checks the same things of
;;; each and prints the same report:
;;;
;;;   - every requested frame is drawn, and how long that takes;
;;;   - a still window draws nothing;
;;;   - a resize is reported to Lisp;
;;;   - input reaches Lisp (built by hand, so this checks wiring, not the OS);
;;;   - closing the window quits the application.

(defstruct (counters (:constructor make-counters ()))
  "Filled in by the rig's draw and resize callbacks."
  (draws 0 :type sb-ext:word)
  (reshapes 0 :type sb-ext:word)
  (width 0 :type fixnum)
  (height 0 :type fixnum))

(defstruct (input-log (:constructor make-input-log ()))
  "What the rig's input callbacks received, the most recent of each kind."
  (mouse-down nil)
  (mouse-moved nil)
  (mouse-up nil)
  (key-down nil))

(defgeneric rig-ring (rig)
  (:documentation "The frame ring that RIG's draw callback records into."))

(defgeneric rig-counters (rig)
  (:documentation "The COUNTERS that RIG's callbacks update."))

(defgeneric rig-input (rig)
  (:documentation "The INPUT-LOG that RIG's callbacks update."))

(defgeneric request-redraw (rig)
  (:documentation "Ask for one frame. Safe from any thread."))

(defgeneric resize-content (rig width height)
  (:documentation "Resize the window's content, as a user dragging the corner would."))

(defgeneric synthesize-input (rig)
  (:documentation "Give the view a mouse down, move and up and a key press, built by
hand, so that a mouse down at 100, 50 from the top left and the letter a arrive."))

(defgeneric close-window (rig)
  (:documentation "Close the window as a user would. This should quit the application."))

(defun wait-until (predicate &key (timeout-ns 1000000000))
  "Poll PREDICATE every half millisecond. Returns true if it became true."
  (let ((deadline (+ (clock:now-ns) timeout-ns)))
    (loop
      (when (funcall predicate) (return t))
      (when (> (clock:now-ns) deadline) (return nil))
      (sleep 0.0005))))

(defun summarize-ring (frame-ring)
  "Mean and maximum frame time in microseconds, and total bytes consed."
  (let ((count (ring:frame-ring-count frame-ring))
        (total-ns 0) (max-ns 0) (consed 0))
    (dotimes (i count)
      (let ((ns (ring:frame-duration-ns frame-ring i)))
        (incf total-ns ns)
        (setf max-ns (max max-ns ns))
        (incf consed (ring:frame-bytes-consed frame-ring i))))
    (values (if (zerop count) 0 (/ total-ns count 1000.0))
            (/ max-ns 1000.0)
            consed)))

(defun run-acceptance (rig &key (frames 300) (title "acceptance") extra-report)
  "Run the script against RIG from the calling thread (not the main thread), print
the report and close the window. EXTRA-REPORT, if given, is called after the
report to print anything specific to the backend. Returns when the application
is quitting."
  (let* ((counters (rig-counters rig))
         (start-consed (sb-ext:get-bytes-consed))
         (latencies '())
         (timeouts 0))
    (sleep 0.5)                         ; let the window appear and settle
    (let ((draws-at-start (counters-draws counters)))
      ;; Request FRAMES frames, one at a time, and time each answer.
      (dotimes (i frames)
        (let ((target (1+ (counters-draws counters)))
              (asked (clock:now-ns)))
          (request-redraw rig)
          (if (wait-until (lambda () (>= (counters-draws counters) target)))
              (push (- (clock:now-ns) asked) latencies)
              (incf timeouts))))
      (let ((requested-draws (- (counters-draws counters) draws-at-start))
            (consed (- (sb-ext:get-bytes-consed) start-consed)))
        ;; Ask for nothing for two seconds. A paused view must not draw.
        (let ((before (counters-draws counters)))
          (sleep 2)
          (let ((still-draws (- (counters-draws counters) before))
                (reshapes-before (counters-reshapes counters)))
            ;; Resize, and see that the view is told.
            (resize-content rig 640 400)
            (let ((told (wait-until (lambda ()
                                      (> (counters-reshapes counters) reshapes-before)))))
              (synthesize-input rig)
              (sleep 0.2)
              (multiple-value-bind (mean-us max-us ring-consed)
                  (summarize-ring (rig-ring rig))
                (let ((input (rig-input rig)))
                  (format t "~&~%--- ~a ---~%" title)
                  (format t "frames requested:           ~d~%" frames)
                  (format t "frames drawn for them:      ~d (timeouts ~d)~%" requested-draws timeouts)
                  (format t "draws in 2 s of stillness:  ~d~%" still-draws)
                  (format t "resize reported:            ~a (now ~d x ~d pixels, ~d reshape callbacks)~%"
                          (if told "yes" "NO") (counters-width counters) (counters-height counters)
                          (counters-reshapes counters))
                  (format t "input reaches Lisp:         mouse down ~a, move ~a, up ~a, key ~s~%"
                          (input-log-mouse-down input) (input-log-mouse-moved input)
                          (input-log-mouse-up input) (input-log-key-down input))
                  (format t "request-to-draw latency:    mean ~,2f ms, max ~,2f ms~%"
                          (/ (reduce #'+ latencies) (max 1 (length latencies)) 1000000.0)
                          (/ (reduce #'max latencies :initial-value 0) 1000000.0))
                  (format t "draw callback time:         mean ~,1f us, max ~,1f us~%" mean-us max-us)
                  (format t "bytes consed in draw:       ~:d (~,1f per frame; region-quantized)~%"
                          ring-consed (/ ring-consed (max 1 requested-draws)))
                  (format t "bytes consed, whole run:    ~:d (~,1f per requested frame, driver included)~%"
                          consed (/ consed frames))
                  (when extra-report (funcall extra-report))
                  (finish-output))))))))
    ;; Close the window the way a user would. That should quit the process; the
    ;; watchdog says so if it does not.
    (sb-thread:make-thread (lambda ()
                             (sleep 5)
                             (format t "~&Closing the window did not quit the application.~%")
                             (finish-output)
                             (sb-ext:exit :code 3 :abort t))
                           :name "acceptance watchdog")
    (close-window rig)))
