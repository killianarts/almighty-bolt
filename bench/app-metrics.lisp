(defpackage #:almighty-bolt-bench/app-metrics
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:results #:almighty-bolt-bench/results))
  (:export #:main
           #:measure-app))

(in-package #:almighty-bolt-bench/app-metrics)

;;; What an app costs a user's machine, measured from outside the process (decision record 0023):
;;; the size of the saved executable, the time from starting it to its first presented frame, the
;;; CPU, wakeups and frames of a window that nothing happens to, and its memory. Each is of an
;;; executable made by make image, running TASK in a real window; the process says when its first
;;; frame ended (on the clock that this process reads too) and, once a second, how many frames it
;;; has drawn and how much of its heap is in use, and is measured by ps and top.
;;;
;;; Idle is judged by the frames: the window is left alone until the process has drawn nothing for
;;; three seconds (the timer animates until its time is up), and the idle window is the ten seconds
;;; after that. The floor of the charter (no variant uses CPU when idle) is met if it draws no
;;; frame in the window and top reports 0.0% CPU to one decimal place. A task that animates for as long
;;; as it is open (the dashboard) is measured in the ten seconds after its first frame instead, and
;;; its record says :state :animating.

(defparameter +variants+ '(:retained :reactive :immediate))
(defparameter +tasks+ '(:counter :temperature :flight :timer :crud :circles :cells))
(defparameter +animated-tasks+ '(:dashboard)
  "The tasks that never stop drawing. They cannot be waited on to be quiet, so the window that is
measured is the first ten seconds after the first frame, and the record says :state :animating: the
numbers are what the animation costs, not an idle floor.")

(defun image-path (variant)
  (format nil "build/~(~a~)" variant))

(defun file-size (path)
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (file-length stream)))

(defun compressed-size (path)
  "The bytes that gzip -9 makes of the file at PATH."
  (parse-integer (results:shell "sh" "-c" (format nil "gzip -9 -c ~a | wc -c" path))))

(defstruct (watched (:constructor make-watched (process)))
  "A process that was started, with the lines it printed that were not read yet."
  process
  (lines '())
  (lock (sb-thread:make-mutex :name "watched"))
  (thread nil))

(defun start-watching (watched)
  (setf (watched-thread watched)
        (sb-thread:make-thread
         (lambda ()
           (let ((stream (uiop:process-info-output (watched-process watched))))
             (loop :for line = (read-line stream nil nil)
                   :while line
                   :do (sb-thread:with-mutex ((watched-lock watched))
                         (setf (watched-lines watched) (append (watched-lines watched) (list line)))))))
         :name "watch")))

(defun take-lines (watched)
  (sb-thread:with-mutex ((watched-lock watched))
    (prog1 (watched-lines watched)
      (setf (watched-lines watched) '()))))

(defun words (line)
  (let ((words '()) (start 0))
    (loop :for i :from 0 :to (length line)
          :do (when (or (= i (length line)) (char= (char line i) #\Space))
                (when (> i start) (push (subseq line start i) words))
                (setf start (1+ i))))
    (nreverse words)))

(defun rss-kb (pid)
  (let ((text (results:shell "ps" "-o" "rss=" "-p" (princ-to-string pid))))
    (and text (plusp (length text)) (parse-integer text :junk-allowed t))))

(defun launch (variant task)
  (let ((process (uiop:launch-program
                  (list "/usr/bin/env" (format nil "TASK=~(~a~)" task) "MEASURE=1"
                        (image-path variant))
                  :output :stream :error-output nil)))
    (let ((watched (make-watched process)))
      (start-watching watched)
      watched)))

(defun terminate (watched)
  (ignore-errors (uiop:terminate-process (watched-process watched) :urgent t))
  (ignore-errors (uiop:wait-process (watched-process watched))))

(defun wait-for (watched predicate &key (timeout 60) (poll 0.05))
  "Poll the lines WATCHED prints until PREDICATE, called with each new line, returns true for one.
Returns that line, or NIL after TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop
      (dolist (line (take-lines watched))
        (when (funcall predicate line) (return-from wait-for line)))
      (when (> (get-internal-real-time) deadline) (return nil))
      (sleep poll))))

(defun startup-only (variant task)
  "Start the executable and say how many nanoseconds it took to present its first frame."
  (let* ((start (core:real-time-source))
         (watched (launch variant task)))
    (unwind-protect
         (let ((line (wait-for watched (lambda (line) (eql 0 (search "first-frame-ns" line))))))
           (and line (- (parse-integer (second (words line))) start)))
      (terminate watched))))

(defun parse-top-line (text)
  "From the output of top -l 2 -stats pid,cpu,idlew,mem: the CPU percent, the idle wakeups of the
interval and the memory, of the second sample."
  (let* ((lines (uiop:split-string text :separator '(#\Newline)))
         (header (position-if (lambda (line) (eql 0 (search "PID" line))) lines :from-end t))
         (data (and header (nth (1+ header) lines)))
         (fields (and data (words data))))
    (when (and fields (>= (length fields) 4))
      (list :cpu-percent (let ((*read-default-float-format* 'double-float))
                           (coerce (read-from-string (second fields)) 'double-float))
            :wakeups (parse-integer (third fields) :junk-allowed t)
            :memory (fourth fields)))))

(defun measure-app (variant task &key (idle-seconds 10) (wait-quiet t))
  "Start the executable of VARIANT, running TASK, and measure it. Returns a plist. With WAIT-QUIET
false the window is measured at once, whatever it is doing: a check that the measurement sees an
app that is not idle."
  (let* ((start (core:real-time-source))
         (watched (launch variant task))
         (pid (uiop:process-info-pid (watched-process watched)))
         (peak 0) (frames nil) (heap nil) (quiet-ticks 0) (last-frames -1) (waited 0))
    (unwind-protect
         (let ((first (wait-for watched (lambda (line) (eql 0 (search "first-frame-ns" line))))))
           (unless first (error "~(~a ~a~) did not present a frame in 60 s." variant task))
           (let ((startup (- (parse-integer (second (words first))) start)))
             ;; Wait until it has stopped drawing, watching how large it gets.
             (loop
               (let ((tick (wait-for watched (lambda (line) (eql 0 (search "tick" line))) :timeout 5)))
                 (unless tick (error "~(~a ~a~) stopped reporting." variant task))
                 (let ((numbers (mapcar #'parse-integer (rest (words tick)))))
                   (setf frames (first numbers) heap (second numbers))
                   (setf peak (max peak (or (rss-kb pid) 0)))
                   (if (= frames last-frames) (incf quiet-ticks) (setf quiet-ticks 0))
                   (setf last-frames frames)
                   (incf waited)
                   (when (or (>= quiet-ticks 3) (not wait-quiet)) (return))
                   (when (> waited 90) (error "~(~a ~a~) kept drawing for 90 s." variant task)))))
             ;; The idle window.
             (let* ((frames-before frames)
                    (rss-before (rss-kb pid))
                    (top (parse-top-line
                          (results:shell "top" "-l" "2" "-s" (princ-to-string idle-seconds)
                                         "-pid" (princ-to-string pid) "-stats" "pid,cpu,idlew,mem")))
                    (rss-after (rss-kb pid))
                    (last-tick nil))
               (dolist (line (take-lines watched))
                 (when (eql 0 (search "tick" line)) (setf last-tick line)))
               (let ((frames-after (if last-tick (parse-integer (second (words last-tick))) frames)))
                 (setf peak (max peak (or rss-before 0) (or rss-after 0)))
                 (list :startup-ns startup
                       :idle (list :seconds idle-seconds
                                   :frames (- frames-after frames-before)
                                   :cpu-percent (getf top :cpu-percent)
                                   :wakeups-per-second (and (getf top :wakeups)
                                                            (/ (getf top :wakeups) idle-seconds 1.0))
                                   :after-quiet-seconds waited)
                       :memory (list :rss-kb rss-after :peak-rss-kb peak :top (getf top :memory)
                                     :heap-bytes heap))))))
      (terminate watched))))

(defun median-of (numbers) (core:median numbers))

(defun main ()
  (let* ((repetitions (results:environment-number "REPS" 5))
         (idle-seconds (results:environment-number "IDLE" 10))
         (directory (uiop:ensure-directory-pathname (or (uiop:getenv "OUT") "results/")))
         (variants (let ((v (uiop:getenv "VARIANTS")))
                     (if (and v (plusp (length v)))
                         (mapcar (lambda (name) (intern (string-upcase name) :keyword))
                                 (uiop:split-string v :separator ","))
                         +variants+)))
         (tasks (let ((v (uiop:getenv "TASKS")))
                  (if (and v (plusp (length v)))
                      (mapcar (lambda (name) (intern (string-upcase name) :keyword))
                              (uiop:split-string v :separator ","))
                      +tasks+)))
         (date (results:timestamp)) (source (results:source-state)) (machine (results:machine-profile)))
    (dolist (variant variants)
      (unless (probe-file (image-path variant))
        (error "There is no ~a: make image VARIANT=~(~a~) first." (image-path variant) variant))
      (let ((image (list :bytes (file-size (image-path variant))
                         :gzip-bytes (compressed-size (image-path variant)))))
        (dolist (task tasks)
          (let* ((animating (and (member task +animated-tasks+) t))
                 (first (measure-app variant task :idle-seconds idle-seconds
                                                  :wait-quiet (not animating)))
                 (startups (cons (getf first :startup-ns)
                                 (loop :repeat (1- repetitions)
                                       :collect (or (startup-only variant task)
                                                    (error "No first frame."))))))
            (let* ((record (append (list :date date) source
                                   (list :mode :app :variant variant :app task :script-name "idle-startup"
                                         :state (if animating :animating :idle)
                                         :machine machine :repetitions repetitions
                                         :image image
                                         :startup (list :ns startups :median-ns (median-of startups)
                                                        :min-ns (reduce #'min startups)
                                                        :max-ns (reduce #'max startups))
                                         :idle (getf first :idle) :memory (getf first :memory))))
                   (idle (getf first :idle)))
              (format t "~&~(~10a ~10a~) ~:[~;[animating] ~]startup ~6,1f ms (~,1f to ~,1f)  idle: ~d frames, ~,1f% cpu, ~,1f wakeups/s   rss ~,1f MB (peak ~,1f)  heap ~,1f MB   image ~,1f MB (gzip ~,1f)~%"
                      variant task animating (/ (getf (getf record :startup) :median-ns) 1d6)
                      (/ (getf (getf record :startup) :min-ns) 1d6)
                      (/ (getf (getf record :startup) :max-ns) 1d6)
                      (getf idle :frames) (or (getf idle :cpu-percent) -1) (or (getf idle :wakeups-per-second) -1)
                      (/ (or (getf (getf record :memory) :rss-kb) 0) 1024.0)
                      (/ (or (getf (getf record :memory) :peak-rss-kb) 0) 1024.0)
                      (/ (or (getf (getf record :memory) :heap-bytes) 0) 1048576.0)
                      (/ (getf image :bytes) 1048576.0) (/ (getf image :gzip-bytes) 1048576.0))
              (results:write-record directory record))))))
    (format t "~&Records written to ~a~%" directory)))
