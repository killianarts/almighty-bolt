(defpackage #:almighty-bolt-bench/results
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:registry #:almighty-bolt-7guis/common/registry))
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt-core/platform/macos/host
                #:make-host
                #:host-context)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt-core/render/metal/target
                #:+pixel-format-bgra8-unorm+)
  (:export #:main
           #:report))

(in-package #:almighty-bolt-bench/results)

;;; The harness of the benchmark protocol (the charter): every variant is driven by the same
;;; recorded input script (replay, 0019), a script is run a number of times after warm-up runs
;;; are thrown away, and one result record is written for each variant, app and script, in
;;; =results/=, never to be edited, only superseded. Each frame's time is that of the variant's
;;; build (the settle loop) on the real clock, with the real text engine and no window or GPU, so
;;; the host's waiting and encoding are not in it. A record has the commit, the machine, the raw
;;; samples of every repetition and the summary.

(defparameter +variants+ '(:retained :reactive :immediate))

(defun environment-number (name default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value))) (parse-integer value) default)))

(defun shell (&rest command)
  "The first line of what COMMAND prints, or NIL if it fails."
  (ignore-errors
   (let ((output (uiop:run-program command :output :string :error-output nil)))
     (string-trim '(#\Newline #\Space) output))))

(defun machine-profile ()
  (list :model (shell "sysctl" "-n" "hw.model")
        :cpu (shell "sysctl" "-n" "machdep.cpu.brand_string")
        :cores (let ((n (shell "sysctl" "-n" "hw.ncpu"))) (and n (parse-integer n)))
        :memory-bytes (let ((n (shell "sysctl" "-n" "hw.memsize"))) (and n (parse-integer n)))
        :os (format nil "macOS ~a" (shell "sw_vers" "-productVersion"))
        :lisp (format nil "~a ~a" (lisp-implementation-type) (lisp-implementation-version))))

(defun source-state ()
  "The commit the code is at, and whether files differ from it."
  (list :commit (shell "git" "rev-parse" "--short" "HEAD")
        :dirty (plusp (length (or (shell "git" "status" "--porcelain" "--untracked-files=no") "")))))

(defun timestamp ()
  (multiple-value-bind (second minute hour day month year) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0dZ" year month day hour minute second)))

(defun run-once (script variant task host-context)
  (let ((object (funcall (registry:app-constructor (registry:find-app variant task)))))
    (core:replay script object host-context :measure t)))

(defun measure-script (script variant task context &key (repetitions 10) (warmups 3))
  "Replay SCRIPT in VARIANT's app of TASK WARMUPS times and then REPETITIONS times, measured.
Returns the repetitions, each a plist of its frames' build nanoseconds and builds, the bytes
consed and garbage collection nanoseconds, and the hashes of its frames. An error if two runs
draw different frames."
  (dotimes (i warmups)
    (run-once script variant task context))
  (let ((runs '()) (reference nil))
    (dotimes (i repetitions)
      (let ((result (run-once script variant task context)))
        (let ((hashes (map 'vector #'cdr (core:replay-result-hashes result))))
          (cond ((null reference) (setf reference hashes))
                ((not (equalp reference hashes))
                 (error "Two runs of the script gave different frames in ~(~a ~a~): the bug comes before the measurement." variant task))))
        (push (list :build-ns (map 'vector #'first (core:replay-result-samples result))
                    :builds (map 'vector #'second (core:replay-result-samples result))
                    :frame-bytes (map 'vector #'third (core:replay-result-samples result))
                    :bytes (core:replay-result-bytes result)
                    :gc-ns (core:replay-result-gc-ns result)
                    :frames (core:replay-result-frames result)
                    :capped (core:replay-result-capped result))
              runs)))
    (nreverse runs)))

(defun after-the-first (vector)
  (subseq vector 1))

(defun summary (runs)
  "What the repetitions say. The first frame of a run is a cold one (a new variant builds its
tables in it) and is summarized apart; the rest are the frames that are measured: their build
times, the median of each repetition and how far those spread, the builds in a frame, and the
bytes consed in a frame."
  (let* ((rest (lambda (key) (mapcar (lambda (run) (after-the-first (getf run key))) runs)))
         (all (apply #'concatenate 'vector (funcall rest :build-ns)))
         (medians (mapcar #'core:median (funcall rest :build-ns)))
         (builds (apply #'concatenate 'vector (funcall rest :builds)))
         (frame-bytes (apply #'concatenate 'vector (funcall rest :frame-bytes))))
    (list :frames-per-run (getf (first runs) :frames)
          :first-frame (list :build-ns (core:summarize
                                        (mapcar (lambda (run) (aref (getf run :build-ns) 0)) runs))
                             :bytes (/ (reduce #'+ (mapcar (lambda (run) (aref (getf run :frame-bytes) 0))
                                                           runs))
                                       (length runs) 1.0))
          :build-ns (core:summarize all)
          :run-medians-ns (list :median (core:median medians) :min (reduce #'min medians)
                                :max (reduce #'max medians))
          :builds (list :mean (/ (reduce #'+ builds) (length builds) 1.0) :max (reduce #'max builds)
                        :cap-hits (reduce #'+ (mapcar (lambda (run) (getf run :capped)) runs)))
          :bytes-per-frame (/ (reduce #'+ frame-bytes) (length frame-bytes) 1.0)
          :gc-ns-per-run (/ (reduce #'+ (mapcar (lambda (run) (getf run :gc-ns)) runs))
                            (length runs) 1.0))))

(defun write-record (directory record)
  (ensure-directories-exist directory)
  (let ((path (merge-pathnames (format nil "~a-~a-~(~a~)-~(~a~)-~a.sexp" (getf record :date)
                                       (or (getf record :commit) "unknown") (getf record :variant)
                                       (getf record :app) (getf record :script-name))
                               directory)))
    (with-open-file (stream path :direction :output :if-exists :error :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-case* :downcase) (*print-pretty* nil))
          (prin1 (list* :almighty-bolt-result 1 record) stream)
          (terpri stream))))
    path))

(defun microseconds (nanoseconds)
  (and nanoseconds (/ nanoseconds 1000.0)))

(defun print-line (variant app script-name summary)
  (let ((ns (getf summary :build-ns)) (spread (getf summary :run-medians-ns))
        (builds (getf summary :builds)))
    (format t "~&~(~10a ~8a~) ~22a ~7,1f us (~,1f to ~,1f)  p95 ~7,1f  p99 ~7,1f  max ~8,1f   builds ~,2f/~d  ~7,0f B/frame   first frame ~7,1f us ~8,0f B~%"
            variant app script-name (microseconds (getf ns :median))
            (microseconds (getf spread :min)) (microseconds (getf spread :max))
            (microseconds (getf ns :p95)) (microseconds (getf ns :p99)) (microseconds (getf ns :max))
            (getf builds :mean) (getf builds :max) (getf summary :bytes-per-frame)
            (microseconds (getf (getf (getf summary :first-frame) :build-ns) :median))
            (getf (getf summary :first-frame) :bytes))))

(defun script-paths ()
  (let ((directory (or (uiop:getenv "SCRIPTS") "apps/7guis/replay/")))
    (directory (merge-pathnames "*.script" (uiop:ensure-directory-pathname directory)))))

(defun main ()
  (let* ((repetitions (environment-number "REPS" 10))
         (warmups (environment-number "WARMUPS" 3))
         (directory (uiop:ensure-directory-pathname (or (uiop:getenv "OUT") "results/")))
         (date (timestamp))
         (source (source-state))
         (machine (machine-profile)))
    (with-masked-float-traps
      (with-autorelease-pool
        (let* ((host (make-host (make-instance 'core:variant) (default-device)
                                +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
               (context (host-context host)))
          (format t "~&~d repetitions after ~d warm-ups; the time is of the variant's build of a frame~%"
                  repetitions warmups)
          (dolist (path (script-paths))
            (let* ((script (core:load-script path))
                   (task (getf (core:script-header script) :app))
                   (name (pathname-name path)))
              (dolist (variant +variants+)
                (let* ((runs (measure-script script variant task context
                                             :repetitions repetitions :warmups warmups))
                       (summary (summary runs))
                       (record (append (list :date date) source
                                       (list :variant variant :app task :script-name name
                                             :script (namestring (enough-namestring path))
                                             :machine machine :repetitions repetitions
                                             :warmups warmups :runs runs :summary summary))))
                  (print-line variant task name summary)
                  (write-record directory record))))))))
    (format t "~&Records written to ~a~%" directory)))

(defun latest-records (directory)
  "The most recent record for each variant, app and script in DIRECTORY."
  (let ((latest (make-hash-table :test 'equal)))
    (dolist (path (sort (directory (merge-pathnames "*.sexp" (uiop:ensure-directory-pathname directory)))
                        #'string< :key #'namestring))
      (with-standard-io-syntax
        (let* ((*read-eval* nil)
               (form (with-open-file (stream path :external-format :utf-8) (read stream))))
          (when (eq (first form) :almighty-bolt-result)
            (let ((record (cddr form)))
              (setf (gethash (list (getf record :variant) (getf record :app) (getf record :script-name))
                             latest)
                    record))))))
    (let ((records '()))
      (maphash (lambda (key record) (declare (ignore key)) (push record records)) latest)
      (sort records #'string< :key (lambda (r) (format nil "~a ~a ~a" (getf r :script-name)
                                                       (getf r :app) (getf r :variant)))))))

(defun report ()
  "Print the latest record of each run in =results/= (or OUT=)."
  (let ((records (latest-records (or (uiop:getenv "OUT") "results/"))))
    (format t "~&~d runs; the median of a frame's build, the spread of the repetitions' medians~%"
            (length records))
    (dolist (record records)
      (print-line (getf record :variant) (getf record :app) (getf record :script-name)
                  (getf record :summary)))))
