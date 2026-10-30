(defpackage #:almighty-bolt-bench/results
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:registry #:almighty-bolt-7guis/common/registry))
  (:import-from #:almighty-bolt/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt/platform/macos/host
                #:make-host
                #:host-context
                #:host-variant
                #:host-ring
                #:host-display-list
                #:host-acquire
                #:run-frame)
  (:import-from #:almighty-bolt/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt/render/metal/target
                #:+pixel-format-bgra8-unorm+
                #:make-offscreen-target
                #:offscreen-target-descriptor)
  (:import-from #:almighty-bolt-bench-null/null
                #:make-null-variant)
  (:export #:main
           #:overhead-main
           #:report
           #:shell
           #:machine-profile
           #:source-state
           #:timestamp
           #:write-record
           #:environment-number))

(in-package #:almighty-bolt-bench/results)

;;; The harness of the benchmark protocol: an app is driven by a recorded input script
;;; (replay), a script is run a number of times after warm-up runs are thrown away, and one
;;; result record is written for each variant, app and script, in
;;; =results/=, never to be edited, only superseded. Each frame's time is that of the variant's
;;; build (the settle loop) on the real clock, with the real text engine and no window or GPU, so
;;; the host's waiting and encoding are not in it. A record has the commit, the machine, the raw
;;; samples of every repetition and the summary.
;;;
;;; There are two kinds of run. The headless one (above) times only the variant's build. The host
;;; one plays the script through the host's own frame, RUN-FRAME, into an
;;; off-screen texture with the real renderer: the frame is the variant's build (with its update,
;;; layout and paint phases), the wait for the GPU's frame before, and the encoding, and the ring
;;; has the garbage collection, the first build against the rest, and the GPU's time besides.
;;; There is no window, so no vsync and no wait for a drawable: those numbers are not those of a
;;; window and are not to be compared with them.

(defparameter +variants+ '(:immediate))

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
  (let ((object (registry:make-app-object (registry:find-app variant task))))
    (core:replay script object host-context :measure t)))

(defun measure-script (script variant task context &key (repetitions 10) (warmups 3))
  "Replay SCRIPT in VARIANT's app of TASK WARMUPS times and then REPETITIONS times, measured.
Returns the repetitions, each a plist of its frames' build nanoseconds and builds, the time,
bytes and number of the events delivered before each frame (a variant handles an event in
HANDLE-EVENT or in the frame, so a frame's cost is its build and its events), the bytes consed
and garbage collection nanoseconds, and the hashes of its frames. An error if two runs draw
different frames."
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
                    :event-ns (map 'vector #'fourth (core:replay-result-samples result))
                    :event-bytes (map 'vector #'fifth (core:replay-result-samples result))
                    :events (map 'vector #'sixth (core:replay-result-samples result))
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
bytes consed in a frame. :build-ns and :bytes-per-frame are of the build only, as they were in
the records before the events were counted; :event-ns, :events-per-frame, :with-events-ns (the
build and the events delivered before it, in each frame) and :bytes-per-frame-with-events say
what the build alone leaves out."
  (let* ((rest (lambda (key) (mapcar (lambda (run) (after-the-first (getf run key))) runs)))
         (all (apply #'concatenate 'vector (funcall rest :build-ns)))
         (medians (mapcar #'core:median (funcall rest :build-ns)))
         (builds (apply #'concatenate 'vector (funcall rest :builds)))
         (frame-bytes (apply #'concatenate 'vector (funcall rest :frame-bytes)))
         (event-ns (apply #'concatenate 'vector (funcall rest :event-ns)))
         (event-bytes (apply #'concatenate 'vector (funcall rest :event-bytes)))
         (events (apply #'concatenate 'vector (funcall rest :events))))
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
          :event-ns (core:summarize event-ns)
          :events-per-frame (/ (reduce #'+ events) (length events) 1.0)
          :with-events-ns (core:summarize (map 'vector #'+ all event-ns))
          :bytes-per-frame-with-events (/ (+ (reduce #'+ frame-bytes) (reduce #'+ event-bytes))
                                          (length frame-bytes) 1.0)
          :gc-ns-per-run (/ (reduce #'+ (mapcar (lambda (run) (getf run :gc-ns)) runs))
                            (length runs) 1.0))))

(defparameter +ring-columns+
  '(:start-ns :end-ns :build-ns :wait-ns :encode-ns :builds :capped :bytes :gc-ns :first-build-ns
    :update-ns :layout-ns :paint-ns :gpu-ns :all-builds-ns :events-ns :events)
  "What a repetition of a host run keeps of each frame of the ring, in this order.")

(defun frame-row (ring index)
  (list (core:frame-start-ns ring index) (core:frame-end-ns ring index)
        (core:frame-build-ns ring index) (core:frame-drawable-wait-ns ring index)
        (core:frame-encode-ns ring index) (core:frame-builds ring index)
        (if (core:frame-capped-p ring index) 1 0) (core:frame-bytes-consed ring index)
        (core:frame-gc-ns ring index) (core:frame-first-build-ns ring index)
        (core:frame-update-ns ring index) (core:frame-layout-ns ring index)
        (core:frame-paint-ns ring index) (core:frame-gpu-ns ring index)
        (core:frame-all-builds-ns ring index) (core:frame-events-ns ring index)
        (core:frame-events ring index)))

(defun host-run-once (script variant task host)
  "Play SCRIPT into a new app of VARIANT and TASK through HOST's frame, drawing into an
off-screen texture of the script's size. Returns the replay result and the rows of the host's
ring, one for each frame (see +RING-COLUMNS+)."
  (let* ((header (core:script-header script))
         (scale 2)
         (device (default-device))
         (target (make-offscreen-target device (* scale (getf header :width 400))
                                        (* scale (getf header :height 300))))
         (pass (offscreen-target-descriptor target))
         (context (host-context host))
         (ring (host-ring host))
         (object (registry:make-app-object (registry:find-app variant task))))
    (setf (host-variant host) object
          (host-acquire host) (lambda () (values pass 0))
          (core:context-scale context) (float scale 1f0))
    (core:clear-frame-ring ring)
    (let ((result (core:replay script object context
                               :display-list (host-display-list host)
                               :frame-function
                               (lambda ()
                                 (with-autorelease-pool
                                   (run-frame host (core:context-width context)
                                              (core:context-height context)
                                              (core:context-scale context)))
                                 (let ((last (1- (core:frame-ring-count ring))))
                                   (values (core:frame-builds ring last)
                                           (core:frame-capped-p ring last)))))))
      (values result (loop :for i :below (core:frame-ring-count ring) :collect (frame-row ring i))))))

(defun measure-host-scripts (script task variants host &key (repetitions 10) (warmups 3))
  "Host replays of SCRIPT in each of VARIANTS: WARMUPS rounds thrown away and then REPETITIONS
rounds kept, each round one replay in each variant in turn, because the GPU's and the CPU's
speed drift over a run (a variant that is measured ten times in a row gets one state of the
machine and the next gets another). Returns a list of (variant . repetitions), the repetitions
lists of frame rows. An error if two runs of a variant draw different frames."
  (dotimes (i warmups)
    (dolist (variant variants)
      (host-run-once script variant task host)))
  (let ((runs (mapcar (lambda (variant) (list variant)) variants))
        (references (mapcar (lambda (variant) (cons variant nil)) variants)))
    (dotimes (i repetitions)
      (dolist (variant variants)
        (multiple-value-bind (result rows) (host-run-once script variant task host)
          (let ((hashes (map 'vector #'cdr (core:replay-result-hashes result)))
                (reference (assoc variant references)))
            (cond ((null (cdr reference)) (setf (cdr reference) hashes))
                  ((not (equalp (cdr reference) hashes))
                   (error "Two host runs of the script gave different frames in ~(~a ~a~)."
                          variant task))))
          (push rows (cdr (assoc variant runs))))))
    (mapcar (lambda (entry) (cons (car entry) (reverse (cdr entry)))) runs)))

(defun rows-ring (runs)
  "One ring with the frames of every repetition but the first frame of each, which is cold."
  (let ((ring (core:make-frame-ring :capacity (max 1 (reduce #'+ (mapcar #'length runs))))))
    (dolist (rows runs ring)
      (dolist (row (rest rows))
        (destructuring-bind (start end build wait encode builds capped bytes gc first update layout
                             paint gpu all-builds events-ns events)
            row
          (core:record-frame ring start end 0 bytes build wait encode builds (plusp capped))
          (core:record-frame-details ring :gc-ns gc :first-build-ns first :update-ns update
                                          :layout-ns layout :paint-ns paint :gpu-ns gpu
                                          :all-builds-ns all-builds :events-ns events-ns
                                          :events events))))))

(defun host-summary (runs)
  "What the repetitions of a host run say: the summary of the ring that all their frames, the
first of each run apart, make (see SUMMARIZE-RING), and the first frames' build time."
  (let ((summary (core:summarize-ring (rows-ring runs))))
    (list* :frames-per-run (length (first runs))
           :first-frame-build-ns (core:summarize
                                  (mapcar (lambda (rows) (third (first rows))) runs))
           summary)))

(defun write-record (directory record)
  (ensure-directories-exist directory)
  (let ((path (merge-pathnames (format nil "~a-~a-~(~a~)-~(~a~)-~a~a.sexp" (getf record :date)
                                       (or (getf record :commit) "unknown") (getf record :variant)
                                       (getf record :app) (getf record :script-name)
                                       (case (getf record :mode) (:host "-host") (:overhead "-overhead") (t "")))
                               directory)))
    (with-open-file (stream path :direction :output :if-exists :error :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-case* :downcase) (*print-pretty* nil))
          (prin1 (list* :almighty-bolt-result 2 record) stream)
          (terpri stream))))
    path))

(defun microseconds (nanoseconds)
  (and nanoseconds (/ nanoseconds 1000.0)))

(defun print-line (variant app script-name summary)
  (let ((ns (getf summary :build-ns)) (spread (getf summary :run-medians-ns))
        (builds (getf summary :builds)))
    (format t "~&~(~10a ~8a~) ~22a ~7,1f us (~,1f to ~,1f)  p95 ~7,1f  p99 ~7,1f  max ~8,1f   builds ~,2f/~d  ~7,0f B/frame   first frame ~7,1f us ~8,0f B   with events ~7,1f us ~7,0f B (~,2f events)~%"
            variant app script-name (microseconds (getf ns :median))
            (microseconds (getf spread :min)) (microseconds (getf spread :max))
            (microseconds (getf ns :p95)) (microseconds (getf ns :p99)) (microseconds (getf ns :max))
            (getf builds :mean) (getf builds :max) (getf summary :bytes-per-frame)
            (microseconds (getf (getf (getf summary :first-frame) :build-ns) :median))
            (getf (getf summary :first-frame) :bytes)
            (microseconds (getf (getf summary :with-events-ns) :median))
            (getf summary :bytes-per-frame-with-events) (getf summary :events-per-frame))))

(defun print-host-line (variant app script-name summary)
  (flet ((us (key part) (microseconds (getf (getf summary key) part))))
    (format t "~&~(~10a ~8a~) ~22a frame ~7,1f us (p95 ~7,1f)  build ~7,1f  update ~6,1f layout ~6,1f paint ~6,1f  wait ~7,1f  encode ~6,1f  gpu ~7,1f   builds ~,2f/~d  extra ~4,1f%   events ~6,1f (~,2f)~%"
            variant app script-name (us :duration :median) (us :duration :p95) (us :build :median)
            (us :update :median) (us :layout :median) (us :paint :median) (us :wait :median)
            (us :encode :median) (us :gpu :median)
            (getf (getf summary :builds) :mean) (getf (getf summary :builds) :max)
            (* 100 (getf (getf summary :settle) :share-of-build))
            (us :events :median) (getf summary :events-per-frame))))

(defun script-paths ()
  "The scripts to run: those of SCRIPTS (a directory, or several separated by commas) or, if it
is not set, those of the 7GUIs tasks and of the stress apps."
  (let ((directories (let ((value (uiop:getenv "SCRIPTS")))
                       (if (and value (plusp (length value)))
                           (uiop:split-string value :separator '(#\,))
                           '("apps/7guis/replay/" "apps/stress/replay/")))))
    (loop :for directory :in directories
          :append (directory (merge-pathnames "*.script" (uiop:ensure-directory-pathname directory))))))

(defun headless-run (scripts repetitions warmups directory date source machine)
  "The build of every variant on every script, with the real text engine and no renderer."
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((host (make-host (make-instance 'core:variant) (default-device)
                              +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
             (context (host-context host)))
        (format t "~&Headless: ~d repetitions after ~d warm-ups; the time is of the variant's build of a frame~%"
                repetitions warmups)
        (dolist (path scripts)
          (let* ((script (core:load-script path))
                 (task (getf (core:script-header script) :app))
                 (name (pathname-name path)))
            (dolist (variant +variants+)
              (let* ((runs (measure-script script variant task context
                                           :repetitions repetitions :warmups warmups))
                     (summary (summary runs))
                     (record (append (list :date date) source
                                     (list :mode :headless :variant variant :app task
                                           :script-name name
                                           :script (namestring (enough-namestring path))
                                           :machine machine :repetitions repetitions
                                           :warmups warmups :runs runs :summary summary))))
                (print-line variant task name summary)
                (write-record directory record)))))))))

(defun host-run (scripts repetitions warmups directory date source machine)
  "Every variant on every script through the host's frame into an off-screen texture, with the
instrumentation on."
  (with-masked-float-traps
    (with-autorelease-pool
      (let ((host (make-host (make-instance 'core:variant) (default-device)
                             +pixel-format-bgra8-unorm+ :capacity 65536 :ring-capacity 65536
                                                        :scale 2 :phases t)))
        (format t "~&Host: ~d repetitions after ~d warm-ups; frames through the host into an off-screen texture (no vsync, no drawable): not a window's numbers~%"
                repetitions warmups)
        (dolist (path scripts)
          (let* ((script (core:load-script path))
                 (task (getf (core:script-header script) :app))
                 (name (pathname-name path)))
            (dolist (entry (measure-host-scripts script task +variants+ host
                                                 :repetitions repetitions :warmups warmups))
              (let* ((variant (car entry))
                     (runs (cdr entry))
                     (summary (host-summary runs))
                     (record (append (list :date date) source
                                     (list :mode :host :variant variant :app task
                                           :script-name name
                                           :script (namestring (enough-namestring path))
                                           :machine machine :repetitions repetitions
                                           :warmups warmups :columns +ring-columns+
                                           :interleaved t
                                           :runs runs :summary summary))))
                (print-host-line variant task name summary)
                (write-record directory record)))))))))

(defun main ()
  (let* ((repetitions (environment-number "REPS" 10))
         (warmups (environment-number "WARMUPS" 3))
         (directory (uiop:ensure-directory-pathname (or (uiop:getenv "OUT") "results/")))
         (mode (string-downcase (or (uiop:getenv "MODE") "both")))
         (date (timestamp))
         (source (source-state))
         (machine (machine-profile))
         (scripts (script-paths)))
    (when (member mode '("headless" "both") :test #'string=)
      (headless-run scripts repetitions warmups directory date source machine))
    (when (member mode '("host" "both") :test #'string=)
      (host-run scripts repetitions warmups directory date source machine))
    (format t "~&Records written to ~a~%" directory)))

(defun median-build-ns (result)
  "The median build time of the frames of a measured replay after the first."
  (core:median (subseq (map 'vector #'first (core:replay-result-samples result)) 1)))

(defun overhead-headless (scripts repetitions context)
  "For each variant and script, what the instrumentation (the phases and the timing of the
builds) adds to the build of a frame: the median of a run's median build time with it off and
on, in rounds that do one and then the other (the other first in every second round)."
  (let ((rows '()))
    (dolist (path scripts (nreverse rows))
      (let* ((script (core:load-script path))
             (task (getf (core:script-header script) :app))
             (name (pathname-name path)))
        (dolist (variant +variants+)
          (let ((off '()) (on '()))
            (flet ((run (phases)
                     (setf (core:context-phases-p context) phases)
                     (let ((result (run-once script variant task context)))
                       (setf (core:context-phases-p context) nil)
                       (median-build-ns result))))
              (dotimes (i 3) (run nil) (run t))                       ; warm up both
              (dotimes (i repetitions)
                (if (evenp i)
                    (progn (push (run nil) off) (push (run t) on))
                    (progn (push (run t) on) (push (run nil) off)))))
            (let* ((off-median (core:median off))
                   (on-median (core:median on)))
              (push (list :variant variant :app task :script-name name
                          :off-ns off-median :on-ns on-median
                          :added-ns (- on-median off-median)
                          :added-share (/ (- on-median off-median) (max 1 off-median) 1.0)
                          :off-runs (coerce (reverse off) 'vector)
                          :on-runs (coerce (reverse on) 'vector))
                    rows))))))))

(defun overhead-null (repetitions frames)
  "The null variant through the host, off-screen, with the instrumentation off and on: the
median over repetitions of a run's median frame time and build time. The null variant does
nothing in a build, so the difference is the instrumentation's own."
  (let* ((host (make-host (make-null-variant)
                          (default-device) +pixel-format-bgra8-unorm+
                          :capacity 4096 :ring-capacity (+ frames 16) :scale 2))
         (context (host-context host))
         (ring (host-ring host))
         (device (default-device))
         (target (make-offscreen-target device 1200 800))
         (pass (offscreen-target-descriptor target))
         (off '()) (on '()))
    (setf (host-acquire host) (lambda () (values pass 0)))
    (flet ((run (phases)
             (setf (core:context-phases-p context) phases)
             (core:clear-frame-ring ring)
             (dotimes (i frames)
               (with-autorelease-pool (run-frame host 600f0 400f0 2f0)))
             (setf (core:context-phases-p context) nil)
             (let ((summary (core:summarize-ring ring :skip 10)))
               (list (getf (getf summary :duration) :median)
                     (getf (getf summary :build) :median)))))
      (dotimes (i 2) (run nil) (run t))
      (dotimes (i repetitions)
        (if (evenp i)
            (progn (push (run nil) off) (push (run t) on))
            (progn (push (run t) on) (push (run nil) off)))))
    (flet ((med (runs index) (core:median (mapcar (lambda (run) (nth index run)) runs))))
      (list :frames-per-run frames
            :frame-off-ns (med off 0) :frame-on-ns (med on 0)
            :build-off-ns (med off 1) :build-on-ns (med on 1)))))

(defun overhead-main ()
  "Measure what the instrumentation costs, write the record, and say it."
  (let* ((repetitions (environment-number "REPS" 10))
         (directory (uiop:ensure-directory-pathname (or (uiop:getenv "OUT") "results/")))
         (date (timestamp)) (source (source-state)) (machine (machine-profile)))
    (with-masked-float-traps
      (with-autorelease-pool
        (let* ((context (host-context (make-host (make-instance 'core:variant) (default-device)
                                                 +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2)))
               (headless (overhead-headless (script-paths) repetitions context))
               (null (overhead-null repetitions 1000)))
          (format t "~&What the instrumentation adds (phases, build timing), median of ~d runs:~%" repetitions)
          (dolist (row headless)
            (format t "~&  ~(~10a ~8a~) ~22a build ~7,3f us off, ~7,3f us on: +~6,3f us (~,1f%)~%"
                    (getf row :variant) (getf row :app) (getf row :script-name)
                    (/ (getf row :off-ns) 1000.0) (/ (getf row :on-ns) 1000.0)
                    (/ (getf row :added-ns) 1000.0) (* 100 (getf row :added-share))))
          (format t "~&  null variant through the host (~d frames a run): frame ~,1f us off, ~,1f us on; build ~,3f us off, ~,3f us on~%"
                  (getf null :frames-per-run) (/ (getf null :frame-off-ns) 1000.0)
                  (/ (getf null :frame-on-ns) 1000.0) (/ (getf null :build-off-ns) 1000.0)
                  (/ (getf null :build-on-ns) 1000.0))
          (write-record directory
                        (append (list :date date) source
                                (list :mode :overhead :variant :all :app :instrumentation
                                      :script-name "overhead" :machine machine
                                      :repetitions repetitions :headless headless :null null))))))))

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
              (setf (gethash (list (or (getf record :mode) :headless) (getf record :variant)
                                   (getf record :app) (getf record :script-name))
                             latest)
                    record))))))
    (let ((records '()))
      (maphash (lambda (key record) (declare (ignore key)) (push record records)) latest)
      (sort records #'string< :key (lambda (r) (format nil "~a ~a ~a" (getf r :script-name)
                                                       (getf r :app) (getf r :variant)))))))

(defun report ()
  "Print the latest record of each run in =results/= (or OUT=), the headless ones and then the
host ones."
  (let ((records (latest-records (or (uiop:getenv "OUT") "results/"))))
    (dolist (record (remove-if-not (lambda (record) (eq (getf record :mode) :overhead)) records))
      (format t "~&~%overhead of the instrumentation, in microseconds of a frame's build:~%")
      (dolist (row (getf record :headless))
        (format t "~&  ~(~10a ~8a~) ~22a off ~7,3f on ~7,3f  +~6,3f (~,1f%)~%"
                (getf row :variant) (getf row :app) (getf row :script-name)
                (/ (getf row :off-ns) 1000.0) (/ (getf row :on-ns) 1000.0)
                (/ (getf row :added-ns) 1000.0) (* 100 (getf row :added-share))))
      (let ((null (getf record :null)))
        (format t "~&  null variant through the host: frame ~,1f us off, ~,1f us on~%"
                (/ (getf null :frame-off-ns) 1000.0) (/ (getf null :frame-on-ns) 1000.0))))
    (dolist (mode '(:headless :host))
      (let ((of-mode (remove-if-not (lambda (record) (eq (or (getf record :mode) :headless) mode))
                                    records)))
        (when of-mode
          (format t "~&~%~(~a~): ~d runs; ~a~%" mode (length of-mode)
                  (if (eq mode :headless)
                      "the median of a frame's build, the spread of the repetitions' medians"
                      "medians, in microseconds, of the frames after the first of each run"))
          (dolist (record of-mode)
            (funcall (if (eq mode :host) #'print-host-line #'print-line)
                     (getf record :variant) (getf record :app) (getf record :script-name)
                     (getf record :summary))))))))
