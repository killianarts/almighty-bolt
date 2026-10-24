(defpackage #:almighty-bolt-spikes/e-frame-loop
  (:use #:cl)
  (:local-nicknames (#:b #:almighty-bolt-spikes/b-own)
                    (#:d #:almighty-bolt-spikes/d-text))
  (:export #:main))

(in-package #:almighty-bolt-spikes/e-frame-loop)

;;; Spike E: the text scene of Spike D, drawn for a long run of requested frames
;;; (how much does the whole path, callbacks and all, allocate?), and then a window
;;; left alone (does the process sleep?).

(defun process-id ()
  (sb-alien:alien-funcall (sb-alien:extern-alien "getpid" (function sb-alien:int))))

(defun top-row (pid seconds)
  "Ask top how PID behaved over SECONDS: its row for the second sample, as text."
  (let* ((output (uiop:run-program (list "top" "-l" "2" "-s" (princ-to-string seconds)
                                         "-pid" (princ-to-string pid)
                                         "-stats" "pid,cpu,idlew")
                                   :output :string :ignore-error-status t))
         (lines (remove "" (mapcar (lambda (line) (string-trim " " line))
                                   (uiop:split-string output :separator '(#\Newline)))
                        :test #'string=))
         (rows (remove-if-not (lambda (line)
                                (and (plusp (length line)) (digit-char-p (char line 0))))
                              lines)))
    (or (car (last rows)) "(no row)")))

(defun idle-report (rig)
  "Leave the window alone twice: once with nothing else running, measuring from
inside, and once with top watching from outside (running top costs a little itself,
so the two are not mixed)."
  (declare (ignore rig))
  (let ((seconds 10))
    (format t "~%Leaving the window alone for ~d seconds, measuring from inside...~%" seconds)
    (finish-output)
    (let ((cpu-before (get-internal-run-time))
          (consed-before (sb-ext:get-bytes-consed)))
      (sleep seconds)
      (format t "CPU time the process used meanwhile:    ~,1f ms~%"
              (/ (- (get-internal-run-time) cpu-before) (/ internal-time-units-per-second 1000.0)))
      (format t "bytes consed meanwhile:                 ~:d~%"
              (- (sb-ext:get-bytes-consed) consed-before)))
    (format t "Again for ~d seconds, with top watching...~%" seconds)
    (finish-output)
    (format t "top, pid / %cpu / idle wakeups:         ~a~%" (top-row (process-id) seconds))
    (finish-output)))

(defun main (&key (frames 2000))
  "Draw FRAMES requested frames of text, leave the window alone, and quit."
  (b:main :frames frames
          :title "Spike E: frame loop and idle"
          :make-frame-function #'d:make-frame-function
          :more-report #'idle-report))
