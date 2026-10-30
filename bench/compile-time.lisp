(defpackage #:almighty-bolt-bench/compile-time
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:results #:almighty-bolt-bench/results))
  (:export #:main))

(in-package #:almighty-bolt-bench/compile-time)

;;; How long each variant takes to compile from nothing, and to compile again after a change to
;;; one file (decision record 0023). Each measurement is a new sbcl process with an empty fasl
;;; cache (scripts/compile-time.lisp), so the core, which every variant needs, is timed apart from
;;; the variant. The files that the incremental steps touch are touched back to what they were:
;;; only their modification times change.

(defparameter +variants+ '("immediate"))

(defun run-once (variant)
  "The steps of one compile of VARIANT, as a list of (name . seconds)."
  (let* ((cache (format nil "/tmp/almighty-bolt-compile-cache-~a-~d/" variant (get-universal-time)))
         (output (unwind-protect
                      (uiop:run-program
                       (list "env" (format nil "VARIANT=~a" variant) (format nil "CACHE=~a" cache)
                             "sbcl" "--noinform" "--non-interactive" "--no-sysinit" "--no-userinit"
                             "--load" "scripts/compile-time.lisp")
                       :output :string :error-output nil)
                   (uiop:delete-directory-tree (uiop:ensure-directory-pathname cache)
                                               :validate t :if-does-not-exist :ignore))))
    (loop :for line :in (uiop:split-string output :separator '(#\Newline))
          :when (eql 0 (search "step " line))
            :collect (let* ((words (uiop:split-string line :separator '(#\Space))))
                       (cons (second words) (let ((*read-default-float-format* 'double-float))
                                              (read-from-string (third words))))))))

(defun main ()
  (let* ((repetitions (results:environment-number "REPS" 3))
         (directory (uiop:ensure-directory-pathname (or (uiop:getenv "OUT") "results/")))
         (date (results:timestamp)) (source (results:source-state))
         (machine (results:machine-profile)))
    (dolist (variant +variants+)
      (let ((runs (loop :repeat repetitions :collect (run-once variant))))
        (let* ((names (mapcar #'car (first runs)))
               (medians (loop :for name :in names
                              :collect (cons name (core:median
                                                   (mapcar (lambda (run) (cdr (assoc name run :test #'string=)))
                                                           runs))))))
          (format t "~&~(~10a~) ~{~a~^   ~}~%" variant
                  (mapcar (lambda (entry) (format nil "~a ~,2f s" (car entry) (cdr entry))) medians))
          (results:write-record
           directory
           (append (list :date date) source
                   (list :mode :compile :variant (intern (string-upcase variant) :keyword)
                         :app :compile :script-name "compile-time" :machine machine
                         :repetitions repetitions :runs runs :medians medians))))))
    (format t "~&Records written to ~a~%" directory)))
