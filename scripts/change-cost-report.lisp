(require :asdf)

(defpackage #:almighty-bolt-scripts/change-cost-report
  (:use #:cl))

(in-package #:almighty-bolt-scripts/change-cost-report)

;;;; The change-cost table (decision record 0024): for each of the five edits, how large the change
;;;; is from the commit it started at to the end of its branch, in the parts it is made of: the
;;;; acceptance tests and model tests (shared), the shared model, and the code of each variant's
;;;; app. Lines of code are those added and those removed, with no blank lines, no comment lines
;;;; and no change of white space (scripts/diff-size.lisp). The patches are written to
;;;;   DIRECTORY/e<n>/<part>.patch
;;;; From the repository root:   OUT=results/change-cost/ sbcl --script scripts/change-cost-report.lisp

(load (merge-pathnames "diff-size.lisp" (or *load-truename* "scripts/")))

(defparameter +edits+ '("e1" "e2" "e3" "e4" "e5"))
(defparameter +parts+
  '(("tests" "apps/7guis/t/")
    ("model" "apps/7guis/crud/model.lisp" "apps/7guis/flight/model.lisp"
     "apps/7guis/temperature/model.lisp")
    ("retained" "apps/7guis/crud/retained.lisp" "apps/7guis/flight/retained.lisp"
     "apps/7guis/temperature/retained.lisp")
    ("reactive" "apps/7guis/crud/reactive.lisp" "apps/7guis/flight/reactive.lisp"
     "apps/7guis/temperature/reactive.lisp")
    ("immediate" "apps/7guis/crud/immediate.lisp" "apps/7guis/flight/immediate.lisp"
     "apps/7guis/temperature/immediate.lisp")))

(defun shell (&rest command)
  (string-trim '(#\Newline) (uiop:run-program command :output :string)))

(let* ((out (uiop:ensure-directory-pathname (or (uiop:getenv "OUT") "change-cost/")))
       (base (shell "git" "merge-base" "change-cost/base" "change-cost/e1"))
       (rows '()))
  (dolist (edit +edits+)
    (let ((branch (format nil "change-cost/~a" edit)))
      (let ((row (list edit)))
        (dolist (part +parts+)
          (let* ((paths (rest part))
                 (measurement (almighty-bolt-scripts/diff-size:measure-diff base branch :paths paths))
                 (patch (uiop:run-program (append (list "git" "diff" "-w" "--no-color" base branch)
                                                  (cons "--" paths))
                                          :output :string)))
            (ensure-directories-exist (merge-pathnames (format nil "~a/" edit) out))
            (with-open-file (stream (merge-pathnames (format nil "~a/~a.patch" edit (first part)) out)
                                    :direction :output :if-exists :supersede)
              (write-string patch stream))
            (setf row (append row (list (list (first part)
                                              (getf measurement :added) (getf measurement :removed)
                                              (getf measurement :files) (getf measurement :hunks)))))))
        (push row rows))))
  (setf rows (nreverse rows))
  (format t "~&Lines of code added and removed (blank, comment and white-space changes not counted), from ~a~%" base)
  (format t "~&~4a ~{~18a~}~%" "edit" (mapcar #'first +parts+))
  (dolist (row rows)
    (format t "~&~4a" (first row))
    (dolist (cell (rest row))
      (format t " ~18a" (format nil "+~d -~d (~d f, ~d h)" (second cell) (third cell)
                                (fourth cell) (fifth cell))))
    (terpri))
  (with-open-file (stream (merge-pathnames "change-cost.sexp" out)
                          :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-case* :downcase))
        (prin1 (list :almighty-bolt-change-cost 1 :base base
                     :parts (mapcar #'first +parts+) :rows rows)
               stream)
        (terpri stream)))))
