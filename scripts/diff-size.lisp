(require :asdf)

(defpackage #:almighty-bolt-scripts/diff-size
  (:use #:cl)
  (:export #:measure-diff
           #:print-measurement))

(in-package #:almighty-bolt-scripts/diff-size)

;;;; How large a change is, in the way the change-cost tests count (decision record 0024): the lines
;;;; added and removed, not counting blank lines or lines that are only a comment, which are counted
;;;; apart; and the files and hunks touched. A change is the difference between two revisions, as
;;;; git shows it with no context lines. From the repository root:
;;;;   FROM=base TO=change-cost/e1-tests sbcl --script scripts/diff-size.lisp
;;;; or, loaded, (measure-diff "from" "to" :paths '("apps/7guis/crud/")).

(defun git-diff (from to paths)
  (uiop:run-program (append (list "git" "diff" "-U0" "--no-color" "--no-ext-diff" from to)
                            (and paths (cons "--" paths)))
                    :output :string))

(defun blank-p (text)
  (every (lambda (character) (member character '(#\Space #\Tab))) text))

(defun comment-p (text)
  (let ((trimmed (string-left-trim '(#\Space #\Tab) text)))
    (and (plusp (length trimmed)) (char= (char trimmed 0) #\;))))

(defun measure-diff (from to &key paths)
  "A plist for the change from revision FROM to TO (limited to PATHS if given): :files, :hunks,
:added and :removed (lines of code), :comments-added and :comments-removed, and :by-file, a list of
(path added removed comments)."
  (let ((files '()) (current nil) (hunks 0)
        (added 0) (removed 0) (comments-added 0) (comments-removed 0))
    (dolist (line (uiop:split-string (git-diff from to paths) :separator '(#\Newline)))
      (cond
        ((and (> (length line) 6) (string= (subseq line 0 6) "+++ b/"))
         (setf current (list (subseq line 6) 0 0 0))
         (push current files))
        ((and (> (length line) 4) (string= (subseq line 0 4) "--- ")))
        ((and (> (length line) 2) (string= (subseq line 0 2) "@@")) (incf hunks))
        ((and (plusp (length line)) (member (char line 0) '(#\+ #\-)) current)
         (let ((text (subseq line 1))
               (plus (char= (char line 0) #\+)))
           (cond ((blank-p text))
                 ((comment-p text)
                  (incf (fourth current))
                  (if plus (incf comments-added) (incf comments-removed)))
                 (plus (incf added) (incf (second current)))
                 (t (incf removed) (incf (third current))))))))
    (list :files (length files) :hunks hunks :added added :removed removed
          :comments-added comments-added :comments-removed comments-removed
          :by-file (reverse files))))

(defun print-measurement (label measurement)
  (format t "~&~a: ~d line~:p of code added, ~d removed (~d changed in all); ~d file~:p, ~d hunk~:p; comments +~d -~d~%"
          label (getf measurement :added) (getf measurement :removed)
          (+ (getf measurement :added) (getf measurement :removed))
          (getf measurement :files) (getf measurement :hunks)
          (getf measurement :comments-added) (getf measurement :comments-removed))
  (dolist (entry (getf measurement :by-file))
    (format t "~&    ~a  +~d -~d~%" (first entry) (second entry) (third entry))))

(let ((from (uiop:getenv "FROM")) (to (uiop:getenv "TO")))
  (when (and from to)
    (print-measurement (format nil "~a..~a" from to) (measure-diff from to))))
