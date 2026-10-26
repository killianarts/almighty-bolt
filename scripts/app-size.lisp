(require :asdf)

(defpackage #:almighty-bolt-scripts/app-size
  (:use #:cl)
  (:export #:app-sizes))

(in-package #:almighty-bolt-scripts/app-size)

;;;; The size of each 7GUIs app in each variant (decision record 0024): the lines of code of its file
;;;; (not blank, not only a comment), and the number of distinct framework concepts it uses, which
;;;; are the distinct symbols it imports from the packages of the variants (the widgets, inputs,
;;;; nodes, signals and views, and the constructor of the variant). From the repository root:
;;;;   sbcl --script scripts/app-size.lisp

(defparameter +tasks+ '("counter" "temperature" "flight" "timer" "crud" "circles" "cells"))
(defparameter +variants+ '("retained" "reactive" "immediate"))

(defun lines-of-code (path)
  (with-open-file (stream path)
    (loop :for line = (read-line stream nil nil)
          :while line
          :count (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (and (plusp (length trimmed)) (char/= (char trimmed 0) #\;))))))

(defun first-form (path)
  (let ((*package* (make-package (gensym "SIZE") :use '(:cl)))
        (*read-eval* nil))
    (unwind-protect
         (with-open-file (stream path) (read stream))
      (delete-package *package*))))

(defun concepts (path)
  "The distinct symbols of PATH's defpackage imported from the packages of a variant."
  (let ((symbols '()))
    (dolist (clause (rest (rest (first-form path))))
      (when (and (consp clause) (eq (first clause) :import-from))
        (let ((package (string-upcase (string (second clause)))))
          (when (eql 0 (search "ALMIGHTY-BOLT-VARIANT-" package))
            (dolist (symbol (cddr clause))
              (pushnew (cons package (string-upcase (string symbol))) symbols :test #'equal))))))
    (length symbols)))

(defun app-sizes ()
  "A list of (task variant lines concepts)."
  (loop :for task :in +tasks+
        :append (loop :for variant :in +variants+
                      :collect (let ((path (format nil "apps/7guis/~a/~a.lisp" task variant)))
                                 (list task variant (lines-of-code path) (concepts path))))))

(let ((sizes (app-sizes)))
  (format t "~&~12a ~{~24a~}~%" "task" (mapcar (lambda (variant) (format nil "~a (lines, concepts)" variant))
                                               +variants+))
  (dolist (task +tasks+)
    (format t "~&~12a" task)
    (dolist (variant +variants+)
      (let ((entry (find-if (lambda (e) (and (string= (first e) task) (string= (second e) variant))) sizes)))
        (format t " ~23a" (format nil "~d, ~d" (third entry) (fourth entry)))))
    (terpri))
  (format t "~&~12a" "total")
  (dolist (variant +variants+)
    (let ((rows (remove-if-not (lambda (e) (string= (second e) variant)) sizes)))
      (format t " ~23a" (format nil "~d, ~d" (reduce #'+ (mapcar #'third rows))
                                (reduce #'+ (mapcar #'fourth rows))))))
  (terpri))
