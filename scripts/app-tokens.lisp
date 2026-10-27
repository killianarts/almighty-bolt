(require :asdf)

(defpackage #:almighty-bolt-scripts/app-tokens
  (:use #:cl)
  (:export #:token-counts))

(in-package #:almighty-bolt-scripts/app-tokens)

;;;; How large each app is in tokens, which does not depend on how a form is laid out in lines (the
;;;; measure of scripts/app-size.lisp does, so a change that only moves children onto lines of their
;;;; own shows in it). A token is a symbol, a number, a string or a character: what is left when the
;;;; comments, the white space, the parentheses and the quote marks are taken away. Two counts for
;;;; each file: all of it, and all of it but its first form (the DEFPACKAGE, which a change of what
;;;; an app imports changes). From the repository root:
;;;;   sbcl --script scripts/app-tokens.lisp

(defparameter +tasks+ '("counter" "temperature" "flight" "timer" "crud" "circles" "cells"
                        "dashboard" "rows" "editor"))
(defparameter +stress-tasks+ '("dashboard" "rows" "editor"))
(defparameter +variants+ '("retained" "reactive" "immediate"))

(defun delimiter-p (character)
  (member character '(#\Space #\Tab #\Newline #\Return #\( #\) #\" #\; #\' #\` #\,)))

(defun token-counts (path)
  "The tokens in the file at PATH, and the tokens after its first form, as two values."
  (let* ((text (with-open-file (stream path)
                 (let ((string (make-string (file-length stream))))
                   (subseq string 0 (read-sequence string stream)))))
         (length (length text))
         (index 0) (depth 0) (tokens 0) (after-first 0) (first-done nil))
    (flet ((count-token ()
             (incf tokens)
             (when first-done (incf after-first))))
      (loop :while (< index length)
            :do (let ((character (char text index)))
                  (cond ((char= character #\;)
                         (loop :while (and (< index length) (char/= (char text index) #\Newline))
                               :do (incf index)))
                        ((char= character #\")
                         (incf index)
                         (loop :while (and (< index length) (char/= (char text index) #\"))
                               :do (when (char= (char text index) #\\) (incf index))
                                   (incf index))
                         (incf index)
                         (count-token))
                        ((char= character #\()
                         (incf depth)
                         (incf index))
                        ((char= character #\))
                         (decf depth)
                         (incf index)
                         (when (and (zerop depth) (not first-done))
                           (setf first-done t)))
                        ((delimiter-p character)
                         (incf index))
                        ((and (char= character #\#) (< (1+ index) length)
                              (char= (char text (1+ index)) #\'))
                         ;; The quote of #'name is not a token, and name is.
                         (incf index 2))
                        (t
                         ;; A character literal (#\Return, #\() takes the character after the
                         ;; backslash whatever it is, and then the rest of the name.
                         (when (and (char= character #\#) (< (1+ index) length)
                                    (char= (char text (1+ index)) #\\))
                           (incf index 3))
                         (loop :while (and (< index length) (not (delimiter-p (char text index))))
                               :do (incf index))
                         (count-token))))))
    (values tokens after-first)))

(let ((rows '()))
  (dolist (task +tasks+)
    (dolist (variant +variants+)
      (let ((path (format nil "apps/~a/~a/~a.lisp"
                          (if (member task +stress-tasks+ :test #'string=) "stress" "7guis")
                          task variant)))
        (multiple-value-bind (all body) (token-counts path)
          (push (list task variant all body) rows)))))
  (setf rows (nreverse rows))
  (format t "~&~12a ~{~28a~}~%" "task"
          (mapcar (lambda (variant) (format nil "~a (all, body)" variant)) +variants+))
  (dolist (task +tasks+)
    (format t "~&~12a" task)
    (dolist (variant +variants+)
      (let ((row (find-if (lambda (r) (and (string= (first r) task) (string= (second r) variant)))
                          rows)))
        (format t " ~27a" (format nil "~d, ~d" (third row) (fourth row)))))
    (terpri))
  (format t "~&~12a" "total")
  (dolist (variant +variants+)
    (let ((mine (remove-if-not (lambda (r) (string= (second r) variant)) rows)))
      (format t " ~27a" (format nil "~d, ~d" (reduce #'+ (mapcar #'third mine))
                                (reduce #'+ (mapcar #'fourth mine))))))
  (terpri))
