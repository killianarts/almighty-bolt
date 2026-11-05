(defpackage #:almighty-bolt-studio/repl
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:import-from #:almighty-bolt-studio/chrome
                #:section-header)
  (:export #:make-repl
           #:evaluate))

(in-package #:almighty-bolt-studio/repl)

;;; The studio's REPL: forms typed and evaluated in the image the studio runs in, in the app's
;;; package, with what they printed and what they returned, or the error they signalled; the forms
;;; typed before, back with Up and Down. It runs on the window's thread, between frames: a form
;;; that takes long holds the window as long.

(defun evaluate (text package)
  "Read the forms in TEXT and evaluate them one by one, in PACKAGE, as a REPL does (setting *, **,
***, + and the rest). Three values: what they printed, what the last returned (each value on a line
of its own, as PRINT writes it), and the error, if one was signalled, as a sentence (then no more
are evaluated)."
  (let ((out (make-string-output-stream))
        (results '())
        (failed nil))
    (handler-case
        (let ((*package* package)
              (*standard-output* out)
              (*error-output* out)
              (*trace-output* out))
          (with-input-from-string (in text)
            (loop :with end = (gensym "END")
                  :for form = (read in nil end)
                  :until (eq form end)
                  :do (let ((values (multiple-value-list (eval form))))
                        (shiftf cl:/// cl:// cl:/ values)
                        (shiftf cl:*** cl:** cl:* (first values))
                        (shiftf cl:+++ cl:++ cl:+ form)
                        (setf results
                              (mapcar (lambda (value) (prin1-to-string value)) values))))))
      (error (condition)
        (setf failed (format nil "~a" condition))))
    (values (get-output-stream-string out)
            (format nil "~{~a~^~%~}" results)
            failed)))

(defun short-name (package)
  "PACKAGE's name as the prompt says it: its shortest name (CL-USER, not COMMON-LISP-USER), the last
part of it after a slash, in small letters."
  (let ((name (first (sort (cons (package-name package) (copy-list (package-nicknames package)))
                           #'< :key #'length))))
    (string-downcase (subseq name (1+ (or (position #\/ name :from-end t) -1))))))

(defun make-repl ()
  "The REPL's part: a function of the name of the package to evaluate in (CL-USER if there is none
by that name) that draws it, keeping what was typed and what it gave."
  (bolt:state-let ((input "")
                   ;; What was evaluated, the latest first: (package input printed returned error).
                   (entries '())
                   ;; The forms typed, the latest first, and how far back Up has gone (NIL if not).
                   (history '())
                   (back nil))
    (lambda (package-name)
      (let* ((package (or (and package-name (find-package (string-upcase package-name)))
                          (find-package "CL-USER")))
             (prompt (format nil "~a>" (short-name package))))
        (bolt:column (:id :repl :flex 1 :fill t :cross-align :stretch :gap 6)
          (section-header :repl-heading "REPL")
          (bolt:scroll (:id :repl-scroll :flex 1 :follow t)
            (bolt:column (:id :repl-lines :gap 2 :cross-align :stretch)
              (loop :for (where typed printed returned error) :in (reverse entries)
                    :for i :from 0
                    :do (bolt:label (format nil "~a> ~a" where typed) :id (list :typed i)
                                    :font :code :selectable t)
                        (when (plusp (length printed))
                          (bolt:paragraph (string-right-trim '(#\Newline) printed)
                                          :id (list :printed i) :font :code :color :dim-text
                                          :selectable t))
                        (if error
                            (bolt:paragraph (format nil "Error: ~a" error) :id (list :error i)
                                            :font :code :color :field-invalid :selectable t)
                            (when (plusp (length returned))
                              (bolt:paragraph returned :id (list :returned i) :font :code
                                                       :selectable t))))))
          (bolt:row (:id :repl-input-row :gap 6 :cross-align :center :fill t)
            (bolt:label prompt :id :repl-prompt :font :code)
            (multiple-value-bind (text changed submitted)
                (bolt:text-field input :id :repl-input :flex 1 :font :code
                                       :labelled-by :repl-prompt)
              (when changed
                (setf input text
                      back nil))
              (when (and submitted (plusp (length (string-trim '(#\Space #\Tab) text))))
                (multiple-value-bind (printed returned error) (evaluate text package)
                  (push (list (short-name package) text printed returned error) entries))
                (push text history)
                (setf input ""
                      back nil))))
          ;; The forms typed before: Up goes back through them, Down forward, to what is being
          ;; typed.
          (when (and history (bolt:shortcut bolt:+key-up+))
            (setf back (min (1- (length history)) (if back (1+ back) 0))
                  input (nth back history)))
          (when (and back (bolt:shortcut bolt:+key-down+))
            (setf back (if (zerop back) nil (1- back))
                  input (if back (nth back history) ""))))))))
