(require :asdf)

(defpackage #:almighty-bolt-scripts/mcclim-size
  (:use #:cl)
  (:export #:main))

(in-package #:almighty-bolt-scripts/mcclim-size)

;;;; How large the stress apps are in McCLIM, by the measures of scripts/app-size.lisp (lines of
;;;; code, not blank and not only a comment) and scripts/app-tokens.lisp (tokens, as the whole file
;;;; and as what is after its first form; the counter is copied from that script, which is on the
;;;; branch builder-macros), with the apps of the three variants next to them, and the files that
;;;; the variants share but McCLIM cannot (the painters and the controller). The concepts of a
;;;; McCLIM app are the distinct symbols that its DEFPACKAGE imports from CLIM and CLIM-EXTENSIONS:
;;;; not the variants' count, whose packages are small and the framework's own. From the repository
;;;; root:
;;;;   sbcl --script scripts/mcclim-size.lisp
;;;; The apps of the variants are read from the directory in VARIANT_ROOT if it is set (a checkout
;;;; of another branch, such as builder-macros, whose ports of retained and reactive are shorter or
;;;; longer), and McCLIM's from the current directory.

(defparameter +tasks+ '("dashboard" "rows" "editor"))
(defparameter +7guis-tasks+ '("crud" "flight" "circles"))

(defun lines-of-code (path)
  (with-open-file (stream path)
    (loop :for line = (read-line stream nil nil)
          :while line
          :count (let ((trimmed (string-trim '(#\Space #\Tab) line)))
                   (and (plusp (length trimmed)) (char/= (char trimmed 0) #\;))))))

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
                         (incf index 2))
                        (t
                         (when (and (char= character #\#) (< (1+ index) length)
                                    (char= (char text (1+ index)) #\\))
                           (incf index 3))
                         (loop :while (and (< index length) (not (delimiter-p (char text index))))
                               :do (incf index))
                         (count-token))))))
    (values tokens after-first)))

(defun first-form (path)
  (let ((*package* (make-package (gensym "SIZE") :use '(:cl)))
        (*read-eval* nil))
    (unwind-protect
         (with-open-file (stream path) (read stream))
      (delete-package *package*))))

(defun imported-from (path package-prefixes)
  "The distinct symbols that the DEFPACKAGE at the start of PATH imports from a package whose name
starts with one of PACKAGE-PREFIXES."
  (let ((symbols '()))
    (dolist (clause (rest (rest (first-form path))))
      (when (and (consp clause) (eq (first clause) :import-from))
        (let ((package (string-upcase (string (second clause)))))
          (when (some (lambda (prefix) (eql 0 (search prefix package))) package-prefixes)
            (dolist (symbol (cddr clause))
              (pushnew (cons package (string-upcase (string symbol))) symbols :test #'equal))))))
    (length symbols)))

(defun path (task name &optional (root (or (uiop:getenv "VARIANT_ROOT") "")) (suite "stress"))
  (format nil "~aapps/~a/~a/~a.lisp" root suite task name))

(defun row (label paths concepts)
  "LABEL, then the lines, all tokens and body tokens of the files at PATHS together."
  (let ((lines 0) (all 0) (body 0))
    (dolist (path paths)
      (incf lines (lines-of-code path))
      (multiple-value-bind (a b) (token-counts path)
        (incf all a) (incf body b)))
    (format t "~&  ~34a ~6d ~6d ~6d ~8a~%" label lines all body (or concepts "-"))))

(defun main ()
  (format t "~&  ~34a ~6a ~6a ~6a ~8a~%" "" "lines" "tokens" "body" "concepts")
  (dolist (task +tasks+)
    (format t "~&~a~%" task)
    (dolist (variant '("retained" "reactive" "immediate"))
      (let ((own (path task variant)))
        (row (format nil "~a, app file" variant) (list own)
             (imported-from own '("ALMIGHTY-BOLT-VARIANT-")))
        (row (format nil "~a, app + shared painter etc." variant)
             (remove-if-not #'probe-file
                            (list own (path task "paint") (path task "control")))
             nil)))
    (let ((own (path task "mcclim" "")))
      (row "mcclim, app file" (list own)
           (imported-from own '("CLIM" "CLIM-EXTENSIONS")))))
  (let ((own (path "dashboard" "mcclim-gadgets" "")))
    (format t "~&dashboard in gadgets~%")
    (row "mcclim gadgets, app file" (list own)
         (imported-from own '("CLIM" "CLIM-EXTENSIONS")))
    (row "mcclim gadgets + the display file"
         (list own (path "dashboard" "mcclim" "")) nil))
  (format t "~&~%The 7GUIs apps (apps/7guis/)~%")
  (dolist (task +7guis-tasks+)
    (format t "~&~a~%" task)
    (dolist (variant '("retained" "reactive" "immediate"))
      (let ((own (path task variant (or (uiop:getenv "VARIANT_ROOT") "") "7guis")))
        (row (format nil "~a, app file" variant) (list own)
             (imported-from own '("ALMIGHTY-BOLT-VARIANT-")))
        (row (format nil "~a, app + shared painter" variant)
             (remove-if-not #'probe-file
                            (list own (path task "paint" (or (uiop:getenv "VARIANT_ROOT") "") "7guis")))
             nil)))
    (dolist (file '("mcclim" "mcclim-gadgets"))
      (let ((own (path task file "" "7guis")))
        (when (probe-file own)
          (row (format nil "~a, app file" file) (list own)
               (imported-from own '("CLIM" "CLIM-EXTENSIONS"))))))))

(main)
