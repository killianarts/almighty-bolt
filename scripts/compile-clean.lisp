;;;; Compile this project from nothing, into a directory that is thrown away, and say what the
;;;; compiler complained about. From the repository root: make compile-clean
;;;; Its dependencies (vendored/) are loaded as they are; only the project's own files are compiled
;;;; again. A warning of any kind, or an error, is printed with its file, and the exit code is 1.

(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))

;;; What the project's systems need, loaded with the usual compiled files, so that only our own
;;; files are compiled below.
(dolist (name '("cffi" "lisp-unit2" "flexi-streams"))
  (let ((*standard-output* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (asdf:load-system name)))

(defvar *cache* (format nil "/tmp/almighty-bolt-compile-clean-~d/" (get-universal-time)))

(let ((root (namestring (uiop:getcwd))))
  (asdf:initialize-output-translations
   `(:output-translations
     ,@(loop :for directory :in '("src" "t" "apps" "examples" "bench")
             :collect `((,(format nil "~a~a/" root directory) :**/ :*.*.*)
                        (,(format nil "~a~a/" *cache* directory) :**/ :*.*.*)))
     :inherit-configuration)))

(defvar *complaints* '())

;;; A macro that a file defines at compile time and again when the compiled file is loaded is
;;; reported as a redefinition; that is how ASDF builds a system and not a fault of the code.
(handler-bind ((warning (lambda (condition)
                          (unless (typep condition 'sb-kernel:redefinition-warning)
                            (push (format nil "~a: ~a" (type-of condition)
                                          (remove #\Newline (princ-to-string condition)))
                                  *complaints*)))))
  (let ((*compile-verbose* nil) (*compile-print* nil) (*load-verbose* nil)
        (*standard-output* (make-broadcast-stream)))
    (dolist (name '("almighty-bolt" "almighty-bolt/tests" "almighty-bolt-examples"
                    "almighty-bolt-7guis/launcher" "almighty-bolt-7guis/acceptance"
                    "almighty-bolt-stress/acceptance" "almighty-bolt-bench"))
      (asdf:load-system name))))

(uiop:delete-directory-tree (uiop:ensure-directory-pathname *cache*) :validate t
                                                                         :if-does-not-exist :ignore)
(cond ((null *complaints*)
       (format t "~&No warnings.~%"))
      (t
       (format t "~&~d warning~:p:~%" (length *complaints*))
       (dolist (complaint (reverse *complaints*))
         (format t "  ~a~%" (subseq complaint 0 (min 200 (length complaint)))))
       (uiop:quit 1)))
