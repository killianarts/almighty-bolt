(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))

(defpackage #:almighty-bolt-scripts/compile-time
  (:use #:cl))

(in-package #:almighty-bolt-scripts/compile-time)

;;;; One variant's compile times, in a process that has nothing compiled: the fasls go into the
;;;; directory CACHE (a new one), so every step starts from nothing. Prints a line for each step:
;;;;   step <name> <seconds of real time>
;;;; The steps are the library, the 7GUIs models, the apps, and then two changes by touching a
;;;; file: one in the library and one in an app, to see what an incremental build costs. Run by bench/compile-time.lisp, or:
;;;;   VARIANT=immediate CACHE=/tmp/cache/ sbcl --non-interactive --load scripts/compile-time.lisp

(let* ((variant (string-downcase (or (uiop:getenv "VARIANT") (error "Say which variant."))))
       (cache (uiop:ensure-directory-pathname (or (uiop:getenv "CACHE") (error "Say where CACHE is."))))
       (apps (format nil "almighty-bolt-7guis/~a" variant)))
  (asdf:initialize-output-translations
   `(:output-translations (t (,(namestring cache) :**/ :*.*.*)) :ignore-inherited-configuration))
  (flet ((timed (name thunk)
           (let ((start (get-internal-real-time)))
             (let ((*standard-output* (make-broadcast-stream))
                   (*error-output* (make-broadcast-stream)))
               (handler-bind ((warning #'muffle-warning))
                 (funcall thunk)))
             (format t "~&step ~a ~,3f~%" name
                     (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0))
             (finish-output)))
         (touch (path)
           ;; ASDF compares file write dates a second at a time. A file that is not there would
           ;; be made by touch, and the measurement would be of nothing.
           (unless (probe-file path) (error "There is no ~a to touch." path))
           (sleep 1.1)
           (uiop:run-program (list "touch" path))))
    (timed "library" (lambda () (asdf:load-system "almighty-bolt")))
    (timed "shared-models" (lambda () (asdf:load-system "almighty-bolt-7guis")))
    (timed "apps" (lambda () (asdf:load-system apps)))
    (touch "src/ui/widgets.lisp")
    (timed "incremental-library-file" (lambda () (asdf:load-system apps)))
    (touch (format nil "examples/7guis/counter/~a.lisp" variant))
    (timed "incremental-app-file" (lambda () (asdf:load-system apps)))))
