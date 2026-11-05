;;;; The bridge to Emacs, end to end: this Lisp starts a Slynk server and loads the bridge
;;;; (almighty-bolt/emacs) and a view from a file; Emacs, in batch, connects to it with Sly, turns on
;;;; the minor modes of emacs/almighty-bolt.el and visits the view's file, and emacs/smoke.el checks
;;;; what each side does for the other, as a person's would. No window is opened. From the
;;;; repository root:
;;;;   make emacs-smoke                       (SLY=/path/to/sly, the directory of sly.el and slynk/)
;;;; It says "The Emacs bridge check passed" when every check holds, and that it was skipped, and why,
;;;; if there is no Emacs or no Sly.

(defpackage #:almighty-bolt-emacs-smoke
  (:use #:cl))

(in-package #:almighty-bolt-emacs-smoke)

(defun sly-directory ()
  "The directory of sly.el and slynk/: SLY in the environment, or where Doom or package.el put it."
  (flet ((sly-p (directory)
           (and directory
                (probe-file (merge-pathnames "sly.el" directory))
                (probe-file (merge-pathnames "slynk/slynk-loader.lisp" directory))
                directory)))
    (or (sly-p (let ((given (uiop:getenv "SLY")))
                 (and given (plusp (length given)) (uiop:ensure-directory-pathname given))))
        (sly-p (merge-pathnames ".emacs.d/.local/straight/repos/sly/" (user-homedir-pathname)))
        (some #'sly-p (directory (merge-pathnames ".emacs.d*/elpa/sly-*/" (user-homedir-pathname)))))))

(defun skip (why)
  (format t "~&The Emacs bridge check was skipped: ~a~%" why)
  (finish-output)
  (uiop:quit 0))

(let ((sly (sly-directory))
      (emacs (ignore-errors (uiop:run-program '("emacs" "--version") :output :string))))
  (unless emacs (skip "there is no emacs on the PATH."))
  (unless sly (skip "Sly was not found; give its directory as SLY=."))
  ;; Slynk, from the same Sly the Emacs side loads, so that the two speak the same protocol.
  (load (merge-pathnames "slynk/slynk-loader.lisp" sly))
  (funcall (intern "INIT" "SLYNK-LOADER") :quiet t)
  (asdf:load-system "almighty-bolt/emacs")
  ;; The view, defined from its file, as an app's system loads it.
  (let* ((file (merge-pathnames (format nil "bolt-smoke-~d.lisp" (random 1000000000))
                                (uiop:temporary-directory)))
         (port (+ 40000 (random 20000))))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (format out "(in-package #:common-lisp-user)~%~%(almighty-bolt:define-view smoke-view~%  ~
                   (:column :page (:gap 4)~%   (:label :words (:text \"Before\"))~%   ~
                   (:button :go (:text \"Go\"))))~%"))
    (load file)
    (let ((*standard-output* (make-broadcast-stream)))
      (funcall (intern "CREATE-SERVER" "SLYNK") :port port :dont-close t))
    (let ((status (unwind-protect
                       (nth-value 2 (uiop:run-program
                                     (list "emacs" "--batch" "-Q"
                                           "-L" (namestring sly)
                                           "-L" (namestring (merge-pathnames "emacs/" (uiop:getcwd)))
                                           "--eval" (format nil "(setq bolt-smoke-port ~d bolt-smoke-file ~s)"
                                                            port (namestring file))
                                           "-l" (namestring (merge-pathnames "emacs/smoke.el"
                                                                             (uiop:getcwd))))
                                     :output :interactive :error-output :interactive
                                     :ignore-error-status t))
                    (delete-file file))))
      (uiop:quit (if (eql status 0) 0 1)))))
