(defpackage #:almighty-bolt-core/t/platform/macos/image
  (:use #:cl #:lisp-unit2))

(in-package #:almighty-bolt-core/t/platform/macos/image)

;;; An image that was saved keeps none of what the process learned from the Objective-C
;;; runtime. This saves a small one, in a process of its own, and runs it: the classes and
;;; selectors of the platform layer must be found again, and the classes made at run time made
;;; again.

(defparameter +build-script+ "
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree ~s) :ignore-inherited-configuration))
(asdf:load-system \"almighty-bolt-core\")
(use-package :almighty-bolt-core/platform/macos/objc)
(sb-ext:save-lisp-and-die ~s
  :executable t
  :save-runtime-options t
  :toplevel (lambda ()
              (let ((delegate (symbol-value (find-symbol \"*DELEGATE-CLASS*\" \"ALMIGHTY-BOLT-CORE/PLATFORM/MACOS/WINDOW\")))
                    (offset (symbol-value (find-symbol \"*DELEGATE-SLOT-OFFSET*\" \"ALMIGHTY-BOLT-CORE/PLATFORM/MACOS/WINDOW\")))
                    (string (ns-string \"hello\")))
                ;; The class of the window delegate is made at run time: in this process it
                ;; is a new one, and has the method the old process gave it.
                (format t \"image-ok ~~a ~~a ~~a~~%\"
                        (msg-send :bool delegate \"instancesRespondToSelector:\"
                                  (:ulong (selector \"windowWillClose:\")))
                        offset
                        (lisp-string string)))
              (sb-ext:exit :code 0)))
")

(define-test a-saved-image-finds-its-classes-and-selectors-again ()
  (let* ((directory (uiop:ensure-directory-pathname
                     (merge-pathnames (format nil "almighty-bolt-image-test-~d/" (get-universal-time))
                                      (uiop:temporary-directory))))
         (script (merge-pathnames "build.lisp" directory))
         (image (namestring (merge-pathnames "image" directory))))
    (ensure-directories-exist directory)
    (unwind-protect
         (progn
           (with-open-file (stream script :direction :output :if-exists :supersede)
             (format stream +build-script+ (namestring (uiop:getcwd)) image))
           (uiop:run-program (list "sbcl" "--noinform" "--non-interactive" "--no-sysinit"
                                   "--no-userinit" "--load" (namestring script))
                             :output nil :error-output nil)
           (let ((output (uiop:run-program (list image) :output :string :error-output :string)))
             (assert-true (search "image-ok T 8 hello" output) output)))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))
