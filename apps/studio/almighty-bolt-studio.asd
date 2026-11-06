(defsystem "almighty-bolt-studio"
  :version "0.1.0"
  :description "The almighty-bolt studio: the GUI designer and the theme designer in one window"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt" "almighty-bolt/emacs" "almighty-bolt-gui-designer"
               "almighty-bolt-theme-designer")
  :components ((:file "theme")
               (:file "chrome")
               (:file "repl")
               (:file "title-bar")
               (:file "wire")
               (:file "studio"))
  :in-order-to ((test-op (test-op "almighty-bolt-studio/tests"))))

(defsystem "almighty-bolt-studio/tests"
  :description "The studio worked as a person would, in a context with no window"
  :depends-on ("almighty-bolt-studio" "almighty-bolt/test-support" "lisp-unit2")
  :pathname "t"
  :components ((:file "studio"))
  :perform (test-op (o s)
                    (let* ((db (uiop:symbol-call :lisp-unit2 :run-tests
                                                 :package :almighty-bolt-studio/t/studio))
                           (bad (append (uiop:symbol-call :lisp-unit2 :failed db)
                                        (uiop:symbol-call :lisp-unit2 :errors db))))
                      (uiop:symbol-call :lisp-unit2 :print-summary db)
                      (when bad
                        (error "~d studio tests failed." (length bad))))))
