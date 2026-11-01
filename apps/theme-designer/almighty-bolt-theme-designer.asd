(defsystem "almighty-bolt-theme-designer"
  :version "0.1.0"
  :description "A theme designer for almighty-bolt: a theme's settings, and every kind of widget drawn in it as they change"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:file "designer"))
  :in-order-to ((test-op (test-op "almighty-bolt-theme-designer/tests"))))

(defsystem "almighty-bolt-theme-designer/tests"
  :description "The designer worked as a person would, in a context with no window"
  :depends-on ("almighty-bolt-theme-designer" "almighty-bolt/test-support" "lisp-unit2")
  :pathname "t"
  :components ((:file "designer"))
  :perform (test-op (o s)
                    (let* ((db (uiop:symbol-call :lisp-unit2 :run-tests
                                                 :package :almighty-bolt-theme-designer/t/designer))
                           (bad (append (uiop:symbol-call :lisp-unit2 :failed db)
                                        (uiop:symbol-call :lisp-unit2 :errors db))))
                      (uiop:symbol-call :lisp-unit2 :print-summary db)
                      (when bad
                        (error "~d theme designer tests failed." (length bad))))))
