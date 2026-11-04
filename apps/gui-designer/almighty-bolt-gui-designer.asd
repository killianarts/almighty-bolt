(defsystem "almighty-bolt-gui-designer"
  :version "0.1.0"
  :description "A GUI designer for almighty-bolt: a view made of widgets, drawn as it will look, to be built by placing widgets instead of writing code"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:file "view")
               (:file "designer"))
  :in-order-to ((test-op (test-op "almighty-bolt-gui-designer/tests"))))

(defsystem "almighty-bolt-gui-designer/tests"
  :description "The designer worked as a person would, in a context with no window"
  :depends-on ("almighty-bolt-gui-designer" "almighty-bolt/test-support" "lisp-unit2")
  :pathname "t"
  :components ((:file "designer"))
  :perform (test-op (o s)
                    (let* ((db (uiop:symbol-call :lisp-unit2 :run-tests
                                                 :package :almighty-bolt-gui-designer/t/designer))
                           (bad (append (uiop:symbol-call :lisp-unit2 :failed db)
                                        (uiop:symbol-call :lisp-unit2 :errors db))))
                      (uiop:symbol-call :lisp-unit2 :print-summary db)
                      (when bad
                        (error "~d GUI designer tests failed." (length bad))))))
