(defsystem "almighty-bolt-variant-cursor"
  :version "0.0.1"
  :description "An experiment, not a contender: a front end that places each widget where the cursor is as the widget is described (Dear ImGui style), with no tree, to count the cases that need a size the frame has not got yet (H6)"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt-core" "almighty-bolt-variant-retained")
  :components ((:file "ui")
               (:file "widgets")
               (:file "cursor"))
  :in-order-to ((test-op (test-op "almighty-bolt-variant-cursor/tests"))))

(defsystem "almighty-bolt-variant-cursor/tests"
  :depends-on ("almighty-bolt-variant-cursor" "almighty-bolt-variant-support" "lisp-unit2")
  :components ((:module "t"
                :serial t
                :components
                ((:file "cursor"))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-variant-cursor/t/cursor)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d cursor variant test~:p failed." (length bad))))))
