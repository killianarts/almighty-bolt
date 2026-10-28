(defsystem "almighty-bolt-variant-reactive"
  :version "0.0.1"
  :description "The reactive hybrid variant: views that are a function of signals, reconciled against a persistent tree of retained nodes"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt-core" "almighty-bolt-variant-retained")
  :components ((:file "signals")
               (:file "views")
               (:file "builder")
               (:file "elements")
               (:file "reactive"))
  :in-order-to ((test-op (test-op "almighty-bolt-variant-reactive/tests"))))

(defsystem "almighty-bolt-variant-reactive/tests"
  :depends-on ("almighty-bolt-variant-reactive" "almighty-bolt-variant-support"
                                                "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :serial t
                :components
                ((:file "signals")
                 (:file "elements")
                 (:file "builder")
                 (:file "reactive")
                 (:file "reload"))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-variant-reactive/t/signals
                                                      :almighty-bolt-variant-reactive/t/elements
                                                      :almighty-bolt-variant-reactive/t/builder
                                                      :almighty-bolt-variant-reactive/t/reactive
                                                      :almighty-bolt-variant-reactive/t/reload)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d reactive variant test~:p failed." (length bad))))))
