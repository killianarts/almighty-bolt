(defsystem "almighty-bolt-variant-retained"
  :version "0.0.1"
  :description "The retained variant: a tree of nodes that lives between frames"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:file "theme")
               (:file "node")
               (:file "widgets")
               (:file "inputs")
               (:file "builder")
               (:file "retained")
               (:file "measure" :if-feature :darwin))
  :in-order-to ((test-op (test-op "almighty-bolt-variant-retained/tests"))))

(defsystem "almighty-bolt-variant-retained/tests"
  :depends-on ("almighty-bolt-variant-retained" "almighty-bolt-variant-support"
                           "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :serial t
                :components
                ((:file "counter")
                 (:file "widgets")
                 (:file "inputs")
                 (:file "containers")
                 (:file "builder")
                 (:file "retained")
                 (:file "cache")
                 (:file "host" :if-feature :darwin))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-variant-retained/t/widgets
                                                      :almighty-bolt-variant-retained/t/inputs
                                                      :almighty-bolt-variant-retained/t/containers
                                                      :almighty-bolt-variant-retained/t/builder
                                                      :almighty-bolt-variant-retained/t/retained
                                                      :almighty-bolt-variant-retained/t/cache
                                                      #+darwin :almighty-bolt-variant-retained/t/host)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d retained variant test~:p failed." (length bad))))))
