(defsystem "almighty-bolt-variant-null"
  :version "0.0.1"
  :description "The null baseline variant: replays a fixed display list, to measure the core's own cost"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:file "null")
               (:file "measure" :if-feature :darwin))
  :in-order-to ((test-op (test-op "almighty-bolt-variant-null/tests"))))

(defsystem "almighty-bolt-variant-null/tests"
  :depends-on ("almighty-bolt-variant-null" "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :components
                ((:file "null" :if-feature :darwin))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(#+darwin :almighty-bolt-variant-null/t/null)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d null variant test~:p failed." (length bad))))))
