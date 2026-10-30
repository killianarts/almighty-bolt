(defsystem "almighty-bolt-variant-immediate"
  :version "0.0.1"
  :description "The immediate-mode variant with a cache: the interface is described again every frame, and unchanged layout and painting are skipped by comparing what was described"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:file "theme")
               (:file "hash")
               (:file "box")
               (:file "ui")
               (:file "widgets")
               (:file "layout")
               (:file "paint")
               (:file "immediate"))
  :in-order-to ((test-op (test-op "almighty-bolt-variant-immediate/tests"))))

(defsystem "almighty-bolt-variant-immediate/tests"
  :depends-on ("almighty-bolt-variant-immediate" "almighty-bolt-variant-support"
                                                 "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :serial t
                :components
                ((:file "immediate"))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-variant-immediate/t/immediate)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d immediate variant test~:p failed." (length bad))))))
