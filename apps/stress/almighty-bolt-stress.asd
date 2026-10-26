(defsystem "almighty-bolt-stress"
  :version "0.0.1"
  :description "The stress apps' data and painters, which every variant's version of them shares"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:module "dashboard" :components ((:file "model") (:file "paint")))
               (:module "rows" :components ((:file "model") (:file "paint")))
               (:module "editor"
                :components ((:file "model") (:file "paint") (:file "control"))))
  :in-order-to ((test-op (test-op "almighty-bolt-stress/tests"))))

(defsystem "almighty-bolt-stress/tests"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-variant-support" "lisp-unit2")
  :components ((:module "t"
                :serial t
                :components ((:file "dashboard") (:file "rows") (:file "editor"))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-stress/t/dashboard
                                                      :almighty-bolt-stress/t/rows
                                                      :almighty-bolt-stress/t/editor)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d stress app test~:p failed." (length bad))))))

(defsystem "almighty-bolt-stress/retained"
  :description "The stress apps in the retained variant"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-7guis" "almighty-bolt-variant-retained")
  :serial t
  :components ((:module "dashboard" :components ((:file "retained")))
               (:module "rows" :components ((:file "retained")))
               (:module "editor" :components ((:file "retained")))))

(defsystem "almighty-bolt-stress/reactive"
  :description "The stress apps in the reactive variant"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-7guis" "almighty-bolt-variant-reactive")
  :serial t
  :components ((:module "dashboard" :components ((:file "reactive")))
               (:module "rows" :components ((:file "reactive")))
               (:module "editor" :components ((:file "reactive")))))

(defsystem "almighty-bolt-stress/reactive-signals"
  :description "The reactive dashboard with a model of signals, in place of the one of reactive (notes/builder-macros.org)"
  :depends-on ("almighty-bolt-stress/reactive")
  :serial t
  :components ((:module "dashboard"
                :components ((:file "signal-model") (:file "reactive-signals")))))

(defsystem "almighty-bolt-stress/immediate"
  :description "The stress apps in the immediate variant"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-7guis" "almighty-bolt-variant-immediate")
  :serial t
  :components ((:module "dashboard" :components ((:file "immediate")))
               (:module "rows" :components ((:file "immediate")))
               (:module "editor" :components ((:file "immediate")))))

(defsystem "almighty-bolt-stress/acceptance"
  :description "The same behavioural tests for every variant's version of every stress app"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-stress/retained" "almighty-bolt-stress/reactive"
               "almighty-bolt-stress/immediate" "almighty-bolt-7guis/acceptance" "lisp-unit2")
  :components ((:module "t"
                :components ((:module "acceptance"
                              :serial t
                              :components ((:file "dashboard") (:file "rows") (:file "editor"))))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-stress/t/acceptance/dashboard
                                                      :almighty-bolt-stress/t/acceptance/rows
                                                      :almighty-bolt-stress/t/acceptance/editor)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d stress acceptance test~:p failed." (length bad))))))
