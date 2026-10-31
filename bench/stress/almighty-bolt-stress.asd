(defsystem "almighty-bolt-stress"
  :version "0.1.0"
  :description "The stress apps' data and painters, which every variant's version of them shares"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:module "dashboard" :components ((:file "model") (:file "paint")))
               (:module "rows" :components ((:file "model") (:file "paint")))
               (:module "editor"
                :components ((:file "model") (:file "paint") (:file "control")))))

(defsystem "almighty-bolt-stress/immediate"
  :description "The stress apps in the immediate variant"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-7guis")
  :serial t
  :components ((:module "dashboard" :components ((:file "immediate")))
               (:module "rows" :components ((:file "immediate")))
               (:module "editor" :components ((:file "immediate")))))

(defsystem "almighty-bolt-stress/acceptance"
  :description "Behavioural tests of every stress app: events in, what the app shows out"
  :depends-on ("almighty-bolt-stress" "almighty-bolt-stress/immediate"
               "almighty-bolt-7guis/acceptance" "lisp-unit2")
  :components ((:module "t"
                :components ((:module "acceptance"
                              :serial t
                              :components ((:file "dashboard") (:file "rows") (:file "editor")
                                           (:file "properties"))))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-stress/t/acceptance/dashboard
                                                      :almighty-bolt-stress/t/acceptance/rows
                                                      :almighty-bolt-stress/t/acceptance/editor
                                                      :almighty-bolt-stress/t/acceptance/properties)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d stress acceptance test~:p failed." (length bad))))))
