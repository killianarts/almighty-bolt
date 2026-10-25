(defsystem "almighty-bolt-7guis"
  :version "0.0.1"
  :description "The logic of the seven 7GUIs tasks, shared by every variant's version of them"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :components ((:module "common" :components ((:file "numbers") (:file "registry")))
               (:module "temperature" :components ((:file "model")))
               (:module "flight" :components ((:file "model")))
               (:module "timer" :components ((:file "model")))
               (:module "crud" :components ((:file "model")))
               (:module "circles" :components ((:file "model")))
               (:module "cells" :components ((:file "model") (:file "geometry"))))
  :in-order-to ((test-op (test-op "almighty-bolt-7guis/tests"))))

(defsystem "almighty-bolt-7guis/tests"
  :depends-on ("almighty-bolt-7guis" "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :serial t
                :components
                ((:file "numbers")
                 (:file "temperature")
                 (:file "flight")
                 (:file "timer")
                 (:file "crud")
                 (:file "circles")
                 (:file "cells"))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-7guis/t/numbers
                                                      :almighty-bolt-7guis/t/temperature
                                                      :almighty-bolt-7guis/t/flight
                                                      :almighty-bolt-7guis/t/timer
                                                      :almighty-bolt-7guis/t/crud
                                                      :almighty-bolt-7guis/t/circles
                                                      :almighty-bolt-7guis/t/cells)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d 7GUIs test~:p failed." (length bad))))))

(defsystem "almighty-bolt-7guis/retained"
  :description "The seven 7GUIs tasks in the retained variant"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-variant-retained")
  :serial t
  :components ((:module "counter" :components ((:file "retained")))
               (:module "temperature" :components ((:file "retained")))
               (:module "flight" :components ((:file "retained")))
               (:module "timer" :components ((:file "retained")))
               (:module "crud" :components ((:file "retained")))
               (:module "circles" :components ((:file "retained")))
               (:module "cells" :components ((:file "retained")))))

(defsystem "almighty-bolt-7guis/launcher"
  :description "Open a task of a variant in a window"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-core")
  :serial t
  :components ((:module "launcher" :if-feature :darwin :components ((:file "launcher")))))

(defsystem "almighty-bolt-7guis/acceptance"
  :description "The same behavioural tests for every variant's version of every task"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-variant-support"
               "almighty-bolt-7guis/retained" "lisp-unit2" "flexi-streams")
  :serial t
  :components ((:module "t"
                :components
                ((:module "acceptance"
                  :serial t
                  :components ((:file "driver")
                               (:file "counter")
                               (:file "temperature")
                               (:file "flight")
                               (:file "timer")
                               (:file "crud")
                               (:file "circles")
                               (:file "cells"))))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-7guis/t/acceptance/counter
                                                      :almighty-bolt-7guis/t/acceptance/temperature
                                                      :almighty-bolt-7guis/t/acceptance/flight
                                                      :almighty-bolt-7guis/t/acceptance/timer
                                                      :almighty-bolt-7guis/t/acceptance/crud
                                                      :almighty-bolt-7guis/t/acceptance/circles
                                                      :almighty-bolt-7guis/t/acceptance/cells)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d 7GUIs acceptance test~:p failed." (length bad))))))
