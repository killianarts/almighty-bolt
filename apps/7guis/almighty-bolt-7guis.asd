(defsystem "almighty-bolt-7guis"
  :version "0.0.1"
  :description "The logic of the seven 7GUIs tasks, shared by every variant's version of them"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :components ((:module "common" :components ((:file "numbers")))
               (:module "temperature" :components ((:file "model")))
               (:module "flight" :components ((:file "model")))
               (:module "timer" :components ((:file "model")))
               (:module "crud" :components ((:file "model")))
               (:module "circles" :components ((:file "model")))
               (:module "cells" :components ((:file "model"))))
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
