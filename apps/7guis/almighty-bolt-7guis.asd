(defsystem "almighty-bolt-7guis"
  :version "0.0.1"
  :description "The logic of the seven 7GUIs tasks, shared by every variant's version of them"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:module "common" :components ((:file "numbers") (:file "registry")))
               (:module "temperature" :components ((:file "model")))
               (:module "flight" :components ((:file "model")))
               (:module "timer" :components ((:file "model")))
               (:module "crud" :components ((:file "model")))
               (:module "circles" :components ((:file "model") (:file "paint")))
               (:module "cells" :components ((:file "model") (:file "geometry") (:file "paint")))
               (:module "layout" :components ((:file "text")))
               (:module "dialog" :components ((:file "text")))))

(defsystem "almighty-bolt-7guis/launcher"
  :description "Open a task of a variant in a window"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt")
  :serial t
  :components ((:module "launcher" :if-feature :darwin :components ((:file "launcher")))))

(defsystem "almighty-bolt-7guis/acceptance"
  :description "Behavioural tests of every task: events in, what the app shows out"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt/test-support"
               "almighty-bolt-7guis/immediate"
               "lisp-unit2" "flexi-streams")
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
                               (:file "cells")
                               (:file "replay")
                               (:file "layout")
                               (:file "dialog")
                               (:file "reload")
                               (:file "semantics")
                               (:file "machine"))))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-7guis/t/acceptance/counter
                                                      :almighty-bolt-7guis/t/acceptance/temperature
                                                      :almighty-bolt-7guis/t/acceptance/flight
                                                      :almighty-bolt-7guis/t/acceptance/timer
                                                      :almighty-bolt-7guis/t/acceptance/crud
                                                      :almighty-bolt-7guis/t/acceptance/circles
                                                      :almighty-bolt-7guis/t/acceptance/cells
                                                      :almighty-bolt-7guis/t/acceptance/replay
                                                      :almighty-bolt-7guis/t/acceptance/layout
                                                      :almighty-bolt-7guis/t/acceptance/dialog
                                                      :almighty-bolt-7guis/t/acceptance/reload
                                                      :almighty-bolt-7guis/t/acceptance/semantics
                                                      :almighty-bolt-7guis/t/acceptance/machine)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d 7GUIs acceptance test~:p failed." (length bad))))))

(defsystem "almighty-bolt-7guis/immediate"
  :description "The seven 7GUIs tasks in the immediate variant"
  :depends-on ("almighty-bolt-7guis")
  :serial t
  :components ((:module "counter" :components ((:file "immediate")))
               (:module "temperature" :components ((:file "immediate")))
               (:module "flight" :components ((:file "immediate")))
               (:module "timer" :components ((:file "immediate")))
               (:module "crud" :components ((:file "immediate")))
               (:module "circles" :components ((:file "immediate")))
               (:module "cells" :components ((:file "immediate")))
               (:module "layout" :components ((:file "immediate")))
               (:module "dialog" :components ((:file "immediate")))))
