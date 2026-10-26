(defsystem "almighty-bolt-7guis"
  :version "0.0.1"
  :description "The logic of the seven 7GUIs tasks, shared by every variant's version of them"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:module "common" :components ((:file "numbers") (:file "registry")))
               (:module "temperature" :components ((:file "model")))
               (:module "flight" :components ((:file "model")))
               (:module "timer" :components ((:file "model")))
               (:module "crud" :components ((:file "model")))
               (:module "circles" :components ((:file "model") (:file "paint")))
               (:module "cells" :components ((:file "model") (:file "geometry") (:file "paint")))
               (:module "layout" :components ((:file "text")))
               (:module "dialog" :components ((:file "text"))))
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
               (:module "cells" :components ((:file "retained")))
               (:module "layout" :components ((:file "retained")))
               (:module "dialog" :components ((:file "retained")))))

(defsystem "almighty-bolt-7guis/launcher"
  :description "Open a task of a variant in a window"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-core")
  :serial t
  :components ((:module "launcher" :if-feature :darwin :components ((:file "launcher")))))

(defsystem "almighty-bolt-7guis/acceptance"
  :description "The same behavioural tests for every variant's version of every task"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-variant-support"
               "almighty-bolt-7guis/retained" "almighty-bolt-7guis/reactive"
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
                               (:file "dialog"))))))
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
                                                      :almighty-bolt-7guis/t/acceptance/dialog)
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
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-variant-immediate")
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

(defsystem "almighty-bolt-7guis/reactive"
  :description "The seven 7GUIs tasks in the reactive variant"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-variant-reactive")
  :serial t
  :components ((:module "counter" :components ((:file "reactive")))
               (:module "temperature" :components ((:file "reactive")))
               (:module "flight" :components ((:file "reactive")))
               (:module "timer" :components ((:file "reactive")))
               (:module "crud" :components ((:file "reactive")))
               (:module "circles" :components ((:file "reactive")))
               (:module "cells" :components ((:file "reactive")))
               (:module "layout" :components ((:file "reactive")))))

(defsystem "almighty-bolt-7guis/cursor"
  :description "The layout spike and the dialog screen in the cursor front end, the experiment of record 0022"
  :depends-on ("almighty-bolt-7guis" "almighty-bolt-variant-cursor")
  :serial t
  :components ((:module "cursor" :components ((:file "mode")))
               (:module "layout" :components ((:file "cursor")))
               (:module "dialog" :components ((:file "cursor")))))

(defsystem "almighty-bolt-7guis/cursor/tests"
  :description "The cursor experiment's screens against the box tree (record 0022, archived: not in make test; make test-cursor)"
  :depends-on ("almighty-bolt-7guis/cursor" "almighty-bolt-7guis/immediate"
               "almighty-bolt-variant-support" "lisp-unit2")
  :components ((:module "t"
                :components
                ((:module "acceptance"
                  :components ((:file "cursor"))))))
  :perform (test-op (o s)
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-7guis/t/acceptance/cursor)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d cursor experiment test~:p failed." (length bad))))))
