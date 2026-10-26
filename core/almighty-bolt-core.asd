(defsystem "almighty-bolt-core"
  :version "0.0.1"
  :description "Shared core for the almighty-bolt GUI experiment: clock, instrumentation, events, display list and layout"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("cffi")
  :components ((:file "clock")
               (:file "ring")
               (:file "event")
               (:file "stats")
               (:file "wheel-lock")
               (:file "text-edit")
               (:file "display-list")
               (:file "variant")
               (:file "settle")
               (:module "replay"
                :serial t
                :components
                ((:file "script")
                 (:file "player")
                 (:file "recorder")))
               (:module "layout"
                :serial t
                :components
                ((:file "constraints")
                 (:file "box")
                 (:file "flex")))
               (:module "platform"
                :serial t
                :if-feature :darwin
                :components
                ((:module "macos"
                  :serial t
                  :components
                  ((:file "objc")
                   (:file "main-queue")
                   (:file "ns-event")
                   (:file "app")
                   (:file "window")
                   (:file "view")))))
               (:module "text"
                :serial t
                :if-feature :darwin
                :components
                ((:file "core-foundation")
                 (:file "font")
                 (:file "line")
                 (:file "glyph-raster")))
               (:module "render"
                :serial t
                :if-feature :darwin
                :components
                ((:module "metal"
                  :serial t
                  :components
                  ((:file "device")
                   (:file "target")
                   (:file "glyph-atlas")
                   (:file "rect-pipeline")
                   (:file "renderer")
                   (:file "draw-text")))))
               ;; The host joins the platform, the text engine and the renderer,
               ;; so it loads after all three.
               (:module "host"
                :pathname "platform/macos"
                :if-feature :darwin
                :components
                ((:file "host")))
               (:file "package"))
  :in-order-to ((test-op (test-op "almighty-bolt-core/tests"))))

(defsystem "almighty-bolt-core/tests"
  ;; flexi-streams is not used here. cl-unicode (lisp-unit2 -> cl-interpol ->
  ;; cl-unicode) generates its source files with it on first load, and vend
  ;; only finds it if some system names it.
  :depends-on ("almighty-bolt-core" "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :serial t
                :components
                ((:file "clock")
                 (:file "ring")
                 (:file "event")
                 (:file "stats")
                 (:file "wheel-lock")
                 (:file "text-edit")
                 (:file "display-list")
                 (:file "variant")
                 (:file "settle")
                 (:module "replay"
                  :serial t
                  :components
                  ((:file "support")
                   (:file "script")
                   (:file "player")
                   (:file "recorder")))
                 (:module "layout"
                  :serial t
                  :components
                  ((:file "constraints")
                   (:file "box")
                   (:file "flex")
                   (:file "tree")))
                 (:file "package")
                 (:module "platform"
                  :serial t
                  :if-feature :darwin
                  :components
                  ((:module "macos"
                    :serial t
                    :components
                    ((:file "objc")
                     (:file "image")
                     (:file "ns-event")
                     (:file "host")))))
                 (:module "text"
                  :serial t
                  :if-feature :darwin
                  :components
                  ((:file "line")
                   (:file "glyph-raster")))
                 (:module "render"
                  :serial t
                  :if-feature :darwin
                  :components
                  ((:module "metal"
                    :serial t
                    :components
                    ((:file "renderer")
                     (:file "draw-text"))))))))
  :perform (test-op (o s)
                    ;; Run every test package, then fail the build (and so
                    ;; `make test`) if any test failed or signalled an error.
                    (let* ((results
                             (loop :for package :in '(:almighty-bolt-core/t/clock
                                                      :almighty-bolt-core/t/ring
                                                      :almighty-bolt-core/t/event
                                                      :almighty-bolt-core/t/stats
                                                      :almighty-bolt-core/t/wheel-lock
                                                      :almighty-bolt-core/t/text-edit
                                                      :almighty-bolt-core/t/display-list
                                                      :almighty-bolt-core/t/variant
                                                      :almighty-bolt-core/t/settle
                                                      :almighty-bolt-core/t/replay/script
                                                      :almighty-bolt-core/t/replay/player
                                                      :almighty-bolt-core/t/replay/recorder
                                                      :almighty-bolt-core/t/layout/constraints
                                                      :almighty-bolt-core/t/layout/box
                                                      :almighty-bolt-core/t/layout/flex
                                                      :almighty-bolt-core/t/layout/tree
                                                      :almighty-bolt-core/t/package
                                                      #+darwin :almighty-bolt-core/t/platform/macos/objc
                                                      #+darwin :almighty-bolt-core/t/platform/macos/image
                                                      #+darwin :almighty-bolt-core/t/platform/macos/ns-event
                                                      #+darwin :almighty-bolt-core/t/platform/macos/host
                                                      #+darwin :almighty-bolt-core/t/text/line
                                                      #+darwin :almighty-bolt-core/t/text/glyph-raster
                                                      #+darwin :almighty-bolt-core/t/render/metal/renderer
                                                      #+darwin :almighty-bolt-core/t/render/metal/draw-text)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d almighty-bolt-core test~:p failed." (length bad))))))
