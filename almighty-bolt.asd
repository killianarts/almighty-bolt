;;; The library is SBCL only for now (CFFI calls into AppKit and Metal, and it uses SBCL's own hooks for
;;; hot reload). Another Lisp gets as far as the first file that reads a constant at read time and fails
;;; with an error about an unbound variable; this says why, first.
#-sbcl
(error "almighty-bolt runs on SBCL only, and this is ~a. In Sly, M-x sly starts your default Lisp, and C-u M-x sly asks which."
       (lisp-implementation-type))

(defsystem "almighty-bolt"
  :version "0.0.1"
  :description "A GUI library for Common Lisp: the platform, the renderer, text and layout, and the immediate-mode ui"
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
               (:file "trace")
               (:file "ring-summary")
               (:file "wheel-lock")
               (:file "text-edit")
               (:file "display-list")
               (:file "variant")
               (:file "settle")
               (:file "redefinition")
               (:file "kept")
               (:file "sources")
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
               (:file "package")
               ;; The ui: the interface is described again every frame, and unchanged layout
               ;; and painting are skipped by comparing what was described.
               (:module "ui"
                :serial t
                :components
                ((:file "theme")
                 (:file "hash")
                 (:file "box")
                 (:file "ui")
                 (:file "widgets")
                 (:file "layout")
                 (:file "paint")
                 (:file "immediate")
                 (:file "semantics")))
               ;; How a program or an agent looks at an app and works it.
               (:module "machine"
                :serial t
                :components
                ((:file "png")
                 (:file "protocol")
                 (:file "host-session" :if-feature :darwin)))
               ;; How an app is started, and the face of the library.
               (:file "app" :if-feature :darwin)
               (:file "almighty-bolt" :if-feature :darwin))
  :in-order-to ((test-op (test-op "almighty-bolt/tests"))))

(defsystem "almighty-bolt/mcp"
  :description "An MCP server for an open app, on a local socket: an agent sees the screen as a tree, works the app by what things are called, and waits for it. Opt-in; nothing starts it but a call."
  :depends-on ("almighty-bolt" "com.inuoe.jzon" "sb-bsd-sockets")
  :pathname "src/machine"
  :components ((:file "mcp" :if-feature :darwin)))

(defsystem "almighty-bolt/test-support"
  :description "What the tests of the apps share: a context with text that is easy to count"
  :depends-on ("almighty-bolt")
  :pathname "t/support"
  :components ((:file "fake-context")))

(defsystem "almighty-bolt/tests"
  ;; Integration tests, against the real operating system, text engine and GPU.
  ;; flexi-streams is not used here. cl-unicode (lisp-unit2 -> cl-interpol ->
  ;; cl-unicode) generates its source files with it on first load, and vend
  ;; only finds it if some system names it.
  :depends-on ("almighty-bolt" "almighty-bolt/test-support" "almighty-bolt-examples"
               "lisp-unit2" "flexi-streams")
  :components ((:module "t"
                :serial t
                :components
                ((:file "app" :if-feature :darwin)
                 (:module "ui"
                  :serial t
                  :components
                  ((:file "widgets")))
                 (:module "platform"
                  :serial t
                  :if-feature :darwin
                  :components
                  ((:module "macos"
                    :serial t
                    :components
                    ((:file "objc")
                     (:file "image")
                     (:file "text-input")
                     (:file "ns-event")))))
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
                             (loop :for package :in '(#+darwin :almighty-bolt/t/app
                                                      :almighty-bolt/t/ui/widgets
                                                      #+darwin :almighty-bolt/t/platform/macos/objc
                                                      #+darwin :almighty-bolt/t/platform/macos/image
                                                      #+darwin :almighty-bolt/t/platform/macos/text-input
                                                      #+darwin :almighty-bolt/t/platform/macos/ns-event
                                                      #+darwin :almighty-bolt/t/text/line
                                                      #+darwin :almighty-bolt/t/text/glyph-raster
                                                      #+darwin :almighty-bolt/t/render/metal/renderer
                                                      #+darwin :almighty-bolt/t/render/metal/draw-text)
                                   :collect (uiop:symbol-call :lisp-unit2 :run-tests
                                                              :package package)))
                           (bad (loop :for db :in results
                                      :append (uiop:symbol-call :lisp-unit2 :failed db)
                                      :append (uiop:symbol-call :lisp-unit2 :errors db))))
                      (dolist (db results)
                        (uiop:symbol-call :lisp-unit2 :print-summary db))
                      (when bad
                        (error "~d almighty-bolt test~:p failed." (length bad))))))
