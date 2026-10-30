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
               (:file "immediate")))
