(defsystem "almighty-bolt-variant-null"
  :version "0.0.1"
  :description "The null baseline variant: replays a fixed display list, to measure the core's own cost"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:file "null")
               (:file "measure" :if-feature :darwin)))
