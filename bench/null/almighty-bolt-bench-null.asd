(defsystem "almighty-bolt-bench-null"
  :version "0.2.0"
  :description "The null baseline app: replays a fixed display list, to measure the core's own cost"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:file "null")
               (:file "measure" :if-feature :darwin)))
