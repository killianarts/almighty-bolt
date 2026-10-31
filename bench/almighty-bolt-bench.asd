(defsystem "almighty-bolt-bench"
  :version "0.1.0"
  :description "The immediate variant's apps, timed: what a frame costs, startup, idle, memory, compile time"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt" "almighty-bolt-7guis/immediate" "almighty-bolt-bench-null"
               "almighty-bolt-stress/immediate")
  :components ((:file "results" :if-feature :darwin)
               (:file "app-metrics" :if-feature :darwin)
               (:file "compile-time" :if-feature :darwin)))
