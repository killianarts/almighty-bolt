(defsystem "almighty-bolt-bench"
  :version "0.0.1"
  :description "The same screen in each variant, timed: what a frame costs the variant"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core" "almighty-bolt-variant-retained"
               "almighty-bolt-variant-reactive" "almighty-bolt-variant-immediate"
               "almighty-bolt-7guis/retained" "almighty-bolt-7guis/reactive"
               "almighty-bolt-7guis/immediate")
  :components ((:file "compare" :if-feature :darwin)
               (:file "layout-spike" :if-feature :darwin)))
