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
               "almighty-bolt-7guis/immediate" "almighty-bolt-variant-null"
               "almighty-bolt-stress/retained" "almighty-bolt-stress/reactive"
               "almighty-bolt-stress/immediate")
  :components ((:file "compare" :if-feature :darwin)
               (:file "layout-spike" :if-feature :darwin)
               (:file "results" :if-feature :darwin)
               (:file "app-metrics" :if-feature :darwin)
               (:file "compile-time" :if-feature :darwin)))

(defsystem "almighty-bolt-bench/cursor"
  :description "The cursor experiment of record 0022 (archived), swept over window widths: make bench-cursor"
  :depends-on ("almighty-bolt-core" "almighty-bolt-7guis/immediate" "almighty-bolt-7guis/cursor"
               "almighty-bolt-variant-support")
  :components ((:file "cursor-spike" :if-feature :darwin)))
