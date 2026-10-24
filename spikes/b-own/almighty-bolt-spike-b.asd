(defsystem "almighty-bolt-spike-b"
  :description "Spike B: a window and an MTKView through our own Objective-C bindings"
  :author "Micah Killian"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core" "almighty-bolt-spike-acceptance")
  :components ((:file "spike")))
