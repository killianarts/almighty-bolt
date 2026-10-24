(defsystem "almighty-bolt-spike-a"
  :description "Spike A: a window and an MTKView through cl-nextstep"
  :author "Micah Killian"
  :license "MIT"
  :serial t
  :depends-on ("cl-nextstep" "almighty-bolt-core" "almighty-bolt-spike-acceptance")
  :components ((:file "spike")))
