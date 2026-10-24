(defsystem "almighty-bolt-spike-c"
  :description "Spike C: rounded rectangles drawn by the Metal renderer, on request"
  :author "Micah Killian"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core" "almighty-bolt-spike-b")
  :components ((:file "spike")))
