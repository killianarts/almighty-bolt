(defsystem "almighty-bolt-spike-d"
  :description "Spike D: text through Core Text, the glyph atlas and the Metal renderer"
  :author "Micah Killian"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core" "almighty-bolt-spike-b")
  :components ((:file "spike")))
