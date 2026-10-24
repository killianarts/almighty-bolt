(defsystem "almighty-bolt-spike-acceptance"
  :description "The acceptance script that Spikes A and B share, so their numbers compare"
  :author "Micah Killian"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt-core")
  :components ((:file "acceptance")))
