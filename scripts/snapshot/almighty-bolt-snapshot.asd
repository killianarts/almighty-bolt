(defsystem "almighty-bolt-snapshot"
  :version "0.0.1"
  :description "Draw a variant off-screen and write the pixels as a PNG, so that its look can be checked without a window"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :pathname "src"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:file "png")
               (:file "snapshot" :if-feature :darwin)))
