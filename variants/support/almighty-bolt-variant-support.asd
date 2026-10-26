(defsystem "almighty-bolt-variant-support"
  :version "0.0.1"
  :description "What the tests of every variant share: a context with text that is easy to count"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :depends-on ("almighty-bolt-core")
  :components ((:file "fake-context")
               (:file "compare")))
