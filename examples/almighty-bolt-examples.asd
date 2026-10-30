(defsystem "almighty-bolt-examples"
  :version "0.0.1"
  :description "Small apps that use almighty-bolt through its public package and nothing else"
  :author "Micah Killian"
  :maintainer "Micah Killian <micah@almightylisp.com>"
  :license "MIT"
  :serial t
  :depends-on ("almighty-bolt")
  :components ((:file "counter")
               (:file "todo")))
