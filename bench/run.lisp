;;;; What a frame costs in each variant. From the repository root:  make bench-compare

(defpackage #:almighty-bolt-bench/run
  (:use #:cl))

(in-package #:almighty-bolt-bench/run)

(asdf:load-system "almighty-bolt-bench")

(let ((which (uiop:getenv "WHICH")))
  (funcall (intern "MAIN" (if (and which (string-equal which "layout"))
                              "ALMIGHTY-BOLT-BENCH/LAYOUT-SPIKE"
                              "ALMIGHTY-BOLT-BENCH/COMPARE"))))
