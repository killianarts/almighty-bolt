;;;; What a frame costs in each variant. From the repository root:  make bench-compare

(defpackage #:almighty-bolt-bench/run
  (:use #:cl))

(in-package #:almighty-bolt-bench/run)

(asdf:load-system "almighty-bolt-bench")

(funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/COMPARE"))
