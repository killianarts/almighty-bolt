;;;; What a frame costs in each variant. From the repository root:  make bench-compare

(defpackage #:almighty-bolt-bench/run
  (:use #:cl))

(in-package #:almighty-bolt-bench/run)

(asdf:load-system "almighty-bolt-bench")

(let ((which (or (uiop:getenv "WHICH") "")))
  (cond ((string-equal which "layout")
         (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/LAYOUT-SPIKE")))
        ((string-equal which "cursor")
         (asdf:load-system "almighty-bolt-bench/cursor")
         (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/CURSOR-SPIKE")))
        ((string-equal which "results")
         (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/RESULTS")))
        ((string-equal which "report")
         (funcall (intern "REPORT" "ALMIGHTY-BOLT-BENCH/RESULTS")))
        (t (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/COMPARE")))))
