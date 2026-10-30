;;;; What a frame costs, and the other numbers of the benchmark protocol. From the repository root:
;;;;   make bench-run | bench-overhead | bench-app | bench-compile | bench-report

(defpackage #:almighty-bolt-bench/run
  (:use #:cl))

(in-package #:almighty-bolt-bench/run)

(asdf:load-system "almighty-bolt-bench")

(let ((which (or (uiop:getenv "WHICH") "")))
  (cond ((string-equal which "overhead")
         (funcall (intern "OVERHEAD-MAIN" "ALMIGHTY-BOLT-BENCH/RESULTS")))
        ((string-equal which "compile")
         (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/COMPILE-TIME")))
        ((string-equal which "app")
         (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/APP-METRICS")))
        ((string-equal which "results")
         (funcall (intern "MAIN" "ALMIGHTY-BOLT-BENCH/RESULTS")))
        ((string-equal which "report")
         (funcall (intern "REPORT" "ALMIGHTY-BOLT-BENCH/RESULTS")))
        (t (error "Say which with WHICH=results, overhead, app, compile or report (see the Makefile)."))))
