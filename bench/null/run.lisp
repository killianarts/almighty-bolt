;;;; The null variant's measurements. From the repository root:  make bench-null
;;;; Run it from a terminal of your own, and leave the window alone while it runs.

(defpackage #:almighty-bolt-bench-null/run
  (:use #:cl))

(in-package #:almighty-bolt-bench-null/run)

(asdf:load-system "almighty-bolt-bench-null")

;; Runs the event loop on this, the main, thread; prints the report and quits.
(almighty-bolt-bench-null/measure:main)
