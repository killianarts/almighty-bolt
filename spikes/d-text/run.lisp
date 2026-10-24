;;;; Spike D in a window. From the repository root:  make spike-d

(defpackage #:almighty-bolt-spikes/d-text/run
  (:use #:cl))

(in-package #:almighty-bolt-spikes/d-text/run)

(asdf:load-system "almighty-bolt-spike-d")

;; Runs the event loop on this, the main, thread; prints the report and quits.
(almighty-bolt-spikes/d-text:main)
