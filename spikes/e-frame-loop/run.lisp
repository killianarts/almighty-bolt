;;;; Spike E. From the repository root:  make spike-e

(defpackage #:almighty-bolt-spikes/e-frame-loop/run
  (:use #:cl))

(in-package #:almighty-bolt-spikes/e-frame-loop/run)

(asdf:load-system "almighty-bolt-spike-e")

;; Runs the event loop on this, the main, thread; prints the report and quits.
(almighty-bolt-spikes/e-frame-loop:main)
