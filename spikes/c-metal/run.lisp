;;;; Spike C in a window. From the repository root:  make spike-c

(defpackage #:almighty-bolt-spikes/c-metal/run
  (:use #:cl))

(in-package #:almighty-bolt-spikes/c-metal/run)

(asdf:load-system "almighty-bolt-spike-c")

;; Runs the event loop on this, the main, thread; prints the report and quits.
(almighty-bolt-spikes/c-metal:main)
