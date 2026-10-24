;;;; Spike B. From the repository root:  make spike-b

(defpackage #:almighty-bolt-spikes/b-own/run
  (:use #:cl))

(in-package #:almighty-bolt-spikes/b-own/run)

(asdf:load-system "almighty-bolt-spike-b")

;; Runs AppKit's event loop on this, the main, thread and does not return. The
;; spike prints its report and quits by itself.
(almighty-bolt-spikes/b-own:main)
