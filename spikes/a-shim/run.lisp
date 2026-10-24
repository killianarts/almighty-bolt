;;;; Spike A. From the repository root:  make spike-a

(defpackage #:almighty-bolt-spikes/a-shim/run
  (:use #:cl))

(in-package #:almighty-bolt-spikes/a-shim/run)

(asdf:load-system "almighty-bolt-spike-a")

;; Starts the AppKit event loop and does not return. The spike prints its
;; report and quits by itself.
(almighty-bolt-spikes/a-shim:main)
