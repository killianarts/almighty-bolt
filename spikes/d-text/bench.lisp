;;;; Spike D without a window: what shaping, rasterizing and drawing text cost.
;;;; From the repository root:  make bench-text

(defpackage #:almighty-bolt-spikes/d-text/bench
  (:use #:cl))

(in-package #:almighty-bolt-spikes/d-text/bench)

(asdf:load-system "almighty-bolt-spike-d")

(almighty-bolt-spikes/d-text:benchmark)

(uiop:quit 0)
