;;;; Spike C without a window: time and allocation of the renderer.
;;;; From the repository root:  make bench-metal

(defpackage #:almighty-bolt-spikes/c-metal/bench
  (:use #:cl))

(in-package #:almighty-bolt-spikes/c-metal/bench)

(asdf:load-system "almighty-bolt-spike-c")

(almighty-bolt-spikes/c-metal:benchmark)

(uiop:quit 0)
