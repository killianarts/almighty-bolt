;;;; The retained variant's Counter in a window. From the repository root:
;;;;   make retained-counter          click it yourself
;;;;   make retained-counter-check    a thread clicks it and reports (RETAINED_CHECK=1)

(defpackage #:almighty-bolt-variant-retained/run
  (:use #:cl))

(in-package #:almighty-bolt-variant-retained/run)

(asdf:load-system "almighty-bolt-variant-retained")

;; Runs the event loop on this, the main, thread; returns when the window closes.
(almighty-bolt-variant-retained/window:main
 :selfcheck (and (uiop:getenv "RETAINED_CHECK") t))
