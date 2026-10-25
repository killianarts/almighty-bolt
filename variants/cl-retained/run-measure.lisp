;;;; What painting costs the retained variant, with and without caches. From the
;;;; repository root:  make bench-retained

(defpackage #:almighty-bolt-variant-retained/run-measure
  (:use #:cl))

(in-package #:almighty-bolt-variant-retained/run-measure)

(asdf:load-system "almighty-bolt-variant-retained")

(almighty-bolt-variant-retained/measure:main)
