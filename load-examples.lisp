;;; Load the library and its examples from this checkout, in a Lisp of your own:
;;;   (load "/path/to/almighty-bolt/load-examples.lisp")
;;; then, for example,
;;;   (almighty-bolt:open-app 'almighty-bolt-examples/counter:make-counter
;;;                           :title "Counter" :width 360 :height 160)
;;; or, for every widget on one page,
;;;   (almighty-bolt:open-app 'almighty-bolt-examples/gallery:make-gallery
;;;                           :title "Gallery" :width 960 :height 760)

(require :asdf)
(asdf:initialize-source-registry
 `(:source-registry (:tree ,(uiop:pathname-directory-pathname *load-truename*))
   :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-examples")
