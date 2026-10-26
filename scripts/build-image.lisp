;;;; Save an executable of a variant with its apps. From the repository root:
;;;;   make image VARIANT=immediate        (writes build/immediate)
;;;; Run it with TASK=counter build/immediate. The image holds the core, the variant and the seven
;;;; 7GUIs tasks in it; the task is chosen when it starts.

(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))

(let* ((variant-name (or (uiop:getenv "VARIANT") (error "Say which variant with VARIANT=immediate.")))
       (variant (intern (string-upcase variant-name) :keyword))
       (out (or (uiop:getenv "OUT") (format nil "build/~a" (string-downcase variant-name)))))
  (asdf:load-system "almighty-bolt-7guis/launcher")
  (asdf:load-system (format nil "almighty-bolt-7guis/~(~a~)" variant-name))
  (ensure-directories-exist out)
  (sb-ext:save-lisp-and-die out
                            :executable t
                            :save-runtime-options t
                            :toplevel (lambda ()
                                        (funcall (intern "IMAGE-MAIN" "ALMIGHTY-BOLT-7GUIS/LAUNCHER")
                                                 variant))))
