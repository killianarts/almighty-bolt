;;;; Save an executable of the example apps. From the repository root:
;;;;   make image        (writes build/apps)
;;;; Run it with TASK=counter build/apps. The image holds the library, the seven 7GUIs tasks and
;;;; the stress apps; the task is chosen when it starts.

(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))

(let ((out (or (uiop:getenv "OUT") "build/apps")))
  (asdf:load-system "almighty-bolt-7guis/launcher")
  (asdf:load-system "almighty-bolt-7guis/apps")
  (asdf:load-system "almighty-bolt-stress/apps")
  (ensure-directories-exist out)
  (sb-ext:save-lisp-and-die out
                            :executable t
                            :save-runtime-options t
                            :toplevel (lambda ()
                                        (funcall (intern "IMAGE-MAIN" "ALMIGHTY-BOLT-7GUIS/LAUNCHER")))))
