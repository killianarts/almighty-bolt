;;;; Open a 7GUIs task in a window. From the repository root:
;;;;   make 7guis VARIANT=retained TASK=counter [LOG=1]
;;;;   make 7guis-smoke VARIANT=retained TASK=counter

(defpackage #:almighty-bolt-7guis/run
  (:use #:cl))

(in-package #:almighty-bolt-7guis/run)

(defun keyword-from-environment (name)
  (let ((value (uiop:getenv name)))
    (unless (and value (plusp (length value)))
      (error "Say which with ~a=..., for example VARIANT=retained TASK=counter." name))
    (intern (string-upcase value) :keyword)))

(let ((variant (keyword-from-environment "VARIANT"))
      (task (keyword-from-environment "TASK"))
      (log (plusp (length (or (uiop:getenv "LOG") ""))))
      (smoke (plusp (length (or (uiop:getenv "SMOKE") "")))))
  (asdf:load-system "almighty-bolt-7guis/launcher")
  (asdf:load-system (format nil "almighty-bolt-7guis/~(~a~)" variant))
  ;; Runs the event loop on this, the main, thread, and returns when the window closes.
  (funcall (intern "RUN" "ALMIGHTY-BOLT-7GUIS/LAUNCHER") variant task :log log :smoke smoke))
