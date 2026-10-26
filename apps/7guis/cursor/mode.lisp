(defpackage #:almighty-bolt-7guis/cursor/mode
  (:use #:cl)
  (:export #:cursor-mode))

(in-package #:almighty-bolt-7guis/cursor/mode)

;;; Which way the cursor front end deals with a size it does not have: the environment's
;;; CURSOR_MODE (lag, request, rebuild, measure or fragment), so that one command line can open a
;;; window in each; rebuild if it says nothing.

(defun cursor-mode ()
  (let ((value (uiop:getenv "CURSOR_MODE")))
    (if (and value (plusp (length value)))
        (let ((mode (intern (string-upcase value) :keyword)))
          (unless (member mode '(:lag :request :rebuild :measure :fragment))
            (error "CURSOR_MODE is ~a; it is lag, request, rebuild, measure or fragment." value))
          mode)
        :rebuild)))
