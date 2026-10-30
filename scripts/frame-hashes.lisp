(require :asdf)
(asdf:load-system "almighty-bolt-7guis/acceptance")
(asdf:load-system "almighty-bolt-stress/acceptance")
(dolist (name (uiop:split-string (or (uiop:getenv "SYSTEMS") "") :separator " "))
  (when (plusp (length name))
    (asdf:load-system name)))

(defpackage #:almighty-bolt-scripts/frame-hashes
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:d #:almighty-bolt-7guis/t/acceptance/driver))
  (:export #:main))

(in-package #:almighty-bolt-scripts/frame-hashes)

;;;; The hash of the frame of each app of each variant after each step of a fixed battery of events,
;;;; as the acceptance driver delivers them (the fake context, a virtual clock). Run before and after
;;;; a change of an app's code and compare the output: the same lines mean the same frames. From the
;;;; repository root:
;;;;   VARIANTS="immediate" sbcl --noinform --non-interactive --no-sysinit --no-userinit \
;;;;     --eval '(require :asdf)' --eval '(asdf:initialize-source-registry (list :source-registry
;;;;     (list :tree (uiop:getcwd)) :ignore-inherited-configuration))' --load scripts/frame-hashes.lisp
;;;; Each line is: variant task step hash commands. SYSTEMS="a-system another" loads systems after
;;;; the apps, which can put another version of an app in place of one.

(defparameter +down+ (code-char #xF701))

(defun repeat (count function)
  (dotimes (i count) (funcall function)))

(defparameter +steps+
  `((:counter
     ("click" ,(lambda (d) (d:click d :count-button)))
     ("click again" ,(lambda (d) (d:click d :count-button))))
    (:temperature
     ("celsius" ,(lambda (d) (d:click d :celsius) (d:type-text d "37")))
     ("fahrenheit" ,(lambda (d) (d:click d :fahrenheit) (d:type-text d "212")))
     ("undo" ,(lambda (d) (when (d:found-p d :undo) (d:click d :undo)))))
    (:flight
     ("return flight" ,(lambda (d) (d:click-at d :kind 0.9 0.5)))
     ("type" ,(lambda (d) (d:click d :return) (d:type-text d "x")))
     ("book" ,(lambda (d) (d:click d :book)))
     ("one way" ,(lambda (d) (d:click-at d :kind 0.1 0.5))))
    (:timer
     ("three seconds" ,(lambda (d) (d:advance-time d 3 :frames 3)))
     ("duration" ,(lambda (d) (d:press-at d :duration 0.3 0.5) (d:release-at d :duration 0.3 0.5)))
     ("two more" ,(lambda (d) (d:advance-time d 2 :frames 2)))
     ("reset" ,(lambda (d) (d:click d :reset))))
    (:crud
     ("select" ,(lambda (d) (d:click-at d :list 0.5 0.2)))
     ("create" ,(lambda (d) (d:click d :name) (d:type-text d "Zed") (d:click d :create)))
     ("filter" ,(lambda (d) (d:click d :filter) (d:type-text d "Z")))
     ("delete" ,(lambda (d) (d:click d :delete))))
    (:circles
     ("three circles" ,(lambda (d) (d:click-at d :canvas 0.3 0.3) (d:click-at d :canvas 0.5 0.5)
                         (d:click-at d :canvas 0.7 0.4)))
     ("undo redo" ,(lambda (d) (d:click d :undo) (d:click d :redo)))
     ("dialog" ,(lambda (d) (d:right-click d :canvas 0.5 0.5)))
     ("done" ,(lambda (d) (when (d:found-p d :done) (d:click d :done)))))
    (:cells
     ("formula" ,(lambda (d) (d:click-at d :grid 0.2 0.2) (d:type-text d "=1+2")
                   (d:press-key d #\Return)))
     ("second cell" ,(lambda (d) (d:click-at d :grid 0.5 0.4) (d:type-text d "7")
                       (d:press-key d #\Return)))
     ("scroll" ,(lambda (d) (d:scroll d :grid 3))))
    (:dashboard
     ("two frames" ,(lambda (d) (d:advance-time d 0.1 :frames 2)))
     ("half a second" ,(lambda (d) (d:advance-time d 0.5 :frames 3))))
    (:rows
     ("select" ,(lambda (d) (d:click-at d :list 0.5 0.2)))
     ("down" ,(lambda (d) (repeat 3 (lambda () (d:press-key d +down+)))))
     ("wheel" ,(lambda (d) (d:scroll d :list 3)))
     ("bar" ,(lambda (d) (d:click-at d :list 0.97 0.5))))
    (:editor
     ("click" ,(lambda (d) (d:click-at d :editor 0.3 0.2)))
     ("type" ,(lambda (d) (d:type-text d "hello") (d:press-key d #\Return)))
     ("input method" ,(lambda (d) (d:input-text d "日本") (d:compose d :started "に")
                        (d:compose d :committed "日")))
     ("backspace" ,(lambda (d) (d:press-key d #\Backspace)))
     ("wheel" ,(lambda (d) (d:scroll d :editor -3)))
     ("second click" ,(lambda (d) (d:click-at d :editor 0.6 0.5))))))

(defun report (variant task step driver)
  (let ((list (d:driver-list driver)))
    (format t "~(~a~) ~(~a~) ~a ~x ~d~%" variant task step (core:display-list-hash list)
            (core:display-list-count list))))

(defun run-task (variant task steps)
  (handler-case
      (let ((driver (d:start variant task)))
        (report variant task "start" driver)
        (dolist (step steps)
          (funcall (second step) driver)
          (report variant task (first step) driver))
        (d:resize driver 500 420)
        (report variant task "resized" driver))
    (error (condition)
      (format t "~(~a~) ~(~a~) ERROR ~a~%" variant task
              (substitute #\Space #\Newline (princ-to-string condition))))))

(defun main ()
  (let ((variants (mapcar (lambda (name) (intern (string-upcase name) :keyword))
                          (uiop:split-string (or (uiop:getenv "VARIANTS") "immediate")
                                             :separator " "))))
    (dolist (variant variants)
      (dolist (entry +steps+)
        (run-task variant (first entry) (rest entry))))))

(main)
