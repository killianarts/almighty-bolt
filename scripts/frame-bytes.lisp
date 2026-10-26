(require :asdf)
(asdf:load-system "almighty-bolt-7guis/acceptance")
(asdf:load-system "almighty-bolt-stress/acceptance")
(dolist (name (uiop:split-string (or (uiop:getenv "SYSTEMS") "") :separator " "))
  (when (plusp (length name))
    (asdf:load-system name)))

(defpackage #:almighty-bolt-scripts/frame-bytes
  (:use #:cl)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver))
  (:export #:main))

(in-package #:almighty-bolt-scripts/frame-bytes)

;;;; The bytes a frame of an animated app conses and the time it takes, in the acceptance driver
;;;; (the fake context, so that the text engine is not in it): the app is started, FRAMES (500) are
;;;; drawn to warm it, and then FRAMES more are timed, each a sixtieth of a second after the one
;;;; before. This is the cost of describing and laying out a screen in which everything changes,
;;;; the same for every variant. From the repository root:
;;;;   VARIANTS="retained reactive immediate" TASKS="dashboard timer" FRAMES=2000 sbcl ... --load scripts/frame-bytes.lisp
;;;; (the command line of scripts/frame-hashes.lisp). Each line is: variant task bytes/frame us/frame.

(defun words (name default)
  (mapcar (lambda (word) (intern (string-upcase word) :keyword))
          (uiop:split-string (or (uiop:getenv name) default) :separator " ")))

(defun measure (variant task frames)
  (let ((driver (d:start variant task)))
    (dotimes (i frames)
      (d:advance-time driver 1/60))
    (sb-ext:gc :full t)
    (let ((bytes (sb-ext:get-bytes-consed))
          (start (get-internal-real-time)))
      (dotimes (i frames)
        (d:advance-time driver 1/60))
      (let ((bytes (- (sb-ext:get-bytes-consed) bytes))
            (microseconds (* 1000000.0 (/ (- (get-internal-real-time) start)
                                          internal-time-units-per-second))))
        (format t "~(~a~) ~(~a~) ~,1f ~,1f~%" variant task (/ bytes frames) (/ microseconds frames))))))

(defun main ()
  (let ((frames (parse-integer (or (uiop:getenv "FRAMES") "500"))))
    (dolist (variant (words "VARIANTS" "retained reactive immediate"))
      (dolist (task (words "TASKS" "dashboard timer"))
        (measure variant task frames)))))

(main)
