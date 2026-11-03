(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-snapshot")
(asdf:load-system "almighty-bolt-7guis/apps")
(asdf:load-system "almighty-bolt-stress/apps")

(defpackage #:almighty-bolt-scripts/snapshot-apps
  (:use #:cl)
  (:local-nicknames (#:s #:almighty-bolt-snapshot/snapshot)
                    (#:r #:almighty-bolt-7guis/common/registry)))

(in-package #:almighty-bolt-scripts/snapshot-apps)

;;;; A picture of each 7GUIs task and stress app after a few events, drawn with the real
;;;; renderer and text engine, from the repository root:
;;;;   OUT=/tmp/shots/ sbcl --noinform --non-interactive --no-sysinit \
;;;;     --no-userinit --load scripts/snapshot-apps.lisp
;;;; (or make snapshots OUT=/tmp/shots/). The files are OUT/<task>.png.

(defun snap (task fn)
  (let* ((app (r:find-app task))
         (object (r:make-app-object app))
         (out (or (uiop:getenv "OUT") "snapshots/")))
    (ensure-directories-exist out)
    (s:with-snapshot-host (host target object :width (r:app-width app) :height (r:app-height app))
      (s:draw-frame host 2)
      (funcall fn host)
      (s:draw-frame host)
      (s:write-snapshot target (format nil "~a~(~a~).png" out task)))))

(progn
  (snap :counter
        (lambda (host) (s:click-control host :count-button) (s:click-control host :count-button)))
  (snap :temperature
        (lambda (host) (s:click-control host :celsius) (s:type-string host "37")))
  (snap :flight
        (lambda (host) (s:click-control host :kind :fx 0.9) (s:click-control host :return)
          (s:type-string host "x")))
  (snap :timer (lambda (host) (declare (ignore host)) nil))
  (snap :crud (lambda (host) (s:click-control host :list :fy 0.2)))
  (snap :circles
        (lambda (host)
          (s:click-control host :canvas :fx 0.3 :fy 0.3)
          (s:click-control host :canvas :fx 0.5 :fy 0.5)
          (s:click-control host :canvas :fx 0.7 :fy 0.4)))
  (snap :cells
        (lambda (host)
          (s:click-control host :grid :fx 0.2 :fy 0.2)
          (s:type-string host "=1+2")
          (s:key-press host #\Return)))
  ;; The stress apps.
  (snap :dashboard
        (lambda (host) (declare (ignore host)) nil))
  (snap :rows
        (lambda (host)
          (s:click-control host :list :fy 0.2)
          (dotimes (i 3) (s:key-press host (code-char #xF701)))))
  (snap :editor
        (lambda (host)
          (s:click-control host :editor :fx 0.4 :fy 0.2)
          (s:type-string host "typed here")
          (dotimes (i 12)
            (s:key-press host (code-char #xF703) :modifiers 1))))
  (format t "~&Wrote the snapshots.~%"))
