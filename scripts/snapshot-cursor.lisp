(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-snapshot")
(asdf:load-system "almighty-bolt-7guis/cursor")
(asdf:load-system "almighty-bolt-7guis/immediate")

(defpackage #:almighty-bolt-scripts/snapshot-cursor
  (:use #:cl)
  (:local-nicknames (#:s #:almighty-bolt-snapshot/snapshot)
                    (#:r #:almighty-bolt-7guis/common/registry)
                    (#:dialog #:almighty-bolt-7guis/dialog/cursor)
                    (#:layout #:almighty-bolt-7guis/layout/cursor)))

(in-package #:almighty-bolt-scripts/snapshot-cursor)

;;;; Pictures for record 0022, drawn with the real renderer and text engine, from the repository
;;;; root:  make snapshots-cursor OUT=/tmp/shots/
;;;; The dialog in a window made 700 wide and then 420 wide, as the cursor front end draws the
;;;; frame at 420 in each mode, and as the box tree does.

(defparameter +out+ (or (uiop:getenv "OUT") "snapshots/"))

(defun path (name) (format nil "~a~a.png" +out+ name))

(defun narrower (mode name)
  "Draw the dialog at 700, then one frame at 420 in MODE, and write that frame."
  (let ((object (if (eq mode :box)
                    (funcall (r:app-constructor (r:find-app :immediate :dialog)))
                    (dialog:make-dialog mode))))
    (s:with-snapshot-host (host target object :width 700 :height 420)
      (s:draw-frame host 2)
      (let ((almighty-bolt-snapshot/snapshot::*size* (list 420 420 2)))
        (s:draw-frame host))
      (s:write-snapshot target (path name)))))

(ensure-directories-exist +out+)
(narrower :box "dialog-420-box-tree")
(narrower :lag "dialog-420-lag")
(narrower :rebuild "dialog-420-rebuild")
(narrower :measure "dialog-420-measure")
(narrower :fragment "dialog-420-fragment")
(s:with-snapshot-host (host target (layout:make-layout :rebuild) :width 640 :height 420)
  (s:draw-frame host 2)
  (s:write-snapshot target (path "layout-640-cursor")))
(format t "~&Wrote the pictures to ~a~%" +out+)
