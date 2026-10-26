(defpackage #:almighty-bolt-variant-cursor/cursor
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:lookup-target
                #:request-frame
                #:push-rect
                #:context-width
                #:context-height)
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+background+)
  (:import-from #:almighty-bolt-variant-cursor/ui
                #:*ui*
                #:make-ui
                #:ui-mode
                #:ui-rects
                #:ui-stale
                #:begin-build
                #:end-build)
  (:export #:cursor-variant
           #:make-cursor-variant
           #:cursor-variant-mode
           #:cursor-variant-stale))

(in-package #:almighty-bolt-variant-cursor/cursor)

;;; The front end of an interface that is a function called every build, whose widgets draw
;;; where the cursor is as they are called (see ui.lisp for the modes). It has no tree, no
;;; layout pass and no cache; what it keeps from a build is the rectangles of the things that
;;; had an id, for the next build and for the machine interface.

(defclass cursor-variant (variant)
  ((function :initarg :function :reader cursor-variant-function)
   (ui :initarg :ui :reader cursor-variant-ui))
  (:documentation "A variant whose FUNCTION draws the interface, in a mode."))

(defun make-cursor-variant (function &key (mode :rebuild))
  "A variant that calls FUNCTION, with no arguments, to build each frame. MODE is :lag,
:request, :rebuild, :measure or :fragment."
  (make-instance 'cursor-variant :function function :ui (make-ui mode)))

(defun cursor-variant-mode (variant)
  (ui-mode (cursor-variant-ui variant)))

(defun cursor-variant-stale (variant)
  "True if the last build was made with a size that it then found was wrong."
  (ui-stale (cursor-variant-ui variant)))

(defmethod frame ((variant cursor-variant) context list)
  (let ((ui (cursor-variant-ui variant)))
    (let ((*ui* ui))
      (begin-build ui context list)
      (push-rect list 0 0 (context-width context) (context-height context) +background+)
      (funcall (cursor-variant-function variant)))
    (let ((stale (end-build ui)))
      (ecase (ui-mode ui)
        (:rebuild stale)
        (:request (when stale (request-frame context)) nil)
        ((:lag :measure :fragment) nil)))))

(defmethod lookup-target ((variant cursor-variant) id)
  (let ((entry (gethash id (ui-rects (cursor-variant-ui variant)))))
    (and entry (values (svref entry 0) (svref entry 1) (svref entry 2) (svref entry 3)))))
