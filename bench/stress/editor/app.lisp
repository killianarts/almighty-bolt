(defpackage #:almighty-bolt-stress/editor/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-stress/editor/model)
                    (#:paint #:almighty-bolt-stress/editor/paint)
                    (#:control #:almighty-bolt-stress/editor/control)
                    (#:bolt #:almighty-bolt))
  (:export #:make-editor-app))

(in-package #:almighty-bolt-stress/editor/app)

;;; The text editor, immediate. The canvas is given a function that draws the document every frame
;;; and returns what happened to it, which is handled where it is described. The canvas draws in its
;;; own coordinates, caret included; the app puts the caret where the canvas is in the window.

(defun make-editor-app ()
  (bolt:state-let ((document (model:make-sample-document)))
    (labels ((draw (list context width height)
                    (paint:paint-editor context list 0f0 0f0 width height document))
             (handle (event)
                    (ecase (first event)
                 (:press
                  (destructuring-bind (x y button click-count) (rest event)
                    (declare (ignore click-count))
                    (when (eql button 0)
                           (control:editor-press (bolt:current-context) document x y))))
                 (:drag (control:editor-drag (bolt:current-context) document (second event) (third event)))
                 (:wheel (control:editor-wheel document (third event)))
                 (:key (control:editor-key document (second event)))
                 (:text (control:editor-text document (second event)))
                 (:composition (apply #'control:editor-composition document (rest event)))
                 ((:move :exit :release) nil))))
      (lambda ()
        (bolt:padding (:all 20)
          (bolt:column (:gap 12)
            (bolt:label (model:status-text document) :id :status)
            (dolist (event (bolt:canvas model:+editor-width+ model:+editor-height+ #'draw :id :editor
                                        :focusable t :autofocus t))
              (handle event))))))))

(registry:register-app :editor 'make-editor-app :width 740 :height 560)
