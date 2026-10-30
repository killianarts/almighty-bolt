(defpackage #:almighty-bolt-stress/editor/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-stress/editor/model
                #:+editor-width+
                #:+editor-height+
                #:make-sample-document
                #:status-text)
  (:import-from #:almighty-bolt-stress/editor/paint
                #:paint-editor)
  (:import-from #:almighty-bolt-stress/editor/control
                #:editor-key
                #:editor-text
                #:editor-composition
                #:editor-wheel
                #:editor-press
                #:editor-drag)
  (:import-from #:almighty-bolt
                #:label
                #:padding
                #:column
                #:canvas
                #:current-context
                #:kept-let)
  (:export #:make-editor-app))

(in-package #:almighty-bolt-stress/editor/immediate)

;;; The text editor, immediate. The canvas is given a function that draws the document every frame
;;; and returns what happened to it, which is handled where it is described. The canvas draws in its
;;; own coordinates, caret included; the variant puts the caret where the canvas is in the window.

(defun make-editor-app ()
  (kept-let ((document (make-sample-document)))
    (labels ((draw (list context width height)
               (paint-editor context list 0f0 0f0 width height document))
             (handle (event)
               (ecase (first event)
                 (:press
                  (destructuring-bind (x y button click-count) (rest event)
                    (declare (ignore click-count))
                    (when (eql button 0)
                      (editor-press (current-context) document x y))))
                 (:drag (editor-drag (current-context) document (second event) (third event)))
                 (:wheel (editor-wheel document (third event)))
                 (:key (editor-key document (second event)))
                 (:text (editor-text document (second event)))
                 (:composition (apply #'editor-composition document (rest event)))
                 ((:move :exit :release) nil))))
      (lambda ()
        (padding (:all 20)
          (column (:gap 12)
            (label (status-text document) :id :status)
            (dolist (event (canvas +editor-width+ +editor-height+ #'draw :id :editor
                                   :focusable t :autofocus t))
              (handle event))))))))

(register-app :immediate :editor 'make-editor-app :width 740 :height 560)
