(defpackage #:almighty-bolt-stress/editor/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:lookup-target)
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
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:label
                #:padding
                #:column
                #:canvas)
  (:import-from #:almighty-bolt-variant-immediate/immediate
                #:make-immediate-variant)
  (:export #:make-editor-app))

(in-package #:almighty-bolt-stress/editor/immediate)

;;; The text editor, immediate. The canvas is given a function that draws the document every frame
;;; and returns what happened to it, which is handled where it is described. The canvas draws in its
;;; own coordinates; where it is in the window, which the input method needs to know, is where the
;;; last frame put it.

(defun make-editor-app ()
  (let ((document (make-sample-document))
        (variant nil))
    (labels ((draw (list context width height)
               (multiple-value-bind (x y) (lookup-target variant :editor)
                 (paint-editor context list 0f0 0f0 width height document
                               (float (or x 0) 1f0) (float (or y 0) 1f0))))
             (handle (event)
               (ecase (first event)
                 (:press
                  (destructuring-bind (x y button click-count) (rest event)
                    (declare (ignore click-count))
                    (when (eql button 0)
                      (editor-press document x y))))
                 (:drag (editor-drag document (second event) (third event)))
                 (:wheel (editor-wheel document (third event)))
                 (:key (editor-key document (second event)))
                 (:text (editor-text document (second event)))
                 (:composition (apply #'editor-composition document (rest event)))
                 ((:move :exit :release) nil))))
      (setf variant
            (make-immediate-variant
             (lambda ()
               (padding (:all 20)
                 (column (:gap 12)
                   (label (status-text document) :id :status)
                   (dolist (event (canvas :editor +editor-width+ +editor-height+ #'draw
                                          :focusable t))
                     (handle event))))))))))

(register-app :immediate :editor #'make-editor-app :width 740 :height 560)
