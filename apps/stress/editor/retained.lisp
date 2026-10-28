(defpackage #:almighty-bolt-stress/editor/retained
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
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:column)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:canvas
                #:canvas-invalidate)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-editor-app))

(in-package #:almighty-bolt-stress/editor/retained)

;;; The text editor, retained: the document is one object that the handlers change; the editor is
;;; one canvas that draws it and handles its own events, and a label above it says where the caret
;;; is. After each change the handlers update the label and say that the canvas must draw again.
;;; A canvas that has not changed is not drawn again, but kept as commands to draw where it is;
;;; the caret it says is in the canvas, and the variant puts it where the canvas is in the window.

(defun make-editor-app ()
  (let ((document (make-sample-document))
        (editor nil) (status nil))
    (labels ((refresh (changed)
               (when changed
                 (let ((text (status-text document)))
                   (unless (string= text (label-text status))
                     (setf (label-text status) text)))
                 (canvas-invalidate editor))
               changed)
             (press (source context x y button click-count)
               (declare (ignore source click-count))
               (when (eql button 0)
                 (refresh (editor-press context document x y))))
             (drag (source context x y)
               (declare (ignore source))
               (refresh (editor-drag context document x y)))
             (scroll (source context dx dy)
               (declare (ignore source context dx))
               (refresh (editor-wheel document dy)))
             (key (source context event)
               (declare (ignore source context))
               (refresh (editor-key document event)))
             (text (source context string)
               (declare (ignore source context))
               (refresh (editor-text document string)))
             (compose (source context kind string start length)
               (declare (ignore source context))
               (refresh (editor-composition document kind string start length)))
             (paint (source context list x y width height)
               (declare (ignore source))
               (paint-editor context list x y width height document)))
      (setf status (label (status-text document) :id :status)
            editor (canvas :id :editor :width +editor-width+ :height +editor-height+ :focusable t
                           :autofocus t
                           :paint #'paint :on-press #'press :on-drag #'drag :on-key #'key
                           :on-text #'text :on-composition #'compose :on-wheel #'scroll))
      (make-retained-variant
       (padding (:all 20)
         (column (:gap 12) status editor))))))

(register-app :retained :editor 'make-editor-app :width 740 :height 560)
