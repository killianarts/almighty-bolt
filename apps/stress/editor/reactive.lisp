(defpackage #:almighty-bolt-stress/editor/reactive
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
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:column
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:canvas)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-editor-app))

(in-package #:almighty-bolt-stress/editor/reactive)

;;; The text editor, reactive. The document is changed in place, which a signal cannot see, so the
;;; one signal is a count of the changes: every handler that changed the document adds one, and the
;;; status label and the canvas are components that read it, so that a change describes both and a
;;; scroll or a caret move that changed nothing describes neither. The caret is said in the canvas,
;;; and the variant puts it where the canvas is in the window.

(defun make-editor-app ()
  (let* ((document (make-sample-document))
         (changes (make-signal 0)))
    (labels ((changed (did)
               (when did (incf (value changes)))
               did)
             (press (context x y button click-count)
               (declare (ignore click-count))
               (when (eql button 0)
                 (changed (editor-press context document x y))))
             (drag (context x y) (changed (editor-drag context document x y)))
             (scroll (context dx dy)
               (declare (ignore context dx))
               (changed (editor-wheel document dy)))
             (key (context event)
               (declare (ignore context))
               (changed (editor-key document event)))
             (text (context string)
               (declare (ignore context))
               (changed (editor-text document string)))
             (compose (context kind string start length)
               (declare (ignore context))
               (changed (editor-composition document kind string start length)))
             (draw (data canvas context list x y width height)
               (declare (ignore data canvas))
               (paint-editor context list x y width height document)))
      (make-reactive-variant
       (lambda ()
         (padding (:all 20)
           (column (:gap 12)
             (component ()
               (value changes)
               (label (status-text document) :id :status))
             (component ()
               (canvas :id :editor :width +editor-width+
                       :height +editor-height+ :focusable t :autofocus t
                       :paint #'draw :data (value changes)
                       :on-press #'press :on-drag #'drag :on-key #'key
                       :on-text #'text :on-composition #'compose
                       :on-wheel #'scroll)))))))))

(register-app :reactive :editor 'make-editor-app :width 740 :height 560)
