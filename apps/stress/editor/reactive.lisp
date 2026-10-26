(defpackage #:almighty-bolt-stress/editor/reactive
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
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:padding
                #:column
                #:canvas
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-editor-app))

(in-package #:almighty-bolt-stress/editor/reactive)

;;; The text editor, reactive. The document is changed in place, which a signal cannot see, so the
;;; one signal is a count of the changes: every handler that changed the document adds one, and the
;;; status label and the canvas are components that read it, so that a change describes both and a
;;; scroll or a caret move that changed nothing describes neither. Where the canvas is in the
;;; window, which the input method needs, is asked of the variant.

(defun make-editor-app ()
  (let* ((document (make-sample-document))
         (changes (make-signal 0))
         (variant nil))
    (labels ((changed (did)
               (when did (incf (value changes)))
               did)
             (press (x y button click-count)
               (declare (ignore click-count))
               (when (eql button 0)
                 (changed (editor-press document x y))))
             (drag (x y) (changed (editor-drag document x y)))
             (scroll (dx dy)
               (declare (ignore dx))
               (changed (editor-wheel document dy)))
             (key (event) (changed (editor-key document event)))
             (text (string) (changed (editor-text document string)))
             (compose (kind string start length)
               (changed (editor-composition document kind string start length)))
             (draw (data canvas context list x y width height)
               (declare (ignore data canvas))
               (multiple-value-bind (origin-x origin-y) (lookup-target variant :editor)
                 (paint-editor context list x y width height document
                               (float (or origin-x x) 1f0) (float (or origin-y y) 1f0)))))
      (setf variant
            (make-reactive-variant
             (lambda ()
               (padding
                (column (component (lambda ()
                                     (value changes)
                                     (label (status-text document) :id :status)))
                        (component (lambda ()
                                     (canvas :id :editor :width +editor-width+
                                             :height +editor-height+ :focusable t
                                             :paint #'draw :data (value changes)
                                             :on-press #'press :on-drag #'drag :on-key #'key
                                             :on-text #'text :on-composition #'compose
                                             :on-wheel #'scroll)))
                        :gap 12)
                :all 20)))))))

(register-app :reactive :editor #'make-editor-app :width 740 :height 560)
