(defpackage #:almighty-bolt-7guis/circles/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/circles/paint
                #:paint-circles)
  (:import-from #:almighty-bolt-7guis/circles/model
                #:make-drawing
                #:drawing-circles
                #:drawing-can-undo-p
                #:drawing-can-redo-p
                #:drawing-add
                #:drawing-resize
                #:drawing-undo
                #:drawing-redo
                #:drawing-circle-at
                #:circle-id
                #:circle-x
                #:circle-y
                #:circle-diameter)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-enabled
                #:node-visible)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:panel
                #:center
                #:stack
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:button
                #:scrim)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:slider
                #:slider-value
                #:canvas
                #:canvas-invalidate)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-circles))

(in-package #:almighty-bolt-7guis/circles/retained)

;;; The circle drawer, retained: the drawing is a value in a variable that each change
;;; replaces; the canvas is a widget that draws it and says it must draw again after each
;;; change; and the dialog is a layer of the tree that is hidden until a circle is asked
;;; to be adjusted.

(defun make-circles ()
  (let ((drawing (make-drawing))
        ;; The circle under the pointer, which is the one that is grey, and the circle
        ;; being adjusted and the diameter the slider has given it so far.
        (hot nil) (adjusting nil) (preview 0)
        (undo nil) (redo nil) (canvas nil) (overlay nil) (diameter-slider nil))
    (labels ((refresh ()
               (setf (node-enabled undo) (drawing-can-undo-p drawing)
                     (node-enabled redo) (drawing-can-redo-p drawing))
               (canvas-invalidate canvas))
             (paint (source context list x y width height)
               (declare (ignore source context))
               (paint-circles list x y width height drawing hot adjusting preview))
             (hover (source x y)
               (declare (ignore source))
               (let ((circle (drawing-circle-at drawing x y)))
                 (let ((id (and circle (circle-id circle))))
                   (unless (eql id hot)
                     (setf hot id)
                     (canvas-invalidate canvas)))))
             (leave (source)
               (declare (ignore source))
               (when hot
                 (setf hot nil)
                 (canvas-invalidate canvas)))
             (press (source x y button click-count)
               (declare (ignore source click-count))
               (let ((circle (drawing-circle-at drawing x y)))
                 (cond ((and (eql button 0) (null circle))
                        (setf drawing (drawing-add drawing x y))
                        (hover nil x y)
                        (refresh))
                       ((and (eql button 1) circle)
                        (open-dialog circle)))))
             (open-dialog (circle)
               (setf adjusting (circle-id circle)
                     preview (circle-diameter circle)
                     (slider-value diameter-slider) preview
                     (node-visible overlay) t)
               (canvas-invalidate canvas))
             (close-dialog ()
               (when adjusting
                 ;; Everything done with the slider is one change, which undo takes back.
                 (setf drawing (drawing-resize drawing adjusting preview)
                       adjusting nil
                       (node-visible overlay) nil)
                 (refresh)))
             (adjust (value)
               (setf preview value)
               (canvas-invalidate canvas)))
      (setf undo (button "Undo" :id :undo
                                :on-click (lambda () (setf drawing (drawing-undo drawing)) (refresh)))
            redo (button "Redo" :id :redo
                                :on-click (lambda () (setf drawing (drawing-redo drawing)) (refresh)))
            canvas (canvas :id :canvas :width 388 :height 260 :paint #'paint
                           :on-press #'press :on-move #'hover :on-exit #'leave)
            diameter-slider (slider :id :diameter :minimum 5 :maximum 200 :value 40 :width 220
                                    :on-change #'adjust)
            overlay (stack ()
                      (scrim :id :scrim :on-press #'close-dialog)
                      (center ()
                        (panel (:all 18 :id :dialog)
                          (column (:gap 14)
                            (label "Adjust diameter" :id :dialog-title)
                            diameter-slider
                            (button "Done" :id :done :on-click #'close-dialog))))))
      (setf (node-visible overlay) nil)
      (refresh)
      (make-retained-variant
       (stack ()
         (padding (:all 16)
           (column (:gap 12)
             (row (:gap 10) undo redo)
             canvas))
         overlay)))))

(register-app :retained :circles #'make-circles :width 420 :height 340)
