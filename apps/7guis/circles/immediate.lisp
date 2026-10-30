(defpackage #:almighty-bolt-7guis/circles/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/circles/paint
                #:paint-circles)
  (:import-from #:almighty-bolt-7guis/circles/model
                #:make-drawing
                #:drawing-can-undo-p
                #:drawing-can-redo-p
                #:drawing-add
                #:drawing-resize
                #:drawing-undo
                #:drawing-redo
                #:drawing-circle-at
                #:circle-id
                #:circle-diameter)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:button
                #:padding
                #:panel
                #:center
                #:stack
                #:scrim
                #:column
                #:row
                #:slider
                #:canvas)
  (:export #:make-circles))

(in-package #:almighty-bolt-7guis/circles/immediate)

;;; The circle drawer, immediate: the drawing, the circle under the pointer, the circle being
;;; adjusted and its diameter so far are variables. The canvas is given a function that draws
;;; from them and returns the events that happened on it; the dialog is described only while a
;;; circle is being adjusted.

(defun make-circles ()
  (let ((drawing (make-drawing))
        (hot nil)
        (adjusting nil)
        (preview 40))
    (labels ((close-dialog ()
               (when adjusting
                 (setf drawing (drawing-resize drawing adjusting preview)
                       adjusting nil)))
             (hover (x y)
               (let ((circle (drawing-circle-at drawing x y)))
                 (setf hot (and circle (circle-id circle)))))
             (press (x y button)
               (let ((circle (drawing-circle-at drawing x y)))
                 (cond ((and (eql button 0) (null circle))
                        (setf drawing (drawing-add drawing x y))
                        (hover x y))
                       ((and (eql button 1) circle)
                        (setf preview (circle-diameter circle)
                              adjusting (circle-id circle))))))
             (draw (list context width height)
               (declare (ignore context))
               (paint-circles list 0f0 0f0 width height drawing hot adjusting preview)))
      (lambda ()
        (stack ()
          (padding (:all 16)
            (column (:gap 12)
              (row (:gap 10)
                (when (button "Undo" :id :undo :enabled (drawing-can-undo-p drawing))
                  (setf drawing (drawing-undo drawing)))
                (when (button "Redo" :id :redo :enabled (drawing-can-redo-p drawing))
                  (setf drawing (drawing-redo drawing))))
              (dolist (event (canvas 388 260 #'draw :id :canvas))
                (ecase (first event)
                  (:press (press (second event) (third event) (fourth event)))
                  ((:move :drag) (hover (second event) (third event)))
                  (:exit (setf hot nil))
                  ((:release :key :wheel) nil)))))
          (when adjusting
            (stack ()
              (when (scrim :id :scrim)
                (close-dialog))
              (center ()
                (panel (:all 18 :id :dialog)
                  (column (:gap 14)
                    (label "Adjust diameter" :id :dialog-title)
                    (multiple-value-bind (new changed)
                        (slider preview 5 200 :id :diameter :width 220)
                      (when changed
                        (setf preview new)))
                    (when (button "Done" :id :done)
                      (close-dialog))))))))))))

(register-app :immediate :circles 'make-circles :width 420 :height 340)
