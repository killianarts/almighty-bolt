(defpackage #:almighty-bolt-7guis/circles/reactive
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
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value
                #:update)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:panel
                #:center
                #:stack
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:button
                #:scrim
                #:slider
                #:canvas)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-circles))

(in-package #:almighty-bolt-7guis/circles/reactive)

;;; The circle drawer, reactive: the drawing, the circle under the pointer, the circle being
;;; adjusted and its diameter so far are signals. The canvas is described with those as its
;;; data and a function that draws from them; the dialog is a layer that is shown while a
;;; circle is being adjusted.

(defun draw (data canvas context list x y width height)
  (declare (ignore canvas context))
  (destructuring-bind (drawing hot adjusting preview) data
    (paint-circles list x y width height drawing hot adjusting preview)))

(defun make-circles ()
  (let ((drawing (make-signal (make-drawing)))
        (hot (make-signal nil))
        (adjusting (make-signal nil))
        (preview (make-signal 40)))
    (flet ((close-dialog ()
             (when (value adjusting)
               (setf (value drawing) (drawing-resize (value drawing) (value adjusting)
                                                     (value preview))
                     (value adjusting) nil)))
           (hover (x y)
             (let ((circle (drawing-circle-at (value drawing) x y)))
               (setf (value hot) (and circle (circle-id circle)))))
           (press (x y button click-count)
             (declare (ignore click-count))
             (let ((circle (drawing-circle-at (value drawing) x y)))
               (cond ((and (eql button 0) (null circle))
                      (setf (value drawing) (drawing-add (value drawing) x y)
                            (value hot) (let ((c (drawing-circle-at (value drawing) x y)))
                                          (and c (circle-id c)))))
                     ((and (eql button 1) circle)
                      (setf (value preview) (circle-diameter circle)
                            (value adjusting) (circle-id circle)))))))
      (make-reactive-variant
       (lambda ()
         (stack ()
           (padding (:all 16)
             (column (:gap 12)
               (row (:gap 10)
                 (button "Undo" :id :undo :enabled (drawing-can-undo-p (value drawing))
                                :on-click (lambda () (update drawing #'drawing-undo)))
                 (button "Redo" :id :redo :enabled (drawing-can-redo-p (value drawing))
                                :on-click (lambda () (update drawing #'drawing-redo))))
               (canvas :id :canvas :width 388 :height 260 :paint #'draw
                       :data (list (value drawing) (value hot) (value adjusting)
                                   (value preview))
                       :on-press #'press :on-move #'hover
                       :on-exit (lambda () (setf (value hot) nil)))))
           (stack (:visible (and (value adjusting) t))
             (scrim :id :scrim :on-press #'close-dialog)
             (center ()
               (panel (:all 18 :id :dialog)
                 (column (:gap 14)
                   (label "Adjust diameter" :id :dialog-title)
                   (slider :id :diameter :minimum 5 :maximum 200 :width 220
                           :value (value preview)
                           :on-change (lambda (new) (setf (value preview) new)))
                   (button "Done" :id :done :on-click #'close-dialog)))))))))))

(register-app :reactive :circles #'make-circles :width 420 :height 340)
