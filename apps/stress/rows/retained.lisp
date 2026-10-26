(defpackage #:almighty-bolt-stress/rows/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:key-event-kind
                #:key-event-logical-key)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-stress/rows/model
                #:+list-width+
                #:+list-height+
                #:+rows-per-line+
                #:clamp-top
                #:row-at
                #:on-bar-p
                #:top-for-thumb
                #:key-move
                #:status-text)
  (:import-from #:almighty-bolt-stress/rows/paint
                #:paint-rows)
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
  (:export #:make-rows-app))

(in-package #:almighty-bolt-stress/rows/retained)

;;; The million-row list, retained: where the list is scrolled to and which row is selected are
;;; variables; the list is one canvas that draws them and handles its own events, and a label above
;;; it says what is selected. After each change the handlers update the label and say that the
;;; canvas must draw again.

(defun make-rows-app ()
  (let ((top 0) (selected nil) (remainder 0f0) (dragging nil)
        (list-canvas nil) (status nil))
    (labels ((refresh ()
               (let ((text (status-text selected)))
                 (unless (string= text (label-text status))
                   (setf (label-text status) text)))
               (canvas-invalidate list-canvas))
             (press (source x y button click-count)
               (declare (ignore source click-count))
               (when (eql button 0)
                 (cond ((on-bar-p x)
                        (setf dragging t
                              top (top-for-thumb y))
                        (refresh))
                       (t (let ((row (row-at x y top)))
                            (when row
                              (setf selected row)
                              (refresh)))))))
             (drag (source x y)
               (declare (ignore source x))
               (when dragging
                 (setf top (top-for-thumb y))
                 (refresh)))
             (release (source x y inside)
               (declare (ignore source x y inside))
               (setf dragging nil))
             (scroll (source dx dy)
               (declare (ignore source dx))
               (incf remainder (- (* dy +rows-per-line+)))
               (let ((rows (truncate remainder)))
                 (decf remainder rows)
                 (let ((new (clamp-top (+ top rows))))
                   (when (/= new top)
                     (setf top new)
                     (refresh)
                     t))))
             (key (source event)
               (declare (ignore source))
               (when (eq (key-event-kind event) :down)
                 (multiple-value-bind (new-selected new-top handled)
                     (key-move (key-event-logical-key event) selected top)
                   (when handled
                     (setf selected new-selected
                           top new-top)
                     (refresh)
                     t))))
             (paint (source context list x y width height)
               (declare (ignore source))
               (paint-rows context list x y width height top selected)))
      (setf status (label (status-text selected) :id :status)
            list-canvas (canvas :id :list :width +list-width+ :height +list-height+ :focusable t
                                :paint #'paint :on-press #'press :on-drag #'drag
                                :on-release #'release :on-key #'key :on-wheel #'scroll))
      (make-retained-variant
       (padding (:all 20)
         (column (:gap 12) status list-canvas))))))

(register-app :retained :rows #'make-rows-app :width 640 :height 470)
