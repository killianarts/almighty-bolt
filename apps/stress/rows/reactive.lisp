(defpackage #:almighty-bolt-stress/rows/reactive
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
  (:export #:make-rows-app))

(in-package #:almighty-bolt-stress/rows/reactive)

;;; The million-row list, reactive: where the list is scrolled to and which row is selected are
;;; signals. The status label and the list are components, so that a scroll describes the canvas
;;; again and a selection describes the label and the canvas, and nothing else.

(defun draw (data canvas context list x y width height)
  (declare (ignore canvas))
  (destructuring-bind (top selected) data
    (paint-rows context list x y width height top selected)))

(defun make-rows-app ()
  (let ((top (make-signal 0))
        (selected (make-signal nil))
        (remainder 0f0)
        (dragging nil))
    (labels ((press (context x y button click-count)
               (declare (ignore context click-count))
               (when (eql button 0)
                 (cond ((on-bar-p x)
                        (setf dragging t
                              (value top) (top-for-thumb y)))
                       (t (let ((row (row-at x y (value top))))
                            (when row
                              (setf (value selected) row)))))))
             (drag (context x y)
               (declare (ignore context x))
               (when dragging
                 (setf (value top) (top-for-thumb y))))
             (release (context x y inside)
               (declare (ignore context x y inside))
               (setf dragging nil))
             (scroll (context dx dy)
               (declare (ignore context dx))
               (incf remainder (- (* dy +rows-per-line+)))
               (let ((rows (truncate remainder)))
                 (decf remainder rows)
                 (let ((new (clamp-top (+ (value top) rows))))
                   (when (/= new (value top))
                     (setf (value top) new)
                     t))))
             (key (context event)
               (declare (ignore context))
               (when (eq (key-event-kind event) :down)
                 (multiple-value-bind (new-selected new-top handled)
                     (key-move (key-event-logical-key event) (value selected) (value top))
                   (when handled
                     (setf (value selected) new-selected
                           (value top) new-top)
                     t)))))
      (make-reactive-variant
       (lambda ()
         (padding (:all 20)
           (column (:gap 12)
             (component () (label (status-text (value selected)) :id :status))
             (component ()
               (canvas :id :list :width +list-width+ :height +list-height+
                       :focusable t :paint #'draw
                       :data (list (value top) (value selected))
                       :on-press #'press :on-drag #'drag :on-release #'release
                       :on-key #'key :on-wheel #'scroll)))))))))

(register-app :reactive :rows #'make-rows-app :width 640 :height 470)
