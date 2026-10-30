(defpackage #:almighty-bolt-stress/rows/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
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
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:padding
                #:column
                #:canvas)
  (:export #:make-rows-app))

(in-package #:almighty-bolt-stress/rows/immediate)

;;; The million-row list, immediate: where the list is scrolled to and which row is selected are
;;; variables. The canvas is given a function that draws them every frame and returns what happened
;;; to it, which is handled where it is described.

(defun make-rows-app ()
  (let ((top 0) (selected nil) (remainder 0f0) (dragging nil))
    (labels ((draw (list context width height)
               (paint-rows context list 0f0 0f0 width height top selected))
             (handle (event)
               (ecase (first event)
                 (:press
                  (destructuring-bind (x y button click-count) (rest event)
                    (declare (ignore click-count))
                    (when (eql button 0)
                      (cond ((on-bar-p x)
                             (setf dragging t
                                   top (top-for-thumb y)))
                            (t (let ((row (row-at x y top)))
                                 (when row
                                   (setf selected row))))))))
                 (:drag
                  (when dragging
                    (setf top (top-for-thumb (third event)))))
                 (:release (setf dragging nil))
                 (:wheel
                  (incf remainder (- (* (third event) +rows-per-line+)))
                  (let ((rows (truncate remainder)))
                    (decf remainder rows)
                    (setf top (clamp-top (+ top rows)))))
                 (:key
                  (let ((key-event (second event)))
                    (when (eq (key-event-kind key-event) :down)
                      (multiple-value-bind (new-selected new-top handled)
                          (key-move (key-event-logical-key key-event) selected top)
                        (when handled
                          (setf selected new-selected
                                top new-top))))))
                 ((:move :exit) nil))))
      (lambda ()
        (padding (:all 20)
          (column (:gap 12)
            (label (status-text selected) :id :status)
            (dolist (event (canvas +list-width+ +list-height+ #'draw :id :list :focusable t))
              (handle event))))))))

(register-app :immediate :rows 'make-rows-app :width 640 :height 470)
