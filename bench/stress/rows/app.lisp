(defpackage #:almighty-bolt-stress/rows/app
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-stress/rows/model)
                    (#:paint #:almighty-bolt-stress/rows/paint))
  (:export #:make-rows-app))

(in-package #:almighty-bolt-stress/rows/app)

;;; The million-row list, immediate: where the list is scrolled to and which row is selected are
;;; variables. The canvas is given a function that draws them every frame and returns what happened
;;; to it, which is handled where it is described.

(defun make-rows-app ()
  (bolt:state-let ((top 0) (selected nil) (remainder 0f0) (dragging nil))
    (labels ((draw (list context width height)
               (paint:paint-rows context list 0f0 0f0 width height top selected))
             (handle (event)
               (ecase (first event)
                 (:press
                  (destructuring-bind (x y button click-count) (rest event)
                    (declare (ignore click-count))
                    (when (eql button 0)
                      (cond ((model:on-bar-p x)
                             (setf dragging t
                                   top (model:top-for-thumb y)))
                            (t (let ((row (model:row-at x y top)))
                                 (when row
                                   (setf selected row))))))))
                 (:drag
                  (when dragging
                    (setf top (model:top-for-thumb (third event)))))
                 (:release (setf dragging nil))
                 (:wheel
                  (incf remainder (- (* (third event) model:+rows-per-line+)))
                  (let ((rows (truncate remainder)))
                    (decf remainder rows)
                    (setf top (model:clamp-top (+ top rows)))))
                 (:key
                  (let ((key-event (second event)))
                    (when (eq (bolt:key-event-kind key-event) :down)
                      (multiple-value-bind (new-selected new-top handled)
                          (model:key-move (bolt:key-event-logical-key key-event) selected top)
                        (when handled
                          (setf selected new-selected
                                top new-top))))))
                 ((:move :exit) nil))))
      (lambda ()
        (bolt:padding (:all 20)
          (bolt:column (:gap 12)
            (bolt:label (model:status-text selected) :id :status)
            (dolist (event (bolt:canvas model:+list-width+ model:+list-height+ #'draw :id :list :focusable t))
              (handle event))))))))

(registry:register-app :rows 'make-rows-app :width 640 :height 470)
