(defpackage #:almighty-bolt-stress/dashboard/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:model #:almighty-bolt-stress/dashboard/model)
                    (#:paint #:almighty-bolt-stress/dashboard/paint)
                    (#:bolt #:almighty-bolt))
  (:export #:make-dashboard-app))

(in-package #:almighty-bolt-stress/dashboard/app)

;;; The dashboard, immediate: the function that describes the screen first brings the model to the
;;; step the time is in (a second build of a frame finds it there already), asks for another frame,
;;; and describes the 36 tiles and the chart.

(defun make-dashboard-app ()
  (bolt:state-let ((model (model:make-dashboard)))
    (lambda ()
      (model:advance-dashboard model (model:step-at (bolt:now)))
      (bolt:ask-for-frame)
      (bolt:padding (:all 12)
        (bolt:column (:gap 8 :cross-align :stretch)
          (bolt:label "Dashboard" :id :title)
          (bolt:column (:gap 8 :flex 1 :fill t :cross-align :stretch)
            (dotimes (r model:+rows+)
              (bolt:row (:gap 8 :flex 1 :fill t :cross-align :stretch)
                (dotimes (c model:+columns+)
                  (let ((tile (+ (* r model:+columns+) c)))
                    (bolt:panel (:all 8 :flex 1)
                      (bolt:column (:gap 4)
                        (bolt:label (model:tile-name tile) :id (model:tile-name-id tile))
                        (bolt:label (model:value-text model tile) :id (model:tile-value-id tile))
                        (bolt:progress (model:tile-fraction model tile) :id (model:tile-bar-id tile)
                                                                        :width 120))))))))
          (bolt:canvas 936 90 (lambda (list context width height)
                                       (declare (ignore context))
                                       (paint:paint-chart list 0f0 0f0 width height model)) :id :chart))))))

(registry:register-app :dashboard 'make-dashboard-app :width 960 :height 680)
