(defpackage #:almighty-bolt-stress/dashboard/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+columns+
                #:+rows+
                #:+tiles+
                #:step-at
                #:tile-name
                #:tile-name-id
                #:tile-value-id
                #:tile-bar-id
                #:make-dashboard
                #:advance-dashboard
                #:value-text
                #:tile-fraction)
  (:import-from #:almighty-bolt-stress/dashboard/paint
                #:paint-chart)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:padding
                #:panel
                #:column
                #:row
                #:progress
                #:canvas)
  (:import-from #:almighty-bolt/ui/immediate
                #:now
                #:ask-for-frame)
  (:export #:make-dashboard-app))

(in-package #:almighty-bolt-stress/dashboard/immediate)

;;; The dashboard, immediate: the function that describes the screen first brings the model to the
;;; step the time is in (a second build of a frame finds it there already), asks for another frame,
;;; and describes the 36 tiles and the chart.

(defun make-dashboard-app ()
  (let ((model (make-dashboard)))
    (lambda ()
      (advance-dashboard model (step-at (now)))
      (ask-for-frame)
      (padding (:all 12)
        (column (:gap 8 :cross-align :stretch)
          (label "Dashboard" :id :title)
          (column (:gap 8 :flex 1 :fill t :cross-align :stretch)
            (dotimes (r +rows+)
              (row (:gap 8 :flex 1 :fill t :cross-align :stretch)
                (dotimes (c +columns+)
                  (let ((tile (+ (* r +columns+) c)))
                    (panel (:all 8 :flex 1)
                      (column (:gap 4)
                        (label (tile-name tile) :id (tile-name-id tile))
                        (label (value-text model tile) :id (tile-value-id tile))
                        (progress (tile-fraction model tile) :id (tile-bar-id tile)
                                                             :width 120))))))))
          (canvas 936 90 (lambda (list context width height)
                                  (declare (ignore context))
                                  (paint-chart list 0f0 0f0 width height model)) :id :chart))))))

(register-app :immediate :dashboard 'make-dashboard-app :width 960 :height 680)
