(defpackage #:almighty-bolt-stress/dashboard/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+columns+
                #:+rows+
                #:+tiles+
                #:step-at
                #:tile-name
                #:tile-value-id
                #:tile-bar-id
                #:make-dashboard
                #:advance-dashboard
                #:value-text
                #:tile-fraction)
  (:import-from #:almighty-bolt-stress/dashboard/paint
                #:paint-chart)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:panel
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:progress
                #:progress-fraction)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:canvas
                #:canvas-invalidate)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant
                #:add-ticker)
  (:export #:make-dashboard-app))

(in-package #:almighty-bolt-stress/dashboard/retained)

;;; The dashboard, retained: 36 tiles built once, each with a label for its value and a bar, and
;;; a chart that draws the model's history. A ticker, run at the start of every frame, brings the
;;; model to the step the time is in and writes every tile's value into its label and bar, and says
;;; that the chart must be drawn again. It returns true, so that frames keep coming.

(defun make-dashboard-app ()
  (let ((model (make-dashboard))
        (values (make-array +tiles+))
        (bars (make-array +tiles+))
        (chart nil))
    (setf chart (canvas :id :chart :width 936 :height 90
                        :paint (lambda (canvas context list x y width height)
                                 (declare (ignore canvas context))
                                 (paint-chart list x y width height model))))
    (advance-dashboard model (step-at 0))
    (dotimes (tile +tiles+)
      (setf (svref values tile) (label (value-text model tile) :id (tile-value-id tile))
            (svref bars tile) (progress :id (tile-bar-id tile) :width 120
                                        :fraction (tile-fraction model tile))))
    (flet ((tile (tile)
             (panel (:all 8 :flex 1)
               (column (:gap 4)
                 (label (tile-name tile)) (svref values tile) (svref bars tile))))
           (tick (now)
             (when (advance-dashboard model (step-at now))
               (dotimes (tile +tiles+)
                 (setf (label-text (svref values tile)) (value-text model tile)
                       (progress-fraction (svref bars tile)) (tile-fraction model tile)))
               (canvas-invalidate chart))
             t))
      (let ((variant
              (make-retained-variant
               (padding (:all 12)
                 (column (:gap 8 :cross-align :stretch)
                   (label "Dashboard" :id :title)
                   (column (:gap 8 :flex 1 :fill t :cross-align :stretch)
                     (loop :for r :below +rows+
                           :collect (row (:gap 8 :flex 1 :fill t :cross-align :stretch)
                                      (loop :for c :below +columns+
                                            :collect (tile (+ (* r +columns+) c))))))
                   chart)))))
        (add-ticker variant #'tick)
        variant))))

(register-app :retained :dashboard #'make-dashboard-app :width 960 :height 680)
