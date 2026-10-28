(defpackage #:almighty-bolt-stress/dashboard/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+columns+
                #:+rows+
                #:+tiles+
                #:step-at
                #:index-text
                #:tile-name
                #:tile-value-id
                #:tile-bar-id
                #:make-dashboard
                #:dashboard-indices
                #:advance-dashboard)
  (:import-from #:almighty-bolt-stress/dashboard/paint
                #:paint-chart)
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:panel
                #:column
                #:row
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:progress
                #:canvas)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant
                #:add-ticker)
  (:export #:make-dashboard-app))

(in-package #:almighty-bolt-stress/dashboard/reactive)

;;; The dashboard, reactive: each tile's value is a signal, and the label and the bar of a tile and
;;; the chart are components, so that a change of a value describes those again and nothing else.
;;; A ticker, run at the start of every frame, brings the model to the step the time is in and
;;; writes every signal; it returns true, so that frames keep coming.

(defun make-dashboard-app ()
  (let ((model (make-dashboard))
        (indices (make-array +tiles+))
        (version (make-signal 0)))
    (advance-dashboard model (step-at 0))
    (dotimes (tile +tiles+)
      (setf (svref indices tile) (make-signal (aref (dashboard-indices model) tile))))
    (flet ((tile (tile)
             (panel (:all 8 :flex 1)
               (column (:gap 4)
                 (label (tile-name tile))
                 (component ()
                   (label (index-text (value (svref indices tile)))
                          :id (tile-value-id tile)))
                 (component ()
                   (progress :id (tile-bar-id tile) :width 120
                             :fraction (/ (float (value (svref indices tile)) 1f0)
                                          999f0)))))))
      (let ((variant
              (make-reactive-variant
               (lambda ()
                 (padding (:all 12)
                   (column (:gap 8 :cross-align :stretch)
                     (label "Dashboard" :id :title)
                     (column (:gap 8 :flex 1 :fill t :cross-align :stretch)
                       (loop :for r :below +rows+
                             :collect (row (:gap 8 :flex 1 :fill t :cross-align :stretch)
                                        (loop :for c :below +columns+
                                              :collect (tile (+ (* r +columns+) c))))))
                     (component ()
                       (value version)
                       (canvas :id :chart :width 936 :height 90 :data (value version)
                               :paint (lambda (data canvas context list x y width height)
                                        (declare (ignore data canvas context))
                                        (paint-chart list x y width height model))))))))))
        (add-ticker variant
                    (lambda (now)
                      (when (advance-dashboard model (step-at now))
                        (dotimes (tile +tiles+)
                          (setf (value (svref indices tile)) (aref (dashboard-indices model) tile)))
                        (setf (value version) (1+ (value version))))
                      t))
        variant))))

(register-app :reactive :dashboard 'make-dashboard-app :width 960 :height 680)
