(defpackage #:almighty-bolt-stress/dashboard/reactive-signals
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+columns+
                #:+rows+
                #:step-at
                #:index-text
                #:tile-name
                #:tile-value-id
                #:tile-bar-id)
  (:import-from #:almighty-bolt-stress/dashboard/signal-model
                #:make-signal-dashboard
                #:signal-dashboard-model
                #:current-step
                #:tile-index
                #:advance-signal-dashboard)
  (:import-from #:almighty-bolt-stress/dashboard/paint
                #:paint-chart)
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

(in-package #:almighty-bolt-stress/dashboard/reactive-signals)

;;; The dashboard, reactive, with a model made of signals (signal-model.lisp): the ticker writes
;;; the step, and the label and the bar of a tile read the tile's value, which is worked out from it.
;;; This replaces the registration of reactive.lisp when it is loaded after it (the system
;;; almighty-bolt-stress/reactive-signals), so that the same driver runs both.

(defun make-dashboard-app ()
  (let ((dashboard (make-signal-dashboard)))
    (flet ((tile (tile)
             (panel (:all 8 :flex 1)
               (column (:gap 4)
                 (label (tile-name tile))
                 (component ()
                   (label (index-text (tile-index dashboard tile)) :id (tile-value-id tile)))
                 (component ()
                   (progress :id (tile-bar-id tile) :width 120
                             :fraction (/ (float (tile-index dashboard tile) 1f0) 999f0)))))))
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
                       (canvas :id :chart :width 936 :height 90 :data (current-step dashboard)
                               :paint (lambda (data canvas context list x y width height)
                                        (declare (ignore data canvas context))
                                        (paint-chart list x y width height
                                                     (signal-dashboard-model dashboard)))))))))))
        (add-ticker variant
                    (lambda (now)
                      (advance-signal-dashboard dashboard (step-at now))
                      t))
        variant))))

(register-app :reactive :dashboard #'make-dashboard-app :width 960 :height 680)
