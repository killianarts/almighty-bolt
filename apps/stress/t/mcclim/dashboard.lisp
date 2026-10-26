(defpackage #:almighty-bolt-stress/t/mcclim/dashboard
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-stress/dashboard/model)
                    (#:d #:almighty-bolt-stress/dashboard/mcclim)
                    (#:g #:almighty-bolt-stress/dashboard/mcclim-gadgets)
                    (#:s #:almighty-bolt-stress/t/mcclim/support)
                    (#:clim #:clim)
                    (#:clime #:clim-extensions)
                    (#:bt #:bordeaux-threads)))

(in-package #:almighty-bolt-stress/t/mcclim/dashboard)

;;; The dashboard in McCLIM: what its panes show, that they show the step that the model is at,
;;; and that the ticker brings the model on.

(defun board (frame) (clim:find-pane-named frame 'almighty-bolt-stress/dashboard/mcclim::board))
(defun chart (frame) (clim:find-pane-named frame 'almighty-bolt-stress/dashboard/mcclim::chart))

(defun shows-every-tile-p (strings model)
  (dotimes (tile m:+tiles+ t)
    (unless (and (member (m:tile-name tile) strings :test #'string=)
                 (member (m:value-text model tile) strings :test #'string=))
      (return nil))))

(define-test the-board-shows-the-name-and-the-value-of-every-tile ()
  (s:with-running-frame (frame #'d:make-dashboard-frame)
    (assert-true (s:await (lambda ()
                            (shows-every-tile-p (s:pane-strings (board frame))
                                                (d:frame-model frame)))))))

(define-test the-board-shows-the-step-that-the-model-is-at-after-a-redisplay ()
  (s:with-running-frame (frame #'d:make-dashboard-frame)
    (let ((model (d:frame-model frame)))
      (m:advance-dashboard model 77)
      (clim:redisplay-frame-panes frame :force-p t)
      (assert-true (shows-every-tile-p (s:pane-strings (board frame)) model))
      (assert-equal (m:value-text model 5)
                    (find (m:value-text model 5) (s:pane-strings (board frame)) :test #'string=)))))

(define-test the-chart-draws-a-ground-and-three-series-of-bars ()
  (s:with-running-frame (frame #'d:make-dashboard-frame)
    ;; The ground, and 240 bars in each of three series; the records that hold them are more.
    (assert-true (>= (s:count-records (chart frame)) (+ 1 (* m:+series+ m:+samples+))))))

(define-test the-ticker-brings-the-model-on-until-the-frame-is-gone ()
  (s:with-running-frame (frame #'d:make-dashboard-frame)
    (let ((thread (bt:make-thread (lambda () (d:animate frame)) :name "ticker under test"))
          (before (m:dashboard-step (d:frame-model frame))))
      ;; The ticker is gone when the frame is: it stops by itself once the frame is disabled.
      (sleep 0.5)
      (assert-true (> (m:dashboard-step (d:frame-model frame)) before))
      (assert-true (bt:thread-alive-p thread)))))

(define-test the-gadget-version-shows-the-same-values-on-its-gadgets ()
  (s:with-running-frame (frame #'g:make-gadget-dashboard-frame)
    (let ((model (g:frame-model frame)))
      (m:advance-dashboard model 42)
      (g:update-gadgets frame)
      (dotimes (tile m:+tiles+)
        (assert-equal (m:value-text model tile)
                      (clime:label-pane-label (svref (g:tile-values frame) tile)))
        (assert-true (< (abs (- (m:tile-fraction model tile)
                                (clim:gadget-value (svref (g:tile-bars frame) tile))))
                        1e-4))))))
