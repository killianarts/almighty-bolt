(defpackage #:almighty-bolt-stress/dashboard/mcclim
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+columns+
                #:+rows+
                #:+samples+
                #:+series+
                #:step-at
                #:tile-name
                #:make-dashboard
                #:advance-dashboard
                #:dashboard-head
                #:dashboard-history
                #:value-text
                #:tile-fraction)
  (:import-from #:clim
                #:define-application-frame
                #:make-application-frame
                #:run-frame-top-level
                #:redisplay-frame-panes
                #:frame-state
                #:vertically
                #:labelling
                #:formatting-table
                #:formatting-row
                #:formatting-cell
                #:surrounding-output-with-border
                #:with-room-for-graphics
                #:draw-rectangle*
                #:make-rgb-color)
  (:export #:make-dashboard-frame
           #:run-dashboard
           #:frame-model
           #:display-chart
           #:now-ns
           #:animate))

(in-package #:almighty-bolt-stress/dashboard/mcclim)

;;; The dashboard, McCLIM: the board and the chart are two panes, each with a display function that
;;; McCLIM runs again when it is asked to redisplay, and that makes the output records of what it
;;; draws. McCLIM has no frame clock and no progress gadget, so a thread asks for a redisplay of
;;; both panes whenever the step the time is in has changed, and a bar is drawn with rectangles.
;;; Every tile changes in every step, so there is nothing for UPDATING-OUTPUT to keep.

(defparameter +track+ (make-rgb-color 0.2 0.22 0.3))
(defparameter +fill+ (make-rgb-color 0.47 0.86 0.63))
(defparameter +ground+ (make-rgb-color 0.12 0.13 0.18))
(defparameter +colors+ (list (make-rgb-color 0.47 0.86 0.63) (make-rgb-color 0.59 0.78 1.0)
                             (make-rgb-color 0.94 0.71 0.43)))

(defun display-board (frame stream)
  (let ((model (frame-model frame)))
    (formatting-table (stream :x-spacing 8 :y-spacing 8)
      (dotimes (row +rows+)
        (formatting-row (stream)
          (dotimes (column +columns+)
            (let ((tile (+ (* row +columns+) column)))
              (formatting-cell (stream)
                (surrounding-output-with-border (stream)
                  (format stream "~a~%~a~%" (tile-name tile) (value-text model tile))
                  (with-room-for-graphics (stream :first-quadrant nil)
                    (draw-rectangle* stream 0 0 120 8 :ink +track+)
                    (draw-rectangle* stream 0 0 (* 120 (tile-fraction model tile)) 8
                                     :ink +fill+)))))))))))

(defun display-chart (frame stream)
  (let* ((model (frame-model frame))
         (history (dashboard-history model))
         (head (dashboard-head model))
         (column (/ 936 +samples+)))
    (draw-rectangle* stream 0 0 936 90 :ink +ground+)
    (loop :for series :below +series+
          :for color :in +colors+
          :do (dotimes (slot +samples+)
                (let ((y (- 86 (* 82 (aref history (+ (* series +samples+)
                                                      (mod (+ head slot) +samples+)))))))
                  (draw-rectangle* stream (* slot column) y (* (1+ slot) column) (+ y 2)
                                   :ink color))))))

(define-application-frame dashboard-frame ()
  ((model :initform (make-dashboard) :reader frame-model))
  (:panes (board :application :display-function 'display-board :scroll-bars nil
                             :width 936 :height 470)
          (chart :application :display-function 'display-chart :scroll-bars nil
                             :width 936 :height 90))
  (:layouts (default (vertically (:spacing 8)
                       (labelling (:label "Dashboard") board)
                       chart)))
  (:default-initargs :width 960 :height 680))

(defun make-dashboard-frame (&rest initargs)
  (let ((frame (apply #'make-application-frame 'dashboard-frame initargs)))
    (advance-dashboard (frame-model frame) (step-at 0))
    frame))

(defun now-ns ()
  (* (get-internal-real-time) (floor 1000000000 internal-time-units-per-second)))

(defun animate (frame)
  "Redisplay FRAME's panes whenever the time has come to a new step, until the frame is gone."
  (loop :until (eq (frame-state frame) :disabled)
        :do (when (and (eq (frame-state frame) :enabled)
                       (advance-dashboard (frame-model frame) (step-at (now-ns))))
              (redisplay-frame-panes frame :force-p t))
            (sleep 1/120)))

(defun run-dashboard (&rest initargs)
  (let* ((frame (apply #'make-dashboard-frame initargs))
         (ticker (make-thread (lambda () (animate frame)) :name "dashboard ticker")))
    (run-frame-top-level frame)
    (join-thread ticker)))
