(defpackage #:almighty-bolt-stress/dashboard/mcclim-gadgets
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread)
  (:import-from #:almighty-bolt-stress/dashboard/model
                #:+columns+
                #:+rows+
                #:+tiles+
                #:step-at
                #:tile-name
                #:make-dashboard
                #:advance-dashboard
                #:value-text
                #:tile-fraction)
  (:import-from #:almighty-bolt-stress/dashboard/mcclim
                #:display-chart
                #:frame-model
                #:now-ns)
  (:import-from #:clim
                #:*application-frame*
                #:define-application-frame
                #:make-application-frame
                #:run-frame-top-level
                #:redisplay-frame-pane
                #:frame-state
                #:vertically
                #:labelling
                #:outlining
                #:make-pane
                #:hbox-pane
                #:vbox-pane
                #:slider
                #:gadget-value)
  (:import-from #:clime
                #:label-pane-label)
  (:export #:make-gadget-dashboard-frame
           #:run-gadget-dashboard
           #:frame-model
           #:tile-values
           #:tile-bars
           #:update-gadgets))

(in-package #:almighty-bolt-stress/dashboard/mcclim-gadgets)

;;; The dashboard, McCLIM again but with McCLIM's gadgets for the tiles: a label with the name of
;;; the tile around another label with its value, around a SLIDER that cannot be used, for the
;;; bar, because there is no progress gadget. The chart is the same pane as in the display-function
;;; version. This is the retained way of writing it with the same library: what each step does
;;; is to set the 72 gadgets.

(define-application-frame gadget-dashboard-frame ()
  ((model :initform (make-dashboard) :reader frame-model)
   (values :initform (make-array +tiles+) :reader tile-values)
   (bars :initform (make-array +tiles+) :reader tile-bars))
  (:panes (chart :application :display-function 'display-chart :scroll-bars nil
                             :width 936 :height 90))
  (:layouts (default (vertically (:spacing 8)
                       (labelling (:label "Dashboard") (make-tiles *application-frame*))
                       chart)))
  (:default-initargs :width 960 :height 680))

(defun make-tile (frame tile)
  (let* ((model (frame-model frame))
         (bar (make-pane 'slider :orientation :horizontal :min-value 0 :max-value 1
                                 :value (tile-fraction model tile) :show-value-p nil
                                 :active nil :width 120))
         (value (labelling (:label (value-text model tile)) bar)))
    (setf (svref (tile-values frame) tile) value
          (svref (tile-bars frame) tile) bar)
    (outlining () (labelling (:label (tile-name tile)) value))))

(defun make-tiles (frame)
  (advance-dashboard (frame-model frame) (step-at 0))
  (make-pane 'vbox-pane
             :contents (loop :for row :below +rows+
                             :collect (make-pane 'hbox-pane
                                                 :contents (loop :for column :below +columns+
                                                                 :collect (make-tile
                                                                           frame
                                                                           (+ (* row +columns+)
                                                                              column)))))))

(defun update-gadgets (frame)
  (let ((model (frame-model frame)))
    (dotimes (tile +tiles+)
      (setf (label-pane-label (svref (tile-values frame) tile)) (value-text model tile)
            (gadget-value (svref (tile-bars frame) tile)) (tile-fraction model tile)))
    (redisplay-frame-pane frame 'chart :force-p t)))

(defun animate (frame)
  (loop :until (eq (frame-state frame) :disabled)
        :do (when (and (eq (frame-state frame) :enabled)
                       (advance-dashboard (frame-model frame) (step-at (now-ns))))
              (update-gadgets frame))
            (sleep 1/120)))

(defun make-gadget-dashboard-frame (&rest initargs)
  (apply #'make-application-frame 'gadget-dashboard-frame initargs))

(defun run-gadget-dashboard (&rest initargs)
  (let* ((frame (apply #'make-gadget-dashboard-frame initargs))
         (ticker (make-thread (lambda () (animate frame)) :name "dashboard ticker")))
    (run-frame-top-level frame)
    (join-thread ticker)))
