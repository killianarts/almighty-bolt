(defpackage #:almighty-bolt-7guis/circles/mcclim
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/circles/model
                #:make-drawing
                #:drawing-circles
                #:drawing-can-undo-p
                #:drawing-can-redo-p
                #:drawing-add
                #:drawing-resize
                #:drawing-undo
                #:drawing-redo
                #:drawing-circle-at
                #:circle-id
                #:circle-x
                #:circle-y
                #:circle-diameter)
  (:import-from #:clim
                #:define-application-frame
                #:make-application-frame
                #:run-frame-top-level
                #:define-presentation-type
                #:define-presentation-to-command-translator
                #:blank-area
                #:updating-output
                #:with-output-as-presentation
                #:draw-circle*
                #:accepting-values
                #:accept
                #:command-enabled
                #:*application-frame*
                #:+gray20+
                #:+light-blue+)
  (:export #:make-circles-frame
           #:run-circles))

(in-package #:almighty-bolt-7guis/circles/mcclim)

;;; The circle drawer, McCLIM: each circle is presented (the type CIRCLE) in a pane that redisplays
;;; only the circles that changed. A click on empty ground is a translator to COM-DRAW, which is
;;; given the place of the click; a click of the secondary button on a circle is a translator to
;;; COM-ADJUST, which asks for the diameter in a dialog of its own, a slider, since that is what
;;; ACCEPT makes of an integer with bounds. Undo and redo are commands in the frame's menu bar, and
;;; the model's.

(define-presentation-type circle ())

(define-application-frame circles-frame ()
  ((drawing :initform (make-drawing)))
  (:panes (canvas :application :display-function 'show-circles :incremental-redisplay t
                  :scroll-bars nil :width 388 :height 260 :background +gray20+))
  (:layouts (default canvas))
  (:default-initargs :width 420 :height 340))

(defun show-circles (frame stream)
  (with-slots (drawing) frame
    (setf (command-enabled 'com-undo frame) (drawing-can-undo-p drawing)
          (command-enabled 'com-redo frame) (drawing-can-redo-p drawing))
    (dolist (circle (drawing-circles drawing))
      (updating-output (stream :unique-id (circle-id circle)
                               :cache-value (list (circle-x circle) (circle-y circle)
                                                  (circle-diameter circle)))
        (with-output-as-presentation (stream circle 'circle)
          (draw-circle* stream (circle-x circle) (circle-y circle) (/ (circle-diameter circle) 2)
                        :filled nil :ink +light-blue+))))))

(define-circles-frame-command (com-draw) ((x 'integer) (y 'integer))
  (with-slots (drawing) *application-frame*
    (setf drawing (drawing-add drawing x y))))

(defun ask-diameter (circle)
  "The diameter that the person gives CIRCLE in a dialog of its own, or NIL if they cancel it."
  (let ((stream *standard-input*))
    (accepting-values (stream :own-window t :label "Adjust diameter")
      (accept '(integer 5 200) :prompt "Diameter" :default (circle-diameter circle)
                               :stream stream))))

(define-circles-frame-command (com-adjust) ((circle 'circle))
  (with-slots (drawing) *application-frame*
    (let ((diameter (ask-diameter circle)))
      (when diameter
        (setf drawing (drawing-resize drawing (circle-id circle) diameter))))))

(define-circles-frame-command (com-undo :menu t) ()
  (with-slots (drawing) *application-frame*
    (setf drawing (drawing-undo drawing))))

(define-circles-frame-command (com-redo :menu t) ()
  (with-slots (drawing) *application-frame*
    (setf drawing (drawing-redo drawing))))

(define-presentation-to-command-translator draw-circle
    (blank-area com-draw circles-frame :gesture :select :menu nil
                :tester ((object x y)
                         (declare (ignore object))
                         (not (drawing-circle-at (slot-value *application-frame* 'drawing) x y))))
    (object x y)
  (declare (ignore object))
  (list (round x) (round y)))

(define-presentation-to-command-translator adjust-circle
    (circle com-adjust circles-frame :gesture :menu :menu nil)
    (object)
  (list object))

(defun make-circles-frame (&rest initargs)
  (apply #'make-application-frame 'circles-frame initargs))

(defun run-circles (&rest initargs)
  (run-frame-top-level (apply #'make-circles-frame initargs)))
