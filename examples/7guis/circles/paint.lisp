(defpackage #:almighty-bolt-7guis/circles/paint
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:model #:almighty-bolt-7guis/circles/model))
  (:export #:paint-circles))

(in-package #:almighty-bolt-7guis/circles/paint)

;;; How the circle drawer's canvas looks: a dark ground and each
;;; circle as an outline with a fill just inside it, grey for the one under the pointer. A
;;; circle being adjusted is drawn with the diameter the slider has now.

(defparameter +canvas-color+ (bolt:pack-color 30 33 46))
(defparameter +outline-color+ (bolt:pack-color 150 200 255))
(defparameter +hot-color+ (bolt:pack-color 110 116 140))

(defun paint-circles (list x y width height drawing hot adjusting preview)
  "Append the commands that draw DRAWING in the area X, Y, WIDTH by HEIGHT. HOT is the id
of the circle that is grey, ADJUSTING the id of the circle whose diameter is PREVIEW."
  (bolt:push-rect list x y width height +canvas-color+)
  (dolist (circle (model:drawing-circles drawing))
    (let* ((d (float (if (eql (model:circle-id circle) adjusting) preview (model:circle-diameter circle)) 1f0))
           (r (/ d 2))
           (left (+ x (model:circle-x circle) (- r)))
           (top (+ y (model:circle-y circle) (- r))))
      (bolt:push-rounded-rect list left top d d r +outline-color+)
      (bolt:push-rounded-rect list (+ left 1.5f0) (+ top 1.5f0) (- d 3f0) (- d 3f0) (- r 1.5f0)
                              (if (eql (model:circle-id circle) hot) +hot-color+ +canvas-color+)))))
