(defpackage #:almighty-bolt-7guis/t/acceptance/circles
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)))

(in-package #:almighty-bolt-7guis/t/acceptance/circles)

;;; The circle drawer: a canvas where a click on empty ground makes a circle of diameter 40
;;; centred there, the circle under the pointer shown in grey, Undo and Redo, and a dialog
;;; with a slider for the diameter of a circle, opened by a click of the secondary button
;;; on it, whose changes are one step of undo. Controls: :canvas, :undo, :redo, and in the
;;; dialog :dialog-title, :diameter (the slider), :done and :scrim.

(defun circles (driver)
  "The circles drawn on the canvas, each as (centre-x centre-y diameter) in canvas
coordinates, from the pairs of rounded rectangles that draw them."
  (destructuring-bind (cx cy cw ch) (d:rectangle driver :canvas)
    (declare (ignore cw ch))
    ;; A circle is an outline and, just inside it, a fill: two squares, one 3 points less
    ;; across than the other and with the same middle. Anything else drawn over the
    ;; canvas (the dialog) is not that, and is passed over.
    (let ((shapes (remove :rect (d:shapes-in driver :canvas) :key #'first))
          (found '()))
      (loop :while shapes
            :do (destructuring-bind (kind x y w h color) (first shapes)
                  (declare (ignore kind color))
                  (let ((next (second shapes)))
                    (if (and next (= w h) (= (fourth next) (- w 3)) (= (fifth next) (- h 3))
                             (= (second next) (+ x 1.5)) (= (third next) (+ y 1.5)))
                        (progn (push (list (round (- (+ x (/ w 2)) cx))
                                           (round (- (+ y (/ w 2)) cy))
                                           (round w))
                                     found)
                               (setf shapes (cddr shapes)))
                        (pop shapes)))))
      (nreverse found))))

(defun draw-at (driver x y)
  "Click the canvas at X, Y points from its top left."
  (destructuring-bind (cx cy cw ch) (d:rectangle driver :canvas)
    (declare (ignore cx cy))
    (d:click-at driver :canvas (/ x cw) (/ y ch))))

(defun hover-at (driver x y)
  (destructuring-bind (cx cy cw ch) (d:rectangle driver :canvas)
    (declare (ignore cx cy))
    (d:move-to driver :canvas (/ x cw) (/ y ch))))

(defun right-click-at (driver x y)
  (destructuring-bind (cx cy cw ch) (d:rectangle driver :canvas)
    (declare (ignore cx cy))
    (d:right-click driver :canvas (/ x cw) (/ y ch))))

(defun dialog-open-p (driver)
  (and (search "Adjust diameter" (d:screen-text driver)) t))

(d:define-acceptance the-canvas-starts-empty :circles
  (assert-equal '() (circles driver)))

(d:define-acceptance clicking-on-empty-ground-draws-a-circle-of-forty-there :circles
  (draw-at driver 100 80)
  (assert-equal '((100 80 40)) (circles driver))
  (draw-at driver 250 150)
  (assert-equal '((100 80 40) (250 150 40)) (circles driver)))

(d:define-acceptance clicking-on-a-circle-does-not-draw-another :circles
  (draw-at driver 100 80)
  (draw-at driver 110 85)
  (assert-equal '((100 80 40)) (circles driver)))

(d:define-acceptance the-circle-under-the-pointer-is-grey :circles
  (draw-at driver 100 80)
  (hover-at driver 400 250)
  (let ((away (d:shapes-in driver :canvas)))
    (hover-at driver 100 80)
    (let ((over (d:shapes-in driver :canvas)))
      (assert-false (equal away over))
      (hover-at driver 400 250)
      (assert-equal away (d:shapes-in driver :canvas)))))

(d:define-acceptance undo-and-redo-go-back-and-forward-through-the-drawing :circles
  (draw-at driver 50 50)
  (draw-at driver 150 150)
  (draw-at driver 250 100)
  (d:click driver :undo)
  (assert-equal '((50 50 40) (150 150 40)) (circles driver))
  (d:click driver :undo)
  (d:click driver :undo)
  (assert-equal '() (circles driver))
  (d:click driver :undo)                           ; nothing more to undo
  (d:click driver :redo)
  (assert-equal '((50 50 40)) (circles driver))
  (d:click driver :redo)
  (d:click driver :redo)
  (assert-equal '((50 50 40) (150 150 40) (250 100 40)) (circles driver)))

(d:define-acceptance drawing-after-an-undo-ends-the-redo :circles
  (draw-at driver 50 50)
  (draw-at driver 150 150)
  (d:click driver :undo)
  (draw-at driver 300 200)
  (d:click driver :redo)
  (assert-equal '((50 50 40) (300 200 40)) (circles driver)))

(d:define-acceptance the-secondary-button-on-a-circle-opens-the-dialog :circles
  (draw-at driver 100 80)
  (right-click-at driver 100 80)
  (assert-true (dialog-open-p driver)))

(d:define-acceptance the-secondary-button-on-empty-ground-opens-nothing :circles
  (draw-at driver 100 80)
  (right-click-at driver 300 200)
  (assert-false (dialog-open-p driver)))

(d:define-acceptance the-slider-sets-the-diameter-live-and-done-keeps-it :circles
  (draw-at driver 100 80)
  (right-click-at driver 100 80)
  (d:press-at driver :diameter 0.99 0.5)
  (d:release-at driver :diameter 0.99 0.5)
  (assert-equal '((100 80 200)) (circles driver))
  (d:click driver :done)
  (assert-false (dialog-open-p driver))
  (assert-equal '((100 80 200)) (circles driver)))

(d:define-acceptance everything-done-with-the-slider-is-one-step-of-undo :circles
  (draw-at driver 100 80)
  (right-click-at driver 100 80)
  (d:press-at driver :diameter 0.99 0.5)
  (d:drag-to driver :diameter 0.5 0.5)
  (d:drag-to driver :diameter 0.0 0.5)
  (d:release-at driver :diameter 0.0 0.5)
  (d:click driver :done)
  (assert-equal '((100 80 5)) (circles driver))
  (d:click driver :undo)
  (assert-equal '((100 80 40)) (circles driver))
  (d:click driver :redo)
  (assert-equal '((100 80 5)) (circles driver)))

(d:define-acceptance clicking-away-from-the-dialog-closes-it-and-keeps-the-change :circles
  (draw-at driver 100 80)
  (right-click-at driver 100 80)
  (d:press-at driver :diameter 0.99 0.5)
  (d:release-at driver :diameter 0.99 0.5)
  (d:click-at driver :scrim 0.02 0.98)
  (assert-false (dialog-open-p driver))
  (assert-equal '((100 80 200)) (circles driver)))

(d:define-acceptance the-canvas-takes-no-clicks-while-the-dialog-is-open :circles
  (draw-at driver 100 80)
  (right-click-at driver 100 80)
  (d:click-at driver :scrim 0.9 0.9)
  (assert-false (dialog-open-p driver))
  (right-click-at driver 100 80)
  ;; With the dialog open, a click where the canvas is draws nothing.
  (d:click-at driver :scrim 0.9 0.1)
  (assert-equal '((100 80 40)) (circles driver)))

(d:define-acceptance a-dialog-that-changed-nothing-leaves-nothing-to-undo :circles
  (draw-at driver 100 80)
  (d:click driver :undo)
  (draw-at driver 100 80)
  (right-click-at driver 100 80)
  (d:click driver :done)
  (d:click driver :undo)
  (assert-equal '() (circles driver)))
