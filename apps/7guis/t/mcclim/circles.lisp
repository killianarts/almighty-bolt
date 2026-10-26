(defpackage #:almighty-bolt-7guis/t/mcclim/circles
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:c #:almighty-bolt-7guis/circles/mcclim)
                    (#:m #:almighty-bolt-7guis/circles/model)
                    (#:s #:almighty-bolt-7guis/t/mcclim/support)
                    (#:base #:almighty-bolt-stress/t/mcclim/support)
                    (#:clim #:clim)))

(in-package #:almighty-bolt-7guis/t/mcclim/circles)

;;; The circle drawer in McCLIM, by the acceptance tests of the other variants
;;; (apps/7guis/t/acceptance/ circles.lisp). The clicks are pointer events through the frame's loop
;;; (the circles are drawn with coordinates, so the null port knows where they are), and a click of
;;; the secondary button on a circle opens McCLIM's dialog, a window of its own, which the test
;;; looks at and cancels. What is not met, and so not tested: the circle under the pointer is
;;; highlighted by McCLIM and is not grey; the diameter does not change behind the dialog; a click
;;; away from the dialog does not close it. What the dialog gives back on OK is checked on XQuartz,
;;; by a click on the dialog.

(defun canvas (frame) (clim:find-pane-named frame 'almighty-bolt-7guis/circles/mcclim::canvas))
(defun drawing (frame) (slot-value frame 'almighty-bolt-7guis/circles/mcclim::drawing))

(defun circles (frame)
  (mapcar (lambda (circle)
            (list (m:circle-x circle) (m:circle-y circle) (m:circle-diameter circle)))
          (m:drawing-circles (drawing frame))))

(defmacro with-circles ((frame) &body body)
  `(base:with-running-frame (,frame #'c:make-circles-frame)
     (base:await (lambda () (plusp (base:count-records (canvas ,frame)))) 1)
     ,@body))

(defun draw-at (frame x y)
  (s:click (canvas frame) x y))

(defun command (frame name)
  (s:run-command frame (find-symbol (string name) '#:almighty-bolt-7guis/circles/mcclim)))

(defun dialogs (frame)
  "The dialogs that are open: the frame manager of the null port remembers the frames of every
test."
  (remove-if-not (lambda (other)
                   (and (typep other 'clim:accept-values) (eq (clim:frame-state other) :enabled)))
                 (clim:frame-manager-frames (clim:frame-manager frame))))

(define-test the-canvas-starts-empty ()
  (with-circles (frame)
    (assert-equal '() (circles frame))))

(define-test clicking-on-empty-ground-draws-a-circle-of-forty-there ()
  (with-circles (frame)
    (draw-at frame 100 80)
    (assert-equal '((100 80 40)) (circles frame))
    (draw-at frame 250 150)
    (assert-equal '((100 80 40) (250 150 40)) (circles frame))))

(define-test clicking-on-a-circle-does-not-draw-another ()
  (with-circles (frame)
    (draw-at frame 100 80)
    (draw-at frame 110 85)
    (assert-equal '((100 80 40)) (circles frame))))

(define-test undo-and-redo-go-back-and-forward-through-the-drawing ()
  (with-circles (frame)
    (draw-at frame 50 50)
    (draw-at frame 150 150)
    (draw-at frame 250 100)
    (command frame :com-undo)
    (assert-equal '((50 50 40) (150 150 40)) (circles frame))
    (command frame :com-undo)
    (command frame :com-undo)
    (assert-equal '() (circles frame))
    (command frame :com-undo)
    (command frame :com-redo)
    (assert-equal '((50 50 40)) (circles frame))
    (command frame :com-redo)
    (command frame :com-redo)
    (assert-equal '((50 50 40) (150 150 40) (250 100 40)) (circles frame))))

(define-test drawing-after-an-undo-ends-the-redo ()
  (with-circles (frame)
    (draw-at frame 50 50)
    (draw-at frame 150 150)
    (command frame :com-undo)
    (draw-at frame 300 200)
    (command frame :com-redo)
    (assert-equal '((50 50 40) (300 200 40)) (circles frame))))

(define-test undo-and-redo-are-enabled-when-they-have-something-to-do ()
  (with-circles (frame)
    (base:await (lambda ()
                  (not (clim:command-enabled 'almighty-bolt-7guis/circles/mcclim::com-undo frame)))
                2)
    (assert-false (clim:command-enabled 'almighty-bolt-7guis/circles/mcclim::com-undo frame))
    (draw-at frame 50 50)
    (assert-true (clim:command-enabled 'almighty-bolt-7guis/circles/mcclim::com-undo frame))
    (assert-false (clim:command-enabled 'almighty-bolt-7guis/circles/mcclim::com-redo frame))))

(define-test the-secondary-button-on-a-circle-opens-the-dialog ()
  (with-circles (frame)
    (draw-at frame 100 80)
    (s:click (canvas frame) 100 80 :button clim:+pointer-right-button+)
    (base:await (lambda () (dialogs frame)) 3)
    (assert-equal 1 (length (dialogs frame)))
    (assert-equal 'clim:accept-values (type-of (first (dialogs frame))))
    (s:close-frame (first (dialogs frame)))
    (base:await (lambda () (null (dialogs frame))) 3)))

(define-test the-dialog-is-a-slider-from-5-to-200-at-the-diameter-of-the-circle ()
  (with-circles (frame)
    (draw-at frame 100 80)
    (s:click (canvas frame) 100 80 :button clim:+pointer-right-button+)
    (base:await (lambda () (dialogs frame)) 3)
    (let* ((dialog (first (dialogs frame)))
           (sliders (s:gadgets-in (clim:frame-top-level-sheet dialog) 'clim:slider)))
      (base:await (lambda () (s:gadgets-in (clim:frame-top-level-sheet dialog) 'clim:slider)) 3)
      (setf sliders (s:gadgets-in (clim:frame-top-level-sheet dialog) 'clim:slider))
      (assert-equal 1 (length sliders))
      (assert-equal 5 (clim:gadget-min-value (first sliders)))
      (assert-equal 200 (clim:gadget-max-value (first sliders)))
      (assert-equal 40 (clim:gadget-value (first sliders)))
      (s:close-frame dialog)
      (base:await (lambda () (null (dialogs frame))) 3))))

(define-test the-secondary-button-on-empty-ground-opens-nothing ()
  (with-circles (frame)
    (draw-at frame 100 80)
    (s:click (canvas frame) 300 200 :button clim:+pointer-right-button+)
    (sleep 1)
    (assert-equal 0 (length (dialogs frame)))))

(define-test a-dialog-that-is-cancelled-changes-nothing-and-leaves-nothing-to-undo ()
  (with-circles (frame)
    (draw-at frame 100 80)
    (command frame :com-undo)
    (draw-at frame 100 80)
    (s:click (canvas frame) 100 80 :button clim:+pointer-right-button+)
    (base:await (lambda () (dialogs frame)) 3)
    (s:close-frame (first (dialogs frame)))
    (base:await (lambda () (null (dialogs frame))) 3)
    (assert-equal '((100 80 40)) (circles frame))
    (command frame :com-undo)
    (assert-equal '() (circles frame))))
