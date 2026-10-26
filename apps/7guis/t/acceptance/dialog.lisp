(defpackage #:almighty-bolt-7guis/t/acceptance/dialog
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-7guis/t/acceptance/dialog)

;;; The dialog screen (record 0022): a header with a label at its right edge, a card as wide
;;; and as tall as what is in it in the middle of the window, and a line at the bottom, in
;;; the variants that lay out after the description. The context's text is 8 points a
;;; character, so the numbers are exact.

(defparameter +widths+ (loop :for width :from 280 :to 900 :by 40 :collect (float width 1f0)))

(defun context-at (width)
  (fake:make-fake-context :width width :height 420f0))

(defun draw (object context list)
  "One frame as the host makes it: settle. Returns the builds."
  (values (core:settle-frame object context list)))

(d:define-acceptance the-dialog-card-is-in-the-middle-of-the-room-between-the-header-and-the-status :dialog
  ;; 420 high: the room is from 12 + 14 + 8 to 420 - 12 - 14 - 8.
  (destructuring-bind (x y width height) (d:rectangle driver :card)
    (assert-true (< (abs (- (+ x (/ width 2)) 320f0)) 0.01))
    (assert-true (< (abs (- (+ y (/ height 2)) 210f0)) 0.01)))
  (d:resize driver 400 420)
  (destructuring-bind (x y width height) (d:rectangle driver :card)
    (assert-true (< (abs (- (+ x (/ width 2)) 200f0)) 0.01))
    (assert-true (< (abs (- (+ y (/ height 2)) 210f0)) 0.01))))

(d:define-acceptance the-dialog-card-is-as-wide-and-as-tall-as-its-paragraph-and-the-padding :dialog
  (destructuring-bind (x y width height) (d:rectangle driver :card)
    (declare (ignore x y))
    (destructuring-bind (text-x text-y text-width text-height) (d:rectangle driver :card-text)
      (declare (ignore text-x text-y))
      ;; 8 of padding each side; above the paragraph, the title, the three rows, and a gap
      ;; after each of them.
      (assert-true (< (abs (- width (+ text-width 16f0))) 0.01))
      (assert-true (< (abs (- height (+ text-height 16f0 14f0 (* 3 14f0) 8f0 8f0))) 0.01)))))

(d:define-acceptance the-dialog-close-is-at-the-right-edge-of-the-window :dialog
  (destructuring-bind (x y width height) (d:rectangle driver :close)
    (declare (ignore y height))
    (assert-true (< (abs (- (+ x width) (- 640f0 12f0))) 0.01)))
  (d:resize driver 400 420)
  (destructuring-bind (x y width height) (d:rectangle driver :close)
    (declare (ignore y height))
    (assert-true (< (abs (- (+ x width) (- 400f0 12f0))) 0.01))))

(define-test the-box-tree-variants-draw-the-dialog-once-at-every-width-and-the-same-list ()
  (let ((variants (d:available-variants :dialog)))
    (assert-true (>= (length variants) 2))
    (dolist (width +widths+)
      (let ((results (mapcar (lambda (variant)
                               (let* ((app (registry:find-app variant :dialog))
                                      (list (core:make-display-list :capacity 65536)))
                                 (list (draw (funcall (registry:app-constructor app))
                                             (context-at width) list)
                                       (core:display-list-hash list))))
                             variants)))
        (assert-true (every (lambda (result) (eql 1 (first result))) results) width)
        (assert-eql 1 (length (remove-duplicates (mapcar #'second results))) width)))))
