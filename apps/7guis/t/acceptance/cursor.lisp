(defpackage #:almighty-bolt-7guis/t/acceptance/cursor
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:compare #:almighty-bolt-variant-support/compare)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:layout #:almighty-bolt-7guis/layout/cursor)
                    (#:dialog #:almighty-bolt-7guis/dialog/cursor)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-7guis/t/acceptance/cursor)

;;; The cursor experiment (record 0022): the layout spike and the dialog in the cursor front end,
;;; in each of its modes, against the box tree. The context's text is 8 points a character, so the
;;; numbers are exact; the real text engine is in make bench-cursor.

(defparameter +modes+ '(:lag :request :rebuild :measure :fragment))
(defparameter +widths+ (loop :for width :from 280 :to 900 :by 40 :collect (float width 1f0)))

(defun box-tree (task)
  (funcall (registry:app-constructor (registry:find-app :immediate task))))

(defun cursor (task mode)
  (ecase task
    (:layout (layout:make-layout mode))
    (:dialog (dialog:make-dialog mode))))

(defun context-at (width)
  (fake:make-fake-context :width width :height 420f0))

(defun draw (object context list)
  "One frame as the host makes it: settle. Returns the builds."
  (values (core:settle-frame object context list)))

(defun same-p (a b)
  (multiple-value-bind (difference why) (compare:compare-display-lists a b)
    (and (not why) (<= difference 0.01f0))))

(defun box-tree-list (task width)
  (let ((list (core:make-display-list :capacity 65536)))
    (draw (box-tree task) (context-at width) list)
    list))

(defun first-frame (task mode width)
  "What a new front end draws at WIDTH: the builds, and the list."
  (let ((list (core:make-display-list :capacity 65536)))
    (values (draw (cursor task mode) (context-at width) list) list)))

;;; --- the layout spike: every size comes from the window, down ---

(define-test the-cursor-front-end-draws-the-layout-spike-as-the-box-tree-does-in-every-mode ()
  (dolist (mode +modes+)
    (dolist (width +widths+)
      (multiple-value-bind (builds list) (first-frame :layout mode width)
        (assert-eql 1 builds mode width)
        (assert-true (same-p list (box-tree-list :layout width)) mode width)))))

;;; --- the dialog: a card that is placed by its own size ---

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

(define-test the-modes-that-find-the-size-in-the-frame-draw-the-dialog-as-the-box-tree-does ()
  (dolist (width +widths+)
    (let ((truth (box-tree-list :dialog width)))
      ;; Rebuilding asks for a second build to learn the size; measuring runs the card twice
      ;; inside one; a fragment is drawn at the origin and moved.
      (dolist (mode '(:rebuild :measure :fragment))
        (multiple-value-bind (builds list) (first-frame :dialog mode width)
          (assert-eql (if (eq mode :rebuild) 2 1) builds mode width)
          (assert-true (same-p list truth) mode width))))))

(define-test the-modes-that-remember-the-size-draw-the-dialog-wrong-on-the-first-frame ()
  (dolist (mode '(:lag :request))
    (dolist (width +widths+)
      (multiple-value-bind (builds list) (first-frame :dialog mode width)
        (assert-eql 1 builds mode width)
        (assert-false (same-p list (box-tree-list :dialog width)) mode width)))))

(define-test a-lagging-front-end-is-wrong-after-a-resize-until-it-is-drawn-again ()
  (let ((object (cursor :dialog :lag))
        (context (context-at 700f0))
        (list (core:make-display-list :capacity 65536)))
    (draw object context list)
    (draw object context list)
    ;; Settled at 700.
    (assert-true (same-p list (box-tree-list :dialog 700f0)))
    ;; The window is made narrower, and the frame has the card as it was.
    (setf (core:context-width context) 400f0)
    (draw object context list)
    (assert-false (same-p list (box-tree-list :dialog 400f0)))
    (assert-false (core:take-frame-request context))
    ;; Nothing asks for another frame. When one is drawn, it is right.
    (draw object context list)
    (assert-true (same-p list (box-tree-list :dialog 400f0)))))

(define-test a-requesting-front-end-asks-for-the-frame-that-puts-it-right ()
  (let ((object (cursor :dialog :request))
        (context (context-at 700f0))
        (list (core:make-display-list :capacity 65536)))
    (loop :repeat 3
          :do (draw object context list)
              (core:take-frame-request context))
    (assert-true (same-p list (box-tree-list :dialog 700f0)))
    (setf (core:context-width context) 400f0)
    (draw object context list)
    (assert-false (same-p list (box-tree-list :dialog 400f0)))
    (assert-true (core:take-frame-request context))
    (draw object context list)
    (assert-true (same-p list (box-tree-list :dialog 400f0)))
    (assert-false (core:take-frame-request context))))

(define-test a-rebuilding-front-end-is-never-wrong-after-a-resize ()
  (let ((object (cursor :dialog :rebuild))
        (context (context-at 700f0))
        (list (core:make-display-list :capacity 65536)))
    (draw object context list)
    (dolist (width '(400f0 900f0 280f0 640f0))
      (setf (core:context-width context) width)
      (let ((builds (draw object context list)))
        (assert-true (<= builds 2) width)
        (assert-true (same-p list (box-tree-list :dialog width)) width)))))

;;; --- the comparison itself ---

(defun list-of (&rest commands)
  "A display list of glyphs, each (x y)."
  (let ((list (core:make-display-list :capacity 16)))
    (dolist (command commands list)
      (core:push-glyph list (first command) (second command) 8f0 14f0 65 #xffffffff))))

(define-test the-comparison-does-not-mind-the-order-and-says-how-far-a-command-moved ()
  (let ((a (list-of '(0f0 0f0) '(100f0 0f0) '(0f0 20f0)))
        (reordered (list-of '(0f0 20f0) '(0f0 0f0) '(100f0 0f0)))
        (moved (list-of '(0f0 0f0) '(100.5f0 0f0) '(0f0 20f0)))
        (crossed (list-of '(0f0 0f0) '(99f0 1f0) '(0f0 20f0))))
    (assert-equal '(0f0 nil) (multiple-value-list (compare:compare-display-lists a reordered)))
    (assert-equal '(0.5f0 nil) (multiple-value-list (compare:compare-display-lists a moved)))
    ;; Moved a point, among commands of the same kind that are far apart: matched to its own.
    (assert-equal '(1f0 nil) (multiple-value-list (compare:compare-display-lists a crossed)))
    (assert-equal '(nil :count)
                  (multiple-value-list (compare:compare-display-lists a (list-of '(0f0 0f0)))))))

(define-test the-comparison-sees-a-glyph-that-is-another-glyph ()
  (let ((a (list-of '(0f0 0f0)))
        (b (core:make-display-list :capacity 4)))
    (core:push-glyph b 0f0 0f0 8f0 14f0 66 #xffffffff)
    (assert-equal '(nil :commands) (multiple-value-list (compare:compare-display-lists a b)))))
