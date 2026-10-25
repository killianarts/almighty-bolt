(defpackage #:almighty-bolt-7guis/t/acceptance/layout
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-7guis/t/acceptance/layout)

;;; The layout spike (H6, the charter): a nested split panel with paragraphs that are broken
;;; into lines to the width of their pane, in a window that is resized. A split is a row or a
;;; column with flex factors: the sidebar 1 and the right side 2 across, and on the right the
;;; top 1 and the bottom 1 down, the bottom split again 1 and 1. The window is 640 by 420 with a
;;; margin of 12 and a gap of 8 (apps/7guis/layout/text.lisp).

(defun width-of (driver id) (third (d:rectangle driver id)))
(defun height-of (driver id) (fourth (d:rectangle driver id)))

(defun close-to (a b &optional (tolerance 0.01))
  (< (abs (- a b)) tolerance))

(defun lines-of (driver id)
  "How many lines the paragraph ID is drawn in: the distinct heights of its glyphs."
  (destructuring-bind (x y width height) (d:rectangle driver id)
    (let ((list (d:driver-list driver))
          (ys '()))
      (dotimes (index (core:display-list-count list))
        (when (and (= (core:display-list-kind list index) core:+command-glyph+)
                   (<= x (core:display-list-x list index) (+ x width))
                   (<= y (core:display-list-y list index) (+ y height)))
          (pushnew (core:display-list-y list index) ys)))
      (length ys))))

(d:define-acceptance the-panes-share-the-window-by-their-flex-factors :layout
  ;; 640 - 24 of margin - 8 of gap = 608 to share: 1 part and 2.
  (assert-true (close-to (width-of driver :sidebar) (/ 608 3f0)))
  (assert-true (close-to (width-of driver :top) (* 2 (/ 608 3f0))))
  ;; The right side is split down: 396 - 8 = 388 to share, 1 and 1.
  (assert-true (close-to (height-of driver :top) 194f0))
  (assert-true (close-to (height-of driver :bottom-left) 194f0))
  ;; And the bottom is split across, 1 and 1: (405.33 - 8) / 2 each.
  (assert-true (close-to (width-of driver :bottom-left) (/ (- (* 2 (/ 608 3f0)) 8) 2)))
  (assert-true (close-to (width-of driver :bottom-right) (width-of driver :bottom-left))))

(d:define-acceptance a-paragraph-is-broken-to-the-width-of-its-pane :layout
  ;; A character is 8 points: the sidebar's paragraph is 202.67 - 16 of padding wide.
  (let ((lines-wide (lines-of driver :sidebar-text)))
    (assert-true (> lines-wide 1))
    (d:resize driver 400 420)
    (let ((lines-narrow (lines-of driver :sidebar-text)))
      (assert-true (> lines-narrow lines-wide)))))

(d:define-acceptance a-narrower-window-gives-narrower-panes-and-the-same-split :layout
  (d:resize driver 400 420)
  (assert-true (close-to (width-of driver :sidebar) (/ (- 400 24 8) 3f0)))
  (assert-true (close-to (width-of driver :top) (* 2 (/ (- 400 24 8) 3f0))))
  (assert-true (close-to (height-of driver :top) 194f0))
  ;; And back again: the first layout, not what the narrow one left behind.
  (d:resize driver 640 420)
  (assert-true (close-to (width-of driver :sidebar) (/ 608 3f0))))

;;; Across the variants: the same frame at every width, built once.

(defun frame-at (variant width)
  "Build a frame of a new VARIANT of the spike at WIDTH. Returns its builds and display list
hash."
  (let* ((app (registry:find-app variant :layout))
         (object (funcall (registry:app-constructor app)))
         (context (fake:make-fake-context :width (float width 1f0) :height 420f0))
         (list (core:make-display-list :capacity 65536)))
    (values (core:settle-frame object context list)
            (core:display-list-hash list))))

(define-test every-variant-builds-the-spike-once-at-every-width-and-draws-the-same-list ()
  (let ((variants (d:available-variants :layout)))
    (assert-true (>= (length variants) 3))
    (loop :for width :from 280 :to 900 :by 40
          :do (let ((results (mapcar (lambda (variant)
                                       (multiple-value-list (frame-at variant width)))
                                     variants)))
                (assert-true (every (lambda (result) (eql 1 (first result))) results))
                (assert-eql 1 (length (remove-duplicates (mapcar #'second results))))))))

(define-test one-variant-resized-through-many-widths-draws-what-a-new-one-draws ()
  ;; The immediate variant skips layout and painting when the frame is the same; a window that
  ;; is resized, and then not, must still get the frame of its size.
  (dolist (variant (d:available-variants :layout))
    (let* ((app (registry:find-app variant :layout))
           (object (funcall (registry:app-constructor app)))
           (context (fake:make-fake-context :width 640f0 :height 420f0))
           (list (core:make-display-list :capacity 65536)))
      (dolist (width '(640 300 520 520 900 280 640))
        (setf (core:context-width context) (float width 1f0))
        (core:settle-frame object context list)
        (assert-eql (nth-value 1 (frame-at variant width)) (core:display-list-hash list)
                    variant width)))))
