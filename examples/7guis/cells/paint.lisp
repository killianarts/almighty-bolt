(defpackage #:almighty-bolt-7guis/cells/paint
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:model #:almighty-bolt-7guis/cells/model)
                    (#:g #:almighty-bolt-7guis/cells/geometry))
  (:export #:paint-grid))

(in-package #:almighty-bolt-7guis/cells/paint)

;;; How the cells grid looks: the commands appended for the state the app holds, apart from how it
;;; holds it and what it does with the events. A cell shows its value (numbers on the right, text on the
;;; left, errors in red), cut to the width of the cell; the selected cell has an outline, and
;;; while it is being typed into, the text and a caret.

(defparameter +background+ (bolt:pack-color 30 33 46))
(defparameter +header-color+ (bolt:pack-color 48 53 74))
(defparameter +cell-color+ (bolt:pack-color 38 42 58))
(defparameter +edit-color+ (bolt:pack-color 20 22 32))
(defparameter +text-color+ (bolt:pack-color 232 234 242))
(defparameter +dim-color+ (bolt:pack-color 150 156 176))
(defparameter +error-color+ (bolt:pack-color 255 140 140))
(defparameter +accent-color+ (bolt:pack-color 150 200 255))
(defparameter +selection-color+ (bolt:pack-color 72 104 200 160))
(defparameter +caret-color+ (bolt:pack-color 240 242 250))

(defconstant +padding+ 6)

(defun draw-text (context list font string x y width color &key right)
  "The start of STRING that fits in WIDTH, at X, with its baseline at Y."
  (let* ((end (bolt:context-index-at-x context font string width))
         (end (loop :while (and (plusp end) (> (bolt:context-caret-x context font string end) width))
                    :do (decf end)
                    :finally (return end)))
         (shown (subseq string 0 end)))
    (bolt:context-push-text context list font shown
                            (if right (+ x (- width (bolt:context-measure context font shown))) x)
                            y color)))

(defun paint-grid (context list x y width height sheet selected-column selected-row
                   first-column first-row editing)
  "Append the commands that draw the grid in the area X, Y, WIDTH by HEIGHT: the cells that
show when it is scrolled to FIRST-COLUMN, FIRST-ROW, the one at SELECTED-COLUMN,
SELECTED-ROW selected, and EDITING (a text state, or NIL) typed into it."
  (let ((font (bolt:context-font context "Helvetica" 14)))
    (multiple-value-bind (text-width ascent descent) (bolt:context-measure context font "Hg")
      (declare (ignore text-width))
      (let ((baseline (+ (/ (- g:+cell-height+ (+ ascent descent)) 2) ascent))
            (last-column (min model:+columns+ (+ first-column g:+visible-columns+)))
            (last-row (min model:+rows+ (+ first-row g:+visible-rows+))))
        (bolt:push-rect list x y width height +background+)
        (loop :for column :from first-column :below last-column
              :do (loop :for row :from first-row :below last-row
                        :do (paint-cell context list font x y baseline sheet column row
                                        first-column first-row
                                        (and (= column selected-column) (= row selected-row))
                                        editing)))
        ;; The cell being typed into, over the cells beside it that its text runs across.
        (when (and editing
                   (<= first-column selected-column (1- last-column))
                   (<= first-row selected-row (1- last-row)))
          (paint-editor context list font x y width baseline selected-column selected-row
                        first-column first-row editing))
        ;; The headers, over the cells' edges.
        (bolt:push-rect list x y g:+header-width+ g:+header-height+ +header-color+)
        (loop :for column :from first-column :below last-column
              :do (let ((left (+ x (g:cell-left column first-column)))
                        (letter (model:column-name column)))
                    (bolt:push-rect list (+ left 1) y (- g:+cell-width+ 1) g:+header-height+ +header-color+)
                    (bolt:context-push-text context list font letter
                                            (+ left (/ (- g:+cell-width+ (bolt:context-measure context font letter))
                                                       2))
                                            (+ y baseline)
                                            (if (= column selected-column) +accent-color+ +dim-color+))))
        (loop :for row :from first-row :below last-row
              :do (let ((top (+ y (g:cell-top row first-row)))
                        (number (princ-to-string row)))
                    (bolt:push-rect list x (+ top 1) g:+header-width+ (- g:+cell-height+ 1) +header-color+)
                    (bolt:context-push-text context list font number
                                            (+ x (- g:+header-width+ +padding+
                                                    (bolt:context-measure context font number)))
                                            (+ top baseline)
                                            (if (= row selected-row) +accent-color+ +dim-color+))))))))

(defun paint-cell (context list font x y baseline sheet column row first-column first-row
                   selected editing)
  (let ((left (+ x (g:cell-left column first-column)))
        (top (+ y (g:cell-top row first-row)))
        (editing-here (and selected editing)))
    (bolt:push-rect list (+ left 1) (+ top 1) (- g:+cell-width+ 1) (- g:+cell-height+ 1)
                    (if editing-here +edit-color+ +cell-color+))
    (unless editing-here
      (let ((display (model:sheet-display sheet column row)))
        (when (plusp (length display))
          (let ((value (model:sheet-value sheet column row)))
            (draw-text context list font display (+ left +padding+) (+ top baseline)
                       (- g:+cell-width+ (* 2 +padding+))
                       (if (keywordp value) +error-color+ +text-color+)
                       :right (numberp value))))))
    (when (and selected (not editing-here))
      (push-outline list left top g:+cell-width+))))

(defun push-outline (list left top width)
  "The outline of the selected cell, WIDTH wide."
  (bolt:push-rect list left top width 2 +accent-color+)
  (bolt:push-rect list left (+ top g:+cell-height+ -2) width 2 +accent-color+)
  (bolt:push-rect list left top 2 g:+cell-height+ +accent-color+)
  (bolt:push-rect list (+ left width -2) top 2 g:+cell-height+ +accent-color+))

(defun paint-editor (context list font x y width baseline column row first-column first-row
                     editing)
  "Draw the cell at COLUMN, ROW as it is typed into, over the cells beside it: a box as wide as
the text needs, up to the right edge of the grid (X and WIDTH), with the text moved left, if
it is longer than that, to keep the caret in it."
  (let* ((left (+ x (g:cell-left column first-column)))
         (top (+ y (g:cell-top row first-row)))
         (string (bolt:text-state-string editing))
         (room (- (+ x width) left))
         (needed (+ (* 2 +padding+) (bolt:context-caret-x context font string (length string)) 2))
         (box-width (min room (max g:+cell-width+ needed)))
         (inner (- box-width (* 2 +padding+)))
         (caret (bolt:context-caret-x context font string (bolt:text-state-caret editing)))
         (text-left (- (+ left +padding+) (max 0 (- (+ caret 2) inner)))))
    (bolt:push-rect list left top box-width g:+cell-height+ +edit-color+)
    (bolt:push-clip list (+ left 1) (+ top 1) (- box-width 2) (- g:+cell-height+ 2))
    (when (bolt:text-state-selection-p editing)
      (let ((a (bolt:context-caret-x context font string (bolt:text-state-selection-start editing)))
            (b (bolt:context-caret-x context font string (bolt:text-state-selection-end editing))))
        (bolt:push-rect list (+ text-left a) (+ top 3) (- b a) (- g:+cell-height+ 6)
                        +selection-color+)))
    (bolt:context-push-text context list font string text-left (+ top baseline) +text-color+)
    (bolt:push-rect list (+ text-left caret) (+ top 4) 1.5f0 (- g:+cell-height+ 8) +caret-color+)
    (bolt:push-unclip list)
    (push-outline list left top box-width)))
