(defpackage #:almighty-bolt-7guis/cells/paint
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-index-at-x
                #:context-caret-x
                #:context-push-text
                #:push-rect
                #:push-clip
                #:push-unclip
                #:pack-color
                #:text-state-string
                #:text-state-caret
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end)
  (:import-from #:almighty-bolt-7guis/cells/model
                #:+columns+
                #:+rows+
                #:column-name
                #:sheet-value
                #:sheet-display)
  (:import-from #:almighty-bolt-7guis/cells/geometry
                #:+cell-width+
                #:+cell-height+
                #:+header-width+
                #:+header-height+
                #:+visible-columns+
                #:+visible-rows+
                #:cell-left
                #:cell-top)
  (:export #:paint-grid))

(in-package #:almighty-bolt-7guis/cells/paint)

;;; How the cells grid looks, which is the same in every variant: the variants differ in how
;;; they hold the state that this draws and in what they do with the events, not in the
;;; commands they append for it. A cell shows its value (numbers on the right, text on the
;;; left, errors in red), cut to the width of the cell; the selected cell has an outline, and
;;; while it is being typed into, the text and a caret.

(defparameter +background+ (pack-color 30 33 46))
(defparameter +header-color+ (pack-color 48 53 74))
(defparameter +cell-color+ (pack-color 38 42 58))
(defparameter +edit-color+ (pack-color 20 22 32))
(defparameter +text-color+ (pack-color 232 234 242))
(defparameter +dim-color+ (pack-color 150 156 176))
(defparameter +error-color+ (pack-color 255 140 140))
(defparameter +accent-color+ (pack-color 150 200 255))
(defparameter +selection-color+ (pack-color 72 104 200 160))
(defparameter +caret-color+ (pack-color 240 242 250))

(defconstant +padding+ 6)

(defun draw-text (context list font string x y width color &key right)
  "The start of STRING that fits in WIDTH, at X, with its baseline at Y."
  (let* ((end (context-index-at-x context font string width))
         (end (loop :while (and (plusp end) (> (context-caret-x context font string end) width))
                    :do (decf end)
                    :finally (return end)))
         (shown (subseq string 0 end)))
    (context-push-text context list font shown
                       (if right (+ x (- width (context-measure context font shown))) x)
                       y color)))

(defun paint-grid (context list x y width height sheet selected-column selected-row
                   first-column first-row editing)
  "Append the commands that draw the grid in the area X, Y, WIDTH by HEIGHT: the cells that
show when it is scrolled to FIRST-COLUMN, FIRST-ROW, the one at SELECTED-COLUMN,
SELECTED-ROW selected, and EDITING (a text state, or NIL) typed into it."
  (let ((font (context-font context "Helvetica" 14)))
    (multiple-value-bind (text-width ascent descent) (context-measure context font "Hg")
      (declare (ignore text-width))
      (let ((baseline (+ (/ (- +cell-height+ (+ ascent descent)) 2) ascent))
            (last-column (min +columns+ (+ first-column +visible-columns+)))
            (last-row (min +rows+ (+ first-row +visible-rows+))))
        (push-rect list x y width height +background+)
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
        (push-rect list x y +header-width+ +header-height+ +header-color+)
        (loop :for column :from first-column :below last-column
              :do (let ((left (+ x (cell-left column first-column)))
                        (letter (column-name column)))
                    (push-rect list (+ left 1) y (- +cell-width+ 1) +header-height+ +header-color+)
                    (context-push-text context list font letter
                                       (+ left (/ (- +cell-width+ (context-measure context font letter))
                                                  2))
                                       (+ y baseline)
                                       (if (= column selected-column) +accent-color+ +dim-color+))))
        (loop :for row :from first-row :below last-row
              :do (let ((top (+ y (cell-top row first-row)))
                        (number (princ-to-string row)))
                    (push-rect list x (+ top 1) +header-width+ (- +cell-height+ 1) +header-color+)
                    (context-push-text context list font number
                                       (+ x (- +header-width+ +padding+
                                               (context-measure context font number)))
                                       (+ top baseline)
                                       (if (= row selected-row) +accent-color+ +dim-color+))))))))

(defun paint-cell (context list font x y baseline sheet column row first-column first-row
                   selected editing)
  (let ((left (+ x (cell-left column first-column)))
        (top (+ y (cell-top row first-row)))
        (editing-here (and selected editing)))
    (push-rect list (+ left 1) (+ top 1) (- +cell-width+ 1) (- +cell-height+ 1)
               (if editing-here +edit-color+ +cell-color+))
    (unless editing-here
      (let ((display (sheet-display sheet column row)))
        (when (plusp (length display))
          (let ((value (sheet-value sheet column row)))
            (draw-text context list font display (+ left +padding+) (+ top baseline)
                       (- +cell-width+ (* 2 +padding+))
                       (if (keywordp value) +error-color+ +text-color+)
                       :right (numberp value))))))
    (when (and selected (not editing-here))
      (push-outline list left top +cell-width+))))

(defun push-outline (list left top width)
  "The outline of the selected cell, WIDTH wide."
  (push-rect list left top width 2 +accent-color+)
  (push-rect list left (+ top +cell-height+ -2) width 2 +accent-color+)
  (push-rect list left top 2 +cell-height+ +accent-color+)
  (push-rect list (+ left width -2) top 2 +cell-height+ +accent-color+))

(defun paint-editor (context list font x y width baseline column row first-column first-row
                     editing)
  "Draw the cell at COLUMN, ROW as it is typed into, over the cells beside it: a box as wide as
the text needs, up to the right edge of the grid (X and WIDTH), with the text moved left, if
it is longer than that, to keep the caret in it."
  (let* ((left (+ x (cell-left column first-column)))
         (top (+ y (cell-top row first-row)))
         (string (text-state-string editing))
         (room (- (+ x width) left))
         (needed (+ (* 2 +padding+) (context-caret-x context font string (length string)) 2))
         (box-width (min room (max +cell-width+ needed)))
         (inner (- box-width (* 2 +padding+)))
         (caret (context-caret-x context font string (text-state-caret editing)))
         (text-left (- (+ left +padding+) (max 0 (- (+ caret 2) inner)))))
    (push-rect list left top box-width +cell-height+ +edit-color+)
    (push-clip list (+ left 1) (+ top 1) (- box-width 2) (- +cell-height+ 2))
    (when (text-state-selection-p editing)
      (let ((a (context-caret-x context font string (text-state-selection-start editing)))
            (b (context-caret-x context font string (text-state-selection-end editing))))
        (push-rect list (+ text-left a) (+ top 3) (- b a) (- +cell-height+ 6)
                   +selection-color+)))
    (context-push-text context list font string text-left (+ top baseline) +text-color+)
    (push-rect list (+ text-left caret) (+ top 4) 1.5f0 (- +cell-height+ 8) +caret-color+)
    (push-unclip list)
    (push-outline list left top box-width)))
