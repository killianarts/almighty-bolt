(defpackage #:almighty-bolt-7guis/cells/geometry
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:+points-per-line+)
  (:export #:+columns-per-line+
           #:+cell-width+
           #:+cell-height+
           #:+header-width+
           #:+header-height+
           #:+grid-width+
           #:+grid-height+
           #:+visible-columns+
           #:+visible-rows+
           #:cell-left
           #:cell-top
           #:cell-at
           #:clamp-first))

(in-package #:almighty-bolt-7guis/cells/geometry)

;;; Where the cells are in the cells task's grid, the same in every variant's version of it
;;; and in the tests: a header row and a header column (the column letters and the row
;;; numbers), and below and to the right of them, the visible columns and rows of cells,
;;; the first of each being whatever the grid is scrolled to. Everything is in points from
;;; the top left of the grid.

(defconstant +cell-width+ 80)
(defconstant +cell-height+ 24)
(defconstant +header-width+ 40)
(defconstant +header-height+ 24)
(defconstant +columns-per-line+ (float (/ +points-per-line+ +cell-width+) 1f0)
  "How far across a line of the wheel scrolls the grid, in columns: a line is the height of a
row (24 points) and a column is 80 points wide. Down, a line is a row.")
(defconstant +grid-width+ 600)
(defconstant +grid-height+ 360)
(defconstant +visible-columns+ (floor (- +grid-width+ +header-width+) +cell-width+))
(defconstant +visible-rows+ (floor (- +grid-height+ +header-height+) +cell-height+))

(defun cell-left (column first-column)
  "The left edge of COLUMN when the grid is scrolled to FIRST-COLUMN."
  (+ +header-width+ (* +cell-width+ (- column first-column))))

(defun cell-top (row first-row)
  (+ +header-height+ (* +cell-height+ (- row first-row))))

(defun cell-at (x y first-column first-row)
  "The column and row of the cell at the point X, Y, as two values, or NIL if it is in the
headers or outside the grid."
  (when (and (<= +header-width+ x) (< x +grid-width+)
             (<= +header-height+ y) (< y +grid-height+))
    (values (+ first-column (floor (- x +header-width+) +cell-width+))
            (+ first-row (floor (- y +header-height+) +cell-height+)))))

(defun clamp-first (first count visible)
  "FIRST, the index of the first column or row shown, kept so that the grid does not
scroll past its COUNT columns or rows when VISIBLE of them show."
  (max 0 (min first (- count visible))))
