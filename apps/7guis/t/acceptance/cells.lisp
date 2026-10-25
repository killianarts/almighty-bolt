(defpackage #:almighty-bolt-7guis/t/acceptance/cells
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:g #:almighty-bolt-7guis/cells/geometry)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-7guis/t/acceptance/cells)

;;; Cells: a grid of 26 columns (A to Z) and 100 rows (0 to 99) with headers, the layout of
;;; the geometry package, 7 columns and 14 rows showing at a time. One cell is selected. A
;;; key that types starts typing into it, Return finishes (and selects the cell below),
;;; Escape gives up, the arrows move, Backspace clears. A cell that starts with = is a
;;; formula. Controls: :grid, :cell-name (the name of the selected cell) and :formula (what
;;; it holds, or what is being typed into it).

(defun cell-rectangle (driver column row first-column first-row)
  "The window's rectangle of a cell, as x, y, width and height, when the grid is scrolled to
FIRST-COLUMN, FIRST-ROW."
  (destructuring-bind (x y width height) (d:rectangle driver :grid)
    (declare (ignore width height))
    (list (+ x (g:cell-left column first-column)) (+ y (g:cell-top row first-row))
          g:+cell-width+ g:+cell-height+)))

(defun cell-text (driver column row &key (first-column 0) (first-row 0))
  "What the cell at COLUMN, ROW shows, when the grid is scrolled to FIRST-COLUMN, FIRST-ROW."
  (destructuring-bind (x y width height) (cell-rectangle driver column row first-column first-row)
    (d:text-within driver x y width height)))

(defun click-cell (driver column row &key (first-column 0) (first-row 0))
  (destructuring-bind (grid-x grid-y grid-width grid-height) (d:rectangle driver :grid)
    (destructuring-bind (x y width height) (cell-rectangle driver column row first-column first-row)
      (d:click-at driver :grid (/ (- (+ x (/ width 2)) grid-x) grid-width)
                  (/ (- (+ y (/ height 2)) grid-y) grid-height)))))

(defun enter (driver column row text &key (first-column 0) (first-row 0))
  "Select a cell, type TEXT into it and finish with Return."
  (click-cell driver column row :first-column first-column :first-row first-row)
  (d:type-text driver text)
  (d:press-key driver #\Return))

(d:define-acceptance the-first-cell-is-selected-and-empty :cells
  (assert-equal "A0" (d:text-in driver :cell-name))
  (assert-equal "" (d:text-in driver :formula))
  (assert-equal "" (cell-text driver 0 0)))

(d:define-acceptance clicking-a-cell-selects-it :cells
  (click-cell driver 1 2)
  (assert-equal "B2" (d:text-in driver :cell-name))
  (click-cell driver 6 13)
  (assert-equal "G13" (d:text-in driver :cell-name)))

(d:define-acceptance typing-and-return-puts-the-text-in-the-cell-and-moves-down :cells
  (click-cell driver 1 2)
  (d:type-text driver "hello")
  (assert-equal "hello" (d:text-in driver :formula))
  (d:press-key driver #\Return)
  (assert-equal "hello" (cell-text driver 1 2))
  (assert-equal "B3" (d:text-in driver :cell-name))
  (assert-equal "" (d:text-in driver :formula)))

(d:define-acceptance a-number-and-a-formula-show-their-values :cells
  (enter driver 0 0 "700")
  (enter driver 0 1 "=A0*2")
  (assert-equal "700" (cell-text driver 0 0))
  (assert-equal "1400" (cell-text driver 0 1))
  (click-cell driver 0 1)
  (assert-equal "=A0*2" (d:text-in driver :formula)))

(d:define-acceptance changing-a-cell-changes-the-cells-that-use-it :cells
  (enter driver 0 0 "700")
  (enter driver 0 1 "=A0*2")
  (enter driver 0 2 "=A1+A0")
  (assert-equal "2100" (cell-text driver 0 2))
  (enter driver 0 0 "1")
  (assert-equal "3" (cell-text driver 0 2)))

(d:define-acceptance functions-work-over-ranges :cells
  (enter driver 0 0 "10")
  (enter driver 0 1 "20")
  (enter driver 0 2 "30")
  (enter driver 1 0 "=SUM(A0:A2)")
  (enter driver 1 1 "=AVG(A0:A2)")
  (enter driver 1 2 "=MAX(A0:A2)")
  (assert-equal "60" (cell-text driver 1 0))
  (assert-equal "20" (cell-text driver 1 1))
  (assert-equal "30" (cell-text driver 1 2)))

(d:define-acceptance faults-and-cycles-are-shown-as-errors :cells
  (enter driver 0 0 "=1/0")
  (enter driver 1 0 "=B0")
  (enter driver 2 0 "=D0+1")
  (enter driver 3 0 "=C0")
  (assert-equal "#ERR" (cell-text driver 0 0))
  (assert-equal "#CYCLE" (cell-text driver 1 0))
  (assert-equal "#CYCLE" (cell-text driver 2 0))
  (assert-equal "#CYCLE" (cell-text driver 3 0)))

(d:define-acceptance escape-gives-up-the-typing :cells
  (click-cell driver 2 2)
  (d:type-text driver "9")
  (d:press-key driver #\Escape)
  (assert-equal "" (cell-text driver 2 2))
  (assert-equal "C2" (d:text-in driver :cell-name))
  (assert-equal "" (d:text-in driver :formula)))

(d:define-acceptance return-on-a-cell-edits-what-it-holds :cells
  (enter driver 0 0 "abc")
  (click-cell driver 0 0)
  (d:press-key driver #\Return)
  (assert-equal "abc" (d:text-in driver :formula))
  (d:type-text driver "d")
  (d:press-key driver #\Return)
  (assert-equal "abcd" (cell-text driver 0 0)))

(d:define-acceptance the-text-being-typed-can-be-edited-with-the-usual-keys :cells
  (click-cell driver 0 0)
  (d:type-text driver "abcd")
  (d:press-key driver #\Backspace)
  (d:press-key driver core:+key-left+)
  (d:type-text driver "X")
  (assert-equal "abXc" (d:text-in driver :formula))
  (d:press-key driver #\Return)
  (assert-equal "abXc" (cell-text driver 0 0)))

(d:define-acceptance the-arrows-move-the-selection-and-backspace-clears-a-cell :cells
  (enter driver 0 0 "x")
  (click-cell driver 0 0)
  (d:press-key driver core:+key-right+)
  (d:press-key driver core:+key-down+)
  (assert-equal "B1" (d:text-in driver :cell-name))
  (d:press-key driver core:+key-left+)
  (d:press-key driver core:+key-up+)
  (assert-equal "A0" (d:text-in driver :cell-name))
  (d:press-key driver #\Backspace)
  (assert-equal "" (cell-text driver 0 0)))

(d:define-acceptance the-selection-stays-inside-the-sheet :cells
  (click-cell driver 0 0)
  (d:press-key driver core:+key-left+)
  (d:press-key driver core:+key-up+)
  (assert-equal "A0" (d:text-in driver :cell-name)))

(d:define-acceptance moving-past-the-last-visible-column-scrolls-to-it :cells
  (click-cell driver 6 0)
  (d:press-key driver core:+key-right+)
  (assert-equal "H0" (d:text-in driver :cell-name))
  ;; The headers now start at B, and H is the last one showing.
  (assert-true (search "BCDEFGH" (d:text-within driver 0 0 640 80)))
  (enter driver 7 0 "seven" :first-column 1)
  (assert-equal "seven" (cell-text driver 7 0 :first-column 1)))

(d:define-acceptance the-wheel-scrolls-the-grid-by-rows :cells
  (d:scroll driver :grid -20)
  (enter driver 0 20 "twenty" :first-row 20)
  (assert-equal "twenty" (cell-text driver 0 20 :first-row 20))
  (d:scroll driver :grid 5)
  (assert-equal "twenty" (cell-text driver 0 20 :first-row 15))
  (assert-equal "" (cell-text driver 0 15 :first-row 15)))

(d:define-acceptance a-trackpad-scrolls-the-grid-by-the-points-a-row-and-a-column-take :cells
  ;; A row is 24 points and a column 80: 48 points down are two rows, 240 across three columns.
  (d:scroll-points driver :grid :dy -48)
  (d:scroll-points driver :grid :dx -240)
  (enter driver 3 2 "moved" :first-column 3 :first-row 2)
  (assert-equal "moved" (cell-text driver 3 2 :first-column 3 :first-row 2))
  (assert-equal "D3" (d:text-in driver :cell-name))
  ;; Small steps add up: two of 12 points up the grid are one row, and none of them is lost.
  (d:scroll-points driver :grid :dy 12)
  (assert-equal "moved" (cell-text driver 3 2 :first-column 3 :first-row 2))
  (d:scroll-points driver :grid :dy 12)
  (assert-equal "moved" (cell-text driver 3 2 :first-column 3 :first-row 1)))

(d:define-acceptance a-swipe-across-with-a-drift-down-moves-the-grid-across-only :cells
  ;; The way a trackpad really goes: ten steps of 24 across and 5 down, which unlocked would
  ;; be 3 columns and 2 rows. The gesture is across, so the rows stay.
  (d:swipe-points driver :grid (loop :repeat 10 :collect (list -24 -5)))
  (enter driver 3 0 "across" :first-column 3 :first-row 0)
  ;; The cell clicked was D0 only if the grid is at column 3 and row 0: Return selected D1.
  (assert-equal "D1" (d:text-in driver :cell-name))
  (assert-equal "across" (cell-text driver 3 0 :first-column 3 :first-row 0)))

(d:define-acceptance a-swipe-down-with-a-drift-across-moves-the-grid-down-only :cells
  ;; Ten steps of 24 down and 12 across: 10 rows, and 120 points across (1.5 columns) lost.
  (d:swipe-points driver :grid (loop :repeat 10 :collect (list -12 -24)))
  (enter driver 0 10 "down" :first-column 0 :first-row 10)
  (assert-equal "A11" (d:text-in driver :cell-name))
  (assert-equal "down" (cell-text driver 0 10 :first-column 0 :first-row 10)))

(d:define-acceptance a-scrolled-grid-selects-the-cell-under-the-pointer :cells
  (d:scroll driver :grid -10)
  (click-cell driver 2 12 :first-row 10)
  (assert-equal "C12" (d:text-in driver :cell-name)))

(d:define-acceptance clicking-another-cell-keeps-what-was-typed :cells
  (click-cell driver 0 0)
  (d:type-text driver "kept")
  (click-cell driver 1 1)
  (assert-equal "kept" (cell-text driver 0 0)))
