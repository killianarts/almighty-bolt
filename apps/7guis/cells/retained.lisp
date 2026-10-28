(defpackage #:almighty-bolt-7guis/cells/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-index-at-x
                #:context-caret-x
                #:context-push-text
                #:push-rect
                #:push-rounded-rect
                #:pack-color
                #:make-text-state
                #:text-state-string
                #:text-state-caret
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end
                #:edit-text
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-super+
                #:+modifier-control+
                #:+key-up+
                #:+key-down+
                #:+key-left+
                #:+key-right+
                #:+key-delete-forward+)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/cells/paint
                #:paint-grid)
  (:import-from #:almighty-bolt-7guis/cells/model
                #:+columns+
                #:+rows+
                #:column-name
                #:format-ref
                #:make-sheet
                #:sheet-text
                #:sheet-set
                #:sheet-value
                #:sheet-display)
  (:import-from #:almighty-bolt-7guis/cells/geometry
                #:+cell-width+
                #:+cell-height+
                #:+header-width+
                #:+header-height+
                #:+grid-width+
                #:+grid-height+
                #:+visible-columns+
                #:+visible-rows+
                #:+columns-per-line+
                #:cell-left
                #:cell-top
                #:cell-at
                #:clamp-first)
  (:import-from #:almighty-bolt-variant-retained/builder
                #:padding
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text)
  (:import-from #:almighty-bolt-variant-retained/inputs
                #:canvas
                #:canvas-invalidate)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-cells))

(in-package #:almighty-bolt-7guis/cells/retained)

;;; The cells task, retained: the sheet and what the grid is showing (which cell is
;;; selected, what is being typed into it, where it is scrolled to) are variables; the grid
;;; is one canvas widget that draws them and handles its own events; and two labels above
;;; it show the selected cell's name and what it holds. After each change the handlers
;;; update the labels and say that the canvas must draw again.

(defun make-cells ()
  (let ((sheet (make-sheet))
        (selected-column 0) (selected-row 0)
        (first-column 0) (first-row 0)
        (editing nil)                   ; a text state while a cell is being typed into
        (remainder-x 0f0) (remainder-y 0f0)
        (grid nil) (name nil) (contents nil))
    (labels ((selected-text ()
               (sheet-text sheet selected-column selected-row))
             (refresh ()
               (let ((text (if editing (text-state-string editing) (selected-text))))
                 (unless (string= text (label-text contents))
                   (setf (label-text contents) text)))
               (let ((ref (format-ref selected-column selected-row)))
                 (unless (string= ref (label-text name))
                   (setf (label-text name) ref)))
               (canvas-invalidate grid))
             (commit ()
               (when editing
                 (sheet-set sheet selected-column selected-row (text-state-string editing))
                 (setf editing nil)))
             (select (column row)
               (commit)
               (setf selected-column (max 0 (min (1- +columns+) column))
                     selected-row (max 0 (min (1- +rows+) row)))
               ;; Scroll just far enough to show it.
               (cond ((< selected-column first-column) (setf first-column selected-column))
                     ((>= selected-column (+ first-column +visible-columns+))
                      (setf first-column (1+ (- selected-column +visible-columns+)))))
               (cond ((< selected-row first-row) (setf first-row selected-row))
                     ((>= selected-row (+ first-row +visible-rows+))
                      (setf first-row (1+ (- selected-row +visible-rows+)))))
               (refresh))
             (press (source context x y button click-count)
               (declare (ignore source context))
               (when (eql button 0)
                 (multiple-value-bind (column row) (cell-at x y first-column first-row)
                   (when (and column (< column +columns+) (< row +rows+))
                     (select column row)
                     ;; A double click edits what the cell holds.
                     (when (>= click-count 2)
                       (start-editing (make-text-state (sheet-text sheet column row))))))))
             (scroll (source context dx dy)
               (declare (ignore source context))
               (incf remainder-x (- (* dx +columns-per-line+)))
               (incf remainder-y (- dy))
               (let ((columns (truncate remainder-x))
                     (rows (truncate remainder-y)))
                 (decf remainder-x columns)
                 (decf remainder-y rows)
                 (let ((new-column (clamp-first (+ first-column columns) +columns+ +visible-columns+))
                       (new-row (clamp-first (+ first-row rows) +rows+ +visible-rows+)))
                   (when (or (/= new-column first-column) (/= new-row first-row))
                     (setf first-column new-column first-row new-row)
                     (refresh)
                     t))))
             (start-editing (state)
               (setf editing state)
               (refresh))
             (key (source context event)
               (declare (ignore source context))
               (when (eq (key-event-kind event) :down)
                 (let ((key (key-event-logical-key event))
                       (modifiers (key-event-modifiers event)))
                   (cond
                     (editing
                      (cond ((eql key #\Return) (select selected-column (1+ selected-row)) t)
                            ((eql key #\Escape) (setf editing nil) (refresh) t)
                            (t (multiple-value-bind (new edited) (edit-text editing event)
                                 (when edited
                                   (start-editing new))
                                 edited))))
                     ((eql key +key-up+) (select selected-column (1- selected-row)) t)
                     ((eql key +key-down+) (select selected-column (1+ selected-row)) t)
                     ((eql key +key-left+) (select (1- selected-column) selected-row) t)
                     ((eql key +key-right+) (select (1+ selected-column) selected-row) t)
                     ((eql key #\Return) (start-editing (make-text-state (selected-text))) t)
                     ((or (eql key #\Backspace) (eql key +key-delete-forward+))
                      (sheet-set sheet selected-column selected-row "")
                      (refresh)
                      t)
                     ((or (modifier-down-p modifiers +modifier-super+)
                          (modifier-down-p modifiers +modifier-control+))
                      nil)
                     (t (multiple-value-bind (new edited)
                            (edit-text (make-text-state "") event)
                          (when edited
                            (start-editing new))
                          edited))))))
             (paint (source context list x y width height)
               (declare (ignore source))
               (paint-grid context list x y width height sheet selected-column selected-row
                           first-column first-row editing)))
      (setf name (label "A0" :id :cell-name)
            contents (label "" :id :formula)
            grid (canvas :id :grid :width +grid-width+ :height +grid-height+ :focusable t
                         :paint #'paint :on-press #'press :on-key #'key :on-wheel #'scroll))
      (refresh)
      (make-retained-variant
       (padding (:all 20)
         (column (:gap 12)
           (row (:gap 10 :cross-align :center)
             (label "Cell:") name (label "Contents:") contents)
           grid))))))

(register-app :retained :cells 'make-cells :width 640 :height 440)
