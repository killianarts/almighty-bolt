(defpackage #:almighty-bolt-7guis/cells/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:make-text-state
                #:text-state-string
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
  (:import-from #:almighty-bolt-7guis/cells/model
                #:+columns+
                #:+rows+
                #:format-ref
                #:make-sheet
                #:sheet-text
                #:sheet-set)
  (:import-from #:almighty-bolt-7guis/cells/geometry
                #:+grid-width+
                #:+grid-height+
                #:+visible-columns+
                #:+visible-rows+
                #:+columns-per-line+
                #:cell-at
                #:clamp-first)
  (:import-from #:almighty-bolt-7guis/cells/paint
                #:paint-grid)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:padding
                #:column
                #:row
                #:canvas)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant)
  (:export #:make-cells))

(in-package #:almighty-bolt-7guis/cells/immediate)

;;; The cells task, immediate: the sheet is a mutable object, and which cell is selected,
;;; where the grid is scrolled to and what is being typed are variables. The grid is drawn by
;;; a function that reads them, every frame, and the events on it are handled in the loop
;;; that follows.

(defun make-cells ()
  (let ((sheet (make-sheet))
        (selected-column 0)
        (selected-row 0)
        (first-column 0)
        (first-row 0)
        (editing nil)
        (remainder-x 0f0)
        (remainder-y 0f0))
    (labels ((commit ()
               (when editing
                 (sheet-set sheet selected-column selected-row (text-state-string editing))
                 (setf editing nil)))
             (select (new-column new-row)
               (commit)
               (let ((new-column (max 0 (min (1- +columns+) new-column)))
                     (new-row (max 0 (min (1- +rows+) new-row))))
                 (setf selected-column new-column
                       selected-row new-row)
                 (cond ((< new-column first-column)
                        (setf first-column new-column))
                       ((>= new-column (+ first-column +visible-columns+))
                        (setf first-column (1+ (- new-column +visible-columns+)))))
                 (cond ((< new-row first-row)
                        (setf first-row new-row))
                       ((>= new-row (+ first-row +visible-rows+))
                        (setf first-row (1+ (- new-row +visible-rows+)))))))
             (press (x y button click-count)
               (when (eql button 0)
                 (multiple-value-bind (new-column new-row) (cell-at x y first-column first-row)
                   (when (and new-column (< new-column +columns+) (< new-row +rows+))
                     (select new-column new-row)
                     ;; A double click edits what the cell holds.
                     (when (>= click-count 2)
                       (setf editing (make-text-state (sheet-text sheet new-column new-row))))))))
             (scroll (dx dy)
               (incf remainder-x (- (* dx +columns-per-line+)))
               (incf remainder-y (- dy))
               (let ((columns (truncate remainder-x))
                     (rows (truncate remainder-y)))
                 (decf remainder-x columns)
                 (decf remainder-y rows)
                 (setf first-column (clamp-first (+ first-column columns) +columns+
                                                 +visible-columns+)
                       first-row (clamp-first (+ first-row rows) +rows+ +visible-rows+))))
             (key (event)
               (when (eq (key-event-kind event) :down)
                 (let ((key (key-event-logical-key event))
                       (modifiers (key-event-modifiers event)))
                   (cond
                     (editing
                      (cond ((eql key #\Return) (select selected-column (1+ selected-row)))
                            ((eql key #\Escape) (setf editing nil))
                            (t (multiple-value-bind (new edited) (edit-text editing event)
                                 (when edited
                                   (setf editing new))))))
                     ((eql key +key-up+) (select selected-column (1- selected-row)))
                     ((eql key +key-down+) (select selected-column (1+ selected-row)))
                     ((eql key +key-left+) (select (1- selected-column) selected-row))
                     ((eql key +key-right+) (select (1+ selected-column) selected-row))
                     ((eql key #\Return)
                      (setf editing (make-text-state (sheet-text sheet selected-column
                                                                 selected-row))))
                     ((or (eql key #\Backspace) (eql key +key-delete-forward+))
                      (sheet-set sheet selected-column selected-row ""))
                     ((or (modifier-down-p modifiers +modifier-super+)
                          (modifier-down-p modifiers +modifier-control+))
                      nil)
                     (t (multiple-value-bind (new edited) (edit-text (make-text-state "") event)
                          (when edited
                            (setf editing new))))))))
             (draw (list context width height)
               (paint-grid context list 0f0 0f0 width height sheet selected-column selected-row
                           first-column first-row editing)))
      (make-immediate-variant
       (lambda ()
         (padding (:all 20)
           (column (:gap 12)
             (row (:gap 10 :cross-align :center)
               (label "Cell:")
               (label (format-ref selected-column selected-row) :id :cell-name)
               (label "Contents:")
               (label (if editing
                          (text-state-string editing)
                          (sheet-text sheet selected-column selected-row))
                      :id :formula))
             (dolist (event (canvas :grid +grid-width+ +grid-height+ #'draw :focusable t))
               (ecase (first event)
                 (:press (press (second event) (third event) (fourth event) (fifth event)))
                 (:key (key (second event)))
                 (:wheel (scroll (second event) (third event)))
                 ((:move :drag :release :exit) nil))))))))))

(register-app :immediate :cells 'make-cells :width 640 :height 440)
