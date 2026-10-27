(defpackage #:almighty-bolt-7guis/cells/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-core
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
  (:import-from #:almighty-bolt-variant-reactive/signals
                #:make-signal
                #:value
                #:update)
  (:import-from #:almighty-bolt-variant-reactive/builder
                #:padding
                #:column
                #:row
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:canvas)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-cells))

(in-package #:almighty-bolt-7guis/cells/reactive)

;;; The cells task, reactive: the sheet is a mutable object whose edits are counted by a
;;; signal, and what the grid shows (the selected cell, where it is scrolled to, what is being
;;; typed) is signals. The two labels and the grid are components, so a keystroke while
;;; typing describes the contents label and the grid, and the rest is left alone.

(defun draw (data canvas context list x y width height)
  (declare (ignore canvas))
  (destructuring-bind (sheet column row first-column first-row editing version) data
    ;; The version is in the data so that an edit of the sheet makes the canvas draw again.
    (declare (ignore version))
    (paint-grid context list x y width height sheet column row first-column first-row editing)))

(defun make-cells ()
  (let ((sheet (make-sheet))
        (version (make-signal 0))
        (column (make-signal 0))
        (row (make-signal 0))
        (first-column (make-signal 0))
        (first-row (make-signal 0))
        (editing (make-signal nil))
        (remainder-x 0f0)
        (remainder-y 0f0))
    (labels ((commit ()
               (when (value editing)
                 (sheet-set sheet (value column) (value row) (text-state-string (value editing)))
                 (update version #'1+)
                 (setf (value editing) nil)))
             (select (new-column new-row)
               (commit)
               (let ((new-column (max 0 (min (1- +columns+) new-column)))
                     (new-row (max 0 (min (1- +rows+) new-row))))
                 (setf (value column) new-column
                       (value row) new-row)
                 (cond ((< new-column (value first-column))
                        (setf (value first-column) new-column))
                       ((>= new-column (+ (value first-column) +visible-columns+))
                        (setf (value first-column) (1+ (- new-column +visible-columns+)))))
                 (cond ((< new-row (value first-row))
                        (setf (value first-row) new-row))
                       ((>= new-row (+ (value first-row) +visible-rows+))
                        (setf (value first-row) (1+ (- new-row +visible-rows+)))))))
             (press (context x y button click-count)
               (declare (ignore context))
               (when (eql button 0)
                 (multiple-value-bind (new-column new-row)
                     (cell-at x y (value first-column) (value first-row))
                   (when (and new-column (< new-column +columns+) (< new-row +rows+))
                     (select new-column new-row)
                     ;; A double click edits what the cell holds.
                     (when (>= click-count 2)
                       (setf (value editing)
                             (make-text-state (sheet-text sheet new-column new-row))))))))
             (scroll (context dx dy)
               (declare (ignore context))
               (incf remainder-x (- (* dx +columns-per-line+)))
               (incf remainder-y (- dy))
               (let ((columns (truncate remainder-x))
                     (rows (truncate remainder-y)))
                 (decf remainder-x columns)
                 (decf remainder-y rows)
                 (let ((new-column (clamp-first (+ (value first-column) columns) +columns+
                                                +visible-columns+))
                       (new-row (clamp-first (+ (value first-row) rows) +rows+ +visible-rows+)))
                   (when (or (/= new-column (value first-column)) (/= new-row (value first-row)))
                     (setf (value first-column) new-column
                           (value first-row) new-row)
                     t))))
             (key (context event)
               (declare (ignore context))
               (when (eq (key-event-kind event) :down)
                 (let ((key (key-event-logical-key event))
                       (modifiers (key-event-modifiers event)))
                   (cond
                     ((value editing)
                      (cond ((eql key #\Return) (select (value column) (1+ (value row))) t)
                            ((eql key #\Escape) (setf (value editing) nil) t)
                            (t (multiple-value-bind (new edited) (edit-text (value editing) event)
                                 (when edited
                                   (setf (value editing) new))
                                 edited))))
                     ((eql key +key-up+) (select (value column) (1- (value row))) t)
                     ((eql key +key-down+) (select (value column) (1+ (value row))) t)
                     ((eql key +key-left+) (select (1- (value column)) (value row)) t)
                     ((eql key +key-right+) (select (1+ (value column)) (value row)) t)
                     ((eql key #\Return)
                      (setf (value editing)
                            (make-text-state (sheet-text sheet (value column) (value row))))
                      t)
                     ((or (eql key #\Backspace) (eql key +key-delete-forward+))
                      (sheet-set sheet (value column) (value row) "")
                      (update version #'1+)
                      t)
                     ((or (modifier-down-p modifiers +modifier-super+)
                          (modifier-down-p modifiers +modifier-control+))
                      nil)
                     (t (multiple-value-bind (new edited) (edit-text (make-text-state "") event)
                          (when edited
                            (setf (value editing) new))
                          edited)))))))
      (make-reactive-variant
       (lambda ()
         (padding (:all 20)
           (column (:gap 12)
             (row (:gap 10 :cross-align :center)
               (label "Cell:")
               (component ()
                 (label (format-ref (value column) (value row))
                        :id :cell-name))
               (label "Contents:")
               (component ()
                 (value version)
                 (label (if (value editing)
                            (text-state-string (value editing))
                            (sheet-text sheet (value column) (value row)))
                        :id :formula)))
             (component ()
               (canvas :id :grid :width +grid-width+ :height +grid-height+
                       :focusable t :paint #'draw
                       :data (list sheet (value column) (value row)
                                   (value first-column) (value first-row)
                                   (value editing) (value version))
                       :on-press #'press :on-key #'key :on-wheel #'scroll)))))))))

(register-app :reactive :cells #'make-cells :width 640 :height 440)
