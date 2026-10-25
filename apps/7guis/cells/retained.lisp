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
                #:cell-left
                #:cell-top
                #:cell-at
                #:clamp-first)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:padding
                #:column
                #:row)
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

(defparameter +background+ (pack-color 30 33 46))
(defparameter +header-color+ (pack-color 48 53 74))
(defparameter +cell-color+ (pack-color 38 42 58))
(defparameter +edit-color+ (pack-color 20 22 32))
(defparameter +text-color+ (pack-color 232 234 242))
(defparameter +dim-color+ (pack-color 150 156 176))
(defparameter +accent-color+ (pack-color 150 200 255))
(defparameter +error-color+ (pack-color 255 140 140))
(defparameter +selection-color+ (pack-color 72 104 200 160))
(defparameter +caret-color+ (pack-color 240 242 250))

(defconstant +padding+ 6)

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
             (press (source x y button)
               (declare (ignore source))
               (when (eql button 0)
                 (multiple-value-bind (column row) (cell-at x y first-column first-row)
                   (when (and column (< column +columns+) (< row +rows+))
                     (select column row)))))
             (scroll (source dx dy)
               (declare (ignore source))
               (incf remainder-x (- dx))
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
             (key (source event)
               (declare (ignore source))
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
             (draw-text (context list font string x y width color &key right)
               ;; The start of STRING that fits in WIDTH, at X, with its baseline at Y.
               (let* ((end (context-index-at-x context font string width))
                      (end (loop :while (and (plusp end)
                                             (> (context-caret-x context font string end) width))
                                 :do (decf end)
                                 :finally (return end)))
                      (shown (subseq string 0 end)))
                 (context-push-text context list font shown
                                    (if right
                                        (+ x (- width (context-measure context font shown)))
                                        x)
                                    y color)))
             (paint (source context list x y width height)
               (declare (ignore source))
               (let ((font (context-font context "Helvetica" 14)))
                 (multiple-value-bind (text-width ascent descent) (context-measure context font "Hg")
                   (declare (ignore text-width))
                   (let ((baseline-offset (+ (/ (- +cell-height+ (+ ascent descent)) 2) ascent)))
                     (push-rect list x y width height +background+)
                     ;; The cells that show.
                     (loop :for column :from first-column
                             :below (min +columns+ (+ first-column +visible-columns+))
                           :do (loop :for row :from first-row
                                       :below (min +rows+ (+ first-row +visible-rows+))
                                     :do (let ((left (+ x (cell-left column first-column)))
                                               (top (+ y (cell-top row first-row)))
                                               (selected (and (= column selected-column)
                                                              (= row selected-row))))
                                           (push-rect list (+ left 1) (+ top 1)
                                                      (- +cell-width+ 1) (- +cell-height+ 1)
                                                      (if (and selected editing)
                                                          +edit-color+
                                                          +cell-color+))
                                           (unless (and selected editing)
                                             (let ((display (sheet-display sheet column row)))
                                               (when (plusp (length display))
                                                 (let ((value (sheet-value sheet column row)))
                                                   (draw-text context list font display
                                                              (+ left +padding+)
                                                              (+ top baseline-offset)
                                                              (- +cell-width+ (* 2 +padding+))
                                                              (cond ((keywordp value) +error-color+)
                                                                    (t +text-color+))
                                                              :right (numberp value))))))
                                           (when selected
                                             (let ((edge (if editing +accent-color+ +accent-color+)))
                                               (push-rect list left top +cell-width+ 2 edge)
                                               (push-rect list left (+ top +cell-height+ -2)
                                                          +cell-width+ 2 edge)
                                               (push-rect list left top 2 +cell-height+ edge)
                                               (push-rect list (+ left +cell-width+ -2) top 2
                                                          +cell-height+ edge)))
                                           (when (and selected editing)
                                             (let* ((string (text-state-string editing))
                                                    (caret (text-state-caret editing))
                                                    (text-left (+ left +padding+)))
                                               (when (text-state-selection-p editing)
                                                 (let ((a (context-caret-x context font string
                                                                           (text-state-selection-start editing)))
                                                       (b (context-caret-x context font string
                                                                           (text-state-selection-end editing))))
                                                   (push-rect list (+ text-left a) (+ top 3)
                                                              (- b a) (- +cell-height+ 6)
                                                              +selection-color+)))
                                               (context-push-text context list font string text-left
                                                                  (+ top baseline-offset) +text-color+)
                                               (push-rect list
                                                          (+ text-left (context-caret-x context font string caret))
                                                          (+ top 4) 1.5f0 (- +cell-height+ 8)
                                                          +caret-color+))))))
                     ;; The headers, over the cells' edges.
                     (push-rect list x y +header-width+ +header-height+ +header-color+)
                     (loop :for column :from first-column
                             :below (min +columns+ (+ first-column +visible-columns+))
                           :do (let ((left (+ x (cell-left column first-column))))
                                 (push-rect list (+ left 1) y (- +cell-width+ 1) +header-height+
                                            +header-color+)
                                 (let ((letter (column-name column)))
                                   (context-push-text context list font letter
                                                      (+ left (/ (- +cell-width+
                                                                    (context-measure context font letter))
                                                                 2))
                                                      (+ y baseline-offset)
                                                      (if (= column selected-column)
                                                          +accent-color+
                                                          +dim-color+)))))
                     (loop :for row :from first-row
                             :below (min +rows+ (+ first-row +visible-rows+))
                           :do (let ((top (+ y (cell-top row first-row)))
                                     (number (princ-to-string row)))
                                 (push-rect list x (+ top 1) +header-width+ (- +cell-height+ 1)
                                            +header-color+)
                                 (context-push-text context list font number
                                                    (+ x (- +header-width+ +padding+
                                                            (context-measure context font number)))
                                                    (+ top baseline-offset)
                                                    (if (= row selected-row)
                                                        +accent-color+
                                                        +dim-color+)))))))))
      (setf name (label "A0" :id :cell-name)
            contents (label "" :id :formula)
            grid (canvas :id :grid :width +grid-width+ :height +grid-height+ :focusable t
                         :paint #'paint :on-press #'press :on-key #'key :on-wheel #'scroll))
      (refresh)
      (make-retained-variant
       (padding (column (row (label "Cell:") name (label "Contents:") contents
                             :gap 10 :cross-align :center)
                        grid
                        :gap 12)
                :all 20)))))

(register-app :retained :cells #'make-cells :width 640 :height 440)
