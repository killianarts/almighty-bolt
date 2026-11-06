(defpackage #:almighty-bolt/ui/layout
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:split #:almighty-bolt/ui/split)
                    (#:box #:almighty-bolt/ui/box)
                    (#:widgets #:almighty-bolt/ui/widgets)
                    (#:theme #:almighty-bolt/ui/theme))
  (:export #:layout-box))

(in-package #:almighty-bolt/ui/layout)

;;; Where the boxes of a frame go, worked out after the frame is described, with the layout
;;; functions of src/layout: constraints down, sizes
;;; up, the parent places its children. The boxes are children of their parents in a vector,
;;; and the functions are given each child by its index.

(defun set-box (box w h)
  (setf (box:box-w box) w
        (box:box-h box) h)
  (values w h))

(defun line-height (context box)
  "How tall a line of BOX's text is: its face's line height, or its font's ascent and descent."
  (values (widgets:text-metrics context (widgets:ui-face box))))

(defun grid-shares (widths columns)
  "Each column's share of a grid's width, as a list of COLUMNS numbers that add up to one."
  (let* ((given (if (listp widths) widths '()))
         (shares (loop :for column :below columns
                       :collect (float (max 0 (or (nth column given) 1)) 1f0)))
         (total (reduce #'+ shares)))
    (if (plusp total)
        (mapcar (lambda (share) (/ share total)) shares)
        (make-list columns :initial-element (/ 1f0 columns)))))

(defun layout-grid (box context min-w max-w min-h max-h)
  "Lay out the grid BOX: its children in rows of (BOX-N3 BOX), each stretched to its cell."
  (declare (type single-float min-w max-w min-h max-h))
  (let* ((children (box:box-children box))
         (count (length children))
         (columns (max 1 (box:box-n3 box)))
         (rows (ceiling count columns))
         (gap (box:box-gap box))
         (row-gap (box:box-n1 box))
         (row-heights (cdr (box:box-data box)))
         (gaps-w (* gap (1- columns)))
         (widths (make-array columns :element-type 'single-float :initial-element 0f0))
         (heights (make-array (max 1 rows) :element-type 'single-float :initial-element 0f0)))
    ;; The columns' widths: shares of the width there is, or, where nothing limits it, what the
    ;; widest child needs (all the same when the shares are equal).
    (if (< max-w core:+unbounded+)
        (let ((room (max 0f0 (- max-w gaps-w))))
          (loop :for share :in (grid-shares (car (box:box-data box)) columns)
                :for column :from 0
                :do (setf (aref widths column) (* room share))))
        (progn
          (dotimes (i count)
            (let ((w (layout-box (aref children i) context 0f0 core:+unbounded+ 0f0 core:+unbounded+)))
              (setf (aref widths (mod i columns)) (max (aref widths (mod i columns)) w))))
          (when (eq (car (box:box-data box)) :equal)
            (let ((widest (reduce #'max widths)))
              (fill widths widest)))))
    ;; The rows' heights, from the children laid out at their columns' widths.
    (dotimes (i count)
      (let* ((w (aref widths (mod i columns)))
             (h (nth-value 1 (layout-box (aref children i) context w w 0f0 core:+unbounded+)))
             (row (floor i columns)))
        (setf (aref heights row) (max (aref heights row) h))))
    (cond ((numberp row-heights) (fill heights (float row-heights 1f0)))
          ((eq row-heights :uniform) (fill heights (reduce #'max heights))))
    ;; Each child again, stretched to its cell, and placed in it.
    (let ((y 0f0))
      (dotimes (row rows)
        (let ((x 0f0) (h (aref heights row)))
          (dotimes (column columns)
            (let ((i (+ (* row columns) column)))
              (when (< i count)
                (let ((child (aref children i)) (w (aref widths column)))
                  (layout-box child context w w h h)
                  (setf (box:box-x child) x (box:box-y child) y))))
            (incf x (+ (aref widths column) gap))))
        (incf y (+ (aref heights row) row-gap))))
    (set-box box
             (core:constrain (+ (reduce #'+ widths) gaps-w) min-w max-w)
             (core:constrain (+ (reduce #'+ heights :end (max 0 rows))
                                (* row-gap (max 0 (1- rows))))
                             min-h max-h))))

(defun layout-box (box context min-w max-w min-h max-h)
  "Work out the size of BOX within the constraints and the places of its children, store
them in the boxes, and return the width and height."
  (declare (type single-float min-w max-w min-h max-h))
  ;; A container's own least and greatest size (:width, :min-height ...), within what its parent
  ;; allows: the parent's limits win.
  (let ((least-w (box:box-min-w box)) (most-w (box:box-max-w box))
        (least-h (box:box-min-h box)) (most-h (box:box-max-h box)))
    (when (> least-w min-w) (setf min-w (min least-w max-w)))
    (when (>= most-w 0f0) (setf max-w (max min-w (min most-w max-w))))
    (when (> least-h min-h) (setf min-h (min least-h max-h)))
    (when (>= most-h 0f0) (setf max-h (max min-h (min most-h max-h)))))
  ;; Laid out in the theme it was described in (its font).
  (let ((theme:*theme* (box:box-theme box)))
    (ecase (box:box-kind box)
      (:label
       (let ((long (float (core:context-measure context (widgets:box-text-font context box) (box:box-text box)) 1f0))
             (short (float (line-height context box) 1f0)))
         ;; A turned label is as wide as a line is tall.
         (if (zerop (box:box-n1 box))
             (set-box box (core:constrain long min-w max-w) (core:constrain short min-h max-h))
             (set-box box (core:constrain short min-w max-w) (core:constrain long min-h max-h)))))
      (:paragraph
       (let* ((state (box:box-data box))
              (text (box:box-text box))
              (font (widgets:box-text-font context box))
              (wrap (if (< max-w core:+unbounded+) max-w 100000f0)))
         ;; Broken again only if the text or the width is not what the lines were made for.
         (unless (and (= (widgets:paragraph-state-wrapped-at state) wrap)
                      (= (widgets:paragraph-state-theme-id state) (theme:theme-id theme:*theme*))
                      (equal (widgets:paragraph-state-font state) font)
                      (string= (widgets:paragraph-state-text state) text))
           (widgets:set-paragraph-lines state text wrap (core:context-break-lines context font text wrap))
           (setf (widgets:paragraph-state-font state) font))
         (let ((step (line-height context box)))
           (let ((widest 0f0))
             (loop :for (start end) :in (widgets:paragraph-state-lines state)
                   :do (setf widest (max widest (float (core:context-measure context font
                                                                             (subseq text start end))
                                                       1f0))))
             (set-box box (core:constrain widest min-w max-w)
                      (core:constrain (* (length (widgets:paragraph-state-lines state)) (float step 1f0))
                                      min-h max-h))))))
      (:button
       (let ((font (widgets:ui-font context box)))
         (multiple-value-bind (width ascent descent) (core:context-measure context font (box:box-text box))
           (set-box box
                    (core:constrain (+ (float width 1f0) (* 2 (theme:theme-button-padding-x theme:*theme*))) min-w max-w)
                    (core:constrain (+ (float (+ ascent descent) 1f0) (* 2 (theme:theme-button-padding-y theme:*theme*)))
                                    min-h max-h)))))
      ((:text-field :combo-box)
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w)
                (core:constrain (+ (float (line-height context box) 1f0) (* 2 (theme:theme-field-padding-y theme:*theme*)))
                                min-h max-h)))
      (:text-area
       ;; ROWS (N3) lines tall; the lines the text is shown in are made for the width it is given.
       ;; A code view's numbers take room at the left.
       (let* ((area (box:box-data box))
              (height (widgets:area-line-height context (widgets:ui-face box)))
              (w (core:constrain (box:box-pref-w box) min-w max-w))
              (h (core:constrain (+ (* (box:box-n3 box) height) (* 2 (theme:theme-field-padding-y theme:*theme*))) min-h max-h))
              (gutter (setf (widgets:area-state-gutter area)
                            (widgets:area-gutter context area (box:box-text box) (widgets:ui-face box))))
              (inner-w (max 1f0 (- w (* 2 (theme:theme-field-padding-x theme:*theme*)) gutter)))
              (inner-h (max 0f0 (- h (* 2 (theme:theme-field-padding-y theme:*theme*)))))
              (lines (widgets:area-lines context area (box:box-text box) inner-w (widgets:ui-face box))))
         (setf (widgets:area-state-inner-width area) inner-w
               (widgets:area-state-inner-height area) inner-h
               (widgets:area-state-offset area) (max 0f0 (min (widgets:area-state-offset area)
                                                               (max 0f0 (- (* (length lines) height) inner-h)))))
         (set-box box w h)))
      (:slider
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w) (core:constrain (theme:theme-slider-height theme:*theme*) min-h max-h)))
      (:image
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w) (core:constrain (box:box-pref-h box) min-h max-h)))
      (:progress
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w) (core:constrain (theme:theme-progress-height theme:*theme*) min-h max-h)))
      (:canvas
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w)
                (core:constrain (box:box-pref-h box) min-h max-h)))
      (:list-box
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w)
                (core:constrain (+ 2f0 (* (theme:theme-row-height theme:*theme*) (box:box-n3 box))) min-h max-h)))
      (:choice
       ;; Side by side, or (MAIN-ALIGN :vertical) one under another, each option as tall as a checkbox.
       (let* ((options (car (box:box-data box)))
              (state (cdr (box:box-data box)))
              (vertical (eq (box:box-main-align box) :vertical))
              (segmented (member (box:box-main-align box) '(:segmented :tabs)))
              (count (length options))
              (lefts (make-array count))
              (tops (make-array count))
              (widths (make-array count))
              (left 0f0)
              (top 0f0)
              (widest 0f0)
              (font (widgets:ui-font context box))
              ;; A segment is as tall as a button, its words a button's padding from its sides.
              (height (if segmented
                          (+ (line-height context box) (* 2 (theme:theme-button-padding-y theme:*theme*)))
                          (max (theme:theme-control-height theme:*theme*) widgets:+dot-size+))))
         (loop :for option :in options
               :for i :from 0
               :do (let ((width (if segmented
                                    (+ (float (core:context-measure context font option) 1f0)
                                       (* 2 (theme:theme-button-padding-x theme:*theme*)))
                                    (+ widgets:+dot-size+ 6f0 (float (core:context-measure context font option) 1f0)))))
                     (setf (svref lefts i) left
                           (svref tops i) top
                           (svref widths i) width
                           widest (max widest width))
                     (cond (vertical (incf top (+ height (theme:theme-option-row-gap theme:*theme*))))
                           (segmented (incf left width))
                           (t (incf left (+ width (theme:theme-option-gap theme:*theme*)))))))
         (setf (widgets:choice-state-lefts state) lefts
               (widgets:choice-state-tops state) tops
               (widgets:choice-state-widths state) widths
               (widgets:choice-state-option-height state) height)
         (if vertical
             (set-box box (core:constrain widest min-w max-w)
                      (core:constrain (max 0f0 (- top (theme:theme-option-row-gap theme:*theme*))) min-h max-h))
             (set-box box (core:constrain (max 0f0 (- left (if segmented 0f0 (theme:theme-option-gap theme:*theme*))))
                                          min-w max-w)
                      (core:constrain height min-h max-h)))))
      (:menu-button
       (let ((font (widgets:ui-font context box)))
         (multiple-value-bind (width ascent descent) (core:context-measure context font (box:box-text box))
           (set-box box
                    (core:constrain (+ (float width 1f0) (* 2 (theme:theme-button-padding-x theme:*theme*)) 16f0) min-w max-w)
                    (core:constrain (+ (float (+ ascent descent) 1f0) (* 2 (theme:theme-button-padding-y theme:*theme*)))
                                    min-h max-h)))))
      (:dropdown
       ;; A button's padding, and room at the right for the arrows; as wide as WIDTH (PREF-W) if given.
       (let ((font (widgets:ui-font context box)))
         (multiple-value-bind (width ascent descent) (core:context-measure context font (box:box-text box))
           (set-box box
                    (core:constrain (if (plusp (box:box-pref-w box))
                                        (box:box-pref-w box)
                                        (+ (float width 1f0) (* 2 (theme:theme-button-padding-x theme:*theme*)) 16f0))
                                    min-w max-w)
                    (core:constrain (+ (float (+ ascent descent) 1f0) (* 2 (theme:theme-button-padding-y theme:*theme*)))
                                    min-h max-h)))))
      (:menu
       ;; Rows one under another, as wide as the widest (or the least width the menu was given), inside
       ;; the menu's padding.
       (let* ((children (box:box-children box))
              (pad (theme:theme-menu-padding theme:*theme*))
              (row-height (theme:theme-row-height theme:*theme*))
              (widest (max 0f0 (- min-w (* 2 pad)))))
         (loop :for row :across children
               :do (setf widest (max widest (layout-box row context 0f0 (- max-w (* 2 pad))
                                                        row-height row-height))))
         (loop :for row :across children
               :for i :from 0
               :do (layout-box row context widest widest row-height row-height)
                   (setf (box:box-x row) pad
                         (box:box-y row) (+ pad (* i row-height))))
         (set-box box (core:constrain (+ widest (* 2 pad)) min-w max-w)
                  (core:constrain (+ (* (length children) row-height) (* 2 pad))
                                  min-h max-h))))
      (:menu-item
       (set-box box (core:constrain (+ widgets:+menu-check-width+ 16f0
                                       (float (core:context-measure context (widgets:ui-font context box) (box:box-text box)) 1f0))
                                    min-w max-w)
                (core:constrain (theme:theme-row-height theme:*theme*) min-h max-h)))
      ((:popover :dialog)
       ;; What is inside, one under another, inside a padding: a dialog has more room.
       (let* ((children (box:box-children box))
              (dialog (eq (box:box-kind box) :dialog))
              (pad (if dialog (theme:theme-dialog-padding theme:*theme*) (theme:theme-popover-padding theme:*theme*)))
              (gap (if dialog (theme:theme-dialog-gap theme:*theme*) (theme:theme-popover-gap theme:*theme*))))
         (flet ((measure (i a b c d)
                  (layout-box (aref children i) context a b c d))
                (place (i x y)
                  (let ((child (aref children i)))
                    (setf (box:box-x child) (+ x pad)
                          (box:box-y child) (+ y pad))))
                (flex-of (i)
                  (declare (ignore i))
                  (values 0f0 t)))
           (declare (dynamic-extent #'measure #'place #'flex-of))
           (multiple-value-bind (w h)
               (core:layout-flex :vertical (max 0f0 (- min-w (* 2 pad))) (max 0f0 (- max-w (* 2 pad)))
                                 0f0 (max 0f0 (- max-h (* 2 pad))) (length children)
                                 #'measure #'place :flex-of #'flex-of :gap gap :main-size :min)
             (set-box box (core:constrain (+ w (* 2 pad)) min-w max-w)
                      (core:constrain (+ h (* 2 pad)) min-h max-h))))))
      (:toasts
       ;; One under another, each in the middle.
       (let ((children (box:box-children box))
             (widest 0f0)
             (y 0f0))
         (loop :for child :across children
               :do (setf widest (max widest (layout-box child context 0f0 max-w 0f0 max-h))))
         (loop :for child :across children
               :do (setf (box:box-x child) (/ (- widest (box:box-w child)) 2)
                         (box:box-y child) y)
                   (incf y (+ (box:box-h child) (theme:spaced 8))))
         (set-box box (core:constrain widest min-w max-w)
                  (core:constrain (max 0f0 (- y (theme:spaced 8))) min-h max-h))))
      (:toast
       (multiple-value-bind (width ascent descent) (core:context-measure context (widgets:ui-font context box)
                                                                         (box:box-text box))
         (set-box box (core:constrain (+ (float width 1f0) (theme:spaced 32)) min-w max-w)
                  (core:constrain (+ (float (+ ascent descent) 1f0) (theme:spaced 20)) min-h max-h))))
      (:tooltip
       (multiple-value-bind (width ascent descent)
           (core:context-measure context (core:context-font context (theme:theme-font-name theme:*theme*) 13) (box:box-text box))
         (set-box box (core:constrain (+ (float width 1f0) (theme:spaced 16)) min-w max-w)
                  (core:constrain (+ (float (+ ascent descent) 1f0) (theme:spaced 8)) min-h max-h))))
      (:grid (layout-grid box context min-w max-w min-h max-h))
      (:spacer
       ;; As small as it may be: a flex gives it its share as its least size.
       (set-box box (core:constrain 0f0 min-w max-w) (core:constrain 0f0 min-h max-h)))
      (:catcher
       (set-box box (if (< max-w core:+unbounded+) max-w min-w) (if (< max-h core:+unbounded+) max-h min-h)))
      (:switch
       (set-box box (core:constrain (+ widgets:+switch-width+ 8f0 (float (core:context-measure context (widgets:ui-font context box)
                                                                                           (box:box-text box))
                                                                          1f0))
                                    min-w max-w)
                (core:constrain (theme:theme-control-height theme:*theme*) min-h max-h)))
      (:spinner
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w) (core:constrain (box:box-pref-h box) min-h max-h)))
      (:checkbox
       (set-box box (core:constrain (+ widgets:+dot-size+ 6f0 (float (core:context-measure context (widgets:ui-font context box)
                                                                                       (box:box-text box))
                                                                      1f0))
                                    min-w max-w)
                (core:constrain (theme:theme-control-height theme:*theme*) min-h max-h)))
      (:scrim
       (set-box box (if (< max-w core:+unbounded+) max-w min-w) (if (< max-h core:+unbounded+) max-h min-h)))
      (:splitter
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w) (core:constrain (box:box-pref-h box) min-h max-h)))
      ((:column :row :context-area :surface :drag-source :pane :split :tree :stage)
       (let ((children (box:box-children box)))
         (flet ((measure (i a b c d)
                  ;; A split's pane of a fixed size is no larger than the room leaves it, the other
                  ;; keeping its least (N1).
                  (when (and (eq (box:box-kind box) :split)
                             (zerop (box:box-flex (aref children i)))
                             (not (eq (box:box-kind (aref children i)) :splitter)))
                    (let* ((horizontal (zerop (box:box-n3 box)))
                           (room (if horizontal max-w max-h)))
                      (when (< room core:+unbounded+)
                        (let ((most (max (box:box-n1 box)
                                         (- room split:+divider-width+ (box:box-n1 box)))))
                          (if horizontal
                              (setf b (min b most) a (min a b))
                              (setf d (min d most) c (min c d)))))))
                  (layout-box (aref children i) context a b c d))
                (place (i x y)
                  (let ((child (aref children i)))
                    (setf (box:box-x child) x
                          (box:box-y child) y)))
                (flex-of (i)
                  (values (box:box-flex (aref children i)) t)))
           (declare (dynamic-extent #'measure #'place #'flex-of))
           (multiple-value-bind (w h)
               (if (eq (box:box-kind box) :stage)
                   ;; What is on a stage is laid out at the stage's own size, whatever the room;
                   ;; the stage is no larger than the room, and cuts off what does not fit.
                   (let ((w (box:box-pref-w box)) (h (box:box-pref-h box)))
                     (core:layout-flex :vertical w w h h (length children) #'measure #'place
                                       :flex-of #'flex-of :gap (box:box-gap box)
                                       :main-align (box:box-main-align box)
                                       :cross-align (box:box-cross-align box) :main-size :max)
                     (values (core:constrain w min-w max-w) (core:constrain h min-h max-h)))
               (core:layout-flex (if (or (eq (box:box-kind box) :row)
                                         (and (eq (box:box-kind box) :split) (zerop (box:box-n3 box))))
                                     :horizontal :vertical)
                                 min-w max-w min-h max-h (length children)
                                 #'measure #'place
                                 :flex-of #'flex-of :gap (box:box-gap box)
                                 :main-align (box:box-main-align box) :cross-align (box:box-cross-align box)
                                 :main-size (if (box:box-fill box) :max :min)))
             (set-box box w h)))))
      (:scroll
       ;; What is inside is a column as tall as it wants; the window onto it is HEIGHT (PREF-H) tall if
       ;; that was given, and otherwise as tall as what is inside, within the constraints, and as wide
       ;; as it may be.
       (let ((children (box:box-children box))
             (state (box:box-data box)))
         (flet ((measure (i a b c d)
                  (layout-box (aref children i) context a b c d))
                (place (i x y)
                  (let ((child (aref children i)))
                    (setf (box:box-x child) x
                          (box:box-y child) y)))
                (flex-of (i)
                  (declare (ignore i))
                  (values 0f0 t)))
           (declare (dynamic-extent #'measure #'place #'flex-of))
           (multiple-value-bind (content-w content-h)
               (core:layout-flex :vertical min-w max-w 0f0 core:+unbounded+ (length children)
                                 #'measure #'place :flex-of #'flex-of :main-size :min)
             ;; As wide as it is allowed to be, as a lazy list is, so that the bar at its edge is
             ;; beside what is inside and not over it; as wide as what is inside where nothing limits it.
             (let* ((w (if (< max-w core:+unbounded+) max-w (core:constrain content-w min-w max-w)))
                    (h (core:constrain (if (plusp (box:box-pref-h box)) (box:box-pref-h box) content-h)
                                       min-h max-h))
                    ;; At the end of what is inside, if it is following, else where it was.
                    (offset (if (widgets:scroll-state-following state)
                                (max 0f0 (- content-h h))
                                (max 0f0 (min (box:box-n1 box) (max 0f0 (- content-h h)))))))
               (setf (widgets:scroll-state-offset state) offset
                     (widgets:scroll-state-content-height state) content-h
                     (widgets:scroll-state-viewport-height state) h)
               (loop :for child :across children
                     :do (decf (box:box-y child) offset))
               (set-box box w h))))))
      (:lazy-list
       ;; The rows that were described are each ROW-HEIGHT (N2) tall and placed by their index (N3);
       ;; the whole is COUNT (N3) of them.
       (let* ((children (box:box-children box))
              (state (box:box-data box))
              (row-height (box:box-n2 box))
              (content-h (* (box:box-n3 box) row-height))
              (w (if (plusp (box:box-pref-w box))
                     (core:constrain (box:box-pref-w box) min-w max-w)
                     (if (< max-w core:+unbounded+)
                         max-w
                         (let ((widest min-w))
                           (loop :for child :across children
                                 :do (setf widest (max widest (layout-box child context 0f0 max-w
                                                                          row-height row-height))))
                           widest))))
              (h (core:constrain (if (plusp (box:box-pref-h box))
                                     (box:box-pref-h box)
                                     (min content-h widgets:+lazy-list-height+))
                                 min-h max-h))
              (offset (max 0f0 (min (box:box-n1 box) (max 0f0 (- content-h h))))))
         (setf (widgets:scroll-state-offset state) offset
               (widgets:scroll-state-content-height state) content-h
               (widgets:scroll-state-viewport-height state) h)
         ;; In doubles: a million rows are tens of millions of points, where a single float's step is
         ;; more than a point, and rows next to each other would overlap.
         (loop :for child :across children
               :do (layout-box child context w w row-height row-height)
                   (setf (box:box-x child) 0f0
                         (box:box-y child) (float (- (* (box:box-n3 child) (float row-height 1d0))
                                                      (float offset 1d0))
                                                   1f0)))
         ;; The rows were chosen for the window the build thought there would be; if the window is
         ;; taller, or somewhere else, the next frame describes the rows it shows.
         (let ((first (if (plusp (length children)) (box:box-n3 (aref children 0)) 0))
               (end (if (plusp (length children))
                        (1+ (box:box-n3 (aref children (1- (length children)))))
                        0)))
           (when (or (> (* first row-height) offset)
                     (< (* end row-height) (min content-h (+ offset h))))
             (core:request-frame context)))
         (set-box box w h)))
      (:outline
       (set-box box (core:constrain (box:box-pref-w box) min-w max-w) (core:constrain (box:box-pref-h box) min-h max-h)))
      ((:padding :panel :pressable :tree-row)
       (let ((children (box:box-children box)))
         (flet ((measure (a b c d)
                  (layout-box (aref children 0) context a b c d)))
           (declare (dynamic-extent #'measure))
           (multiple-value-bind (w h x y)
               (core:layout-padding min-w max-w min-h max-h (and (plusp (length children)) #'measure)
                                    :left (box:box-pad-left box) :top (box:box-pad-top box)
                                    :right (box:box-pad-right box) :bottom (box:box-pad-bottom box))
             (when (plusp (length children))
               (setf (box:box-x (aref children 0)) x
                     (box:box-y (aref children 0)) y))
             (set-box box w h)))))
      (:center
       (let ((children (box:box-children box)))
         (flet ((measure (a b c d)
                  (layout-box (aref children 0) context a b c d)))
           (declare (dynamic-extent #'measure))
           (multiple-value-bind (w h x y)
               (core:layout-align min-w max-w min-h max-h (and (plusp (length children)) #'measure))
             (when (plusp (length children))
               (setf (box:box-x (aref children 0)) x
                     (box:box-y (aref children 0)) y))
             (set-box box w h)))))
      (:stack
       (let ((width 0f0) (height 0f0))
         (loop :for child :across (box:box-children box)
               :do (multiple-value-bind (w h) (layout-box child context 0f0 max-w 0f0 max-h)
                     (setf (box:box-x child) 0f0 (box:box-y child) 0f0
                           width (max width w) height (max height h))))
         (set-box box (core:constrain width min-w max-w) (core:constrain height min-h max-h)))))))
