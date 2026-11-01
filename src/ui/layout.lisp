(defpackage #:almighty-bolt/ui/layout
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:context-font
                #:context-measure
                #:context-break-lines
                #:layout-padding
                #:layout-flex
                #:layout-align
                #:constrain
                #:request-frame
                #:+unbounded+)
  (:import-from #:almighty-bolt/ui/box
                #:box-kind
                #:box-text
                #:box-size
                #:box-flex
                #:box-gap
                #:box-main-align
                #:box-cross-align
                #:box-fill
                #:box-pad-left
                #:box-pad-top
                #:box-pad-right
                #:box-pad-bottom
                #:box-pref-w
                #:box-pref-h
                #:box-n1
                #:box-n2
                #:box-n3
                #:box-data
                #:box-children
                #:box-x
                #:box-y
                #:box-w
                #:box-h)
  (:import-from #:almighty-bolt/ui/widgets
                #:ui-font
                #:paragraph-state-text
                #:paragraph-state-lines
                #:paragraph-state-wrapped-at
                #:set-paragraph-lines
                #:scroll-state-offset
                #:scroll-state-content-height
                #:scroll-state-viewport-height
                #:+lazy-list-height+
                #:area-lines
                #:area-line-height
                #:area-state-offset
                #:area-state-inner-width
                #:area-state-inner-height
                #:+area-padding+
                #:+field-padding+
                #:choice-state-lefts
                #:choice-state-widths
                #:choice-state-tops
                #:choice-state-option-height
                #:+option-row-gap+
                #:+switch-width+
                #:+menu-row-height+
                #:+menu-padding+
                #:+menu-check-width+
                #:+row-height+
                #:+dot-size+
                #:+option-gap+)
  (:import-from #:almighty-bolt/ui/theme
                #:+font-name+)
  (:export #:layout-box))

(in-package #:almighty-bolt/ui/layout)

;;; Where the boxes of a frame go, worked out after the frame is described, with the layout
;;; functions of src/layout: constraints down, sizes
;;; up, the parent places its children. The boxes are children of their parents in a vector,
;;; and the functions are given each child by its index.

(defconstant +button-padding-x+ 14f0)
(defconstant +button-padding-y+ 7f0)

(defun set-box (box w h)
  (setf (box-w box) w
        (box-h box) h)
  (values w h))

(defun line-height (context)
  (multiple-value-bind (width ascent descent) (context-measure context (ui-font context) "Hg")
    (declare (ignore width))
    (+ ascent descent)))

(defun layout-box (box context min-w max-w min-h max-h)
  "Work out the size of BOX within the constraints and the places of its children, store
them in the boxes, and return the width and height."
  (declare (type single-float min-w max-w min-h max-h))
  (ecase (box-kind box)
    (:label
     (let ((font (context-font context +font-name+ (box-size box))))
       (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
         (set-box box
                  (constrain (float width 1f0) min-w max-w)
                  (constrain (float (+ ascent descent) 1f0) min-h max-h)))))
    (:paragraph
     (let* ((state (box-data box))
            (text (box-text box))
            (font (context-font context +font-name+ (box-size box)))
            (wrap (if (< max-w +unbounded+) max-w 100000f0)))
       ;; Broken again only if the text or the width is not what the lines were made for.
       (unless (and (= (paragraph-state-wrapped-at state) wrap)
                    (string= (paragraph-state-text state) text))
         (set-paragraph-lines state text wrap (context-break-lines context font text wrap)))
       (multiple-value-bind (width ascent descent) (context-measure context font "Hg")
         (declare (ignore width))
         (let ((widest 0f0))
           (loop :for (start end) :in (paragraph-state-lines state)
                 :do (setf widest (max widest (float (context-measure context font
                                                                      (subseq text start end))
                                                     1f0))))
           (set-box box (constrain widest min-w max-w)
                    (constrain (* (length (paragraph-state-lines state))
                                  (float (+ ascent descent) 1f0))
                               min-h max-h))))))
    (:button
     (let ((font (ui-font context)))
       (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
         (set-box box
                  (constrain (+ (float width 1f0) (* 2 +button-padding-x+)) min-w max-w)
                  (constrain (+ (float (+ ascent descent) 1f0) (* 2 +button-padding-y+))
                             min-h max-h)))))
    (:text-field
     (set-box box (constrain (box-pref-w box) min-w max-w)
              (constrain (+ (float (line-height context) 1f0) 12f0) min-h max-h)))
    (:text-area
     ;; ROWS (N3) lines tall; the lines the text is shown in are made for the width it is given.
     (let* ((area (box-data box))
            (height (area-line-height context))
            (w (constrain (box-pref-w box) min-w max-w))
            (h (constrain (+ (* (box-n3 box) height) (* 2 +area-padding+)) min-h max-h))
            (inner-w (max 1f0 (- w (* 2 +field-padding+))))
            (inner-h (max 0f0 (- h (* 2 +area-padding+))))
            (lines (area-lines context area (box-text box) inner-w)))
       (setf (area-state-inner-width area) inner-w
             (area-state-inner-height area) inner-h
             (area-state-offset area) (max 0f0 (min (area-state-offset area)
                                                     (max 0f0 (- (* (length lines) height) inner-h)))))
       (set-box box w h)))
    (:slider
     (set-box box (constrain (box-pref-w box) min-w max-w) (constrain 24f0 min-h max-h)))
    (:progress
     (set-box box (constrain (box-pref-w box) min-w max-w) (constrain 14f0 min-h max-h)))
    (:canvas
     (set-box box (constrain (box-pref-w box) min-w max-w)
              (constrain (box-pref-h box) min-h max-h)))
    (:list-box
     (set-box box (constrain (box-pref-w box) min-w max-w)
              (constrain (+ 2f0 (* +row-height+ (box-n3 box))) min-h max-h)))
    (:choice
     ;; Side by side, or (MAIN-ALIGN :vertical) one under another, each option 22 points tall.
     (let* ((options (car (box-data box)))
            (state (cdr (box-data box)))
            (vertical (eq (box-main-align box) :vertical))
            (count (length options))
            (lefts (make-array count))
            (tops (make-array count))
            (widths (make-array count))
            (left 0f0)
            (top 0f0)
            (widest 0f0)
            (height (max 22f0 +dot-size+))
            (font (ui-font context)))
       (loop :for option :in options
             :for i :from 0
             :do (let ((width (+ +dot-size+ 6f0 (float (context-measure context font option) 1f0))))
                   (setf (svref lefts i) left
                         (svref tops i) top
                         (svref widths i) width
                         widest (max widest width))
                   (if vertical
                       (incf top (+ height +option-row-gap+))
                       (incf left (+ width +option-gap+)))))
       (setf (choice-state-lefts state) lefts
             (choice-state-tops state) tops
             (choice-state-widths state) widths
             (choice-state-option-height state) height)
       (if vertical
           (set-box box (constrain widest min-w max-w)
                    (constrain (max 0f0 (- top +option-row-gap+)) min-h max-h))
           (set-box box (constrain (max 0f0 (- left +option-gap+)) min-w max-w)
                    (constrain height min-h max-h)))))
    (:menu-button
     (let ((font (ui-font context)))
       (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
         (set-box box
                  (constrain (+ (float width 1f0) (* 2 +button-padding-x+) 16f0) min-w max-w)
                  (constrain (+ (float (+ ascent descent) 1f0) (* 2 +button-padding-y+))
                             min-h max-h)))))
    (:dropdown
     ;; A button's padding, and room at the right for the arrows; as wide as WIDTH (PREF-W) if given.
     (let ((font (ui-font context)))
       (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
         (set-box box
                  (constrain (if (plusp (box-pref-w box))
                                 (box-pref-w box)
                                 (+ (float width 1f0) (* 2 +button-padding-x+) 16f0))
                             min-w max-w)
                  (constrain (+ (float (+ ascent descent) 1f0) (* 2 +button-padding-y+))
                             min-h max-h)))))
    (:menu
     ;; Rows one under another, as wide as the widest (or the least width the menu was given), inside
     ;; the menu's padding.
     (let* ((children (box-children box))
            (widest (max 0f0 (- min-w (* 2 +menu-padding+)))))
       (loop :for row :across children
             :do (setf widest (max widest (layout-box row context 0f0 (- max-w (* 2 +menu-padding+))
                                                      +menu-row-height+ +menu-row-height+))))
       (loop :for row :across children
             :for i :from 0
             :do (layout-box row context widest widest +menu-row-height+ +menu-row-height+)
                 (setf (box-x row) +menu-padding+
                       (box-y row) (+ +menu-padding+ (* i +menu-row-height+))))
       (set-box box (constrain (+ widest (* 2 +menu-padding+)) min-w max-w)
                (constrain (+ (* (length children) +menu-row-height+) (* 2 +menu-padding+))
                           min-h max-h))))
    (:menu-item
     (set-box box (constrain (+ +menu-check-width+ 16f0
                                (float (context-measure context (ui-font context) (box-text box)) 1f0))
                             min-w max-w)
              (constrain +menu-row-height+ min-h max-h)))
    (:catcher
     (set-box box (if (< max-w +unbounded+) max-w min-w) (if (< max-h +unbounded+) max-h min-h)))
    (:switch
     (set-box box (constrain (+ +switch-width+ 8f0 (float (context-measure context (ui-font context)
                                                                            (box-text box))
                                                           1f0))
                             min-w max-w)
              (constrain 22f0 min-h max-h)))
    (:spinner
     (set-box box (constrain (box-pref-w box) min-w max-w) (constrain (box-pref-h box) min-h max-h)))
    (:checkbox
     (set-box box (constrain (+ +dot-size+ 6f0 (float (context-measure context (ui-font context)
                                                                        (box-text box))
                                                       1f0))
                             min-w max-w)
              (constrain 22f0 min-h max-h)))
    (:scrim
     (set-box box (if (< max-w +unbounded+) max-w min-w) (if (< max-h +unbounded+) max-h min-h)))
    ((:column :row :context-area)
     (let ((children (box-children box)))
       (flet ((measure (i a b c d)
                (layout-box (aref children i) context a b c d))
              (place (i x y)
                (let ((child (aref children i)))
                  (setf (box-x child) x
                        (box-y child) y)))
              (flex-of (i)
                (values (box-flex (aref children i)) t)))
         (declare (dynamic-extent #'measure #'place #'flex-of))
         (multiple-value-bind (w h)
             (layout-flex (if (eq (box-kind box) :row) :horizontal :vertical)
                          min-w max-w min-h max-h (length children)
                          #'measure #'place
                          :flex-of #'flex-of :gap (box-gap box)
                          :main-align (box-main-align box) :cross-align (box-cross-align box)
                          :main-size (if (box-fill box) :max :min))
           (set-box box w h)))))
    (:scroll
     ;; What is inside is a column as tall as it wants; the window onto it is HEIGHT (PREF-H) tall if
     ;; that was given, and otherwise as tall as what is inside, within the constraints, and as wide
     ;; as it may be.
     (let ((children (box-children box))
           (state (box-data box)))
       (flet ((measure (i a b c d)
                (layout-box (aref children i) context a b c d))
              (place (i x y)
                (let ((child (aref children i)))
                  (setf (box-x child) x
                        (box-y child) y)))
              (flex-of (i)
                (declare (ignore i))
                (values 0f0 t)))
         (declare (dynamic-extent #'measure #'place #'flex-of))
         (multiple-value-bind (content-w content-h)
             (layout-flex :vertical min-w max-w 0f0 +unbounded+ (length children)
                          #'measure #'place :flex-of #'flex-of :main-size :min)
           ;; As wide as it is allowed to be, as a lazy list is, so that the bar at its edge is
           ;; beside what is inside and not over it; as wide as what is inside where nothing limits it.
           (let* ((w (if (< max-w +unbounded+) max-w (constrain content-w min-w max-w)))
                  (h (constrain (if (plusp (box-pref-h box)) (box-pref-h box) content-h)
                                min-h max-h))
                  (offset (max 0f0 (min (box-n1 box) (max 0f0 (- content-h h))))))
             (setf (scroll-state-offset state) offset
                   (scroll-state-content-height state) content-h
                   (scroll-state-viewport-height state) h)
             (loop :for child :across children
                   :do (decf (box-y child) offset))
             (set-box box w h))))))
    (:lazy-list
     ;; The rows that were described are each ROW-HEIGHT (N2) tall and placed by their index (N3);
     ;; the whole is COUNT (N3) of them.
     (let* ((children (box-children box))
            (state (box-data box))
            (row-height (box-n2 box))
            (content-h (* (box-n3 box) row-height))
            (w (if (plusp (box-pref-w box))
                   (constrain (box-pref-w box) min-w max-w)
                   (if (< max-w +unbounded+)
                       max-w
                       (let ((widest min-w))
                         (loop :for child :across children
                               :do (setf widest (max widest (layout-box child context 0f0 max-w
                                                                        row-height row-height))))
                         widest))))
            (h (constrain (if (plusp (box-pref-h box))
                              (box-pref-h box)
                              (min content-h +lazy-list-height+))
                          min-h max-h))
            (offset (max 0f0 (min (box-n1 box) (max 0f0 (- content-h h))))))
       (setf (scroll-state-offset state) offset
             (scroll-state-content-height state) content-h
             (scroll-state-viewport-height state) h)
       ;; In doubles: a million rows are tens of millions of points, where a single float's step is
       ;; more than a point, and rows next to each other would overlap.
       (loop :for child :across children
             :do (layout-box child context w w row-height row-height)
                 (setf (box-x child) 0f0
                       (box-y child) (float (- (* (box-n3 child) (float row-height 1d0))
                                                (float offset 1d0))
                                             1f0)))
       ;; The rows were chosen for the window the build thought there would be; if the window is
       ;; taller, or somewhere else, the next frame describes the rows it shows.
       (let ((first (if (plusp (length children)) (box-n3 (aref children 0)) 0))
             (end (if (plusp (length children))
                      (1+ (box-n3 (aref children (1- (length children)))))
                      0)))
         (when (or (> (* first row-height) offset)
                   (< (* end row-height) (min content-h (+ offset h))))
           (request-frame context)))
       (set-box box w h)))
    ((:padding :panel)
     (let ((children (box-children box)))
       (flet ((measure (a b c d)
                (layout-box (aref children 0) context a b c d)))
         (declare (dynamic-extent #'measure))
         (multiple-value-bind (w h x y)
             (layout-padding min-w max-w min-h max-h (and (plusp (length children)) #'measure)
                             :left (box-pad-left box) :top (box-pad-top box)
                             :right (box-pad-right box) :bottom (box-pad-bottom box))
           (when (plusp (length children))
             (setf (box-x (aref children 0)) x
                   (box-y (aref children 0)) y))
           (set-box box w h)))))
    (:center
     (let ((children (box-children box)))
       (flet ((measure (a b c d)
                (layout-box (aref children 0) context a b c d)))
         (declare (dynamic-extent #'measure))
         (multiple-value-bind (w h x y)
             (layout-align min-w max-w min-h max-h (and (plusp (length children)) #'measure))
           (when (plusp (length children))
             (setf (box-x (aref children 0)) x
                   (box-y (aref children 0)) y))
           (set-box box w h)))))
    (:stack
     (let ((width 0f0) (height 0f0))
       (loop :for child :across (box-children box)
             :do (multiple-value-bind (w h) (layout-box child context 0f0 max-w 0f0 max-h)
                   (setf (box-x child) 0f0 (box-y child) 0f0
                         width (max width w) height (max height h))))
       (set-box box (constrain width min-w max-w) (constrain height min-h max-h))))))
