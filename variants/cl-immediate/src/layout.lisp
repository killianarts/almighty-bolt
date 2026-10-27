(defpackage #:almighty-bolt-variant-immediate/layout
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-break-lines
                #:layout-padding
                #:layout-flex
                #:layout-align
                #:constrain
                #:+unbounded+)
  (:import-from #:almighty-bolt-variant-immediate/box
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
                #:box-n3
                #:box-data
                #:box-children
                #:box-x
                #:box-y
                #:box-w
                #:box-h)
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:ui-font
                #:paragraph-state-text
                #:paragraph-state-lines
                #:paragraph-state-wrapped-at
                #:set-paragraph-lines
                #:choice-state-lefts
                #:choice-state-widths
                #:+row-height+
                #:+dot-size+
                #:+option-gap+)
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+font-name+)
  (:export #:layout-box))

(in-package #:almighty-bolt-variant-immediate/layout)

;;; Where the boxes of a frame go, worked out after the frame is described, with the same
;;; layout functions as the other variants (decision record 0010): constraints down, sizes
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
     (let* ((options (car (box-data box)))
            (state (cdr (box-data box)))
            (count (length options))
            (lefts (make-array count))
            (widths (make-array count))
            (left 0f0)
            (font (ui-font context)))
       (loop :for option :in options
             :for i :from 0
             :do (let ((text-width (context-measure context font option)))
                   (setf (svref lefts i) left
                         (svref widths i) (+ +dot-size+ 6f0 (float text-width 1f0)))
                   (incf left (+ (svref widths i) +option-gap+))))
       (setf (choice-state-lefts state) lefts
             (choice-state-widths state) widths)
       (set-box box (constrain (max 0f0 (- left +option-gap+)) min-w max-w)
                (constrain (max 22f0 +dot-size+) min-h max-h))))
    (:checkbox
     (set-box box (constrain (+ +dot-size+ 6f0 (float (context-measure context (ui-font context)
                                                                        (box-text box))
                                                       1f0))
                             min-w max-w)
              (constrain 22f0 min-h max-h)))
    (:scrim
     (set-box box (if (< max-w +unbounded+) max-w min-w) (if (< max-h +unbounded+) max-h min-h)))
    ((:column :row)
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
