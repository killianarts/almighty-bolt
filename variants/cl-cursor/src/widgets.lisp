(defpackage #:almighty-bolt-variant-cursor/widgets
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-break-lines
                #:context-push-text
                #:context-scale
                #:clear-display-list
                #:append-display-list
                #:push-rounded-rect)
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+font-name+
                #:+text-color+
                #:+panel-color+)
  (:import-from #:almighty-bolt-variant-cursor/ui
                #:*ui*
                #:ui-mode
                #:ui-context
                #:ui-list
                #:ui-leaf
                #:ui-region
                #:region-width
                #:region-height
                #:region-cursor-x
                #:region-cursor-y
                #:region-pending-x
                #:region-pending-y
                #:region-used-w
                #:region-used-h
                #:region-direction
                #:peek-position
                #:place
                #:open-region
                #:close-region
                #:note-rect
                #:remembered-size
                #:ui-measure-state
                #:measure-pass
                #:note-measured
                #:fragment-mark
                #:begin-fragment
                #:end-fragment
                #:place-fragment
                #:snap-to-pixel)
  (:export #:label
           #:paragraph
           #:text-width
           #:line-height
           #:avail-width
           #:avail-height
           #:room-width
           #:skip
           #:move-to-x
           #:split-sizes
           #:call-with-region
           #:call-with-panel
           #:call-centered
           #:with-column
           #:with-row
           #:with-panel
           #:centered))

(in-package #:almighty-bolt-variant-cursor/widgets)

;;; Widgets that draw where the cursor is as they are called. A widget that is a leaf knows
;;; its size before it draws (it can measure its text), so it takes its place and draws. A
;;; container does not: its size is what its children take, and they have not been called
;;; yet. WITH-PANEL and CENTERED are where that shows.

(defun font-of (size)
  (context-font (ui-context *ui*) +font-name+ size))

(defun text-width (text &key (size 15))
  "How wide TEXT is, before it is drawn."
  (float (context-measure (ui-context *ui*) (font-of size) text) 1f0))

(defun line-height (&key (size 15))
  (multiple-value-bind (width ascent descent)
      (context-measure (ui-context *ui*) (font-of size) "Hg")
    (declare (ignore width))
    (float (+ ascent descent) 1f0)))

(defun avail-width ()
  "The room across, from the cursor to the end of the current region."
  (let ((region (ui-region *ui*)))
    (- (region-width region)
       (if (eq (region-direction region) :row) (region-cursor-x region) 0f0))))

(defun avail-height ()
  "The room down, from the cursor to the end of the current region."
  (let ((region (ui-region *ui*)))
    (- (region-height region)
       (if (eq (region-direction region) :column) (region-cursor-y region) 0f0))))

(defun room-width ()
  (region-width (ui-region *ui*)))

(defun skip (across down)
  "Move the next child ACROSS and DOWN from where the cursor is."
  (let ((region (ui-region *ui*)))
    (incf (region-pending-x region) (float across 1f0))
    (incf (region-pending-y region) (float down 1f0))
    nil))

(defun move-to-x (x)
  "Put the cursor of a row X from the row's left."
  (setf (region-cursor-x (ui-region *ui*)) (float x 1f0))
  nil)

(defun split-sizes (total gap &rest factors)
  "TOTAL less the GAPS between, shared out in proportion to FACTORS, as a flex shares it."
  (let* ((total (float total 1f0))
         (gap (float gap 1f0))
         (allocated (* gap (float (max 0 (1- (length factors))) 1f0)))
         (total-flex (float (reduce #'+ factors) 1f0))
         (per-flex (/ (max 0f0 (- total allocated)) total-flex)))
    (loop :for factor :in factors
          :collect (* per-flex (float factor 1f0)))))

;;; --- leaves ---

(defun append-leaf (ui x y)
  "Put what is in the leaf list at X, Y, on a whole pixel."
  (let ((scale (context-scale (ui-context ui))))
    (append-display-list (ui-list ui) (ui-leaf ui) (snap-to-pixel x scale) (snap-to-pixel y scale))))

(defun label (text &key id (size 15) color)
  "A line of TEXT at the cursor."
  (let* ((ui *ui*)
         (context (ui-context ui))
         (font (font-of size)))
    (multiple-value-bind (width ascent descent) (context-measure context font text)
      (let ((w (float width 1f0))
            (h (float (+ ascent descent) 1f0)))
        (multiple-value-bind (x y w h) (place ui w h)
          (clear-display-list (ui-leaf ui))
          (context-push-text context (ui-leaf ui) font text 0f0 (+ 0f0 ascent)
                             (or color +text-color+))
          (append-leaf ui x y)
          (note-rect ui id x y w h)))))
  nil)

(defun paragraph (text &key id (size 15) color)
  "TEXT broken into lines to the room across the cursor has, and drawn."
  (let* ((ui *ui*)
         (context (ui-context ui))
         (font (font-of size))
         (wrap (avail-width))
         (lines (context-break-lines context font text wrap)))
    (multiple-value-bind (width ascent descent) (context-measure context font "Hg")
      (declare (ignore width))
      (let ((widest 0f0))
        (loop :for (start end) :in lines
              :do (setf widest (max widest (float (context-measure context font
                                                                   (subseq text start end))
                                                  1f0))))
        (let ((w widest)
              (h (* (length lines) (float (+ ascent descent) 1f0))))
          (multiple-value-bind (x y w h) (place ui w h)
            (clear-display-list (ui-leaf ui))
            (loop :for (start end) :in lines
                  :for line :from 0
                  :do (context-push-text context (ui-leaf ui) font (subseq text start end) 0f0
                                         (+ 0f0 (* line (+ ascent descent)) ascent)
                                         (or color +text-color+)))
            (append-leaf ui x y)
            (note-rect ui id x y w h))))))
  nil)

;;; --- containers ---

(defun call-with-region (thunk direction &key id gap inset width height)
  "A row or a column from the cursor. Its room is WIDTH by HEIGHT or what is left, less INSET
all round; it takes that size in its parent if it was given one, and what its children took
if not."
  (let* ((ui *ui*)
         (parent (ui-region ui))
         (inset (float (or inset 0) 1f0))
         (room-w (float (or width (avail-width)) 1f0))
         (room-h (float (or height (avail-height)) 1f0)))
    (multiple-value-bind (x y) (peek-position parent)
      (open-region ui (+ x inset) (+ y inset) (- room-w (* 2 inset)) (- room-h (* 2 inset))
                   direction (float (or gap 0) 1f0))
      (funcall thunk)
      (let* ((region (close-region ui))
             (w (float (or width (+ (region-used-w region) (* 2 inset))) 1f0))
             (h (float (or height (+ (region-used-h region) (* 2 inset))) 1f0)))
        (multiple-value-bind (px py) (place ui w h)
          (note-rect ui id px py w h)))))
  nil)

(defmacro with-column ((&rest options) &body body)
  `(call-with-region (lambda () ,@body) :column ,@options))

(defmacro with-row ((&rest options) &body body)
  `(call-with-region (lambda () ,@body) :row ,@options))

(defun call-with-panel (thunk &key id (pad 0) gap width height (radius 8) color)
  "A rounded background with a column of children PAD in from its edge. With a WIDTH and a
HEIGHT it is that size. Without, its size is what its children take, which the background
needs before they are drawn: it is the size the build before found for ID, or, in the
fragment mode, the children are drawn into a fragment first and the background after it, or,
in the measure mode, they are run once to find it."
  (let* ((ui *ui*)
         (parent (ui-region ui))
         (pad (float pad 1f0))
         (room-w (float (or width (avail-width)) 1f0))
         (room-h (float (or height (avail-height)) 1f0))
         (color (or color +panel-color+))
         (radius (float radius 1f0))
         (gap (float (or gap 0) 1f0))
         (sized (and width height))
         (measuring-here (and (eq (ui-mode ui) :measure) (not sized) (null (ui-measure-state ui)))))
    (multiple-value-bind (x y) (peek-position parent)
      (when measuring-here
        (multiple-value-bind (used-w used-h)
            (measure-pass ui (- room-w (* 2 pad)) (- room-h (* 2 pad)) gap thunk)
          (note-measured ui id (float (or width (+ used-w (* 2 pad))) 1f0)
                         (float (or height (+ used-h (* 2 pad))) 1f0))))
      (unwind-protect
           (if (and (eq (ui-mode ui) :fragment) (not sized))
               (let* ((outer (ui-list ui))
                      (mark (fragment-mark ui))
                      (fragment (begin-fragment ui (- room-w (* 2 pad)) (- room-h (* 2 pad)) gap)))
                 (funcall thunk)
                 (multiple-value-bind (used-w used-h) (end-fragment ui outer)
                   (let ((w (float (or width (+ used-w (* 2 pad))) 1f0))
                         (h (float (or height (+ used-h (* 2 pad))) 1f0)))
                     (push-rounded-rect (ui-list ui) x y w h radius color)
                     (place-fragment ui fragment (+ x pad) (+ y pad) mark)
                     (multiple-value-bind (px py) (place ui w h)
                       (note-rect ui id px py w h)))))
               (multiple-value-bind (remembered-w remembered-h)
                   (if sized (values 0f0 0f0) (remembered-size ui id))
                 (push-rounded-rect (ui-list ui) x y (float (or width remembered-w) 1f0)
                                    (float (or height remembered-h) 1f0) radius color)
                 (open-region ui (+ x pad) (+ y pad) (- room-w (* 2 pad)) (- room-h (* 2 pad))
                              :column gap)
                 (funcall thunk)
                 (let* ((region (close-region ui))
                        (w (float (or width (+ (region-used-w region) (* 2 pad))) 1f0))
                        (h (float (or height (+ (region-used-h region) (* 2 pad))) 1f0)))
                   (multiple-value-bind (px py) (place ui w h)
                     (note-rect ui id px py w h)))))
        (when measuring-here
          (setf (ui-measure-state ui) nil)))))
  nil)

(defmacro with-panel ((&rest options) &body body)
  `(call-with-panel (lambda () ,@body) ,@options))

(defun call-centered (thunk &key id height)
  "THUNK's one child in the middle of the room across the cursor, and HEIGHT down (what is left
if none). Where the middle is depends on the child's size, which is not known until it is
drawn: the child is drawn where the build before found it to be centred (ID is its id), or, in
the fragment mode, drawn into a fragment and put there after, or, in the measure mode, run
once first to find its size."
  (let* ((ui *ui*)
         (parent (ui-region ui))
         (room-w (float (avail-width) 1f0))
         (room-h (float (or height (avail-height)) 1f0))
         (measuring-here (and (eq (ui-mode ui) :measure) (null (ui-measure-state ui)))))
    (multiple-value-bind (x y) (peek-position parent)
      (if (eq (ui-mode ui) :fragment)
          (let* ((outer (ui-list ui))
                 (mark (fragment-mark ui))
                 (fragment (begin-fragment ui room-w room-h)))
            (funcall thunk)
            (multiple-value-bind (w h) (end-fragment ui outer)
              (place-fragment ui fragment (+ x (* 0.5f0 (- room-w w))) (+ y (* 0.5f0 (- room-h h)))
                              mark)))
          (progn
            (when measuring-here
              (measure-pass ui room-w room-h 0f0 thunk))
            (unwind-protect
                 (progn
                   (open-region ui x y room-w room-h :column 0f0)
                   (multiple-value-bind (w h) (remembered-size ui id)
                     (skip (* 0.5f0 (- room-w w)) (* 0.5f0 (- room-h h))))
                   (funcall thunk)
                   (close-region ui))
              (when measuring-here
                (setf (ui-measure-state ui) nil)))))
      (place ui room-w room-h)))
  nil)

(defmacro centered ((&rest options) &body body)
  `(call-centered (lambda () ,@body) ,@options))
