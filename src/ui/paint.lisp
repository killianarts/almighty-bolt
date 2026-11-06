(defpackage #:almighty-bolt/ui/paint
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:widgets #:almighty-bolt/ui/widgets)
                    (#:theme #:almighty-bolt/ui/theme))
  (:export #:paint-box))

(in-package #:almighty-bolt/ui/paint)

;;; Drawing the boxes of a frame. A box that is a thing to look at (a label, a button, a
;;; field, ...) is drawn into a list of its own, relative to itself, and what it drew is
;;; kept by its key together with the hash of what decided how it looked; the next frame
;;; that has the same box with the same hash puts the kept commands where the box now is and
;;; draws nothing. That is the cache of an immediate-mode interface: it is not told what
;;; changed, it compares what the interface said.

(defstruct (entry (:constructor make-entry ()))
  (hash -1 :type fixnum)
  ;; What layout made of the box besides its size (the lines of a paragraph).
  (salt 0 :type fixnum)
  ;; The size of the box when the fragment was drawn: a box that a flex has made wider is not
  ;; the box that was drawn, though it has the same hash.
  (width -1f0 :type single-float)
  (height -1f0 :type single-float)
  (fragment (core:make-display-list :capacity 64)))

(declaim (inline snap-to-pixel))
(defun snap-to-pixel (position scale)
  (declare (type single-float position scale))
  (* (/ 1f0 scale)
     (round (the (single-float -1f6 1f6) (* scale position)))))

(defun look-state (box)
  "The look of BOX as :normal, :hover, :pressed or :disabled, and whether it has the keyboard."
  (let ((look (box:box-look box)))
    (values (case (mod look 4) (0 :normal) (1 :hover) (2 :pressed) (t :disabled))
            (>= look 4))))

;;; --- drawing one box, relative to itself ---

(defun draw-outlined-box (list x y w h fill focused)
  (when focused
    (core:push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme:theme-radius theme:*theme*) 2)
                            (theme:theme-ring theme:*theme*)))
  (core:push-rounded-rect list x y w h (theme:theme-radius theme:*theme*) (theme:theme-field-border theme:*theme*))
  (core:push-rounded-rect list (+ x 1) (+ y 1) (- w 2) (- h 2) (- (theme:theme-radius theme:*theme*) 1) fill))

(defun line-metrics (context box)
  "How tall a line of BOX's text is, and how far down it its baseline is (see TEXT-METRICS)."
  (widgets:text-metrics context (widgets:ui-face box)))

(defun fit-text (context font string width)
  (let ((end (core:context-index-at-x context font string width)))
    (loop :while (and (plusp end) (> (core:context-caret-x context font string end) width))
          :do (decf end))
    (subseq string 0 end)))

(defun shown-selection (box)
  "The text state of the selectable text BOX if some of it is selected and it has the keyboard, which
is when its selection shows."
  (let ((selection (box:box-data box)))
    (and (= (box:box-n2 box) 1)
         (widgets:selection-state-p selection)
         (nth-value 1 (look-state box))
         (core:text-state-selection-p (widgets:selection-state-state selection))
         (widgets:selection-state-state selection))))

(defun draw-selection-on-line (context list font text start end state x top height)
  "The selection of STATE where it is on the line of TEXT from START to END, drawn at X, TOP."
  (let ((from (max start (core:text-state-selection-start state)))
        (to (min end (core:text-state-selection-end state))))
    (when (< from to)
      (let ((line (subseq text start end)))
        (flet ((x-at (index) (+ x (core:context-caret-x context font line (- index start)))))
          (core:push-rect list (x-at from) top (- (x-at to) (x-at from)) height
                          (theme:theme-selection theme:*theme*)))))))

(defun draw-label (box context list x y)
  (let* ((font (widgets:box-text-font context box))
         (text (box:box-text box))
         (selected (shown-selection box)))
    (multiple-value-bind (step baseline) (line-metrics context box)
      (when selected
        (draw-selection-on-line context list font text 0 (length text) selected x y step))
      (let ((color (or (box:box-color box) (theme:theme-text theme:*theme*))))
        ;; Turned, the line's top is the box's left edge (up, read from the bottom) or its right
        ;; (down, read from the top).
        (case (round (box:box-n1 box))
          (0 (core:context-push-text context list font text x (+ y baseline) color))
          (1 (core:context-push-text context list font text (+ x baseline) (+ y (box:box-h box)) color :up))
          (2 (core:context-push-text context list font text (+ x (- step baseline)) y color :down)))))))

(defun draw-paragraph (box context list x y)
  (let* ((state (box:box-data box))
         (text (widgets:paragraph-state-text state))
         (font (widgets:box-text-font context box))
         (selected (shown-selection box)))
    (multiple-value-bind (step baseline) (line-metrics context box)
      (loop :for (start end) :in (widgets:paragraph-state-lines state)
            :for line :from 0
            :for top = (+ y (* line step))
            :do (when selected
                  (draw-selection-on-line context list font text start end selected x top step))
                (core:context-push-text context list font (subseq text start end) x (+ top baseline)
                                        (or (box:box-color box) (theme:theme-text theme:*theme*)))))))

(defun draw-button (box context list x y)
  (let ((w (box:box-w box)) (h (box:box-h box)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (core:push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme:theme-radius theme:*theme*) 2)
                                (theme:theme-ring theme:*theme*)))
      (core:push-rounded-rect list x y w h (theme:theme-radius theme:*theme*) (theme:button-color theme:*theme* state))
      (let ((font (widgets:ui-font context box)))
        (multiple-value-bind (width ascent descent) (core:context-measure context font (box:box-text box))
          (declare (ignore width descent))
          (core:context-push-text context list font (box:box-text box) (+ x 14) (+ y 7 ascent)
                                  (if (eq state :disabled)
                                      (theme:theme-dim-text theme:*theme*)
                                      (theme:theme-button-text theme:*theme*))))))))

(defun draw-text-field (box context list x y)
  (let* ((w (box:box-w box)) (h (box:box-h box))
         (field (box:box-data box))
         (state (widgets:shown-text-state field (widgets:field-state-state field)))
         (font (widgets:ui-font context box))
         (string (core:text-state-string state))
         (start (widgets:field-state-view-start field))
         (focused (nth-value 1 (look-state box)))
         (enabled (box:box-enabled box)))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (draw-outlined-box list x y w h
                         (cond ((not enabled) (theme:theme-field-disabled theme:*theme*))
                               ((plusp (box:box-n3 box)) (theme:theme-field-invalid theme:*theme*))
                               (t (theme:theme-field-fill theme:*theme*)))
                         focused)
      (let* ((side (theme:theme-field-padding-x theme:*theme*))
             (pad (theme:theme-field-padding-y theme:*theme*))
             (inner (- w (* 2 side)))
             (shown (fit-text context font (subseq string start) inner))
             (end (+ start (length shown)))
             (left (+ x side)))
        (flet ((x-at (index)
                 (+ left (core:context-caret-x context font shown
                                               (max 0 (min (length shown) (- index start)))))))
          (when (and (core:text-state-selection-p state) focused)
            (let ((from (max start (core:text-state-selection-start state)))
                  (to (min end (core:text-state-selection-end state))))
              (when (< from to)
                (core:push-rect list (x-at from) (+ y pad -2) (- (x-at to) (x-at from))
                                (max 0f0 (- h (* 2 (- pad 2))))
                                (theme:theme-selection theme:*theme*)))))
          (core:context-push-text context list font shown left (+ y pad ascent)
                                  (if enabled (theme:theme-text theme:*theme*) (theme:theme-dim-text theme:*theme*)))
          ;; What the input method is composing, underlined, and the part it has selected more so.
          (multiple-value-bind (from to chosen-from chosen-to)
              (widgets:marked-range field (widgets:field-state-state field))
            (when from
              (flet ((underline (a b thickness)
                       (let ((a (max a start)) (b (min b end)))
                         (when (< a b)
                           (core:push-rect list (x-at a) (+ y pad ascent 2) (- (x-at b) (x-at a))
                                           thickness (theme:theme-text theme:*theme*))))))
                (underline from to 1f0)
                (underline chosen-from chosen-to 2f0))))
          (when (and focused (not (core:text-state-selection-p state))
                     (<= start (core:text-state-caret state) end))
            (core:push-rect list (x-at (core:text-state-caret state)) (+ y pad -1) 1.5f0
                            (max 0f0 (- h (* 2 (- pad 1))))
                            (theme:theme-caret theme:*theme*))))))))

(defun draw-text-area (box context list x y)
  (let* ((w (box:box-w box)) (h (box:box-h box))
         (area (box:box-data box))
         (state (widgets:shown-text-state area (widgets:area-state-state area)))
         (string (core:text-state-string state))
         (lines (widgets:area-state-lines area))
         (offset (widgets:area-state-offset area))
         (font (widgets:ui-font context box))
         (focused (nth-value 1 (look-state box)))
         (enabled (box:box-enabled box))
         (selecting (and focused (core:text-state-selection-p state)))
         (from (core:text-state-selection-start state))
         (to (core:text-state-selection-end state))
         (gutter (widgets:area-state-gutter area))
         (numbered (and (widgets:area-state-numbered area) (plusp gutter)))
         (marks (widgets:area-state-marks area))
         ;; The numbers end a gap (as wide as the padding) before the lines start.
         (numbers-right (+ x gutter))
         (left (+ x (theme:theme-field-padding-x theme:*theme*) gutter))
         (pad (theme:theme-field-padding-y theme:*theme*))
         (number 0))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (draw-outlined-box list x y w h
                         (if enabled (theme:theme-field-fill theme:*theme*) (theme:theme-field-disabled theme:*theme*))
                         focused)
      (core:push-clip list (+ x 1) (+ y 1) (- w 2) (- h 2))
      (when numbered
        ;; A rule between the numbers and the lines, halfway along the gap.
        (core:push-rect list (+ numbers-right (floor (theme:theme-field-padding-x theme:*theme*) 2)) (+ y 1)
                        1f0 (- h 2) (theme:theme-field-border theme:*theme*)))
      (loop :for line :across lines
            :for row :from 0
            :for top = (- (+ y pad (* row height)) offset)
            ;; A row that starts a line has its number; one where a long line goes on has none.
            :for starts = (or (zerop (car line))
                              (char= (char string (1- (car line))) #\Linefeed))
            :do (when starts (incf number))
            :when (and (< top (+ y h)) (> (+ top height) y))
              :do (let* ((start (car line))
                         (end (cdr line))
                         (text (subseq string start end))
                         (mark (and numbered (assoc number marks))))
                    (when mark
                      ;; The band behind the line, and a bar of the mark's colour at its left.
                      (core:push-rect list (+ x 1) top (- w 2) height (third mark))
                      (core:push-rect list (+ x 1) top 3f0 height (second mark)))
                    (when (and numbered starts)
                      (let ((digits (princ-to-string number)))
                        (core:context-push-text context list font digits
                                                (- numbers-right
                                                   (core:context-caret-x context font digits (length digits)))
                                                (+ top ascent)
                                                (if mark (theme:theme-text theme:*theme*) (theme:theme-dim-text theme:*theme*)))))
                    (flet ((x-at (index)
                             (+ left (core:context-caret-x context font text (- index start)))))
                      (when (and selecting (< from (1+ end)) (> to start))
                        (let* ((a (max from start))
                               (b (min to end))
                               ;; A selection that goes on past the end of the line takes a little
                               ;; more, for the line feed.
                               (more (if (> to end) 4f0 0f0)))
                          (when (or (< a b) (plusp more))
                            (core:push-rect list (x-at a) top (+ (- (x-at (max a b)) (x-at a)) more)
                                            height (theme:theme-selection theme:*theme*)))))
                      (core:context-push-text context list font text left (+ top ascent)
                                              (if enabled (theme:theme-text theme:*theme*) (theme:theme-dim-text theme:*theme*)))
                      ;; What the input method is composing, underlined where it is on this line.
                      (multiple-value-bind (marked-from marked-to chosen-from chosen-to)
                          (widgets:marked-range area (widgets:area-state-state area))
                        (when marked-from
                          (flet ((underline (a b thickness)
                                   (let ((a (max a start)) (b (min b end)))
                                     (when (< a b)
                                       (core:push-rect list (x-at a) (+ top ascent 2) (- (x-at b) (x-at a))
                                                       thickness (theme:theme-text theme:*theme*))))))
                            (underline marked-from marked-to 1f0)
                            (underline chosen-from chosen-to 2f0)))))))
      (when (and focused (not (core:text-state-selection-p state)) (plusp (length lines)))
        (let* ((caret (core:text-state-caret state))
               (row (widgets:area-line-of lines caret))
               (line (svref lines row))
               (top (- (+ y pad (* row height)) offset)))
          (core:push-rect list (+ left (core:context-caret-x context font (subseq string (car line) (cdr line))
                                                             (- caret (car line))))
                          (+ top 1) 1.5f0 (- height 2) (theme:theme-caret theme:*theme*))))
      (core:push-unclip list))))

(defun draw-slider (box list x y)
  (let* ((w (box:box-w box)) (h (box:box-h box))
         (thumb-x (+ x (/ widgets:+thumb-size+ 2) (* (max 1f0 (- w widgets:+thumb-size+)) (box:box-n1 box))))
         (middle (+ y (/ h 2))))
    (core:push-rounded-rect list x (- middle 3) w 6 3 (theme:theme-track theme:*theme*))
    (core:push-rounded-rect list x (- middle 3) (- thumb-x x) 6 3 (theme:theme-row-selected theme:*theme*))
    (when (nth-value 1 (look-state box))
      (core:push-rounded-rect list (- thumb-x (/ widgets:+thumb-size+ 2) 2) (- middle (/ widgets:+thumb-size+ 2) 2)
                              (+ widgets:+thumb-size+ 4) (+ widgets:+thumb-size+ 4) (+ (/ widgets:+thumb-size+ 2) 2)
                              (theme:theme-ring theme:*theme*)))
    (core:push-rounded-rect list (- thumb-x (/ widgets:+thumb-size+ 2)) (- middle (/ widgets:+thumb-size+ 2))
                            widgets:+thumb-size+ widgets:+thumb-size+ (/ widgets:+thumb-size+ 2) (theme:theme-thumb theme:*theme*))))

(defun draw-progress (box list x y)
  (let* ((w (box:box-w box)) (h (box:box-h box))
         (filled (* w (box:box-n1 box))))
    (core:push-rounded-rect list x y w h (* 0.5f0 h) (theme:theme-track theme:*theme*))
    (when (> filled 0f0)
      (core:push-rounded-rect list x y (max filled h) h (* 0.5f0 h) (theme:theme-thumb theme:*theme*)))))

(defun draw-segments (box context list x y)
  "A choice drawn as joined segments: one rounded field, the chosen segment filled in the selected
row's colour with its words in the button's text colour, and a line between the others."
  (let* ((font (widgets:ui-font context box))
         (options (car (box:box-data box)))
         (state (cdr (box:box-data box)))
         (h (widgets:choice-state-option-height state))
         (w (box:box-w box))
         (radius (theme:theme-radius theme:*theme*))
         (selected (box:box-n3 box))
         (focused (nth-value 1 (look-state box))))
    (when focused
      (core:push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ radius 2) (theme:theme-ring theme:*theme*)))
    (core:push-rounded-rect list x y w h radius (theme:theme-field-border theme:*theme*))
    (core:push-rounded-rect list (+ x 1) (+ y 1) (- w 2) (- h 2) (max 0 (1- radius))
                            (theme:theme-field-fill theme:*theme*))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (loop :for option :in options
            :for i :from 0
            :do (let ((left (+ x (svref (widgets:choice-state-lefts state) i)))
                      (width (svref (widgets:choice-state-widths state) i)))
                  (cond ((= i selected)
                         (core:push-rounded-rect list (+ left 2) (+ y 2) (- width 4) (- h 4)
                                                 (max 0 (- radius 2)) (theme:theme-row-selected theme:*theme*)))
                        ;; A line between two segments, neither of them the chosen one.
                        ((and (plusp i) (/= (1- i) selected))
                         (core:push-rect list left (+ y 5) 1 (- h 10) (theme:theme-field-border theme:*theme*))))
                  (core:context-push-text context list font option
                                          (+ left (/ (- width (core:context-measure context font option)) 2))
                                          (+ y (/ (- h (+ ascent 4f0)) 2) ascent)
                                          (cond ((not (box:box-enabled box)) (theme:theme-dim-text theme:*theme*))
                                                ((= i selected) (theme:theme-button-text theme:*theme*))
                                                (t (theme:theme-text theme:*theme*)))))))))

(defun draw-choice (box context list x y)
  (case (box:box-main-align box)
    (:segmented (draw-segments box context list x y))
    (:tabs (draw-tabs box context list x y))
    (t (draw-radio-choice box context list x y))))

(defun draw-tabs (box context list x y)
  "The tabs of a tab bar: a hairline along the bottom, each tab's words, dim but for the chosen one's,
which has a bar of the selection's colour under it (the ring's when the bar has the keyboard)."
  (let* ((font (widgets:ui-font context box))
         (options (car (box:box-data box)))
         (state (cdr (box:box-data box)))
         (h (widgets:choice-state-option-height state))
         (selected (box:box-n3 box))
         (focused (nth-value 1 (look-state box))))
    (core:push-rect list x (+ y h -1) (box:box-w box) 1 (theme:theme-field-border theme:*theme*))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (loop :for option :in options
            :for i :from 0
            :do (let ((left (+ x (svref (widgets:choice-state-lefts state) i)))
                      (width (svref (widgets:choice-state-widths state) i)))
                  (when (= i selected)
                    (core:push-rect list (+ left 4) (+ y h -3) (- width 8) 3
                                    (if focused (theme:theme-ring theme:*theme*) (theme:theme-row-selected theme:*theme*))))
                  (core:context-push-text context list font option
                                          (+ left (/ (- width (core:context-measure context font option)) 2))
                                          (+ y (/ (- h (+ ascent 4f0)) 2) ascent)
                                          (if (and (= i selected) (box:box-enabled box))
                                              (theme:theme-text theme:*theme*)
                                              (theme:theme-dim-text theme:*theme*))))))))

(defun draw-radio-choice (box context list x y)
  (let* ((font (widgets:ui-font context box))
         (options (car (box:box-data box)))
         (state (cdr (box:box-data box)))
         (h (widgets:choice-state-option-height state))
         (selected (box:box-n3 box))
         (focused (nth-value 1 (look-state box))))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (loop :for option :in options
            :for i :from 0
            :do (let* ((left (+ x (svref (widgets:choice-state-lefts state) i)))
                       (row (+ y (svref (widgets:choice-state-tops state) i)))
                       (top (+ row (/ (- h widgets:+dot-size+) 2))))
                  (when (and focused (= i selected))
                    (core:push-rounded-rect list (- left 2) (- top 2) (+ widgets:+dot-size+ 4) (+ widgets:+dot-size+ 4)
                                            (+ (/ widgets:+dot-size+ 2) 2) (theme:theme-ring theme:*theme*)))
                  (core:push-rounded-rect list left top widgets:+dot-size+ widgets:+dot-size+ (/ widgets:+dot-size+ 2)
                                          (theme:theme-field-border theme:*theme*))
                  (core:push-rounded-rect list (+ left 1) (+ top 1) (- widgets:+dot-size+ 2) (- widgets:+dot-size+ 2)
                                          (/ (- widgets:+dot-size+ 2) 2) (theme:theme-field-fill theme:*theme*))
                  (when (= i selected)
                    (core:push-rounded-rect list (+ left 4) (+ top 4) (- widgets:+dot-size+ 8) (- widgets:+dot-size+ 8)
                                            (/ (- widgets:+dot-size+ 8) 2) (theme:theme-row-selected theme:*theme*)))
                  (core:context-push-text context list font option (+ left widgets:+dot-size+ 6f0)
                                          (+ row (/ (- h (+ ascent 4f0)) 2) ascent)
                                          (if (box:box-enabled box) (theme:theme-text theme:*theme*) (theme:theme-dim-text theme:*theme*))))))))

(defun draw-dropdown (box context list x y)
  "A button that shows its option, with the arrows of a pop-up button at the right."
  (let ((w (box:box-w box)) (h (box:box-h box))
        (font (widgets:ui-font context box)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (core:push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme:theme-radius theme:*theme*) 2)
                                (theme:theme-ring theme:*theme*)))
      (core:push-rounded-rect list x y w h (theme:theme-radius theme:*theme*)
                              (theme:button-color theme:*theme* (if (eql (box:box-n3 box) 1) :pressed state)))
      (multiple-value-bind (width ascent) (core:context-measure context font (box:box-text box))
        (declare (ignore width))
        (core:context-push-text context list font (fit-text context font (box:box-text box) (- w 44))
                                (+ x 14) (+ y 7 ascent)
                                (if (box:box-enabled box) (theme:theme-button-text theme:*theme*) (theme:theme-dim-text theme:*theme*)))
        ;; Two small arrows, one over the other.
        (let ((cx (+ x w -16)) (cy (+ y (/ h 2))))
          (loop :for i :from 0 :below 4
                :do (core:push-rect list (- cx i) (- cy 2 (- 3 i)) (+ 1 (* 2 i)) 1 (theme:theme-button-text theme:*theme*))
                    (core:push-rect list (- cx i) (+ cy 2 (- 3 i)) (+ 1 (* 2 i)) 1 (theme:theme-button-text theme:*theme*))))))))

(defun draw-menu-button (box context list x y)
  "A button whose words are followed by an arrow down, for the menu it opens."
  (let ((w (box:box-w box)) (h (box:box-h box))
        (font (widgets:ui-font context box)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (core:push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme:theme-radius theme:*theme*) 2)
                                (theme:theme-ring theme:*theme*)))
      (core:push-rounded-rect list x y w h (theme:theme-radius theme:*theme*)
                              (theme:button-color theme:*theme* (if (eql (box:box-n3 box) 1) :pressed state)))
      (multiple-value-bind (width ascent) (core:context-measure context font (box:box-text box))
        (declare (ignore width))
        (core:context-push-text context list font (box:box-text box) (+ x 14) (+ y 7 ascent)
                                (if (box:box-enabled box) (theme:theme-button-text theme:*theme*) (theme:theme-dim-text theme:*theme*)))
        (let ((cx (+ x w -16)) (cy (+ y (/ h 2) -1)))
          (loop :for i :from 0 :below 4
                :do (core:push-rect list (- cx (- 3 i)) (+ cy i) (+ 1 (* 2 (- 3 i))) 1
                                    (theme:theme-button-text theme:*theme*))))))))

(defun draw-tooltip (box context list x y)
  "Small words on a light background, with a thin edge."
  (let ((font (core:context-font context (theme:theme-font-name theme:*theme*) 13)))
    (core:push-rounded-rect list (- x 1) (- y 1) (+ (box:box-w box) 2) (+ (box:box-h box) 2) 5
                            (theme:theme-field-border theme:*theme*))
    (core:push-rounded-rect list x y (box:box-w box) (box:box-h box) 4 (theme:theme-tooltip theme:*theme*))
    (multiple-value-bind (width ascent) (core:context-measure context font (box:box-text box))
      (declare (ignore width))
      (core:context-push-text context list font (box:box-text box) (+ x (theme:spaced 8)) (+ y (theme:spaced 4) ascent)
                              (theme:theme-tooltip-text theme:*theme*)))))

(defun draw-toast (box context list x y)
  "Words on a light rounded background, as a tooltip but larger."
  (let ((font (widgets:ui-font context box)))
    (core:push-rounded-rect list x y (box:box-w box) (box:box-h box) 10 (theme:theme-tooltip theme:*theme*))
    (multiple-value-bind (width ascent) (core:context-measure context font (box:box-text box))
      (declare (ignore width))
      (core:context-push-text context list font (box:box-text box) (+ x (theme:spaced 16)) (+ y (theme:spaced 10) ascent)
                              (theme:theme-tooltip-text theme:*theme*)))))

(defun draw-menu-item (box context list x y)
  "A row of a menu: the highlight under it if it has it, the mark of the chosen one, and its words."
  (let ((font (widgets:ui-font context box))
        (highlighted (eql (box:box-n3 box) 1)))
    (when highlighted
      (core:push-rounded-rect list x y (box:box-w box) (box:box-h box) 4 (theme:theme-row-selected theme:*theme*)))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (let ((baseline (+ y (/ (- (box:box-h box) height) 2) ascent)))
        (when (= (box:box-n2 box) 1f0)
          (core:context-push-text context list font "✓" (+ x 6) baseline (theme:theme-text theme:*theme*)))
        (core:context-push-text context list font (box:box-text box) (+ x widgets:+menu-check-width+) baseline
                                (if highlighted (theme:theme-button-text theme:*theme*) (theme:theme-text theme:*theme*)))))))

(defun draw-switch (box context list x y)
  "A track as wide as +SWITCH-WIDTH+ and half as tall, filled with the accent when on, and a knob at
the end it is at."
  (let* ((h (/ widgets:+switch-width+ 2))
         (top (+ y (/ (- (box:box-h box) h) 2)))
         (on (eql (box:box-n3 box) 1))
         (enabled (box:box-enabled box))
         (focused (nth-value 1 (look-state box)))
         (knob (- h 4)))
    (when focused
      (core:push-rounded-rect list (- x 2) (- top 2) (+ widgets:+switch-width+ 4) (+ h 4) (+ (/ h 2) 2)
                              (theme:theme-ring theme:*theme*)))
    (core:push-rounded-rect list x top widgets:+switch-width+ h (/ h 2)
                            (if on (theme:theme-row-selected theme:*theme*) (theme:theme-track theme:*theme*)))
    (core:push-rounded-rect list (if on (+ x widgets:+switch-width+ (- knob) -2) (+ x 2)) (+ top 2) knob knob
                            (/ knob 2) (if enabled (theme:theme-thumb theme:*theme*) (theme:theme-dim-text theme:*theme*)))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (core:context-push-text context list (widgets:ui-font context box) (box:box-text box) (+ x widgets:+switch-width+ 8f0)
                              (+ y (/ (- (box:box-h box) (+ ascent 4f0)) 2) ascent)
                              (if enabled (theme:theme-text theme:*theme*) (theme:theme-dim-text theme:*theme*))))))

(defun draw-spinner (box list x y)
  "+SPINNER-STEPS+ dots round a ring, the one at the step the spinner is at the brightest and those
behind it fading."
  (let* ((size (min (box:box-w box) (box:box-h box)))
         (dot (max 2f0 (* size 0.22f0)))
         (radius (/ (- size dot) 2))
         (cx (+ x (/ (box:box-w box) 2)))
         (cy (+ y (/ (box:box-h box) 2)))
         (step (box:box-n3 box)))
    (dotimes (i widgets:+spinner-steps+)
      (let* ((angle (* 2 pi (/ i widgets:+spinner-steps+)))
             (behind (mod (- step i) widgets:+spinner-steps+))
             (alpha (max 40 (- 255 (* behind 30)))))
        (core:push-rounded-rect list (- (+ cx (* radius (float (sin angle) 1f0))) (/ dot 2))
                                (- (- cy (* radius (float (cos angle) 1f0))) (/ dot 2))
                                dot dot (/ dot 2)
                                (logior (logand (theme:theme-text theme:*theme*) #xFFFFFF00) alpha))))))

(defun draw-list-box (box context list x y)
  (let* ((font (widgets:ui-font context box))
         (w (box:box-w box)) (h (box:box-h box))
         (items (car (box:box-data box)))
         (state (cdr (box:box-data box)))
         (top (widgets:list-state-top state))
         (rows (box:box-n3 box))
         (count (length items))
         (selected (round (box:box-n2 box)))
         (row-height (theme:theme-row-height theme:*theme*)))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (draw-outlined-box list x y w h (theme:theme-field-fill theme:*theme*) (nth-value 1 (look-state box)))
      (loop :for item :in (nthcdr top items)
            :for index :from top
            :for i :from 0 :below rows
            :do (let ((row-y (+ y 1 (* i row-height))))
                  (when (= index selected)
                    (core:push-rounded-rect list (+ x 3) row-y (- w 6 (if (> count rows) 8 0))
                                            row-height 4 (theme:theme-row-selected theme:*theme*)))
                  (core:context-push-text context list font (fit-text context font item (- w 24))
                                          (+ x 10) (+ row-y (/ (- row-height (+ ascent 4)) 2) ascent)
                                          ;; On the highlight, as a menu's highlighted row is.
                                          (if (= index selected)
                                              (theme:theme-button-text theme:*theme*)
                                              (theme:theme-text theme:*theme*)))))
      (when (> count rows)
        (let* ((track (- h 8))
               (bar (max 12f0 (* track (/ (float rows 1f0) count))))
               (travel (- track bar))
               (max-top (max 1 (- count rows)))
               (position (* travel (/ (float top 1f0) max-top))))
          (core:push-rounded-rect list (+ x w -8) (+ y 4 position) 4 bar 2 (theme:theme-field-border theme:*theme*)))))))

(defun draw-checkbox (box context list x y)
  (let ((top (+ y (/ (- (box:box-h box) widgets:+dot-size+) 2)))
        (focused (nth-value 1 (look-state box))))
    (draw-outlined-box list x top widgets:+dot-size+ widgets:+dot-size+ (theme:theme-field-fill theme:*theme*) focused)
    (when (eql (box:box-n3 box) 1)
      (core:push-rounded-rect list (+ x 4) (+ top 4) (- widgets:+dot-size+ 8) (- widgets:+dot-size+ 8) 2f0
                              (theme:theme-row-selected theme:*theme*)))
    (multiple-value-bind (height ascent) (line-metrics context box)
      (declare (ignore height))
      (core:context-push-text context list (widgets:ui-font context box) (box:box-text box) (+ x widgets:+dot-size+ 6f0)
                              (+ y (/ (- (box:box-h box) (+ ascent 4f0)) 2) ascent)
                              (if (box:box-enabled box) (theme:theme-text theme:*theme*) (theme:theme-dim-text theme:*theme*))))))

(defun draw-leaf (box context list x y)
  (ecase (box:box-kind box)
    (:label (draw-label box context list x y))
    (:paragraph (draw-paragraph box context list x y))
    (:button (draw-button box context list x y))
    ((:text-field :combo-box) (draw-text-field box context list x y))
    (:text-area (draw-text-area box context list x y))
    (:slider (draw-slider box list x y))
    (:progress (draw-progress box list x y))
    (:image (core:context-push-image context list (box:box-data box) x y (box:box-w box) (box:box-h box)
                                     :tint (box:box-color box) :opacity (box:box-n1 box)))
    (:choice (draw-choice box context list x y))
    (:checkbox (draw-checkbox box context list x y))
    (:switch (draw-switch box context list x y))
    (:dropdown (draw-dropdown box context list x y))
    (:menu-button (draw-menu-button box context list x y))
    (:menu-item (draw-menu-item box context list x y))
    (:tooltip (draw-tooltip box context list x y))
    (:toast (draw-toast box context list x y))
    (:spinner (draw-spinner box list x y))
    (:splitter (draw-splitter box list x y))
    (:list-box (draw-list-box box context list x y))))

;; The divider of a split: a hairline down (or across) its middle, the ring's colour and wider under
;; the pointer, pressed, or with the keyboard.
(defun draw-splitter (box list x y)
  (multiple-value-bind (state focused) (look-state box)
    (let* ((w (box:box-w box)) (h (box:box-h box))
           (strong (or focused (member state '(:hover :pressed))))
           (thick (if strong 2f0 1f0))
           (color (if strong (theme:theme-ring theme:*theme*) (theme:theme-field-border theme:*theme*))))
      (if (zerop (box:box-n3 box))
          (core:push-rect list (+ x (/ (- w thick) 2)) y thick h color)
          (core:push-rect list x (+ y (/ (- h thick) 2)) w thick color)))))

(defun draw-menu-bar (box list x y)
  "The bar at the right edge of a menu that shows some of its rows, which says where they are among
them all: (BOX-N3 BOX) rows, (BOX-N2 BOX) shown from the row (BOX-N1 BOX)."
  (let ((count (box:box-n3 box))
        (shown (box:box-n2 box))
        (top (box:box-n1 box)))
    (when (< 0 shown count)
      (let* ((track (- (box:box-h box) 8))
             (bar (max 12f0 (* track (/ shown count))))
             (position (* (- track bar) (/ top (max 1 (- count shown))))))
        (core:push-rounded-rect list (+ x (box:box-w box) -6) (+ y 4 position) 3 bar 1.5
                                (theme:theme-field-border theme:*theme*))))))

(defun draw-scroll-bar (box list x y)
  "The bar at the right edge of the scroll container BOX that says how much of what is inside shows,
and where: drawn only when not all of it does."
  (let* ((state (box:box-data box))
         (h (box:box-h box))
         (content (widgets:scroll-state-content-height state)))
    (when (> content h)
      (let* ((track (- h 8))
             (bar (max 12f0 (* track (/ h content))))
             (position (* (- track bar) (/ (widgets:scroll-state-offset state) (- content h)))))
        (core:push-rounded-rect list (+ x (box:box-w box) -8) (+ y 4 position) 4 bar 2
                                (theme:theme-field-border theme:*theme*))))))

;;; --- the cache ---

(defun paint-cached (ui box context list x y)
  "Draw the leaf BOX at X, Y: from what was kept for it if its hash is the same as when it was
kept, and otherwise afresh, which is then kept."
  (let* ((key (box:box-key box))
         (hash (box:box-paint-hash box))
         (entry (or (gethash key (ui:ui-paint-cache ui))
                    (setf (gethash key (ui:ui-paint-cache ui)) (make-entry))))
         (salt (case (box:box-kind box)
                 (:paragraph (widgets:paragraph-state-generation (box:box-data box)))
                 (:text-area (widgets:area-state-generation (box:box-data box)))
                 (t 0)))
         (fragment (entry-fragment entry)))
    (unless (and (= (entry-hash entry) hash)
                 (= (entry-salt entry) salt)
                 (= (entry-width entry) (box:box-w box))
                 (= (entry-height entry) (box:box-h box)))
      (loop
        (core:clear-display-list fragment)
        (handler-case (progn (draw-leaf box context fragment 0f0 0f0)
                             (return))
          (core:display-list-full ()
            (setf fragment (core:make-display-list :capacity (* 2 (core:display-list-capacity fragment)))
                  (entry-fragment entry) fragment))))
      (setf (entry-hash entry) hash
            (entry-salt entry) salt
            (entry-width entry) (box:box-w box)
            (entry-height entry) (box:box-h box)))
    (let ((scale (core:context-scale context)))
      (core:append-display-list list fragment (snap-to-pixel x scale) (snap-to-pixel y scale)))))

(defun paint-box (ui box context list x y)
  "Draw BOX, whose parent is at X, Y in the window, and everything in it."
  (let ((theme:*theme* (box:box-theme box)))
    (let ((x (+ x (box:box-x box)))
          (y (+ y (box:box-y box))))
      (ecase (box:box-kind box)
        ((:label :paragraph :button :text-field :combo-box :text-area :slider :progress :image :choice :checkbox :switch :spinner
          :list-box :dropdown :menu-button :menu-item :tooltip
          :toast :splitter)
         (paint-cached ui box context list x y))
        (:canvas
         (let ((scale (core:context-scale context))
               (fragment (box:box-data box)))
           (let ((x (snap-to-pixel x scale)) (y (snap-to-pixel y scale)))
             (core:push-clip list x y (box:box-w box) (box:box-h box))
             (core:append-display-list list fragment x y)
             (core:push-unclip list))))
        (:scrim
         (core:push-rect list x y (box:box-w box) (box:box-h box) (theme:theme-scrim theme:*theme*)))
        ((:catcher :spacer))
        (:toasts (paint-children ui box context list x y))
        ((:popover :dialog)
         (core:push-rounded-rect list (- x 1) (- y 1) (+ (box:box-w box) 2) (+ (box:box-h box) 2) 11
                                 (theme:theme-field-border theme:*theme*))
         (core:push-rounded-rect list x y (box:box-w box) (box:box-h box) 10 (theme:theme-panel theme:*theme*))
         (paint-children ui box context list x y))
        (:menu
         (core:push-rounded-rect list (- x 1) (- y 1) (+ (box:box-w box) 2) (+ (box:box-h box) 2) 7
                                 (theme:theme-field-border theme:*theme*))
         (core:push-rounded-rect list x y (box:box-w box) (box:box-h box) 6 (theme:theme-panel theme:*theme*))
         (paint-children ui box context list x y)
         (draw-menu-bar box list x y))
        ((:pane :stage)
         ;; A pane of a split, and a stage, cut what does not fit at their edges.
         (core:push-clip list x y (box:box-w box) (box:box-h box))
         (paint-contents ui box context list x y)
         (core:push-unclip list))
        ((:scroll :lazy-list)
         (core:push-clip list x y (box:box-w box) (box:box-h box))
         (paint-children ui box context list x y)
         (core:push-unclip list)
         (paint-ring-past-edges ui box context list x y)
         (draw-scroll-bar box list x y))
        (:outline
         (let ((w (box:box-w box)) (h (box:box-h box)) (color (box:box-color box)))
           (if (eql (box:box-n1 box) 1f0)
               ;; Corner brackets, two points wide, just outside the edges, each arm ten long
               ;; (or a third of a side that is short).
               (let* ((l (- x 2)) (top (- y 2)) (r (+ x w)) (b (+ y h))
                      (across (min 10f0 (/ (+ w 4) 3))) (down (min 10f0 (/ (+ h 4) 3))))
                 (core:push-rect list l top across 2 color)
                 (core:push-rect list l top 2 down color)
                 (core:push-rect list (- (+ r 2) across) top across 2 color)
                 (core:push-rect list r top 2 down color)
                 (core:push-rect list l b across 2 color)
                 (core:push-rect list l (- (+ b 2) down) 2 down color)
                 (core:push-rect list (- (+ r 2) across) b across 2 color)
                 (core:push-rect list r (- (+ b 2) down) 2 down color))
               ;; A frame two points wide, inside the edges.
               (progn
                 (core:push-rect list x y w 2 color)
                 (core:push-rect list x (+ y h -2) w 2 color)
                 (core:push-rect list x y 2 h color)
                 (core:push-rect list (+ x w -2) y 2 h color)))))
        (:panel
         (draw-shadows box list x y)
         (core:push-rounded-rect list x y (box:box-w box) (box:box-h box) (box:box-n1 box)
                                 (or (box:box-color box) (theme:theme-panel theme:*theme*)))
         (draw-border box list x y)
         (paint-contents ui box context list x y))
        (:tree
         (when (nth-value 1 (look-state box))
           (core:push-rounded-stroke list (- x 2) (- y 2) (+ (box:box-w box) 4) (+ (box:box-h box) 4)
                                     (theme:theme-radius theme:*theme*) 2 (theme:theme-ring theme:*theme*)))
         (paint-contents ui box context list x y))
        (:tree-row
         (draw-tree-row box list x y)
         (paint-contents ui box context list x y))
        (:pressable
         (draw-shadows box list x y)
         (draw-pressable box list x y)
         (draw-border box list x y)
         (paint-contents ui box context list x y))
        ((:column :row :padding :center :stack :context-area :grid :surface :drag-source :split)
         (paint-contents ui box context list x y))))))

;; A row of a tree view: the selection's colour if its item is selected, the hover colour under the
;; pointer, and before its words the mark of an item with children, pointing right while they are
;; hidden and down while they are shown.
(defun draw-tree-row (box list x y)
  (let ((w (box:box-w box)) (h (box:box-h box))
        (state (look-state box)))
    (cond ((= (box:box-n3 box) 1)
           (core:push-rounded-rect list x y w h 4 (theme:theme-row-selected theme:*theme*)))
          ((member state '(:hover :pressed))
           (core:push-rounded-rect list x y w h 4 (theme:theme-row-hover theme:*theme*))))
    (unless (zerop (box:box-n2 box))
      (let ((cx (+ x (box:box-pad-left box) -10)) (cy (+ y (/ h 2)))
            (color (theme:theme-dim-text theme:*theme*)))
        (if (= (box:box-n2 box) 1)
            (loop :for i :from 0 :below 4
                  :do (core:push-rect list (+ cx i) (- cy (- 3 i)) 1 (+ 1 (* 2 (- 3 i))) color))
            (loop :for i :from 0 :below 4
                  :do (core:push-rect list (- cx (- 3 i)) (+ cy i -1) (+ 1 (* 2 (- 3 i))) 1 color)))))))

(defun draw-pressable (box list x y)
  "The background of a pressable: the theme's selected row for a toggle that is on, a button's colours
for the look of a button, or its own colour (the panel's if none), mixed toward the text colour under
the pointer and more so pressed; with the ring when it has the keyboard."
  (multiple-value-bind (state focused) (look-state box)
    (let* ((w (box:box-w box)) (h (box:box-h box)) (radius (box:box-n1 box))
           (fill (cond ((= (box:box-n3 box) 2) (theme:theme-row-selected theme:*theme*))
                       ((= (box:box-n2 box) 1f0) (theme:button-color theme:*theme* state))
                       (t (let ((base (or (box:box-color box) (theme:theme-panel theme:*theme*))))
                            (case state
                              (:hover (theme:mix-colors base (theme:theme-text theme:*theme*) 0.08))
                              (:pressed (theme:mix-colors base (theme:theme-text theme:*theme*) 0.16))
                              (t base)))))))
      (when focused
        (core:push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ radius 2) (theme:theme-ring theme:*theme*)))
      (core:push-rounded-rect list x y w h radius fill))))

(defun draw-shadows (box list x y)
  "The shadows of a panel or a pressable, under it: each its rounded rectangle moved, grown by its
spread (and its corners with it), and blurred."
  (loop :for (dx dy blur spread color) :in (box:box-shadows box)
        :do (core:push-shadow list (+ x dx (- spread)) (+ y dy (- spread))
                              (max 0f0 (+ (box:box-w box) spread spread)) (max 0f0 (+ (box:box-h box) spread spread))
                              (max 0f0 (+ (box:box-n1 box) spread)) blur color)))

(defun draw-border (box list x y)
  "The edge of a panel or a pressable given a :border-color, inside it, dashed if it is given a
:border-dash."
  (when (and (box:box-border-color box) (plusp (box:box-border-width box)))
    (let ((dash (box:box-border-dash box)))
      (core:push-rounded-stroke list x y (box:box-w box) (box:box-h box) (box:box-n1 box) (box:box-border-width box)
                                (box:box-border-color box) (car dash) (cdr dash)))))

(defun paint-contents (ui box context list x y)
  "Draw what is in the container BOX. One given a greatest size (:height, :max-width ...) cuts off
what is too large for it, as a scroll container does with what is out of its window."
  (if (or (>= (box:box-max-w box) 0f0) (>= (box:box-max-h box) 0f0))
      (progn
        (core:push-clip list x y (box:box-w box) (box:box-h box))
        (paint-children ui box context list x y)
        (core:push-unclip list))
      (paint-children ui box context list x y)))

;;; A scroll container or a lazy list clips what is in it to its edges, so that what is out of its
;;; window does not show; but the ring round what has the keyboard is drawn +RING-REACH+ points
;;; outside it, and at an edge that would be cut off. So after what is inside, the one that has the
;;; keyboard is drawn again, clipped to the band +RING-REACH+ wide just outside each edge that it is
;;; wholly inside of: nothing else of it can be there, so what shows there is its ring. An edge it
;;; reaches past (it is scrolled half out of the window) gets nothing, as the ring is cut off with
;;; the rest of it. The kept drawing is used, so this draws nothing new.

(defun focused-inside (box x y)
  "The box under BOX, whose parent is at X, Y, that has the keyboard, and the X and Y of its parent,
or NIL. A scroll container or a lazy list inside is not looked in, as it draws its own band."
  (loop :for child :across (box:box-children box)
        :do (when (nth-value 1 (look-state child))
              (return-from focused-inside (values child x y)))
            (unless (member (box:box-kind child) '(:scroll :lazy-list))
              (multiple-value-bind (found fx fy)
                  (focused-inside child (+ x (box:box-x child)) (+ y (box:box-y child)))
                (when found (return-from focused-inside (values found fx fy))))))
  nil)

(defun paint-ring-past-edges (ui box context list x y)
  "Draw the ring of what has the keyboard inside BOX, a scroll container or a lazy list at X, Y in the
window, where it reaches past BOX's edges."
  (multiple-value-bind (focused px py) (focused-inside box x y)
    (when focused
      (let* ((r widgets:+ring-reach+)
             (w (box:box-w box)) (h (box:box-h box))
             (fx (+ px (box:box-x focused))) (fy (+ py (box:box-y focused)))
             (left (>= fx x)) (right (<= (+ fx (box:box-w focused)) (+ x w)))
             (top (>= fy y)) (bottom (<= (+ fy (box:box-h focused)) (+ y h)))
             ;; The bands above and below reach round the corners on a side the ring shows at.
             (band-left (if left (- x r) x))
             (band-width (- (if right (+ x w r) (+ x w)) band-left)))
        (flet ((band (bx by bw bh)
                 (core:push-clip list bx by bw bh)
                 (paint-box ui focused context list px py)
                 (core:push-unclip list)))
          (when top (band band-left (- y r) band-width r))
          (when bottom (band band-left (+ y h) band-width r))
          (when left (band (- x r) y r h))
          (when right (band (+ x w) y r h)))))))

(defun paint-children (ui box context list x y)
  (loop :for child :across (box:box-children box)
        :do (paint-box ui child context list x y)))
