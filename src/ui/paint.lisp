(defpackage #:almighty-bolt/ui/paint
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:context-font
                #:context-measure
                #:context-caret-x
                #:context-index-at-x
                #:context-push-text
                #:context-scale
                #:push-rect
                #:push-rounded-rect
                #:push-clip
                #:push-unclip
                #:make-display-list
                #:clear-display-list
                #:display-list-capacity
                #:display-list-full
                #:append-display-list
                #:text-state-string
                #:text-state-caret
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end)
  (:import-from #:almighty-bolt/ui/box
                #:box-theme
                #:box-key
                #:box-kind
                #:box-text
                #:box-size
                #:box-color
                #:box-pad-left
                #:box-enabled
                #:box-look
                #:box-n1
                #:box-n2
                #:box-n3
                #:box-data
                #:box-children
                #:box-x
                #:box-y
                #:box-w
                #:box-h
                #:box-paint-hash)
  (:import-from #:almighty-bolt/ui/ui
                #:ui-paint-cache)
  (:import-from #:almighty-bolt/ui/widgets
                #:ui-font
                #:paragraph-state-text
                #:paragraph-state-lines
                #:paragraph-state-generation
                #:selection-state-p
                #:selection-state-state
                #:area-state-state
                #:area-state-offset
                #:area-state-generation
                #:area-state-lines
                #:area-line-of
                #:field-state-state
                #:field-state-view-start
                #:choice-state-lefts
                #:choice-state-tops
                #:choice-state-option-height
                #:+switch-width+
                #:+menu-check-width+
                #:+spinner-steps+
                #:list-state-top
                #:scroll-state-offset
                #:scroll-state-content-height
                #:+thumb-size+
                #:+dot-size+
                #:+ring-reach+)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:theme-background #:theme-panel #:theme-text #:theme-dim-text
                #:theme-button-text #:theme-ring #:theme-field-border #:theme-field-fill
                #:theme-field-invalid #:theme-field-disabled #:theme-selection #:theme-caret
                #:theme-track #:theme-thumb #:theme-row-selected #:theme-scrim #:theme-tooltip
                #:theme-tooltip-text #:theme-font-name #:theme-radius
                #:theme-field-padding-x #:theme-field-padding-y #:theme-row-height #:spaced
                #:button-color)
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
  (fragment (make-display-list :capacity 64)))

(declaim (inline snap-to-pixel))
(defun snap-to-pixel (position scale)
  (declare (type single-float position scale))
  (* (/ 1f0 scale)
     (round (the (single-float -1f6 1f6) (* scale position)))))

(defun look-state (box)
  "The look of BOX as :normal, :hover, :pressed or :disabled, and whether it has the keyboard."
  (let ((look (box-look box)))
    (values (case (mod look 4) (0 :normal) (1 :hover) (2 :pressed) (t :disabled))
            (>= look 4))))

;;; --- drawing one box, relative to itself ---

(defun draw-outlined-box (list x y w h fill focused)
  (when focused
    (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme-radius *theme*) 2)
                       (theme-ring *theme*)))
  (push-rounded-rect list x y w h (theme-radius *theme*) (theme-field-border *theme*))
  (push-rounded-rect list (+ x 1) (+ y 1) (- w 2) (- h 2) (- (theme-radius *theme*) 1) fill))

(defun line-metrics (context)
  (multiple-value-bind (width ascent descent) (context-measure context (ui-font context) "Hg")
    (declare (ignore width))
    (values (+ ascent descent) ascent)))

(defun fit-text (context font string width)
  (let ((end (context-index-at-x context font string width)))
    (loop :while (and (plusp end) (> (context-caret-x context font string end) width))
          :do (decf end))
    (subseq string 0 end)))

(defun shown-selection (box)
  "The text state of the selectable text BOX if some of it is selected and it has the keyboard, which
is when its selection shows."
  (let ((selection (box-data box)))
    (and (= (box-n2 box) 1)
         (selection-state-p selection)
         (nth-value 1 (look-state box))
         (text-state-selection-p (selection-state-state selection))
         (selection-state-state selection))))

(defun draw-selection-on-line (context list font text start end state x top height)
  "The selection of STATE where it is on the line of TEXT from START to END, drawn at X, TOP."
  (let ((from (max start (text-state-selection-start state)))
        (to (min end (text-state-selection-end state))))
    (when (< from to)
      (let ((line (subseq text start end)))
        (flet ((x-at (index) (+ x (context-caret-x context font line (- index start)))))
          (push-rect list (x-at from) top (- (x-at to) (x-at from)) height
                     (theme-selection *theme*)))))))

(defun draw-label (box context list x y)
  (let* ((font (context-font context (theme-font-name *theme*) (box-size box)))
         (text (box-text box))
         (selected (shown-selection box)))
    (multiple-value-bind (width ascent descent) (context-measure context font text)
      (declare (ignore width))
      (when selected
        (draw-selection-on-line context list font text 0 (length text) selected x y
                                (+ ascent descent)))
      (context-push-text context list font text x (+ y ascent)
                         (if (zerop (box-color box)) (theme-text *theme*) (box-color box))))))

(defun draw-paragraph (box context list x y)
  (let* ((state (box-data box))
         (text (paragraph-state-text state))
         (font (context-font context (theme-font-name *theme*) (box-size box)))
         (selected (shown-selection box)))
    (multiple-value-bind (width ascent descent) (context-measure context font "Hg")
      (declare (ignore width))
      (loop :for (start end) :in (paragraph-state-lines state)
            :for line :from 0
            :for top = (+ y (* line (+ ascent descent)))
            :do (when selected
                  (draw-selection-on-line context list font text start end selected x top
                                          (+ ascent descent)))
                (context-push-text context list font (subseq text start end) x (+ top ascent)
                                   (if (zerop (box-color box)) (theme-text *theme*) (box-color box)))))))

(defun draw-button (box context list x y)
  (let ((w (box-w box)) (h (box-h box)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme-radius *theme*) 2)
                           (theme-ring *theme*)))
      (push-rounded-rect list x y w h (theme-radius *theme*) (button-color *theme* state))
      (let ((font (ui-font context)))
        (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
          (declare (ignore width descent))
          (context-push-text context list font (box-text box) (+ x 14) (+ y 7 ascent)
                             (if (eq state :disabled)
                                 (theme-dim-text *theme*)
                                 (theme-button-text *theme*))))))))

(defun draw-text-field (box context list x y)
  (let* ((w (box-w box)) (h (box-h box))
         (field (box-data box))
         (state (field-state-state field))
         (font (ui-font context))
         (string (text-state-string state))
         (start (field-state-view-start field))
         (focused (nth-value 1 (look-state box)))
         (enabled (box-enabled box)))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (draw-outlined-box list x y w h
                         (cond ((not enabled) (theme-field-disabled *theme*))
                               ((plusp (box-n3 box)) (theme-field-invalid *theme*))
                               (t (theme-field-fill *theme*)))
                         focused)
      (let* ((side (theme-field-padding-x *theme*))
             (pad (theme-field-padding-y *theme*))
             (inner (- w (* 2 side)))
             (shown (fit-text context font (subseq string start) inner))
             (end (+ start (length shown)))
             (left (+ x side)))
        (flet ((x-at (index)
                 (+ left (context-caret-x context font shown
                                          (max 0 (min (length shown) (- index start)))))))
          (when (and (text-state-selection-p state) focused)
            (let ((from (max start (text-state-selection-start state)))
                  (to (min end (text-state-selection-end state))))
              (when (< from to)
                (push-rect list (x-at from) (+ y pad -2) (- (x-at to) (x-at from))
                           (max 0f0 (- h (* 2 (- pad 2))))
                           (theme-selection *theme*)))))
          (context-push-text context list font shown left (+ y pad ascent)
                             (if enabled (theme-text *theme*) (theme-dim-text *theme*)))
          (when (and focused (not (text-state-selection-p state))
                     (<= start (text-state-caret state) end))
            (push-rect list (x-at (text-state-caret state)) (+ y pad -1) 1.5f0
                       (max 0f0 (- h (* 2 (- pad 1))))
                       (theme-caret *theme*))))))))

(defun draw-text-area (box context list x y)
  (let* ((w (box-w box)) (h (box-h box))
         (area (box-data box))
         (state (area-state-state area))
         (string (text-state-string state))
         (lines (area-state-lines area))
         (offset (area-state-offset area))
         (font (ui-font context))
         (focused (nth-value 1 (look-state box)))
         (enabled (box-enabled box))
         (selecting (and focused (text-state-selection-p state)))
         (from (text-state-selection-start state))
         (to (text-state-selection-end state))
         (left (+ x (theme-field-padding-x *theme*)))
         (pad (theme-field-padding-y *theme*)))
    (multiple-value-bind (height ascent) (line-metrics context)
      (draw-outlined-box list x y w h
                         (if enabled (theme-field-fill *theme*) (theme-field-disabled *theme*))
                         focused)
      (push-clip list (+ x 1) (+ y 1) (- w 2) (- h 2))
      (loop :for line :across lines
            :for row :from 0
            :for top = (- (+ y pad (* row height)) offset)
            :when (and (< top (+ y h)) (> (+ top height) y))
              :do (let* ((start (car line))
                         (end (cdr line))
                         (text (subseq string start end)))
                    (flet ((x-at (index)
                             (+ left (context-caret-x context font text (- index start)))))
                      (when (and selecting (< from (1+ end)) (> to start))
                        (let* ((a (max from start))
                               (b (min to end))
                               ;; A selection that goes on past the end of the line takes a little
                               ;; more, for the line feed.
                               (more (if (> to end) 4f0 0f0)))
                          (when (or (< a b) (plusp more))
                            (push-rect list (x-at a) top (+ (- (x-at (max a b)) (x-at a)) more)
                                       height (theme-selection *theme*)))))
                      (context-push-text context list font text left (+ top ascent)
                                         (if enabled (theme-text *theme*) (theme-dim-text *theme*))))))
      (when (and focused (not (text-state-selection-p state)) (plusp (length lines)))
        (let* ((caret (text-state-caret state))
               (row (area-line-of lines caret))
               (line (svref lines row))
               (top (- (+ y pad (* row height)) offset)))
          (push-rect list (+ left (context-caret-x context font (subseq string (car line) (cdr line))
                                                   (- caret (car line))))
                     (+ top 1) 1.5f0 (- height 2) (theme-caret *theme*))))
      (push-unclip list))))

(defun draw-slider (box list x y)
  (let* ((w (box-w box)) (h (box-h box))
         (thumb-x (+ x (/ +thumb-size+ 2) (* (max 1f0 (- w +thumb-size+)) (box-n1 box))))
         (middle (+ y (/ h 2))))
    (push-rounded-rect list x (- middle 3) w 6 3 (theme-track *theme*))
    (push-rounded-rect list x (- middle 3) (- thumb-x x) 6 3 (theme-row-selected *theme*))
    (when (nth-value 1 (look-state box))
      (push-rounded-rect list (- thumb-x (/ +thumb-size+ 2) 2) (- middle (/ +thumb-size+ 2) 2)
                         (+ +thumb-size+ 4) (+ +thumb-size+ 4) (+ (/ +thumb-size+ 2) 2)
                         (theme-ring *theme*)))
    (push-rounded-rect list (- thumb-x (/ +thumb-size+ 2)) (- middle (/ +thumb-size+ 2))
                       +thumb-size+ +thumb-size+ (/ +thumb-size+ 2) (theme-thumb *theme*))))

(defun draw-progress (box list x y)
  (let* ((w (box-w box)) (h (box-h box))
         (filled (* w (box-n1 box))))
    (push-rounded-rect list x y w h (* 0.5f0 h) (theme-track *theme*))
    (when (> filled 0f0)
      (push-rounded-rect list x y (max filled h) h (* 0.5f0 h) (theme-thumb *theme*)))))

(defun draw-choice (box context list x y)
  (let* ((font (ui-font context))
         (options (car (box-data box)))
         (state (cdr (box-data box)))
         (h (choice-state-option-height state))
         (selected (box-n3 box))
         (focused (nth-value 1 (look-state box))))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (loop :for option :in options
            :for i :from 0
            :do (let* ((left (+ x (svref (choice-state-lefts state) i)))
                       (row (+ y (svref (choice-state-tops state) i)))
                       (top (+ row (/ (- h +dot-size+) 2))))
                  (when (and focused (= i selected))
                    (push-rounded-rect list (- left 2) (- top 2) (+ +dot-size+ 4) (+ +dot-size+ 4)
                                       (+ (/ +dot-size+ 2) 2) (theme-ring *theme*)))
                  (push-rounded-rect list left top +dot-size+ +dot-size+ (/ +dot-size+ 2)
                                     (theme-field-border *theme*))
                  (push-rounded-rect list (+ left 1) (+ top 1) (- +dot-size+ 2) (- +dot-size+ 2)
                                     (/ (- +dot-size+ 2) 2) (theme-field-fill *theme*))
                  (when (= i selected)
                    (push-rounded-rect list (+ left 4) (+ top 4) (- +dot-size+ 8) (- +dot-size+ 8)
                                       (/ (- +dot-size+ 8) 2) (theme-row-selected *theme*)))
                  (context-push-text context list font option (+ left +dot-size+ 6f0)
                                     (+ row (/ (- h (+ ascent 4f0)) 2) ascent)
                                     (if (box-enabled box) (theme-text *theme*) (theme-dim-text *theme*))))))))

(defun draw-dropdown (box context list x y)
  "A button that shows its option, with the arrows of a pop-up button at the right."
  (let ((w (box-w box)) (h (box-h box))
        (font (ui-font context)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme-radius *theme*) 2)
                           (theme-ring *theme*)))
      (push-rounded-rect list x y w h (theme-radius *theme*)
                         (button-color *theme* (if (eql (box-n3 box) 1) :pressed state)))
      (multiple-value-bind (width ascent) (context-measure context font (box-text box))
        (declare (ignore width))
        (context-push-text context list font (fit-text context font (box-text box) (- w 44))
                           (+ x 14) (+ y 7 ascent)
                           (if (box-enabled box) (theme-button-text *theme*) (theme-dim-text *theme*)))
        ;; Two small arrows, one over the other.
        (let ((cx (+ x w -16)) (cy (+ y (/ h 2))))
          (loop :for i :from 0 :below 4
                :do (push-rect list (- cx i) (- cy 2 (- 3 i)) (+ 1 (* 2 i)) 1 (theme-button-text *theme*))
                    (push-rect list (- cx i) (+ cy 2 (- 3 i)) (+ 1 (* 2 i)) 1 (theme-button-text *theme*))))))))

(defun draw-menu-button (box context list x y)
  "A button whose words are followed by an arrow down, for the menu it opens."
  (let ((w (box-w box)) (h (box-h box))
        (font (ui-font context)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ (theme-radius *theme*) 2)
                           (theme-ring *theme*)))
      (push-rounded-rect list x y w h (theme-radius *theme*)
                         (button-color *theme* (if (eql (box-n3 box) 1) :pressed state)))
      (multiple-value-bind (width ascent) (context-measure context font (box-text box))
        (declare (ignore width))
        (context-push-text context list font (box-text box) (+ x 14) (+ y 7 ascent)
                           (if (box-enabled box) (theme-button-text *theme*) (theme-dim-text *theme*)))
        (let ((cx (+ x w -16)) (cy (+ y (/ h 2) -1)))
          (loop :for i :from 0 :below 4
                :do (push-rect list (- cx (- 3 i)) (+ cy i) (+ 1 (* 2 (- 3 i))) 1
                               (theme-button-text *theme*))))))))

(defun draw-tooltip (box context list x y)
  "Small words on a light background, with a thin edge."
  (let ((font (context-font context (theme-font-name *theme*) 13)))
    (push-rounded-rect list (- x 1) (- y 1) (+ (box-w box) 2) (+ (box-h box) 2) 5
                       (theme-field-border *theme*))
    (push-rounded-rect list x y (box-w box) (box-h box) 4 (theme-tooltip *theme*))
    (multiple-value-bind (width ascent) (context-measure context font (box-text box))
      (declare (ignore width))
      (context-push-text context list font (box-text box) (+ x (spaced 8)) (+ y (spaced 4) ascent)
                         (theme-tooltip-text *theme*)))))

(defun draw-toast (box context list x y)
  "Words on a light rounded background, as a tooltip but larger."
  (let ((font (ui-font context)))
    (push-rounded-rect list x y (box-w box) (box-h box) 10 (theme-tooltip *theme*))
    (multiple-value-bind (width ascent) (context-measure context font (box-text box))
      (declare (ignore width))
      (context-push-text context list font (box-text box) (+ x (spaced 16)) (+ y (spaced 10) ascent)
                         (theme-tooltip-text *theme*)))))

(defun draw-menu-item (box context list x y)
  "A row of a menu: the highlight under it if it has it, the mark of the chosen one, and its words."
  (let ((font (ui-font context))
        (highlighted (eql (box-n3 box) 1)))
    (when highlighted
      (push-rounded-rect list x y (box-w box) (box-h box) 4 (theme-row-selected *theme*)))
    (multiple-value-bind (height ascent) (line-metrics context)
      (let ((baseline (+ y (/ (- (box-h box) height) 2) ascent)))
        (when (= (box-n2 box) 1f0)
          (context-push-text context list font "✓" (+ x 6) baseline (theme-text *theme*)))
        (context-push-text context list font (box-text box) (+ x +menu-check-width+) baseline
                           (if highlighted (theme-button-text *theme*) (theme-text *theme*)))))))

(defun draw-switch (box context list x y)
  "A track as wide as +SWITCH-WIDTH+ and half as tall, filled with the accent when on, and a knob at
the end it is at."
  (let* ((h (/ +switch-width+ 2))
         (top (+ y (/ (- (box-h box) h) 2)))
         (on (eql (box-n3 box) 1))
         (enabled (box-enabled box))
         (focused (nth-value 1 (look-state box)))
         (knob (- h 4)))
    (when focused
      (push-rounded-rect list (- x 2) (- top 2) (+ +switch-width+ 4) (+ h 4) (+ (/ h 2) 2)
                         (theme-ring *theme*)))
    (push-rounded-rect list x top +switch-width+ h (/ h 2)
                       (if on (theme-row-selected *theme*) (theme-track *theme*)))
    (push-rounded-rect list (if on (+ x +switch-width+ (- knob) -2) (+ x 2)) (+ top 2) knob knob
                       (/ knob 2) (if enabled (theme-thumb *theme*) (theme-dim-text *theme*)))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (context-push-text context list (ui-font context) (box-text box) (+ x +switch-width+ 8f0)
                         (+ y (/ (- (box-h box) (+ ascent 4f0)) 2) ascent)
                         (if enabled (theme-text *theme*) (theme-dim-text *theme*))))))

(defun draw-spinner (box list x y)
  "+SPINNER-STEPS+ dots round a ring, the one at the step the spinner is at the brightest and those
behind it fading."
  (let* ((size (min (box-w box) (box-h box)))
         (dot (max 2f0 (* size 0.22f0)))
         (radius (/ (- size dot) 2))
         (cx (+ x (/ (box-w box) 2)))
         (cy (+ y (/ (box-h box) 2)))
         (step (box-n3 box)))
    (dotimes (i +spinner-steps+)
      (let* ((angle (* 2 pi (/ i +spinner-steps+)))
             (behind (mod (- step i) +spinner-steps+))
             (alpha (max 40 (- 255 (* behind 30)))))
        (push-rounded-rect list (- (+ cx (* radius (float (sin angle) 1f0))) (/ dot 2))
                           (- (- cy (* radius (float (cos angle) 1f0))) (/ dot 2))
                           dot dot (/ dot 2)
                           (logior (logand (theme-text *theme*) #xFFFFFF00) alpha))))))

(defun draw-list-box (box context list x y)
  (let* ((font (ui-font context))
         (w (box-w box)) (h (box-h box))
         (items (car (box-data box)))
         (state (cdr (box-data box)))
         (top (list-state-top state))
         (rows (box-n3 box))
         (count (length items))
         (selected (round (box-n2 box)))
         (row-height (theme-row-height *theme*)))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (draw-outlined-box list x y w h (theme-field-fill *theme*) (nth-value 1 (look-state box)))
      (loop :for item :in (nthcdr top items)
            :for index :from top
            :for i :from 0 :below rows
            :do (let ((row-y (+ y 1 (* i row-height))))
                  (when (= index selected)
                    (push-rounded-rect list (+ x 3) row-y (- w 6 (if (> count rows) 8 0))
                                       row-height 4 (theme-row-selected *theme*)))
                  (context-push-text context list font (fit-text context font item (- w 24))
                                     (+ x 10) (+ row-y (/ (- row-height (+ ascent 4)) 2) ascent)
                                     ;; On the highlight, as a menu's highlighted row is.
                                     (if (= index selected)
                                         (theme-button-text *theme*)
                                         (theme-text *theme*)))))
      (when (> count rows)
        (let* ((track (- h 8))
               (bar (max 12f0 (* track (/ (float rows 1f0) count))))
               (travel (- track bar))
               (max-top (max 1 (- count rows)))
               (position (* travel (/ (float top 1f0) max-top))))
          (push-rounded-rect list (+ x w -8) (+ y 4 position) 4 bar 2 (theme-field-border *theme*)))))))

(defun draw-checkbox (box context list x y)
  (let ((top (+ y (/ (- (box-h box) +dot-size+) 2)))
        (focused (nth-value 1 (look-state box))))
    (draw-outlined-box list x top +dot-size+ +dot-size+ (theme-field-fill *theme*) focused)
    (when (eql (box-n3 box) 1)
      (push-rounded-rect list (+ x 4) (+ top 4) (- +dot-size+ 8) (- +dot-size+ 8) 2f0
                         (theme-row-selected *theme*)))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (context-push-text context list (ui-font context) (box-text box) (+ x +dot-size+ 6f0)
                         (+ y (/ (- (box-h box) (+ ascent 4f0)) 2) ascent)
                         (if (box-enabled box) (theme-text *theme*) (theme-dim-text *theme*))))))

(defun draw-leaf (box context list x y)
  (ecase (box-kind box)
    (:label (draw-label box context list x y))
    (:paragraph (draw-paragraph box context list x y))
    (:button (draw-button box context list x y))
    ((:text-field :combo-box) (draw-text-field box context list x y))
    (:text-area (draw-text-area box context list x y))
    (:slider (draw-slider box list x y))
    (:progress (draw-progress box list x y))
    (:choice (draw-choice box context list x y))
    (:checkbox (draw-checkbox box context list x y))
    (:switch (draw-switch box context list x y))
    (:dropdown (draw-dropdown box context list x y))
    (:menu-button (draw-menu-button box context list x y))
    (:menu-item (draw-menu-item box context list x y))
    (:tooltip (draw-tooltip box context list x y))
    (:toast (draw-toast box context list x y))
    (:spinner (draw-spinner box list x y))
    (:list-box (draw-list-box box context list x y))))

(defun draw-scroll-bar (box list x y)
  "The bar at the right edge of the scroll container BOX that says how much of what is inside shows,
and where: drawn only when not all of it does."
  (let* ((state (box-data box))
         (h (box-h box))
         (content (scroll-state-content-height state)))
    (when (> content h)
      (let* ((track (- h 8))
             (bar (max 12f0 (* track (/ h content))))
             (position (* (- track bar) (/ (scroll-state-offset state) (- content h)))))
        (push-rounded-rect list (+ x (box-w box) -8) (+ y 4 position) 4 bar 2
                           (theme-field-border *theme*))))))

;;; --- the cache ---

(defun paint-cached (ui box context list x y)
  "Draw the leaf BOX at X, Y: from what was kept for it if its hash is the same as when it was
kept, and otherwise afresh, which is then kept."
  (let* ((key (box-key box))
         (hash (box-paint-hash box))
         (entry (or (gethash key (ui-paint-cache ui))
                    (setf (gethash key (ui-paint-cache ui)) (make-entry))))
         (salt (case (box-kind box)
                 (:paragraph (paragraph-state-generation (box-data box)))
                 (:text-area (area-state-generation (box-data box)))
                 (t 0)))
         (fragment (entry-fragment entry)))
    (unless (and (= (entry-hash entry) hash)
                 (= (entry-salt entry) salt)
                 (= (entry-width entry) (box-w box))
                 (= (entry-height entry) (box-h box)))
      (loop
        (clear-display-list fragment)
        (handler-case (progn (draw-leaf box context fragment 0f0 0f0)
                             (return))
          (display-list-full ()
            (setf fragment (make-display-list :capacity (* 2 (display-list-capacity fragment)))
                  (entry-fragment entry) fragment))))
      (setf (entry-hash entry) hash
            (entry-salt entry) salt
            (entry-width entry) (box-w box)
            (entry-height entry) (box-h box)))
    (let ((scale (context-scale context)))
      (append-display-list list fragment (snap-to-pixel x scale) (snap-to-pixel y scale)))))

(defun paint-box (ui box context list x y)
  "Draw BOX, whose parent is at X, Y in the window, and everything in it."
  (let ((*theme* (box-theme box)))
    (let ((x (+ x (box-x box)))
          (y (+ y (box-y box))))
      (ecase (box-kind box)
        ((:label :paragraph :button :text-field :combo-box :text-area :slider :progress :choice :checkbox :switch :spinner
          :list-box :dropdown :menu-button :menu-item :tooltip
          :toast)
         (paint-cached ui box context list x y))
        (:canvas
         (let ((scale (context-scale context))
               (fragment (box-data box)))
           (let ((x (snap-to-pixel x scale)) (y (snap-to-pixel y scale)))
             (push-clip list x y (box-w box) (box-h box))
             (append-display-list list fragment x y)
             (push-unclip list))))
        (:scrim
         (push-rect list x y (box-w box) (box-h box) (theme-scrim *theme*)))
        (:catcher)
        (:toasts (paint-children ui box context list x y))
        ((:popover :dialog)
         (push-rounded-rect list (- x 1) (- y 1) (+ (box-w box) 2) (+ (box-h box) 2) 11
                            (theme-field-border *theme*))
         (push-rounded-rect list x y (box-w box) (box-h box) 10 (theme-panel *theme*))
         (paint-children ui box context list x y))
        (:menu
         (push-rounded-rect list (- x 1) (- y 1) (+ (box-w box) 2) (+ (box-h box) 2) 7
                            (theme-field-border *theme*))
         (push-rounded-rect list x y (box-w box) (box-h box) 6 (theme-panel *theme*))
         (paint-children ui box context list x y))
        ((:scroll :lazy-list)
         (push-clip list x y (box-w box) (box-h box))
         (paint-children ui box context list x y)
         (push-unclip list)
         (paint-ring-past-edges ui box context list x y)
         (draw-scroll-bar box list x y))
        (:panel
         (push-rounded-rect list x y (box-w box) (box-h box) (box-n1 box)
                            (if (zerop (box-color box)) (theme-panel *theme*) (box-color box)))
         (paint-children ui box context list x y))
        ((:column :row :padding :center :stack :context-area)
         (paint-children ui box context list x y))))))

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
  (loop :for child :across (box-children box)
        :do (when (nth-value 1 (look-state child))
              (return-from focused-inside (values child x y)))
            (unless (member (box-kind child) '(:scroll :lazy-list))
              (multiple-value-bind (found fx fy)
                  (focused-inside child (+ x (box-x child)) (+ y (box-y child)))
                (when found (return-from focused-inside (values found fx fy))))))
  nil)

(defun paint-ring-past-edges (ui box context list x y)
  "Draw the ring of what has the keyboard inside BOX, a scroll container or a lazy list at X, Y in the
window, where it reaches past BOX's edges."
  (multiple-value-bind (focused px py) (focused-inside box x y)
    (when focused
      (let* ((r +ring-reach+)
             (w (box-w box)) (h (box-h box))
             (fx (+ px (box-x focused))) (fy (+ py (box-y focused)))
             (left (>= fx x)) (right (<= (+ fx (box-w focused)) (+ x w)))
             (top (>= fy y)) (bottom (<= (+ fy (box-h focused)) (+ y h)))
             ;; The bands above and below reach round the corners on a side the ring shows at.
             (band-left (if left (- x r) x))
             (band-width (- (if right (+ x w r) (+ x w)) band-left)))
        (flet ((band (bx by bw bh)
                 (push-clip list bx by bw bh)
                 (paint-box ui focused context list px py)
                 (push-unclip list)))
          (when top (band band-left (- y r) band-width r))
          (when bottom (band band-left (+ y h) band-width r))
          (when left (band (- x r) y r h))
          (when right (band (+ x w) y r h)))))))

(defun paint-children (ui box context list x y)
  (loop :for child :across (box-children box)
        :do (paint-box ui child context list x y)))
