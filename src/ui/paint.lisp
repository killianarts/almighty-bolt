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
                #:field-state-state
                #:field-state-view-start
                #:choice-state-lefts
                #:list-state-top
                #:+field-padding+
                #:+row-height+
                #:+thumb-size+
                #:+dot-size+)
  (:import-from #:almighty-bolt/ui/theme
                #:+text-color+
                #:+dim-text-color+
                #:+button-text-color+
                #:+accent-color+
                #:+panel-color+
                #:+field-border-color+
                #:+field-fill-color+
                #:+field-invalid-color+
                #:+field-disabled-color+
                #:+selection-color+
                #:+caret-color+
                #:+track-color+
                #:+thumb-color+
                #:+row-selected-color+
                #:+scrim-color+
                #:+font-name+
                #:+font-size+
                #:+control-radius+
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
    (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ +control-radius+ 2)
                       +accent-color+))
  (push-rounded-rect list x y w h +control-radius+ +field-border-color+)
  (push-rounded-rect list (+ x 1) (+ y 1) (- w 2) (- h 2) (- +control-radius+ 1) fill))

(defun line-metrics (context)
  (multiple-value-bind (width ascent descent) (context-measure context (ui-font context) "Hg")
    (declare (ignore width))
    (values (+ ascent descent) ascent)))

(defun fit-text (context font string width)
  (let ((end (context-index-at-x context font string width)))
    (loop :while (and (plusp end) (> (context-caret-x context font string end) width))
          :do (decf end))
    (subseq string 0 end)))

(defun draw-label (box context list x y)
  (let* ((font (context-font context +font-name+ (box-size box))))
    (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
      (declare (ignore width descent))
      (context-push-text context list font (box-text box) x (+ y ascent)
                         (if (zerop (box-color box)) +text-color+ (box-color box))))))

(defun draw-paragraph (box context list x y)
  (let* ((state (box-data box))
         (text (paragraph-state-text state))
         (font (context-font context +font-name+ (box-size box))))
    (multiple-value-bind (width ascent descent) (context-measure context font "Hg")
      (declare (ignore width))
      (loop :for (start end) :in (paragraph-state-lines state)
            :for line :from 0
            :do (context-push-text context list font (subseq text start end) x
                                   (+ y (* line (+ ascent descent)) ascent)
                                   (if (zerop (box-color box)) +text-color+ (box-color box)))))))

(defun draw-button (box context list x y)
  (let ((w (box-w box)) (h (box-h box)))
    (multiple-value-bind (state focused) (look-state box)
      (when focused
        (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ +control-radius+ 2)
                           +accent-color+))
      (push-rounded-rect list x y w h +control-radius+ (button-color state))
      (let ((font (ui-font context)))
        (multiple-value-bind (width ascent descent) (context-measure context font (box-text box))
          (declare (ignore width descent))
          (context-push-text context list font (box-text box) (+ x 14) (+ y 7 ascent)
                             +button-text-color+))))))

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
                         (cond ((not enabled) +field-disabled-color+)
                               ((plusp (box-n3 box)) +field-invalid-color+)
                               (t +field-fill-color+))
                         focused)
      (let* ((inner (- w (* 2 +field-padding+)))
             (shown (fit-text context font (subseq string start) inner))
             (end (+ start (length shown)))
             (left (+ x +field-padding+)))
        (flet ((x-at (index)
                 (+ left (context-caret-x context font shown
                                          (max 0 (min (length shown) (- index start)))))))
          (when (and (text-state-selection-p state) focused)
            (let ((from (max start (text-state-selection-start state)))
                  (to (min end (text-state-selection-end state))))
              (when (< from to)
                (push-rect list (x-at from) (+ y 4) (- (x-at to) (x-at from)) (- h 8)
                           +selection-color+))))
          (context-push-text context list font shown left (+ y 6 ascent)
                             (if enabled +text-color+ +dim-text-color+))
          (when (and focused (not (text-state-selection-p state))
                     (<= start (text-state-caret state) end))
            (push-rect list (x-at (text-state-caret state)) (+ y 5) 1.5f0 (- h 10)
                       +caret-color+)))))))

(defun draw-slider (box list x y)
  (let* ((w (box-w box)) (h (box-h box))
         (thumb-x (+ x (/ +thumb-size+ 2) (* (max 1f0 (- w +thumb-size+)) (box-n1 box))))
         (middle (+ y (/ h 2))))
    (push-rounded-rect list x (- middle 3) w 6 3 +track-color+)
    (push-rounded-rect list x (- middle 3) (- thumb-x x) 6 3 +row-selected-color+)
    (when (nth-value 1 (look-state box))
      (push-rounded-rect list (- thumb-x (/ +thumb-size+ 2) 2) (- middle (/ +thumb-size+ 2) 2)
                         (+ +thumb-size+ 4) (+ +thumb-size+ 4) (+ (/ +thumb-size+ 2) 2)
                         +accent-color+))
    (push-rounded-rect list (- thumb-x (/ +thumb-size+ 2)) (- middle (/ +thumb-size+ 2))
                       +thumb-size+ +thumb-size+ (/ +thumb-size+ 2) +thumb-color+)))

(defun draw-progress (box list x y)
  (let* ((w (box-w box)) (h (box-h box))
         (filled (* w (box-n1 box))))
    (push-rounded-rect list x y w h (* 0.5f0 h) +track-color+)
    (when (> filled 0f0)
      (push-rounded-rect list x y (max filled h) h (* 0.5f0 h) +thumb-color+))))

(defun draw-choice (box context list x y)
  (let* ((font (ui-font context))
         (h (box-h box))
         (options (car (box-data box)))
         (state (cdr (box-data box)))
         (selected (box-n3 box))
         (focused (nth-value 1 (look-state box))))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (loop :for option :in options
            :for i :from 0
            :do (let ((left (+ x (svref (choice-state-lefts state) i)))
                      (top (+ y (/ (- h +dot-size+) 2))))
                  (when (and focused (= i selected))
                    (push-rounded-rect list (- left 2) (- top 2) (+ +dot-size+ 4) (+ +dot-size+ 4)
                                       (+ (/ +dot-size+ 2) 2) +accent-color+))
                  (push-rounded-rect list left top +dot-size+ +dot-size+ (/ +dot-size+ 2)
                                     +field-border-color+)
                  (push-rounded-rect list (+ left 1) (+ top 1) (- +dot-size+ 2) (- +dot-size+ 2)
                                     (/ (- +dot-size+ 2) 2) +field-fill-color+)
                  (when (= i selected)
                    (push-rounded-rect list (+ left 4) (+ top 4) (- +dot-size+ 8) (- +dot-size+ 8)
                                       (/ (- +dot-size+ 8) 2) +row-selected-color+))
                  (context-push-text context list font option (+ left +dot-size+ 6f0)
                                     (+ y (/ (- h (+ ascent 4f0)) 2) ascent)
                                     (if (box-enabled box) +text-color+ +dim-text-color+)))))))

(defun draw-list-box (box context list x y)
  (let* ((font (ui-font context))
         (w (box-w box)) (h (box-h box))
         (items (car (box-data box)))
         (state (cdr (box-data box)))
         (top (list-state-top state))
         (rows (box-n3 box))
         (count (length items))
         (selected (round (box-n2 box))))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (draw-outlined-box list x y w h +field-fill-color+ (nth-value 1 (look-state box)))
      (loop :for item :in (nthcdr top items)
            :for index :from top
            :for i :from 0 :below rows
            :do (let ((row-y (+ y 1 (* i +row-height+))))
                  (when (= index selected)
                    (push-rounded-rect list (+ x 3) row-y (- w 6 (if (> count rows) 8 0))
                                       +row-height+ 4 +row-selected-color+))
                  (context-push-text context list font (fit-text context font item (- w 24))
                                     (+ x 10) (+ row-y (/ (- +row-height+ (+ ascent 4)) 2) ascent)
                                     +text-color+)))
      (when (> count rows)
        (let* ((track (- h 8))
               (bar (max 12f0 (* track (/ (float rows 1f0) count))))
               (travel (- track bar))
               (max-top (max 1 (- count rows)))
               (position (* travel (/ (float top 1f0) max-top))))
          (push-rounded-rect list (+ x w -8) (+ y 4 position) 4 bar 2 +field-border-color+))))))

(defun draw-checkbox (box context list x y)
  (let ((top (+ y (/ (- (box-h box) +dot-size+) 2)))
        (focused (nth-value 1 (look-state box))))
    (draw-outlined-box list x top +dot-size+ +dot-size+ +field-fill-color+ focused)
    (when (eql (box-n3 box) 1)
      (push-rounded-rect list (+ x 4) (+ top 4) (- +dot-size+ 8) (- +dot-size+ 8) 2f0
                         +row-selected-color+))
    (multiple-value-bind (height ascent) (line-metrics context)
      (declare (ignore height))
      (context-push-text context list (ui-font context) (box-text box) (+ x +dot-size+ 6f0)
                         (+ y (/ (- (box-h box) (+ ascent 4f0)) 2) ascent)
                         (if (box-enabled box) +text-color+ +dim-text-color+)))))

(defun draw-leaf (box context list x y)
  (ecase (box-kind box)
    (:label (draw-label box context list x y))
    (:paragraph (draw-paragraph box context list x y))
    (:button (draw-button box context list x y))
    (:text-field (draw-text-field box context list x y))
    (:slider (draw-slider box list x y))
    (:progress (draw-progress box list x y))
    (:choice (draw-choice box context list x y))
    (:checkbox (draw-checkbox box context list x y))
    (:list-box (draw-list-box box context list x y))))

;;; --- the cache ---

(defun paint-cached (ui box context list x y)
  "Draw the leaf BOX at X, Y: from what was kept for it if its hash is the same as when it was
kept, and otherwise afresh, which is then kept."
  (let* ((key (box-key box))
         (hash (box-paint-hash box))
         (entry (or (gethash key (ui-paint-cache ui))
                    (setf (gethash key (ui-paint-cache ui)) (make-entry))))
         (salt (if (eq (box-kind box) :paragraph)
                   (paragraph-state-generation (box-data box))
                   0))
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
  (let ((x (+ x (box-x box)))
        (y (+ y (box-y box))))
    (ecase (box-kind box)
      ((:label :paragraph :button :text-field :slider :progress :choice :checkbox :list-box)
       (paint-cached ui box context list x y))
      (:canvas
       (let ((scale (context-scale context))
             (fragment (box-data box)))
         (let ((x (snap-to-pixel x scale)) (y (snap-to-pixel y scale)))
           (push-clip list x y (box-w box) (box-h box))
           (append-display-list list fragment x y)
           (push-unclip list))))
      (:scrim
       (push-rect list x y (box-w box) (box-h box) +scrim-color+))
      (:panel
       (push-rounded-rect list x y (box-w box) (box-h box) (box-n1 box)
                          (if (zerop (box-color box)) +panel-color+ (box-color box)))
       (paint-children ui box context list x y))
      ((:column :row :padding :center :stack)
       (paint-children ui box context list x y)))))

(defun paint-children (ui box context list x y)
  (loop :for child :across (box-children box)
        :do (paint-box ui child context list x y)))
