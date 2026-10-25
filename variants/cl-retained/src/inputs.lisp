(defpackage #:almighty-bolt-variant-retained/inputs
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-caret-x
                #:context-index-at-x
                #:context-push-text
                #:push-rect
                #:push-rounded-rect
                #:push-clip
                #:push-unclip
                #:constrain
                #:make-text-state
                #:text-state-string
                #:text-state-caret
                #:text-state-anchor
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end
                #:text-state=
                #:text-place-caret
                #:edit-text
                #:key-event-kind
                #:key-event-logical-key
                #:+key-left+
                #:+key-right+
                #:+key-up+
                #:+key-down+
                #:+key-home+
                #:+key-end+)
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+text-color+
                #:+dim-text-color+
                #:+accent-color+
                #:+field-border-color+
                #:+field-fill-color+
                #:+field-invalid-color+
                #:+field-disabled-color+
                #:+selection-color+
                #:+caret-color+
                #:+track-color+
                #:+thumb-color+
                #:+row-selected-color+
                #:+font-name+
                #:+font-size+
                #:+control-radius+)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node
                #:node-flex
                #:node-w
                #:node-h
                #:node-enabled
                #:node-focused
                #:touch
                #:set-size
                #:layout-node
                #:paint-node
                #:node-interactive-p
                #:node-focusable-p
                #:node-focus-on-click-p
                #:pointer-press
                #:pointer-move
                #:pointer-exit
                #:pointer-drag
                #:pointer-release
                #:pointer-wheel
                #:key-input
                #:session-context
                #:session-click-count)
  (:export #:text-field
           #:text-field-p
           #:text-field-state
           #:text-field-text
           #:text-field-invalid
           #:text-field-on-change
           #:slider
           #:slider-p
           #:slider-value
           #:choice
           #:choice-p
           #:choice-selected
           #:choice-lefts
           #:list-box
           #:list-box-p
           #:list-box-items
           #:list-box-selected
           #:list-box-top
           #:canvas
           #:canvas-p
           #:canvas-invalidate))

(in-package #:almighty-bolt-variant-retained/inputs)

;;; The widgets that take input: a line of editable text, a slider, a choice between
;;; options, a list to pick from, and a canvas the app draws on and handles events for.

(defconstant +field-padding+ 8f0)

(defun ui-font (context)
  (context-font context +font-name+ +font-size+))

(defun line-height (context)
  (multiple-value-bind (width ascent descent) (context-measure context (ui-font context) "Hg")
    (declare (ignore width))
    (values (+ ascent descent) ascent)))

(defun fit-text (context font string width)
  "The longest start of STRING that fits in WIDTH points."
  (let ((end (context-index-at-x context font string width)))
    (loop :while (and (plusp end) (> (context-caret-x context font string end) width))
          :do (decf end))
    (subseq string 0 end)))

(defun draw-outlined-box (list x y w h fill focused)
  "A rounded box with a one-point border, in FILL, with a ring round it if FOCUSED."
  (when focused
    (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ +control-radius+ 2)
                       +accent-color+))
  (push-rounded-rect list x y w h +control-radius+ +field-border-color+)
  (push-rounded-rect list (+ x 1) (+ y 1) (- w 2) (- h 2) (- +control-radius+ 1) fill))

;;; --- a line of editable text ---

(defstruct (text-field (:include node) (:constructor %make-text-field))
  (state (make-text-state "") :type almighty-bolt-core:text-state)
  (width 180f0 :type single-float)
  (on-change nil :type (or null function))
  (on-submit nil :type (or null function))
  (%invalid nil :type boolean)
  ;; The index of the first character shown, when the text is longer than the field.
  (view-start 0 :type fixnum))

(defun text-field (&key id (text "") (width 180) on-change on-submit invalid (flex 0))
  "An editable line of TEXT, WIDTH points wide. ON-CHANGE is called with the new text
when a key or the pointer changes it, and ON-SUBMIT with the text when Return is pressed.
INVALID makes it red."
  (%make-text-field :id id :state (make-text-state text) :width (float width 1f0)
                    :on-change on-change :on-submit on-submit :%invalid (and invalid t)
                    :flex (float flex 1f0) :cacheable t))

(defun text-field-text (field)
  (text-state-string (text-field-state field)))

(defun (setf text-field-text) (text field)
  "Put TEXT in FIELD, with the caret at its end. Calls no handler: this is the program
speaking and not the person."
  (unless (string= text (text-field-text field))
    (setf (text-field-state field) (make-text-state text)
          (text-field-view-start field) 0)
    (touch field nil))
  text)

(defun text-field-invalid (field)
  (text-field-%invalid field))

(defun (setf text-field-invalid) (invalid field)
  (let ((invalid (and invalid t)))
    (unless (eq invalid (text-field-%invalid field))
      (setf (text-field-%invalid field) invalid)
      (touch field nil))
    invalid))

(defmethod node-interactive-p ((field text-field)) t)
(defmethod node-focusable-p ((field text-field)) t)
(defmethod node-focus-on-click-p ((field text-field)) t)

(defmethod layout-node ((field text-field) context min-w max-w min-h max-h)
  (set-size field
            (constrain (text-field-width field) min-w max-w)
            (constrain (+ (line-height context) 12f0) min-h max-h))
  (values (node-w field) (node-h field)))

(defun inner-width (field)
  (- (node-w field) (* 2 +field-padding+)))

(defun keep-caret-in-view (field context)
  "Move the first shown character so that the caret is shown."
  (let* ((font (ui-font context))
         (string (text-field-text field))
         (caret (text-state-caret (text-field-state field)))
         (start (min (text-field-view-start field) caret)))
    (loop :while (and (< start caret)
                      (> (context-caret-x context font (subseq string start) (- caret start))
                         (inner-width field)))
          :do (incf start))
    (setf (text-field-view-start field) start)))

(defun index-at (field context x)
  "The index in the text that the point X, from the field's left edge, is nearest."
  (let ((string (text-field-text field))
        (start (text-field-view-start field)))
    (+ start (context-index-at-x context (ui-font context) (subseq string start)
                                 (- x +field-padding+)))))

(defmethod paint-node ((field text-field) context list x y)
  (let* ((w (node-w field))
         (h (node-h field))
         (font (ui-font context))
         (state (text-field-state field))
         (string (text-field-text field)))
    (multiple-value-bind (height ascent) (line-height context)
      (declare (ignore height))
      (keep-caret-in-view field context)
      (draw-outlined-box list x y w h
                         (cond ((not (node-enabled field)) +field-disabled-color+)
                               ((text-field-%invalid field) +field-invalid-color+)
                               (t +field-fill-color+))
                         (node-focused field))
      (let* ((start (text-field-view-start field))
             (shown (fit-text context font (subseq string start) (inner-width field)))
             (end (+ start (length shown)))
             (left (+ x +field-padding+)))
        (flet ((x-at (index)
                 (+ left (context-caret-x context font shown
                                          (max 0 (min (length shown) (- index start)))))))
          (when (and (text-state-selection-p state) (node-focused field))
            (let ((from (max start (text-state-selection-start state)))
                  (to (min end (text-state-selection-end state))))
              (when (< from to)
                (push-rect list (x-at from) (+ y 4) (- (x-at to) (x-at from)) (- h 8)
                           +selection-color+))))
          (context-push-text context list font shown left (+ y 6 ascent)
                             (if (node-enabled field) +text-color+ +dim-text-color+))
          (when (and (node-focused field) (not (text-state-selection-p state))
                     (<= start (text-state-caret state) end))
            (push-rect list (x-at (text-state-caret state)) (+ y 5) 1.5f0 (- h 10)
                       +caret-color+)))))))

(defun set-field-state (field new)
  "Make NEW the state of FIELD. Returns true if it differs, after calling ON-CHANGE if the
text did."
  (let ((old (text-field-state field)))
    (unless (eq old new)
      (setf (text-field-state field) new)
      (touch field nil)
      (when (and (text-field-on-change field)
                 (string/= (text-state-string old) (text-state-string new)))
        (funcall (text-field-on-change field) (text-state-string new)))
      t)))

(defmethod pointer-press ((field text-field) session x y button)
  (declare (ignore y))
  (when (eql button 0)
    (let ((context (session-context session)))
      (when context
        (set-field-state field (text-place-caret (text-field-state field)
                                                 (index-at field context x)))))))

(defmethod pointer-drag ((field text-field) session x y)
  (declare (ignore y))
  (let ((context (session-context session)))
    (when context
      (set-field-state field (text-place-caret (text-field-state field)
                                               (index-at field context x) :extend t)))))

(defmethod key-input ((field text-field) session event)
  (declare (ignore session))
  (cond
    ((not (eq (key-event-kind event) :down)) nil)
    ((eql (key-event-logical-key event) #\Return)
     (when (text-field-on-submit field)
       (funcall (text-field-on-submit field) (text-field-text field)))
     t)
    (t
     (multiple-value-bind (new edited) (edit-text (text-field-state field) event)
       (when edited
         (set-field-state field new))
       edited))))

;;; --- a slider ---

(defconstant +thumb-size+ 16f0)

(defstruct (slider (:include node) (:constructor %make-slider))
  (minimum 0d0 :type double-float)
  (maximum 1d0 :type double-float)
  (%value 0d0 :type double-float)
  (width 200f0 :type single-float)
  (on-change nil :type (or null function)))

(defun slider (&key id (minimum 0) (maximum 1) (value 0) (width 200) on-change (flex 0))
  "A slider from MINIMUM to MAXIMUM. ON-CHANGE is called with the new value (a
double-float) when it is dragged or moved with the arrow keys."
  (%make-slider :id id :minimum (float minimum 1d0) :maximum (float maximum 1d0)
                :%value (float (max minimum (min maximum value)) 1d0)
                :width (float width 1f0) :on-change on-change :flex (float flex 1f0)
                :cacheable t))

(defun slider-value (slider)
  (slider-%value slider))

(defun (setf slider-value) (value slider)
  "Move SLIDER to VALUE, which is kept between its ends. Calls no handler."
  (let ((value (float (max (slider-minimum slider) (min (slider-maximum slider) value)) 1d0)))
    (unless (= value (slider-%value slider))
      (setf (slider-%value slider) value)
      (touch slider nil))
    value))

(defmethod node-interactive-p ((slider slider)) t)
(defmethod node-focusable-p ((slider slider)) t)
(defmethod node-focus-on-click-p ((slider slider)) t)

(defmethod layout-node ((slider slider) context min-w max-w min-h max-h)
  (declare (ignore context))
  (set-size slider (constrain (slider-width slider) min-w max-w) (constrain 24f0 min-h max-h))
  (values (node-w slider) (node-h slider)))

(defun slider-travel (slider)
  "How far the middle of the thumb moves, in points."
  (max 1f0 (- (node-w slider) +thumb-size+)))

(defun slider-fraction (slider)
  (let ((range (- (slider-maximum slider) (slider-minimum slider))))
    (if (plusp range)
        (/ (- (slider-%value slider) (slider-minimum slider)) range)
        0d0)))

(defmethod paint-node ((slider slider) context list x y)
  (declare (ignore context))
  (let* ((w (node-w slider))
         (h (node-h slider))
         (thumb-x (+ x (/ +thumb-size+ 2)
                     (* (slider-travel slider) (float (slider-fraction slider) 1f0))))
         (middle (+ y (/ h 2))))
    (push-rounded-rect list x (- middle 3) w 6 3 +track-color+)
    (push-rounded-rect list x (- middle 3) (- thumb-x x) 6 3 +row-selected-color+)
    (when (node-focused slider)
      (push-rounded-rect list (- thumb-x (/ +thumb-size+ 2) 2) (- middle (/ +thumb-size+ 2) 2)
                         (+ +thumb-size+ 4) (+ +thumb-size+ 4) (+ (/ +thumb-size+ 2) 2)
                         +accent-color+))
    (push-rounded-rect list (- thumb-x (/ +thumb-size+ 2)) (- middle (/ +thumb-size+ 2))
                       +thumb-size+ +thumb-size+ (/ +thumb-size+ 2) +thumb-color+)))

(defun slide-to (slider x)
  "Move SLIDER so that the middle of its thumb is at X, from its left edge, and call its
handler if the value changed."
  (let* ((fraction (max 0d0 (min 1d0 (/ (- x (/ +thumb-size+ 2)) (slider-travel slider)))))
         (old (slider-%value slider)))
    (setf (slider-value slider)
          (+ (slider-minimum slider)
             (* fraction (- (slider-maximum slider) (slider-minimum slider)))))
    (when (and (/= old (slider-%value slider)) (slider-on-change slider))
      (funcall (slider-on-change slider) (slider-%value slider)))))

(defmethod pointer-press ((slider slider) session x y button)
  (declare (ignore session y))
  (when (eql button 0)
    (slide-to slider x)))

(defmethod pointer-drag ((slider slider) session x y)
  (declare (ignore session y))
  (slide-to slider x))

(defmethod key-input ((slider slider) session event)
  (declare (ignore session))
  (when (eq (key-event-kind event) :down)
    (let* ((range (- (slider-maximum slider) (slider-minimum slider)))
           (step (/ range 20))
           (key (key-event-logical-key event))
           (old (slider-%value slider))
           (new (cond ((or (eql key +key-left+) (eql key +key-down+)) (- old step))
                      ((or (eql key +key-right+) (eql key +key-up+)) (+ old step))
                      ((eql key +key-home+) (slider-minimum slider))
                      ((eql key +key-end+) (slider-maximum slider)))))
      (when new
        (setf (slider-value slider) new)
        (when (and (/= old (slider-%value slider)) (slider-on-change slider))
          (funcall (slider-on-change slider) (slider-%value slider)))
        t))))

;;; --- a choice between options, shown side by side ---

(defstruct (choice (:include node) (:constructor %make-choice))
  (options #() :type simple-vector)
  (%selected 0 :type fixnum)
  (on-change nil :type (or null function))
  ;; The left edge and the width of each option, from the last layout.
  (lefts #() :type simple-vector)
  (widths #() :type simple-vector))

(defun choice (options &key id (selected 0) on-change (flex 0))
  "A choice between OPTIONS, strings, the SELECTED one (an index) marked. ON-CHANGE is
called with the index of the option that is chosen."
  (%make-choice :id id :options (coerce options 'simple-vector) :%selected selected
                :on-change on-change :flex (float flex 1f0) :cacheable t))

(defun choice-selected (choice)
  (choice-%selected choice))

(defun (setf choice-selected) (index choice)
  "Choose option INDEX. Calls no handler."
  (unless (= index (choice-%selected choice))
    (setf (choice-%selected choice) index)
    (touch choice nil))
  index)

(defmethod node-interactive-p ((choice choice)) t)
(defmethod node-focusable-p ((choice choice)) t)

(defconstant +dot-size+ 16f0)
(defconstant +option-gap+ 20f0)

(defmethod layout-node ((choice choice) context min-w max-w min-h max-h)
  (let* ((font (ui-font context))
         (count (length (choice-options choice)))
         (lefts (make-array count))
         (widths (make-array count))
         (left 0f0))
    (dotimes (i count)
      (let ((text-width (context-measure context font (svref (choice-options choice) i))))
        (setf (svref lefts i) left
              (svref widths i) (+ +dot-size+ 6f0 (float text-width 1f0)))
        (incf left (+ (svref widths i) +option-gap+))))
    (setf (choice-lefts choice) lefts
          (choice-widths choice) widths)
    (set-size choice
              (constrain (max 0f0 (- left +option-gap+)) min-w max-w)
              (constrain (max 22f0 +dot-size+) min-h max-h))
    (values (node-w choice) (node-h choice))))

(defmethod paint-node ((choice choice) context list x y)
  (let* ((font (ui-font context))
         (h (node-h choice)))
    (multiple-value-bind (height ascent) (line-height context)
      (declare (ignore height))
      (dotimes (i (length (choice-options choice)))
        (let ((left (+ x (svref (choice-lefts choice) i)))
              (top (+ y (/ (- h +dot-size+) 2))))
          (when (and (node-focused choice) (= i (choice-%selected choice)))
            (push-rounded-rect list (- left 2) (- top 2) (+ +dot-size+ 4) (+ +dot-size+ 4)
                               (+ (/ +dot-size+ 2) 2) +accent-color+))
          (push-rounded-rect list left top +dot-size+ +dot-size+ (/ +dot-size+ 2)
                             +field-border-color+)
          (push-rounded-rect list (+ left 1) (+ top 1) (- +dot-size+ 2) (- +dot-size+ 2)
                             (/ (- +dot-size+ 2) 2) +field-fill-color+)
          (when (= i (choice-%selected choice))
            (push-rounded-rect list (+ left 4) (+ top 4) (- +dot-size+ 8) (- +dot-size+ 8)
                               (/ (- +dot-size+ 8) 2) +row-selected-color+))
          (context-push-text context list font (svref (choice-options choice) i)
                             (+ left +dot-size+ 6f0) (+ y (/ (- h (+ ascent 4f0)) 2) ascent)
                             (if (node-enabled choice) +text-color+ +dim-text-color+)))))))

(defun choose (choice index)
  (when (and (<= 0 index) (< index (length (choice-options choice)))
             (/= index (choice-%selected choice)))
    (setf (choice-selected choice) index)
    (when (choice-on-change choice)
      (funcall (choice-on-change choice) index))))

(defmethod pointer-release ((choice choice) session x y inside-p)
  (declare (ignore session y))
  (when inside-p
    (dotimes (i (length (choice-options choice)))
      (let ((left (svref (choice-lefts choice) i)))
        (when (and (<= left x) (< x (+ left (svref (choice-widths choice) i))))
          (choose choice i)
          (return))))))

(defmethod key-input ((choice choice) session event)
  (declare (ignore session))
  (when (eq (key-event-kind event) :down)
    (let ((key (key-event-logical-key event)))
      (cond ((or (eql key +key-left+) (eql key +key-up+))
             (choose choice (1- (choice-%selected choice))) t)
            ((or (eql key +key-right+) (eql key +key-down+))
             (choose choice (1+ (choice-%selected choice))) t)))))

;;; --- a list to pick from ---

(defconstant +row-height+ 24f0)

(defstruct (list-box (:include node) (:constructor %make-list-box))
  (%items #() :type simple-vector)
  (%selected nil :type (or null fixnum))
  (%top 0 :type fixnum)
  (rows 8 :type fixnum)
  (width 200f0 :type single-float)
  ;; Wheel movement that has not yet made a whole row.
  (remainder 0f0 :type single-float)
  (on-select nil :type (or null function)))

(defun list-box (&key id items (rows 8) (width 200) selected on-select (flex 0))
  "A list of ITEMS, strings, showing ROWS at a time. ON-SELECT is called with the index of
the item that is picked."
  (%make-list-box :id id :%items (coerce items 'simple-vector) :rows rows
                  :width (float width 1f0) :%selected selected :on-select on-select
                  :flex (float flex 1f0) :cacheable t))

(defun list-box-items (list-box)
  (list-box-%items list-box))

(defun list-box-selected (list-box)
  "The index of the picked item, or NIL."
  (list-box-%selected list-box))

(defun list-box-top (list-box)
  "The index of the first row shown."
  (list-box-%top list-box))

(defun max-top (list-box)
  (max 0 (- (length (list-box-%items list-box)) (list-box-rows list-box))))

(defun clamp-list-box (list-box)
  (let ((count (length (list-box-%items list-box))))
    (when (and (list-box-%selected list-box) (>= (list-box-%selected list-box) count))
      (setf (list-box-%selected list-box) nil))
    (setf (list-box-%top list-box) (max 0 (min (list-box-%top list-box) (max-top list-box))))))

(defun (setf list-box-items) (items list-box)
  "Show ITEMS. The selection stays on the same index if there is still an item there,
and the list stays scrolled as far as it can. Calls no handler."
  (setf (list-box-%items list-box) (coerce items 'simple-vector))
  (clamp-list-box list-box)
  (touch list-box nil)
  items)

(defun (setf list-box-selected) (index list-box)
  "Pick item INDEX, or nothing if NIL, and scroll to show it. Calls no handler."
  (setf (list-box-%selected list-box) index)
  (when index
    (cond ((< index (list-box-%top list-box)) (setf (list-box-%top list-box) index))
          ((>= index (+ (list-box-%top list-box) (list-box-rows list-box)))
           (setf (list-box-%top list-box) (1+ (- index (list-box-rows list-box)))))))
  (clamp-list-box list-box)
  (touch list-box nil)
  index)

(defmethod node-interactive-p ((list-box list-box)) t)
(defmethod node-focusable-p ((list-box list-box)) t)
(defmethod node-focus-on-click-p ((list-box list-box)) t)

(defmethod layout-node ((list-box list-box) context min-w max-w min-h max-h)
  (declare (ignore context))
  (set-size list-box
            (constrain (list-box-width list-box) min-w max-w)
            (constrain (+ 2f0 (* +row-height+ (list-box-rows list-box))) min-h max-h))
  (values (node-w list-box) (node-h list-box)))

(defmethod paint-node ((list-box list-box) context list x y)
  (let* ((font (ui-font context))
         (w (node-w list-box))
         (h (node-h list-box))
         (items (list-box-%items list-box))
         (top (list-box-%top list-box))
         (shown (min (list-box-rows list-box) (max 0 (- (length items) top)))))
    (multiple-value-bind (height ascent) (line-height context)
      (declare (ignore height))
      (draw-outlined-box list x y w h +field-fill-color+ (node-focused list-box))
      (dotimes (i shown)
        (let* ((index (+ top i))
               (row-y (+ y 1 (* i +row-height+)))
               (selected (eql index (list-box-%selected list-box))))
          (when selected
            (push-rounded-rect list (+ x 3) row-y (- w 6 (if (> (length items) (list-box-rows list-box)) 8 0))
                               +row-height+ 4 +row-selected-color+))
          (context-push-text context list font
                             (fit-text context font (svref items index) (- w 24))
                             (+ x 10) (+ row-y (/ (- +row-height+ (+ ascent 4)) 2) ascent)
                             +text-color+)))
      ;; A thin bar at the right shows where the rows shown are among all of them.
      (when (> (length items) (list-box-rows list-box))
        (let* ((track (- h 8))
               (fraction (/ (float (list-box-rows list-box) 1f0) (length items)))
               (bar (max 12f0 (* track fraction)))
               (travel (- track bar))
               (position (if (plusp (max-top list-box))
                             (* travel (/ (float top 1f0) (max-top list-box)))
                             0f0)))
          (push-rounded-rect list (+ x w -8) (+ y 4 position) 4 bar 2 +field-border-color+))))))

(defun pick (list-box index)
  (let ((count (length (list-box-%items list-box))))
    (when (and (<= 0 index) (< index count)
               (not (eql index (list-box-%selected list-box))))
      (setf (list-box-selected list-box) index)
      (when (list-box-on-select list-box)
        (funcall (list-box-on-select list-box) index)))))

(defmethod pointer-press ((list-box list-box) session x y button)
  (declare (ignore session x))
  (when (eql button 0)
    (let ((index (+ (list-box-%top list-box) (floor (- y 1) +row-height+))))
      (when (< index (length (list-box-%items list-box)))
        (pick list-box index)))))

(defmethod pointer-wheel ((list-box list-box) session dx dy)
  (declare (ignore session dx))
  ;; A positive DY is the content moving down, which shows earlier rows.
  (let* ((rows (+ (list-box-remainder list-box) (/ (- dy) 1f0)))
         (whole (truncate rows))
         (old (list-box-%top list-box)))
    (setf (list-box-remainder list-box) (- rows whole))
    (setf (list-box-%top list-box) (max 0 (min (max-top list-box) (+ old whole))))
    (when (/= old (list-box-%top list-box))
      (touch list-box nil))
    (/= whole 0)))

(defmethod key-input ((list-box list-box) session event)
  (declare (ignore session))
  (when (eq (key-event-kind event) :down)
    (let ((key (key-event-logical-key event))
          (selected (list-box-%selected list-box)))
      (cond ((eql key +key-down+)
             (pick list-box (if selected (1+ selected) 0)) t)
            ((eql key +key-up+)
             (pick list-box (if selected (1- selected) 0)) t)))))

;;; --- a canvas the app draws on ---

(defstruct (canvas (:include node) (:constructor %make-canvas))
  (width 300f0 :type single-float)
  (height 200f0 :type single-float)
  (paint nil :type (or null function))
  (on-press nil :type (or null function))
  (on-move nil :type (or null function))
  (on-exit nil :type (or null function))
  (on-drag nil :type (or null function))
  (on-release nil :type (or null function))
  (on-key nil :type (or null function))
  (on-wheel nil :type (or null function))
  (focusable nil :type boolean))

(defun canvas (&key id (width 300) (height 200) paint on-press on-move on-exit on-drag
                 on-release on-key on-wheel focusable (flex 0))
  "An area of WIDTH by HEIGHT points that the app draws and handles events for. PAINT is
called with the canvas, the context, the display list and the top left and size of the
area, and appends commands. ON-PRESS and ON-DRAG are called with the canvas and a point in
the canvas, and ON-PRESS with the button as well (0 for the primary one); ON-MOVE with
the canvas and a point as the pointer moves over it, and ON-EXIT with the canvas when it
leaves; ON-RELEASE with the canvas, a point and whether the pointer is over it; ON-KEY with the canvas and the key
event (returning true if it used the key); ON-WHEEL with the canvas and the movement.
The canvas keeps what PAINT drew until CANVAS-INVALIDATE says it changed."
  (%make-canvas :id id :width (float width 1f0) :height (float height 1f0) :paint paint
                :on-press on-press :on-move on-move :on-exit on-exit :on-drag on-drag
                :on-release on-release :on-key on-key
                :on-wheel on-wheel :focusable (and focusable t) :flex (float flex 1f0)
                :cacheable t))

(defun canvas-invalidate (canvas)
  "Say that what the canvas draws has changed."
  (touch canvas nil))

(defmethod node-interactive-p ((canvas canvas)) t)
(defmethod node-focusable-p ((canvas canvas)) (canvas-focusable canvas))
(defmethod node-focus-on-click-p ((canvas canvas)) (canvas-focusable canvas))

(defmethod layout-node ((canvas canvas) context min-w max-w min-h max-h)
  (declare (ignore context))
  (set-size canvas (constrain (canvas-width canvas) min-w max-w)
            (constrain (canvas-height canvas) min-h max-h))
  (values (node-w canvas) (node-h canvas)))

(defmethod paint-node ((canvas canvas) context list x y)
  (when (canvas-paint canvas)
    ;; What the app draws is kept inside the canvas.
    (push-clip list x y (node-w canvas) (node-h canvas))
    (funcall (canvas-paint canvas) canvas context list x y (node-w canvas) (node-h canvas))
    (push-unclip list)))

(defmethod pointer-press ((canvas canvas) session x y button)
  (when (canvas-on-press canvas)
    (funcall (canvas-on-press canvas) canvas x y button (session-click-count session))))

(defmethod pointer-move ((canvas canvas) session x y)
  (declare (ignore session))
  (when (canvas-on-move canvas)
    (funcall (canvas-on-move canvas) canvas x y)))

(defmethod pointer-exit ((canvas canvas) session)
  (declare (ignore session))
  (when (canvas-on-exit canvas)
    (funcall (canvas-on-exit canvas) canvas)))

(defmethod pointer-drag ((canvas canvas) session x y)
  (declare (ignore session))
  (when (canvas-on-drag canvas)
    (funcall (canvas-on-drag canvas) canvas x y)))

(defmethod pointer-release ((canvas canvas) session x y inside-p)
  (declare (ignore session))
  (when (canvas-on-release canvas)
    (funcall (canvas-on-release canvas) canvas x y inside-p)))

(defmethod pointer-wheel ((canvas canvas) session dx dy)
  (declare (ignore session))
  (and (canvas-on-wheel canvas)
       (funcall (canvas-on-wheel canvas) canvas dx dy)))

(defmethod key-input ((canvas canvas) session event)
  (declare (ignore session))
  (and (canvas-on-key canvas)
       (funcall (canvas-on-key canvas) canvas event)))
