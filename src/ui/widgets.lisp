(defpackage #:almighty-bolt/ui/widgets
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/hash
                #:mix
                #:hash-of
                #:+seed+)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
                #:box-kind
                #:box-text
                #:box-size
                #:box-color
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
                #:box-enabled
                #:box-look
                #:box-n1
                #:box-n2
                #:box-n3
                #:box-data
                #:box-aux-doubles
                #:box-interactive
                #:box-focusable
                #:box-moves
                #:box-children)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:ui-context
                #:ui-consumed
                #:ui-hot
                #:ui-active
                #:ui-focus
                #:ui-started
                #:ui-pointer-x
                #:ui-pointer-y
                #:ui-down
                #:ui-pressed
                #:ui-click-count
                #:ui-released
                #:ui-moved
                #:ui-keys
                #:ui-inputs
                #:ui-wheel-dy
                #:ui-boxes
                #:ui-count
                #:consume-input
                #:ui-states
                #:open-box
                #:close-box
                #:consume-press
                #:focus-by-click
                #:consume-release
                #:consume-secondary-press
                #:consume-key
                #:consume-wheel
                #:rect-of
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h
                #:state-of)
  (:import-from #:almighty-bolt/internals
                #:with-painting
                #:context-font
                #:context-measure
                #:context-caret-x
                #:context-index-at-x
                #:make-text-state
                #:text-state-string
                #:text-state-caret
                #:text-state-anchor
                #:text-place-caret
                #:text-insert
                #:text-state-selection-p
                #:text-state-selected-string
                #:edit-text
                #:context-clipboard-text
                #:context-set-clipboard-text
                #:key-event-kind
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-super+
                #:+modifier-control+
                #:key-event-logical-key
                #:make-display-list
                #:clear-display-list
                #:display-list-hash
                #:+key-left+
                #:+key-right+
                #:+key-up+
                #:+key-down+
                #:+key-home+
                #:+key-end+
                #:+points-per-line+)
  (:import-from #:almighty-bolt/ui/theme
                #:+font-name+
                #:+font-size+)
  (:export #:missing-id
           #:missing-id-widget
           #:label
           #:paragraph
           #:paragraph-state
           #:paragraph-state-text
           #:paragraph-state-lines
           #:paragraph-state-wrapped-at
           #:paragraph-state-generation
           #:set-paragraph-lines
           #:button
           #:text-field
           #:slider
           #:choice
           #:checkbox
           #:list-box
           #:progress
           #:canvas
           #:scrim
           #:column
           #:row
           #:stack
           #:padding
           #:panel
           #:center
           #:scroll
           #:begin-scroll
           #:end-scroll
           #:scroll-state
           #:scroll-state-offset
           #:scroll-state-content-height
           #:scroll-state-viewport-height
           #:lazy-list
           #:begin-lazy-list
           #:lazy-list-rows
           #:+lazy-list-height+
           #:own-hashes
           #:field-state
           #:field-state-state
           #:field-state-view-start
           #:choice-state
           #:choice-state-lefts
           #:choice-state-widths
           #:list-state
           #:list-state-top
           #:canvas-state
           #:canvas-state-fragment
           #:ui-font
           #:+field-padding+
           #:+row-height+
           #:+thumb-size+
           #:+dot-size+
           #:+option-gap+))

(in-package #:almighty-bolt/ui/widgets)

;;; The widgets of the immediate variant, which are functions the interface calls, in order,
;;; every build. A widget that can be used returns what happened to it (a button, whether it
;;; was clicked; a field, its text and whether the person changed it), and holds nothing but
;;; the little that must outlive a frame (a caret), which is kept by its key. What the
;;; application holds (the text, the number, the choice) is passed in each time and is what
;;; the widget shows, so the application is in charge.

(defconstant +field-padding+ 8f0)
(defconstant +row-height+ 24f0)
(defconstant +thumb-size+ 16f0)
(defconstant +dot-size+ 16f0)
(defconstant +option-gap+ 20f0)

(defun ui-font (context)
  (context-font context +font-name+ +font-size+))

;;; Every widget takes an :id, and the call is an error without one. The id is what keeps a
;;; widget's caret, selection, scroll and the keyboard with it from frame to frame and across a
;;; reload, and what a test or a replay script finds it by.

(define-condition missing-id (error)
  ((widget :initarg :widget :reader missing-id-widget))
  (:report (lambda (condition stream)
             (format stream "A ~(~a~) needs an :id, a name that stays with it from frame to frame, ~
                             as in (~(~a~) ... :id :a-name)."
                     (missing-id-widget condition) (missing-id-widget condition)))))

(defun required-id (widget)
  "The default of the :id of WIDGET (a symbol), which is to say that there is none."
  (error 'missing-id :widget widget))

(defmacro hash-numbers (hash &rest numbers)
  "HASH with each of NUMBERS folded into it, in order. A macro, so that it makes no list."
  (let ((h (gensym "HASH")))
    `(let ((,h ,hash))
       ,@(loop :for number :in numbers
               :collect `(setf ,h (mix ,h (hash-of ,number))))
       ,h)))

;;; --- the look of a box from the pointer and keyboard ---

(defun look-of (ui key enabled)
  "A small number for how the box KEY looks: 0 plain, 1 hovered, 2 pressed (the pointer over
the box a press began on), 3 disabled, and 4 more if it has the keyboard."
  (+ (cond ((not enabled) 3)
           ((and (= (ui-active ui) key) (= (ui-hot ui) key)) 2)
           ((and (= (ui-hot ui) key) (zerop (ui-active ui))) 1)
           (t 0))
     (if (and enabled (= (ui-focus ui) key)) 4 0)))

;;; --- containers ---

(defmacro with-container ((box kind &rest keys) &body body)
  `(let ((,box (open-box *ui* ,kind :id ,(getf keys :id))))
     ,@body
     (close-box *ui*)
     nil))

(defmacro column ((&key id (gap 0) (main-align :start) (cross-align :start) fill (flex 0))
                  &body body)
  "Children one under another."
  (let ((box (gensym "BOX")))
    `(with-container (,box :column :id ,id)
       (setf (box-gap ,box) (float ,gap 1f0) (box-main-align ,box) ,main-align
             (box-cross-align ,box) ,cross-align (box-fill ,box) ,fill
             (box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro row ((&key id (gap 0) (main-align :start) (cross-align :start) fill (flex 0))
               &body body)
  "Children side by side."
  (let ((box (gensym "BOX")))
    `(with-container (,box :row :id ,id)
       (setf (box-gap ,box) (float ,gap 1f0) (box-main-align ,box) ,main-align
             (box-cross-align ,box) ,cross-align (box-fill ,box) ,fill
             (box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro stack ((&key id (flex 0)) &body body)
  "Children on top of each other, the later on top."
  (let ((box (gensym "BOX")))
    `(with-container (,box :stack :id ,id)
       (setf (box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro padding ((&key id all left top right bottom (flex 0)) &body body)
  (let ((box (gensym "BOX")))
    `(with-container (,box :padding :id ,id)
       (setf (box-pad-left ,box) (float (or ,left ,all 0) 1f0)
             (box-pad-top ,box) (float (or ,top ,all 0) 1f0)
             (box-pad-right ,box) (float (or ,right ,all 0) 1f0)
             (box-pad-bottom ,box) (float (or ,bottom ,all 0) 1f0)
             (box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro panel ((&key id all left top right bottom color (radius 8) (flex 0)) &body body)
  "Children on a rounded background."
  (let ((box (gensym "BOX")))
    `(with-container (,box :panel :id ,id)
       (setf (box-pad-left ,box) (float (or ,left ,all 0) 1f0)
             (box-pad-top ,box) (float (or ,top ,all 0) 1f0)
             (box-pad-right ,box) (float (or ,right ,all 0) 1f0)
             (box-pad-bottom ,box) (float (or ,bottom ,all 0) 1f0)
             (box-color ,box) (or ,color 0)
             (box-n1 ,box) (float ,radius 1f0)
             (box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro center ((&key id (flex 0)) &body body)
  "The child in the middle of all the room there is."
  (let ((box (gensym "BOX")))
    `(with-container (,box :center :id ,id)
       (setf (box-flex ,box) (float ,flex 1f0))
       ,@body)))

;;; --- a scroll container ---
;;;
;;; What is inside is laid out as a column as tall as it wants to be, and the container shows a
;;; window onto it, moved by the wheel. How far down the window is (the offset, in points) is kept by
;;; the container's id, and is part of what decides the layout: the children are placed above the
;;; window by it. The wheel goes first to what is inside (a list inside scrolls its rows), and what is
;;; left of it, while the pointer is over the container or anything in it, moves the window. When the
;;; keyboard comes to something inside that is out of the window, the window moves to show it.

(defstruct (scroll-state (:constructor make-scroll-state ()))
  (offset 0f0 :type single-float)
  ;; From the last layout: how tall what is inside is, and how tall the window onto it.
  (content-height 0f0 :type single-float)
  (viewport-height 0f0 :type single-float)
  ;; The index of the first box inside the container, this build.
  (first-inside 0 :type fixnum)
  ;; The key of what had the keyboard when the container was last described.
  (last-focus 0 :type fixnum))

(defun begin-scroll (id height flex)
  (let* ((ui *ui*)
         (box (open-box ui :scroll :id (or id (required-id 'scroll))))
         (state (state-of ui (box-key box) #'make-scroll-state)))
    (setf (box-interactive box) t
          (box-pref-h box) (if height (float height 1f0) 0f0)
          (box-flex box) (float flex 1f0)
          (box-data box) state
          (scroll-state-first-inside state) (ui-count ui))
    box))

(defun inside-scroll-p (ui state key)
  "True if the box KEY was made inside the scroll container whose state is STATE, this build."
  (let ((boxes (ui-boxes ui)))
    (loop :for index :from (scroll-state-first-inside state) :below (ui-count ui)
            :thereis (= (box-key (svref boxes index)) key))))

(defun end-scroll (box)
  (let* ((ui *ui*)
         (key (box-key box))
         (state (box-data box))
         (rect (rect-of ui key))
         (offset (scroll-state-offset state))
         (viewport (scroll-state-viewport-height state)))
    (when (and rect (/= (ui-wheel-dy ui) 0f0)
               (or (= (ui-hot ui) key) (inside-scroll-p ui state (ui-hot ui))))
      (multiple-value-bind (dx dy) (consume-wheel ui)
        (declare (ignore dx))
        (decf offset (* dy (float +points-per-line+ 1f0)))))
    ;; The keyboard came to something inside: show all of it, moving the window as little as can be.
    (let ((focus (ui-focus ui)))
      (when (and rect (/= focus (scroll-state-last-focus state)) (/= focus 0)
                 (inside-scroll-p ui state focus))
        (let ((target (rect-of ui focus)))
          (when target
            (let* ((top (+ (- (rect-y target) (rect-y rect)) offset))
                   (bottom (+ top (rect-h target))))
              (cond ((< top offset) (setf offset top))
                    ((> bottom (+ offset viewport)) (setf offset (- bottom viewport))))))))
      (setf (scroll-state-last-focus state) focus))
    (setf offset (max 0f0 (min offset (max 0f0 (- (scroll-state-content-height state) viewport)))))
    (setf (scroll-state-offset state) offset
          (box-n1 box) offset)
    (close-box ui)
    nil))

(defmacro scroll ((&key id height (flex 0)) &body body)
  "What BODY describes, one under another, in a window that the wheel moves up and down. The window
is HEIGHT points tall where the container is not given a height (in a column, which gives only the
room it has), or as tall as what is inside; FLEX gives it a share of a column's free space. The
:id is required: the place the window is at is kept by it."
  (let ((box (gensym "BOX")))
    `(let ((,box (begin-scroll ,id ,height ,flex)))
       ,@body
       (end-scroll ,box))))

;;; --- a list that builds only the rows that show ---
;;;
;;; A scroll container whose rows are all ROW-HEIGHT tall, so that which of them show is known before
;;; any is built: only those are described, each in a column of its own placed where its index puts
;;; it, and a list of a million rows costs what the rows in the window do. Its offset, wheel, clipping
;;; and keyboard are the scroll container's (END-SCROLL). The window's height is known from the last
;;; layout; before there is one, HEIGHT (or +LAZY-LIST-HEIGHT+) is taken for it, and if the window
;;; turns out taller, layout asks for another frame.

(defconstant +lazy-list-height+ 400f0
  "How tall a lazy list is when it is given no height and nothing gives it one.")

(defun begin-lazy-list (id count row-height height width flex)
  (let* ((ui *ui*)
         (box (open-box ui :lazy-list :id (or id (required-id 'lazy-list))))
         (state (state-of ui (box-key box) #'make-scroll-state))
         (row-height (float row-height 1f0)))
    (setf (box-interactive box) t
          (box-pref-h box) (if height (float height 1f0) 0f0)
          (box-pref-w box) (if width (float width 1f0) 0f0)
          (box-flex box) (float flex 1f0)
          (box-n2 box) row-height
          (box-n3 box) count
          (box-data box) state
          (scroll-state-content-height state) (* count row-height)
          (scroll-state-first-inside state) (ui-count ui))
    ;; Where the window is, kept inside what there is now (the list may have become shorter).
    (let ((viewport (lazy-list-viewport box state)))
      (setf (box-n1 box) (max 0f0 (min (scroll-state-offset state)
                                       (- (* count row-height) viewport)))))
    box))

(defun lazy-list-viewport (box state)
  "How tall the window of the lazy list BOX is: from the last layout, or before there is one, what it
will be given if nothing gives it a height."
  (if (plusp (scroll-state-viewport-height state))
      (scroll-state-viewport-height state)
      (if (plusp (box-pref-h box)) (box-pref-h box) +lazy-list-height+)))

(defun lazy-list-rows (box items key function)
  "Describe the rows of the lazy list BOX that show: FUNCTION is called with each of those ITEMS, in a
column of its own whose id is what KEY returns for the item (or, with no KEY, its index)."
  (let* ((ui *ui*)
         (state (box-data box))
         (row-height (box-n2 box))
         (count (box-n3 box))
         (offset (box-n1 box))
         (first (min count (floor offset row-height)))
         (end (min count (ceiling (+ offset (lazy-list-viewport box state)) row-height))))
    (flet ((row (item index)
             (let ((row (open-box ui :column :id (if key (funcall key item) index))))
               (setf (box-n3 row) index)
               (funcall function item)
               (close-box ui))))
      (etypecase items
        (vector (loop :for index :from first :below end
                      :do (row (aref items index) index)))
        (list (loop :for item :in (nthcdr first items)
                    :for index :from first :below end
                    :do (row item index)))))
    nil))

(defmacro lazy-list ((var items &key id key (row-height 24) height width (flex 0)) &body body)
  "A list of ITEMS (a vector, or a list) in a window that the wheel moves up and down, of which only
the rows that show are described: BODY is run for each of them with VAR bound to its item, and what it
describes is the row, ROW-HEIGHT points tall. KEY is a function that gives an item's id, so that what
is kept for a row (a caret, the keyboard) stays with its item when the items are reordered; with no
KEY, a row is known by its index. The window is HEIGHT points tall where nothing gives it a height
(400 if there is no HEIGHT), and FLEX gives it a share of a column's free space; it is WIDTH points
wide, or as wide as it is allowed to be. The :id is required."
  (let ((box (gensym "BOX"))
        (list (gensym "ITEMS")))
    `(let* ((,list ,items)
            (,box (begin-lazy-list ,id (length ,list) ,row-height ,height ,width ,flex)))
       (lazy-list-rows ,box ,list ,key (lambda (,var) (declare (ignorable ,var)) ,@body))
       (end-scroll ,box))))

;;; --- text ---

(defun label (text &key (id (required-id 'label)) (size 15) color (flex 0) live)
  "A line of TEXT. A LIVE label is read out by a screen reader whenever its text changes, wherever the
reader is: for a result, a count or a status that a person should hear without going to look for it."
  (let ((box (open-box *ui* :label :id id)))
    (setf (box-text box) text
          (box-size box) size
          (box-color box) (or color 0)
          (box-flex box) (float flex 1f0)
          (box-n3 box) (if live 1 0))
    (close-box *ui*)
    nil))

;;; A paragraph is text broken into lines to the width it is given, so its height depends on
;;; its width: layout works the lines out (they go in a state kept by the box's key, which is
;;; there when layout is skipped for a frame that is the same) and painting draws them.

(defstruct (paragraph-state (:constructor make-paragraph-state ()))
  (text "" :type string)
  (wrapped-at -1f0 :type single-float)
  (lines '() :type list)
  (generation 0 :type fixnum))

(defun set-paragraph-lines (state text wrapped-at lines)
  "Note the LINES that TEXT breaks into at WRAPPED-AT; a new generation says that the lines
are not the ones that were drawn before."
  (setf (paragraph-state-text state) text
        (paragraph-state-wrapped-at state) wrapped-at
        (paragraph-state-lines state) lines)
  (incf (paragraph-state-generation state))
  state)

(defun paragraph (text &key (id (required-id 'paragraph)) (size 15) color (flex 0) live)
  "TEXT broken into lines to the width it is given, SIZE points high. LIVE is as for a label."
  (let* ((ui *ui*)
         (box (open-box ui :paragraph :id id)))
    (setf (box-text box) text
          (box-size box) size
          (box-color box) (or color 0)
          (box-flex box) (float flex 1f0)
          (box-n3 box) (if live 1 0)
          (box-data box) (state-of ui (box-key box) #'make-paragraph-state))
    (close-box ui)
    nil))

(defun progress (fraction &key (id (required-id 'progress)) labelled-by (width 200) (flex 0))
  "A bar that is FRACTION (0 to 1) full."
  (let ((box (open-box *ui* :progress :id id :labelled-by labelled-by)))
    (setf (box-n1 box) (float (max 0 (min 1 fraction)) 1f0)
          (box-pref-w box) (float width 1f0)
          (box-flex box) (float flex 1f0))
    (close-box *ui*)
    nil))

;;; --- a button ---

(defun button (text &key (id (required-id 'button)) (enabled t) (flex 0))
  "A button. Returns true when it is clicked: the primary button went down and up over it,
or the space bar or Return was pressed with the keyboard on it."
  (let* ((ui *ui*)
         (box (open-box ui :button :id id))
         (key (box-key box))
         (clicked nil))
    (setf (box-text box) text
          (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui-hot ui) key) (ui-pressed ui))
        (consume-press ui)
        (setf (ui-active ui) key))
      (when (and (= (ui-active ui) key) (ui-released ui))
        (consume-release ui)
        (setf (ui-active ui) 0)
        (when (= (ui-hot ui) key)
          (setf clicked t)))
      (when (= (ui-focus ui) key)
        (dolist (event (copy-list (ui-keys ui)))
          (when (member (key-event-logical-key event) '(#\Space #\Return))
            (consume-key ui event)
            (setf clicked t)))))
    (setf (box-look box) (look-of ui key enabled))
    (close-box ui)
    clicked))

;;; --- copy, cut and paste ---

(defun one-line (text)
  "TEXT with each line break in it (a return, a line feed, or the two together) made a space."
  (with-output-to-string (out)
    (let ((after-return nil))
      (loop :for character :across text
            :do (cond ((char= character #\Return) (write-char #\Space out) (setf after-return t))
                      ((char= character #\Linefeed)
                       (unless after-return (write-char #\Space out))
                       (setf after-return nil))
                      (t (write-char character out) (setf after-return nil)))))))

(defun clipboard-edit (state event context &key single-line)
  "What the key EVENT does to the text STATE if it is Command-C, Command-X or Command-V, as two
values: the new state, and true if it was one of them. Copy and cut take the selection, and do
nothing without one; paste puts the clipboard's text in place of the selection, with its line breaks
made spaces if SINGLE-LINE."
  (let ((key (key-event-logical-key event))
        (modifiers (key-event-modifiers event)))
    (if (not (and (eq (key-event-kind event) :down)
                  (characterp key)
                  (modifier-down-p modifiers +modifier-super+)
                  (not (modifier-down-p modifiers +modifier-control+))
                  (member key '(#\c #\x #\v) :test #'char-equal)))
        (values state nil)
        (values (cond ((char-equal key #\v)
                       (let ((text (context-clipboard-text context)))
                         (if text
                             (text-insert state (if single-line (one-line text) text))
                             state)))
                      ((not (text-state-selection-p state)) state)
                      (t (context-set-clipboard-text context (text-state-selected-string state))
                         (if (char-equal key #\x) (text-insert state "") state)))
                t))))

;;; --- a line of editable text ---

(defstruct (field-state (:constructor make-field-state ()))
  (state (make-text-state "") :type almighty-bolt/internals:text-state)
  (view-start 0 :type fixnum))

(defun field-index-at (ui key state x)
  "The index in the text of the field KEY that the pointer, at X, is nearest."
  (let ((rect (rect-of ui key))
        (context (ui-context ui)))
    (if (and rect context)
        (let* ((string (text-state-string (field-state-state state)))
               (start (field-state-view-start state)))
          (+ start (context-index-at-x context (ui-font context) (subseq string start)
                                       (- x (rect-x rect) +field-padding+))))
        (text-state-caret (field-state-state state)))))

(defun keep-caret-visible (ui state width)
  (let ((context (ui-context ui)))
    (when context
      (let* ((font (ui-font context))
             (string (text-state-string (field-state-state state)))
             (caret (text-state-caret (field-state-state state)))
             (start (min (field-state-view-start state) caret))
             (inner (- width (* 2 +field-padding+))))
        (loop :while (and (< start caret)
                          (> (context-caret-x context font (subseq string start) (- caret start))
                             inner))
              :do (incf start))
        (setf (field-state-view-start state) start)))))

(defun text-field (string
                   &key (id (required-id 'text-field)) labelled-by
                     (width 180) invalid (enabled t) (flex 0))
  "An editable line showing STRING. Returns the text now, whether the person changed it this
frame, and whether Return was pressed. Command-C, Command-X and Command-V copy, cut and paste
(a line break that is pasted becomes a space). Pass what it returned back in the next frame; if the
application passes something else, that is what is shown."
  (let* ((ui *ui*)
         (box (open-box ui :text-field :id id :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-field-state))
         (changed nil)
         (submitted nil))
    (unless (string= (text-state-string (field-state-state state)) string)
      (setf (field-state-state state) (make-text-state string)
            (field-state-view-start state) 0))
    (setf (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-pref-w box) (float width 1f0)
          (box-n3 box) (if invalid 1 0)
          (box-data box) state
          (box-flex box) (float flex 1f0))
    (when enabled
      (let ((x (ui-pointer-x ui)))
        ;; A press on the field gives it the keyboard and puts the caret there.
        (when (and (= (ui-hot ui) key) (ui-pressed ui))
          (consume-press ui)
          (focus-by-click ui key)
          (setf (ui-active ui) key
                (field-state-state state)
                (text-place-caret (field-state-state state) (field-index-at ui key state x))))
        ;; Dragging with the button down extends the selection.
        (when (and (= (ui-active ui) key) (ui-down ui))
          (setf (field-state-state state)
                (text-place-caret (field-state-state state) (field-index-at ui key state x)
                                  :extend t)))
        (when (and (= (ui-active ui) key) (ui-released ui))
          (consume-release ui)
          (setf (ui-active ui) 0)))
      (when (= (ui-focus ui) key)
        (dolist (event (copy-list (ui-keys ui)))
          (cond
            ((eql (key-event-logical-key event) #\Return)
             (consume-key ui event)
             (setf submitted t))
            (t
             (multiple-value-bind (new edited)
                 (multiple-value-bind (pasted command)
                     (clipboard-edit (field-state-state state) event (ui-context ui) :single-line t)
                   (if command
                       (values pasted t)
                       (edit-text (field-state-state state) event)))
               (when edited
                 (consume-key ui event)
                 (unless (string= (text-state-string new)
                                  (text-state-string (field-state-state state)))
                   (setf changed t))
                 (setf (field-state-state state) new))))))))
    (keep-caret-visible ui state (float width 1f0))
    (setf (box-look box) (look-of ui key enabled)
          (box-text box) (text-state-string (field-state-state state)))
    (close-box ui)
    (values (text-state-string (field-state-state state)) changed submitted)))

;;; --- a slider ---

(defun slider (value minimum maximum
               &key (id (required-id 'slider)) labelled-by (width 200) (enabled t) (flex 0))
  "A slider showing VALUE between MINIMUM and MAXIMUM. Returns the value now and whether the
person changed it this frame."
  (let* ((ui *ui*)
         (box (open-box ui :slider :id id :labelled-by labelled-by))
         (key (box-key box))
         (new (float value 1d0))
         (range (- (float maximum 1d0) (float minimum 1d0))))
    (flet ((slide-to (x)
             (let ((rect (rect-of ui key)))
               (when rect
                 (let ((travel (max 1f0 (- (rect-w rect) +thumb-size+))))
                   (setf new (+ minimum
                                (* (max 0d0 (min 1d0 (/ (- x (rect-x rect) (/ +thumb-size+ 2))
                                                        travel)))
                                   range))))))))
      (setf (box-enabled box) enabled
            (box-interactive box) enabled
            (box-focusable box) enabled
            (box-pref-w box) (float width 1f0)
            (box-flex box) (float flex 1f0))
      (when enabled
        (when (and (= (ui-hot ui) key) (ui-pressed ui))
          (consume-press ui)
          (focus-by-click ui key)
          (setf (ui-active ui) key))
        (when (= (ui-active ui) key)
          (when (ui-down ui)
            (slide-to (ui-pointer-x ui)))
          (when (ui-released ui)
            (consume-release ui)
            (setf (ui-active ui) 0)))
        (when (= (ui-focus ui) key)
          (dolist (event (copy-list (ui-keys ui)))
            (let* ((k (key-event-logical-key event))
                   (step (/ range 20))
                   (moved (cond ((or (eql k +key-left+) (eql k +key-down+)) (- new step))
                                ((or (eql k +key-right+) (eql k +key-up+)) (+ new step))
                                ((eql k +key-home+) (float minimum 1d0))
                                ((eql k +key-end+) (float maximum 1d0)))))
              (when moved
                (consume-key ui event)
                (setf new (max (float minimum 1d0) (min (float maximum 1d0) moved))))))))
      (setf new (max (float minimum 1d0) (min (float maximum 1d0) new)))
      ;; A new value may change what the application shows before the slider.
      (when (/= new (float value 1d0))
        (setf (ui-consumed ui) t))
      (setf (box-n1 box) (if (plusp range)
                             (float (/ (- new (float minimum 1d0)) range) 1f0)
                             0f0)
            (box-look box) (look-of ui key enabled))
      (let ((aux (box-aux-doubles box)))
        (setf (aref aux 0) new
              (aref aux 1) (float minimum 1d0)
              (aref aux 2) (float maximum 1d0)))
      (close-box ui)
      (values new (/= new (float value 1d0))))))

;;; --- a choice between options ---

(defstruct (choice-state (:constructor make-choice-state ()))
  (lefts #() :type simple-vector)
  (widths #() :type simple-vector))

(defun choice (options selected &key (id (required-id 'choice)) labelled-by (enabled t) (flex 0))
  "A choice among OPTIONS (strings) side by side, with the SELECTED one (an index) marked.
Returns the index now and whether the person changed it this frame."
  (let* ((ui *ui*)
         (box (open-box ui :choice :id id :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-choice-state))
         (new selected))
    (setf (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-data box) (cons options state)
          (box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui-hot ui) key) (ui-pressed ui))
        (consume-press ui)
        (setf (ui-active ui) key))
      (when (and (= (ui-active ui) key) (ui-released ui))
        (consume-release ui)
        (setf (ui-active ui) 0)
        (let ((rect (rect-of ui key))
              (lefts (choice-state-lefts state))
              (widths (choice-state-widths state)))
          (when (and rect (= (ui-hot ui) key))
            (dotimes (i (length lefts))
              (let ((left (+ (rect-x rect) (svref lefts i))))
                (when (and (<= left (ui-pointer-x ui))
                           (< (ui-pointer-x ui) (+ left (svref widths i))))
                  (setf new i)
                  (return)))))))
      (when (= (ui-focus ui) key)
        (dolist (event (copy-list (ui-keys ui)))
          (let ((k (key-event-logical-key event))
                (count (length options)))
            (cond ((and (or (eql k +key-left+) (eql k +key-up+)) (plusp new))
                   (consume-key ui event) (decf new))
                  ((and (or (eql k +key-right+) (eql k +key-down+)) (< new (1- count)))
                   (consume-key ui event) (incf new)))))))
    (setf (box-n3 box) new
          (box-look box) (look-of ui key enabled))
    (close-box ui)
    (values new (/= new selected))))

;;; --- a box that is checked or not, with a label ---

(defun checkbox (label checked &key (id (required-id 'checkbox)) (enabled t) (flex 0))
  "A box with LABEL beside it, CHECKED or not (what the application holds). Returns the state now
and whether the person changed it this frame: the primary button went down and up over the box or
the label, or the space bar was pressed with the keyboard on it."
  (let* ((ui *ui*)
         (box (open-box ui :checkbox :id id))
         (key (box-key box))
         (new (and checked t)))
    (setf (box-text box) label
          (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui-hot ui) key) (ui-pressed ui))
        (consume-press ui)
        (setf (ui-active ui) key))
      (when (and (= (ui-active ui) key) (ui-released ui))
        (consume-release ui)
        (setf (ui-active ui) 0)
        (when (= (ui-hot ui) key)
          (setf new (not new))))
      (when (= (ui-focus ui) key)
        (dolist (event (copy-list (ui-keys ui)))
          (when (eql (key-event-logical-key event) #\Space)
            (consume-key ui event)
            (setf new (not new))))))
    (setf (box-n3 box) (if new 1 0)
          (box-look box) (look-of ui key enabled))
    (close-box ui)
    (values new (not (eq new (and checked t))))))

;;; --- a list to pick from ---

(defstruct (list-state (:constructor make-list-state ()))
  (top 0 :type fixnum)
  (remainder 0f0 :type single-float))

(defun list-box (items selected
                 &key (id (required-id 'list-box)) labelled-by
                   (rows 8) (width 200) (enabled t) (flex 0))
  "A list of ITEMS (strings), showing ROWS at a time, with the SELECTED one (an index, or
NIL) marked. Returns the index now and whether the person changed it this frame."
  (let* ((ui *ui*)
         (box (open-box ui :list-box :id id :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-list-state))
         (count (length items))
         (new selected))
    (flet ((pick (index)
             (when (and (<= 0 index) (< index count))
               (setf new index)
               ;; Scroll to show it.
               (cond ((< index (list-state-top state)) (setf (list-state-top state) index))
                     ((>= index (+ (list-state-top state) rows))
                      (setf (list-state-top state) (1+ (- index rows))))))))
      (setf (box-enabled box) enabled
            (box-interactive box) enabled
            (box-focusable box) enabled
            (box-pref-w box) (float width 1f0)
            (box-n3 box) rows
            (box-flex box) (float flex 1f0))
      (when enabled
        (let ((rect (rect-of ui key)))
          (when (and (= (ui-hot ui) key) (ui-pressed ui))
            (consume-press ui)
            (focus-by-click ui key)
            (setf (ui-active ui) key)
            (when rect
              (let ((index (+ (list-state-top state)
                              (floor (- (ui-pointer-y ui) (rect-y rect) 1) +row-height+))))
                (when (and (< index count) (not (eql index selected)))
                  (pick index)))))
          (when (and (= (ui-active ui) key) (ui-released ui))
            (consume-release ui)
            (setf (ui-active ui) 0)))
        (when (= (ui-hot ui) key)
          (multiple-value-bind (dx dy) (consume-wheel ui)
            (declare (ignore dx))
            (let ((amount (+ (list-state-remainder state) (- dy))))
              (let ((whole (truncate amount)))
                (setf (list-state-remainder state) (- amount whole))
                (setf (list-state-top state)
                      (max 0 (min (max 0 (- count rows)) (+ (list-state-top state) whole))))))))
        (when (= (ui-focus ui) key)
          (dolist (event (copy-list (ui-keys ui)))
            (let ((k (key-event-logical-key event)))
              (cond ((eql k +key-down+)
                     (consume-key ui event)
                     (pick (if new (min (1- count) (1+ new)) 0)))
                    ((eql k +key-up+)
                     (consume-key ui event)
                     (pick (if new (max 0 (1- new)) 0))))))))
      (setf (list-state-top state) (max 0 (min (list-state-top state) (max 0 (- count rows)))))
      (setf (box-data box) (cons items state)
            (box-n2 box) (float (or new -1) 1f0)
            (box-look box) (look-of ui key enabled))
      (close-box ui)
      (values new (not (eql new selected))))))

;;; --- a canvas the application draws on ---

(defstruct (canvas-state (:constructor make-canvas-state ()))
  (fragment (make-display-list :capacity 256))
  (was-hot nil))

(defun canvas (width height painter
               &key (id (required-id 'canvas)) labelled-by focusable autofocus (flex 0))
  "An area of WIDTH by HEIGHT. PAINTER is called every frame with the list to append to, the
context, and the width and height, and draws from the top left of the area, which it clips
to; if what it draws is what it drew last frame, nothing more is done for it. Returns the
events for the canvas, oldest first: (:press x y button click-count), (:move x y), (:drag x y),
(:release x y inside-p), (:key event), (:text string), (:composition kind text selection-start
selection-length), (:wheel dx dy) and (:exit), with points in the canvas. The code that handles
them can ask CURRENT-CONTEXT for the context, which measures text. A FOCUSABLE canvas that is
also AUTOFOCUS has the keyboard when the window opens, if nothing has it by then."
  (let* ((ui *ui*)
         (box (open-box ui :canvas :id id :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-canvas-state))
         (events '())
         (rect (rect-of ui key))
         (hot (= (ui-hot ui) key)))
    (when (and autofocus focusable (not (ui-started ui)) (zerop (ui-focus ui)))
      (setf (ui-focus ui) key))
    (setf (box-interactive box) t
          (box-focusable box) focusable
          (box-moves box) t
          (box-pref-w box) (float width 1f0)
          (box-pref-h box) (float height 1f0)
          (box-flex box) (float flex 1f0))
    (when rect
      (let ((x (- (ui-pointer-x ui) (rect-x rect)))
            (y (- (ui-pointer-y ui) (rect-y rect))))
        (when (and hot (ui-pressed ui))
          (consume-press ui)
          (setf (ui-active ui) key)
          (when focusable (focus-by-click ui key))
          (push (list :press x y 0 (ui-click-count ui)) events))
        (when (and hot (consume-secondary-press ui))
          (push (list :press x y 1 1) events))
        (when (and (ui-moved ui) (or hot (= (ui-active ui) key)))
          (setf (ui-moved ui) nil
                (ui-consumed ui) t)
          (push (list (if (and (= (ui-active ui) key) (ui-down ui)) :drag :move) x y) events))
        (when (and (= (ui-active ui) key) (ui-released ui))
          (consume-release ui)
          (setf (ui-active ui) 0)
          (push (list :release x y hot) events))
        (when hot
          (multiple-value-bind (dx dy) (consume-wheel ui)
            (when (or (/= dx 0f0) (/= dy 0f0))
              (push (list :wheel dx dy) events))))))
    (when (and (canvas-state-was-hot state) (not hot))
      (setf (ui-consumed ui) t)
      (push (list :exit) events))
    (setf (canvas-state-was-hot state) hot)
    (when (and focusable (= (ui-focus ui) key))
      (dolist (event (copy-list (ui-inputs ui)))
        (typecase event
          (almighty-bolt/internals:key-event
           ;; A key that a widget took is gone from the keys, and is not the canvas's.
           (when (member event (ui-keys ui) :test #'eq)
             (consume-key ui event)
             (consume-input ui event)
             (push (list :key event) events)))
          (almighty-bolt/internals:text-input-event
           (consume-input ui event)
           (push (list :text (almighty-bolt/internals:text-input-event-text event)) events))
          (almighty-bolt/internals:composition-event
           (consume-input ui event)
           (push (list :composition (almighty-bolt/internals:composition-event-kind event)
                       (almighty-bolt/internals:composition-event-text event)
                       (almighty-bolt/internals:composition-event-selection-start event)
                       (almighty-bolt/internals:composition-event-selection-length event))
                 events)))))
    ;; What it draws now, compared with what it drew before.
    (let ((fragment (canvas-state-fragment state)))
      (loop
        (clear-display-list fragment)
        (handler-case (progn (with-painting ((ui-context ui) id)
                               (funcall painter fragment (ui-context ui) (float width 1f0)
                                        (float height 1f0)))
                             (return))
          (almighty-bolt/internals:display-list-full ()
            (setf fragment (make-display-list
                            :capacity (* 2 (almighty-bolt/internals:display-list-capacity fragment)))
                  (canvas-state-fragment state) fragment))))
      (setf (box-data box) fragment
            (box-n3 box) (display-list-hash fragment)))
    (setf (box-look box) (look-of ui key t))
    (close-box ui)
    (nreverse events)))

;;; --- a veil that takes the pointer ---

(defun scrim (&key (id (required-id 'scrim)))
  "A translucent layer over everything before it in a stack, as large as it is allowed to be,
that takes the pointer from what is under it. Returns true when it is pressed."
  (let* ((ui *ui*)
         (box (open-box ui :scrim :id id))
         (key (box-key box))
         (pressed nil))
    (setf (box-interactive box) t)
    (when (and (= (ui-hot ui) key) (ui-pressed ui))
      (consume-press ui)
      (setf pressed t))
    (close-box ui)
    pressed))

;;; --- what decides size and look ---

(defun own-hashes (box)
  "The hash of what decides BOX's size and of what decides how it looks, as two values, not
counting its children."
  (let* ((layout (hash-numbers +seed+ (hash-of (box-kind box)) (hash-of (box-text box))
                               (box-size box) (hash-of (box-flex box)) (hash-of (box-gap box))
                               (hash-of (box-main-align box)) (hash-of (box-cross-align box))
                               (hash-of (box-fill box)) (hash-of (box-id box))
                               (hash-of (box-interactive box)) (hash-of (box-focusable box))
                               (hash-of (box-moves box))
                               (hash-of (box-pad-left box)) (hash-of (box-pad-top box))
                               (hash-of (box-pad-right box)) (hash-of (box-pad-bottom box))
                               (hash-of (box-pref-w box)) (hash-of (box-pref-h box))))
         (layout (case (box-kind box)
                   (:choice (mix layout (hash-options (car (box-data box)))))
                   (:list-box (mix layout (box-n3 box)))
                   (:scroll (mix layout (hash-of (box-n1 box))))
                   (:lazy-list (hash-numbers layout (hash-of (box-n1 box)) (hash-of (box-n2 box))
                                             (box-n3 box)))
                   ;; A row of a lazy list is placed by its index.
                   (:column (mix layout (box-n3 box)))
                   (t layout)))
         (paint (hash-numbers layout (box-color box) (box-look box) (hash-of (box-n1 box))
                              (hash-of (box-n2 box)) (box-n3 box)
                              (hash-of (box-enabled box)))))
    (setf paint
          (case (box-kind box)
            (:text-field (let ((state (box-data box)))
                           (hash-numbers paint
                                         (text-state-caret (field-state-state state))
                                         (text-state-anchor (field-state-state state))
                                         (field-state-view-start state))))
            (:list-box (destructuring-bind (items . state) (box-data box)
                         (hash-numbers paint (length items) (list-state-top state)
                                       (visible-items-hash items (list-state-top state)
                                                           (box-n3 box)))))
            (t paint)))
    (values layout paint)))

(defun hash-options (options)
  (let ((hash +seed+))
    (dolist (option options hash)
      (setf hash (mix hash (hash-of option))))))

(defun visible-items-hash (items top rows)
  (let ((hash +seed+))
    (loop :for item :in (nthcdr top items)
          :repeat rows
          :do (setf hash (mix hash (hash-of item))))
    hash))
