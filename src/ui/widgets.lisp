(defpackage #:almighty-bolt/ui/widgets
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/hash
                #:mix
                #:hash-of
                #:+seed+)
  (:import-from #:almighty-bolt/ui/box
                #:box-theme
                #:box-key
                #:box-parent
                #:box-tooltip
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
                #:ui-parent
                #:ui-consumed
                #:ui-hot
                #:ui-hot-since
                #:ui-toasts
                #:ui-tooltip-dismissed
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
                #:ui-secondary-pressed
                #:ui-wheel-dy
                #:ui-boxes
                #:ui-count
                #:consume-input
                #:ui-states
                #:open-box
                #:close-box
                #:consume-press
                #:focus-by-click
                #:want-cursor
                #:consume-release
                #:consume-secondary-press
                #:consume-key
                #:consume-wheel
                #:rect-of
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h
                #:state-of
                #:open-float
                #:close-float
                #:make-float-spec
                #:float-spec-placement
                #:float-spec-x
                #:float-spec-y
                #:float-spec-min-width
                #:float-spec-anchor)
  (:import-from #:almighty-bolt/internals
                #:+cursors+
                #:with-painting
                #:context-font
                #:context-measure
                #:context-caret-x
                #:context-index-at-x
                #:context-break-lines
                #:make-text-state
                #:text-state-string
                #:text-state-caret
                #:text-state-anchor
                #:text-place-caret
                #:text-select-all
                #:text-select-units
                #:unit-range
                #:text-move
                #:text-insert
                #:text-state-selection-p
                #:text-state-selected-string
                #:edit-text
                #:context-clipboard-text
                #:context-set-clipboard-text
                #:context-choose-file
                #:key-event-kind
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-super+
                #:+modifier-control+
                #:+modifier-shift+
                #:key-event-logical-key
                #:make-display-list
                #:clear-display-list
                #:display-list-hash
                #:+key-left+
                #:+key-right+
                #:+key-up+
                #:+key-delete-forward+
                #:+key-down+
                #:+key-home+
                #:+key-end+
                #:+points-per-line+
                #:now-ns
                #:request-frame)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:find-theme
                #:theme-id
                #:theme-font-name
                #:theme-font-size)
  (:export #:missing-id
           #:missing-id-widget
           #:label
           #:paragraph
           #:paragraph-state
           #:selection-state-p
           #:selection-state-state
           #:paragraph-state-text
           #:paragraph-state-lines
           #:paragraph-state-theme-id
           #:with-theme
           #:paragraph-state-wrapped-at
           #:paragraph-state-generation
           #:set-paragraph-lines
           #:button
           #:text-field
           #:slider
           #:choice
           #:checkbox
           #:dropdown
           #:menu-button
           #:context-menu
           #:popover
           #:combo-box
           #:open-file-dialog
           #:save-file-dialog
           #:begin-popover
           #:end-popover
           #:dialog
           #:begin-dialog
           #:end-dialog
           #:missing-anchor
           #:missing-anchor-anchor
           #:begin-context-menu
           #:end-context-menu
           #:menu-state
           #:menu-state-open
           #:+menu-row-height+
           #:+menu-padding+
           #:+menu-check-width+
           #:switch
           #:spinner
           #:+spinner-steps+
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
           #:+ring-reach+
           #:lazy-list
           #:begin-lazy-list
           #:lazy-list-rows
           #:+lazy-list-height+
           #:own-hashes
           #:describe-tooltip
           #:describe-toasts
           #:toast
           #:+tooltip-delay-ns+
           #:text-area
           #:area-state
           #:area-state-state
           #:area-state-offset
           #:area-lines
           #:area-line-of
           #:area-line-height
           #:area-state-generation
           #:area-state-inner-width
           #:area-state-inner-height
           #:+area-padding+
           #:field-state
           #:field-state-state
           #:field-state-view-start
           #:choice-state
           #:choice-state-lefts
           #:choice-state-widths
           #:choice-state-tops
           #:choice-state-option-height
           #:list-state
           #:list-state-top
           #:canvas-state
           #:canvas-state-fragment
           #:ui-font
           #:+field-padding+
           #:+row-height+
           #:+thumb-size+
           #:+dot-size+
           #:+option-gap+
           #:+option-row-gap+
           #:+switch-width+))

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

(defconstant +option-row-gap+ 6f0 "Between the options of a choice that goes down.")

(defconstant +switch-width+ 32f0 "How wide the track of a switch is; it is half as tall.")

(defun ui-font (context)
  "The font of the widgets, in the theme it is now."
  (context-font context (theme-font-name *theme*) (theme-font-size *theme*)))

(defmacro with-theme ((theme) &body body)
  "What BODY describes, drawn in THEME (a theme, or the name of one) rather than the theme it is in.
A menu, a popover or a dialog that BODY opens is in THEME too."
  `(let ((*theme* (find-theme ,theme)))
     ,@body))

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

(defconstant +ring-reach+ 2f0
  "How far outside a widget the ring round it, when it has the keyboard, is drawn.")

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

(defun inside-scroll-p (ui scroll key)
  "True if the box KEY was made inside the scroll container SCROLL this build: after it, and with it
among its parents. (A float that a widget inside opens, a menu, is made after it too, but is in no
container, and is not inside.)"
  (let ((boxes (ui-boxes ui)))
    (loop :for index :from (scroll-state-first-inside (box-data scroll)) :below (ui-count ui)
          :for box = (svref boxes index)
            :thereis (and (= (box-key box) key)
                          (loop :for parent = (box-parent box) :then (box-parent parent)
                                :while parent
                                  :thereis (eq parent scroll))))))

(defun end-scroll (box)
  (let* ((ui *ui*)
         (key (box-key box))
         (state (box-data box))
         (rect (rect-of ui key))
         (offset (scroll-state-offset state))
         (viewport (scroll-state-viewport-height state)))
    (when (and rect (/= (ui-wheel-dy ui) 0f0)
               (or (= (ui-hot ui) key) (inside-scroll-p ui box (ui-hot ui))))
      (multiple-value-bind (dx dy) (consume-wheel ui)
        (declare (ignore dx))
        (decf offset (* dy (float +points-per-line+ 1f0)))))
    ;; The keyboard came to something inside: show all of it, moving the window as little as can be.
    (let ((focus (ui-focus ui)))
      (when (and rect (/= focus (scroll-state-last-focus state)) (/= focus 0)
                 (inside-scroll-p ui box focus))
        (let ((target (rect-of ui focus)))
          (when target
            (let* ((top (+ (- (rect-y target) (rect-y rect)) offset))
                   (bottom (+ top (rect-h target))))
              ;; With room for its ring: the edge of the window may be where nothing is drawn.
              (cond ((< (- top +ring-reach+) offset) (setf offset (- top +ring-reach+)))
                    ((> (+ bottom +ring-reach+) (+ offset viewport))
                     (setf offset (- (+ bottom +ring-reach+) viewport))))))))
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

;;; --- selecting text with the pointer ---
;;;
;;; A press puts the caret where it is; two in a row select the word under the pointer, three the line
;;; (in a text area, the paragraph between line feeds), and a drag after them goes on by whole words or
;;; lines. What the press chose is kept, so that the drag, which is looked at in the build of the press
;;; as well and in the builds after it, makes the same selection each time from it.

(defstruct (text-press (:constructor nil))
  ;; :char, :word or :line: what the last press selects by, from how many presses it was.
  (unit :char)
  ;; The word or line the press chose.
  (from 0 :type fixnum)
  (to 0 :type fixnum))

(defun index-at (context font string x)
  "Two values: the place between characters of STRING nearest X, and the character X is over."
  (let* ((index (context-index-at-x context font string x)))
    (values index
            (if (and (plusp index) (< x (context-caret-x context font string index)))
                (1- index)
                index))))

(defun press-text (press state index character clicks)
  "The text STATE after a press at INDEX (over CHARACTER), which was the press CLICKS in a row."
  (let ((unit (case clicks (1 :char) (2 :word) (t :line))))
    (setf (text-press-unit press) unit)
    (if (eq unit :char)
        (text-place-caret state index)
        (multiple-value-bind (from to) (unit-range (text-state-string state) character unit)
          (setf (text-press-from press) from
                (text-press-to press) to)
          (text-select-units state character unit from to)))))

(defun drag-text (press state index character)
  "The text STATE with the button still down after a press, and the pointer at INDEX (over
CHARACTER): the selection goes from the press to there, by what the press selected by."
  (if (eq (text-press-unit press) :char)
      (text-place-caret state index :extend t)
      (text-select-units state character (text-press-unit press)
                         (text-press-from press) (text-press-to press))))

;;; --- text ---

;;; Text that is not edited can be selected and copied when the app says so (:SELECTABLE): a press
;;; gives it the keyboard, the pointer selects in it as in a text field, and Command-A and Command-C
;;; select all of it and copy. It is not in the order Tab goes round, and the selection shows only
;;; while it has the keyboard.

(defstruct (selection-state (:include text-press) (:constructor make-selection-state ()))
  (state (make-text-state "") :type almighty-bolt/internals:text-state))

(defun text-font (ui box)
  "The font BOX's text is drawn in: the theme's, at the box's size."
  (let ((context (ui-context ui)))
    (and context (context-font context (theme-font-name *theme*) (box-size box)))))

(defun select-text (ui box selection index-at)
  "Let the pointer select in the text of BOX, whose SELECTION it is, and Command-A and Command-C
select all of it and copy. INDEX-AT is called with the pointer's x and y and gives the place in the
text nearest them and the character they are over."
  (let ((key (box-key box))
        (text (box-text box)))
    (unless (string= (text-state-string (selection-state-state selection)) text)
      (setf (selection-state-state selection) (make-text-state text :caret 0)))
    (setf (box-interactive box) t
          (box-n2 box) 1f0
          (box-data box) selection)
    (want-cursor ui key :text)
    (let ((x (ui-pointer-x ui)) (y (ui-pointer-y ui)))
      (when (and (= (ui-hot ui) key) (ui-pressed ui))
        (consume-press ui)
        (focus-by-click ui key)
        (setf (ui-active ui) key)
        (multiple-value-bind (index character) (funcall index-at x y)
          (setf (selection-state-state selection)
                (press-text selection (selection-state-state selection) index character
                            (ui-click-count ui)))))
      (when (and (= (ui-active ui) key) (ui-down ui))
        (multiple-value-bind (index character) (funcall index-at x y)
          (setf (selection-state-state selection)
                (drag-text selection (selection-state-state selection) index character))))
      (when (and (= (ui-active ui) key) (ui-released ui))
        (consume-release ui)
        (setf (ui-active ui) 0)))
    (when (= (ui-focus ui) key)
      (dolist (event (copy-list (ui-keys ui)))
        (let ((character (key-event-logical-key event))
              (modifiers (key-event-modifiers event)))
          (when (and (eq (key-event-kind event) :down)
                     (characterp character)
                     (modifier-down-p modifiers +modifier-super+)
                     (not (modifier-down-p modifiers +modifier-control+)))
            (cond ((char-equal character #\a)
                   (consume-key ui event)
                   (setf (selection-state-state selection)
                         (text-select-all (selection-state-state selection))))
                  ((char-equal character #\c)
                   (consume-key ui event)
                   (let ((state (selection-state-state selection)))
                     (when (text-state-selection-p state)
                       (context-set-clipboard-text (ui-context ui)
                                                   (text-state-selected-string state))))))))))
    (setf (box-look box) (look-of ui key t))))

(defun label (text &key (id (required-id 'label)) tooltip size color (flex 0) live selectable)
  "A line of TEXT, SIZE points high (the theme's size if not given). A LIVE label is read out by a screen reader whenever its text changes, wherever the
reader is: for a result, a count or a status that a person should hear without going to look for it.
A SELECTABLE label's text can be selected with the pointer and copied."
  (let* ((ui *ui*)
         (box (open-box ui :label :id id :tooltip tooltip)))
    (setf (box-text box) text
          (box-size box) (or size (theme-font-size *theme*))
          (box-color box) (or color 0)
          (box-flex box) (float flex 1f0)
          (box-n3 box) (if live 1 0))
    (when selectable
      (select-text ui box (state-of ui (box-key box) #'make-selection-state)
                   (lambda (x y)
                     (declare (ignore y))
                     (let ((rect (rect-of ui (box-key box)))
                           (font (text-font ui box)))
                       (if (and rect font)
                           (index-at (ui-context ui) font text (- x (rect-x rect)))
                           (values 0 0))))))
    (close-box ui)
    nil))

;;; A paragraph is text broken into lines to the width it is given, so its height depends on
;;; its width: layout works the lines out (they go in a state kept by the box's key, which is
;;; there when layout is skipped for a frame that is the same) and painting draws them.

(defstruct (paragraph-state (:include selection-state) (:constructor make-paragraph-state ()))
  (text "" :type string)
  (wrapped-at -1f0 :type single-float)
  (lines '() :type list)
  ;; The theme whose font the lines were made with.
  (theme-id 0 :type fixnum)
  (generation 0 :type fixnum))

(defun set-paragraph-lines (state text wrapped-at lines)
  "Note the LINES that TEXT breaks into at WRAPPED-AT; a new generation says that the lines
are not the ones that were drawn before."
  (setf (paragraph-state-text state) text
        (paragraph-state-wrapped-at state) wrapped-at
        (paragraph-state-lines state) lines
        (paragraph-state-theme-id state) (theme-id *theme*))
  (incf (paragraph-state-generation state))
  state)

(defun paragraph-index-at (ui box state x y)
  "The place in the text of the paragraph BOX (with STATE) nearest X, Y, and the character there, by
the lines it was drawn in."
  (let ((rect (rect-of ui (box-key box)))
        (font (text-font ui box))
        (lines (paragraph-state-lines state))
        (text (paragraph-state-text state)))
    (if (not (and rect font lines))
        (values 0 0)
        (multiple-value-bind (width ascent descent) (context-measure (ui-context ui) font "Hg")
          (declare (ignore width))
          (let* ((row (max 0 (min (1- (length lines))
                                  (floor (- y (rect-y rect)) (+ ascent descent)))))
                 (line (nth row lines)))
            (destructuring-bind (start end) line
              ;; Not after the line feed that ends a line: that is the start of the next.
              (when (and (> end start) (char= (char text (1- end)) #\Linefeed))
                (decf end))
              (multiple-value-bind (index character)
                  (index-at (ui-context ui) font (subseq text start end) (- x (rect-x rect)))
                (values (+ start index) (+ start character)))))))))

(defun paragraph (text &key (id (required-id 'paragraph)) tooltip size color (flex 0) live
                         selectable)
  "TEXT broken into lines to the width it is given, SIZE points high (the theme's size if not
given). LIVE and SELECTABLE are as for a label."
  (let* ((ui *ui*)
         (box (open-box ui :paragraph :id id :tooltip tooltip))
         (state (state-of ui (box-key box) #'make-paragraph-state)))
    (setf (box-text box) text
          (box-size box) (or size (theme-font-size *theme*))
          (box-color box) (or color 0)
          (box-flex box) (float flex 1f0)
          (box-n3 box) (if live 1 0)
          (box-data box) state)
    (when selectable
      (select-text ui box state (lambda (x y) (paragraph-index-at ui box state x y))))
    (close-box ui)
    nil))

(defun progress (fraction &key (id (required-id 'progress)) tooltip labelled-by (width 200) (flex 0))
  "A bar that is FRACTION (0 to 1) full."
  (let ((box (open-box *ui* :progress :id id :tooltip tooltip :labelled-by labelled-by)))
    (setf (box-n1 box) (float (max 0 (min 1 fraction)) 1f0)
          (box-pref-w box) (float width 1f0)
          (box-flex box) (float flex 1f0))
    (close-box *ui*)
    nil))

;;; --- a button ---

(defun button (text &key (id (required-id 'button)) tooltip (enabled t) (flex 0))
  "A button. Returns true when it is clicked: the primary button went down and up over it,
or the space bar or Return was pressed with the keyboard on it."
  (let* ((ui *ui*)
         (box (open-box ui :button :id id :tooltip tooltip))
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

(defun newlines (text)
  "TEXT with each line break in it (a return, a line feed, or the two together) made a line feed."
  (if (not (find #\Return text))
      text
      (with-output-to-string (out)
        (loop :for i :from 0 :below (length text)
              :for character = (char text i)
              :do (cond ((char/= character #\Return) (write-char character out))
                        ((and (< (1+ i) (length text)) (char= (char text (1+ i)) #\Linefeed)))
                        (t (write-char #\Linefeed out)))))))

(defun clipboard-edit (state event context &key single-line)
  "What the key EVENT does to the text STATE if it is Command-C, Command-X or Command-V, as two
values: the new state, and true if it was one of them. Copy and cut take the selection, and do
nothing without one; paste puts the clipboard's text in place of the selection, with its line breaks
made spaces if SINGLE-LINE and line feeds if not."
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
                             (text-insert state (if single-line (one-line text) (newlines text)))
                             state)))
                      ((not (text-state-selection-p state)) state)
                      (t (context-set-clipboard-text context (text-state-selected-string state))
                         (if (char-equal key #\x) (text-insert state "") state)))
                t))))

;;; --- a line of editable text ---

;;; --- undo ---
;;;
;;; Text that is edited keeps what it was before each edit, to be put back by Command-Z (and taken
;;; again by Command-Shift-Z). A run of typing is one edit, and so is a run of deleting, as in a Mac
;;; app; any other edit (a paste, a cut, a Return) is one of its own, and a press of the pointer or a
;;; key that only moves the caret ends a run. Text the app gives instead of what was edited starts
;;; the history again.

(defconstant +undo-limit+ 100 "The most edits that can be undone.")

(defstruct (editable (:include text-press) (:constructor nil))
  ;; The states before each edit, the latest first, and the states undone, the latest first.
  (undo '() :type list)
  (redo '() :type list)
  ;; :typing or :deleting while a run of them goes on, which the next of the same kind joins.
  (run nil))

(defun forget-edits (editable)
  (setf (editable-undo editable) '()
        (editable-redo editable) '()
        (editable-run editable) nil))

(defun edit-kind (event)
  "What the key EVENT does to text, if it edits it: :typing, :deleting or :other."
  (let ((key (key-event-logical-key event))
        (modifiers (key-event-modifiers event)))
    (cond ((or (modifier-down-p modifiers +modifier-super+)
               (modifier-down-p modifiers +modifier-control+))
           :other)
          ((or (eql key #\Backspace) (eql key +key-delete-forward+)) :deleting)
          ((and (characterp key) (graphic-char-p key)) :typing)
          (t :other))))

(defun undo-key (event)
  "The key EVENT as :undo (Command-Z) or :redo (Command-Shift-Z), or NIL."
  (let ((key (key-event-logical-key event))
        (modifiers (key-event-modifiers event)))
    (and (eq (key-event-kind event) :down)
         (characterp key)
         (char-equal key #\z)
         (modifier-down-p modifiers +modifier-super+)
         (not (modifier-down-p modifiers +modifier-control+))
         (if (modifier-down-p modifiers +modifier-shift+) :redo :undo))))

(defun edit-with-history (editable state event edit)
  "What the key EVENT does to the text STATE of EDITABLE, as two values, the new state and true if the
key was taken: Command-Z and Command-Shift-Z undo and redo, and any other key is EDIT's (a function of
the state and the event that gives the same two values). An edit that changes the text is noted so
that it can be undone."
  (let ((undo (undo-key event)))
    (cond
      ((eq undo :undo)
       (setf (editable-run editable) nil)
       (let ((before (pop (editable-undo editable))))
         (cond (before (push state (editable-redo editable))
                       (values before t))
               (t (values state t)))))
      ((eq undo :redo)
       (setf (editable-run editable) nil)
       (let ((after (pop (editable-redo editable))))
         (cond (after (push state (editable-undo editable))
                      (values after t))
               (t (values state t)))))
      (t
       (multiple-value-bind (new taken) (funcall edit state event)
         (when taken
           (if (string= (text-state-string new) (text-state-string state))
               ;; Only the caret or the selection moved: the next edit is one of its own.
               (setf (editable-run editable) nil)
               (let ((kind (edit-kind event)))
                 (unless (and (member kind '(:typing :deleting))
                              (eq kind (editable-run editable)))
                   (push state (editable-undo editable))
                   (when (> (length (editable-undo editable)) +undo-limit+)
                     (setf (editable-undo editable)
                           (subseq (editable-undo editable) 0 +undo-limit+))))
                 (setf (editable-redo editable) '()
                       (editable-run editable) (and (member kind '(:typing :deleting)) kind)))))
         (values new taken))))))

(defstruct (field-state (:include editable) (:constructor make-field-state ()))
  (state (make-text-state "") :type almighty-bolt/internals:text-state)
  (view-start 0 :type fixnum))

(defun field-index-at (ui key state x)
  "Two values: the index in the text of the field KEY that the pointer, at X, is nearest, and the
index of the character it is over."
  (let ((rect (rect-of ui key))
        (context (ui-context ui)))
    (if (and rect context)
        (let* ((string (text-state-string (field-state-state state)))
               (start (field-state-view-start state)))
          (multiple-value-bind (index character)
              (index-at context (ui-font context) (subseq string start)
                        (- x (rect-x rect) +field-padding+))
            (values (+ start index) (+ start character))))
        (let ((caret (text-state-caret (field-state-state state))))
          (values caret caret)))))

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
                   &key (id (required-id 'text-field)) tooltip labelled-by
                     (width 180) invalid (enabled t) (flex 0))
  "An editable line showing STRING. Returns the text now, whether the person changed it this
frame, and whether Return was pressed. Command-C, Command-X and Command-V copy, cut and paste
(a line break that is pasted becomes a space). Pass what it returned back in the next frame; if the
application passes something else, that is what is shown."
  (let* ((ui *ui*)
         (box (open-box ui :text-field :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-field-state))
         (changed nil)
         (submitted nil))
    (unless (string= (text-state-string (field-state-state state)) string)
      (setf (field-state-state state) (make-text-state string)
            (field-state-view-start state) 0)
      (forget-edits state))
    (setf (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-pref-w box) (float width 1f0)
          (box-n3 box) (if invalid 1 0)
          (box-data box) state
          (box-flex box) (float flex 1f0))
    (when enabled
      (want-cursor ui key :text)
      (let ((x (ui-pointer-x ui)))
        ;; A press on the field gives it the keyboard and puts the caret there (two select a word,
        ;; three all of it).
        (when (and (= (ui-hot ui) key) (ui-pressed ui))
          (consume-press ui)
          (focus-by-click ui key)
          (setf (ui-active ui) key
                ;; What is typed after a press is an edit of its own.
                (editable-run state) nil)
          (multiple-value-bind (index character) (field-index-at ui key state x)
            (setf (field-state-state state)
                  (press-text state (field-state-state state) index character
                              (ui-click-count ui)))))
        ;; Dragging with the button down extends the selection.
        (when (and (= (ui-active ui) key) (ui-down ui))
          (multiple-value-bind (index character) (field-index-at ui key state x)
            (setf (field-state-state state)
                  (drag-text state (field-state-state state) index character))))
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
                 (edit-with-history
                  state (field-state-state state) event
                  (lambda (text event)
                    (multiple-value-bind (pasted command)
                        (clipboard-edit text event (ui-context ui) :single-line t)
                      (if command
                          (values pasted t)
                          (edit-text text event)))))
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

;;; --- several lines of editable text ---
;;;
;;; The text is one string with line feeds in it, edited as a text field's is (EDIT-TEXT), and broken
;;; into the lines that are shown at the width of the area: a line that is too long goes on in the next.
;;; Those lines are kept with the text and the width they were made for, and made again when either
;;; changes; the keys that go up and down and the pointer find their place by them. The area shows
;;; ROWS lines and scrolls itself to keep the caret in sight.

(defconstant +area-padding+ 6f0 "Above and below the lines of a text area.")

(defstruct (area-state (:include editable) (:constructor make-area-state ()))
  (state (make-text-state "") :type almighty-bolt/internals:text-state)
  ;; How far down the lines are scrolled, in points.
  (offset 0f0 :type single-float)
  ;; The lines that are shown, each (START . END), END before the line feed that ends it if one does;
  ;; with the text and the width they were made for. A new generation says they are not the ones
  ;; drawn before.
  (lines #() :type simple-vector)
  (lines-text "" :type string)
  (wrapped-at -1f0 :type single-float)
  (theme-id 0 :type fixnum)
  (generation 0 :type fixnum)
  ;; The room inside the box for lines, from the last layout (0 before there is one).
  (inner-width 0f0 :type single-float)
  (inner-height 0f0 :type single-float)
  ;; The x that Up and Down keep to, from the first of them in a row.
  (goal-x nil))

(defun area-line-height (context)
  (multiple-value-bind (width ascent descent) (context-measure context (ui-font context) "Hg")
    (declare (ignore width))
    (float (+ ascent descent) 1f0)))

(defun area-lines (context state string width)
  "The lines that STRING is shown in at WIDTH, as a vector of (START . END), from STATE if they were
made for this string and width. There is always one: an empty text, and a text that ends in a line
feed, end in an empty line, where the caret can be."
  (unless (and (= (area-state-wrapped-at state) width)
               (= (area-state-theme-id state) (theme-id *theme*))
               (string= (area-state-lines-text state) string))
    (let* ((length (length string))
           (lines (loop :for (start end) :in (if (zerop length)
                                                  '()
                                                  (context-break-lines context (ui-font context)
                                                                       string width))
                        :collect (cons start (if (and (> end start)
                                                      (char= (char string (1- end)) #\Linefeed))
                                                 (1- end)
                                                 end)))))
      (when (or (zerop length) (char= (char string (1- length)) #\Linefeed))
        (setf lines (append lines (list (cons length length)))))
      (setf (area-state-lines state) (coerce lines 'simple-vector)
            (area-state-lines-text state) (copy-seq string)
            (area-state-wrapped-at state) width
            (area-state-theme-id state) (theme-id *theme*))
      (incf (area-state-generation state))))
  (area-state-lines state))

(defun area-line-of (lines index)
  "The number of the line that the caret at INDEX is on: the last that starts at or before it (so
that a caret where a long line goes on is at the start of the next)."
  (let ((found 0))
    (loop :for line :across lines
          :for i :from 0
          :while (<= (car line) index)
          :do (setf found i))
    found))

(defun area-inner-width (state width)
  (if (plusp (area-state-inner-width state))
      (area-state-inner-width state)
      (- width (* 2 +field-padding+))))

(defun area-index-at (ui key area x y)
  "Two values: the index in the text of the area KEY that the pointer at X, Y is nearest, and the
index of the character it is over."
  (let* ((rect (rect-of ui key))
         (context (ui-context ui))
         (state (area-state-state area))
         (string (text-state-string state)))
    (if (not (and rect context))
        (values (text-state-caret state) (text-state-caret state))
        (let* ((lines (area-lines context area string (area-inner-width area (rect-w rect))))
               (height (area-line-height context))
               (row (max 0 (min (1- (length lines))
                                (floor (+ (- y (rect-y rect) +area-padding+) (area-state-offset area))
                                       height))))
               (line (svref lines row)))
          (multiple-value-bind (index character)
              (index-at context (ui-font context) (subseq string (car line) (cdr line))
                        (- x (rect-x rect) +field-padding+))
            (values (+ (car line) index) (+ (car line) character)))))))

(defun area-move-vertically (context area lines down extend)
  "The text state of AREA with the caret a line up (or DOWN), at the x it was at when Up or Down
was first pressed; at the first or last line, to the start or the end."
  (let* ((state (area-state-state area))
         (string (text-state-string state))
         (caret (text-state-caret state))
         (font (ui-font context))
         (row (area-line-of lines caret))
         (line (svref lines row))
         (x (or (area-state-goal-x area)
                (setf (area-state-goal-x area)
                      (float (context-caret-x context font (subseq string (car line) (cdr line))
                                              (- caret (car line)))
                             1f0))))
         (target (+ row (if down 1 -1))))
    (cond ((< target 0) (text-place-caret state 0 :extend extend))
          ((>= target (length lines)) (text-place-caret state (length string) :extend extend))
          (t (let ((line (svref lines target)))
               (text-place-caret state
                                 (+ (car line)
                                    (context-index-at-x context font
                                                        (subseq string (car line) (cdr line)) x))
                                 :extend extend))))))

(defun area-key (ui area event width)
  "What the key EVENT does to the text of AREA, as two values: the new text state and true if the
key was one for the area."
  (let* ((context (ui-context ui))
         (state (area-state-state area))
         (key (key-event-logical-key event))
         (modifiers (key-event-modifiers event))
         (shift (modifier-down-p modifiers +modifier-shift+))
         (command (modifier-down-p modifiers +modifier-super+))
         (down (eq (key-event-kind event) :down)))
    (flet ((lines ()
             (area-lines context area (text-state-string state) (area-inner-width area width))))
      (cond
        ((not down) (values state nil))
        ((eql key #\Return) (values (text-insert state (string #\Linefeed)) t))
        ((and (or (eql key +key-up+) (eql key +key-down+)) command)
         (values (text-move state (if (eql key +key-up+) :start :end) :extend shift) t))
        ;; The x it keeps to is kept for the next Up or Down; any other key forgets it (TEXT-AREA).
        ((or (eql key +key-up+) (eql key +key-down+))
         (values (area-move-vertically context area (lines) (eql key +key-down+) shift) t))
        ((and (or (eql key +key-left+) (eql key +key-right+)) command)
         ;; The start or the end of the line the caret is on.
         (let* ((lines (lines))
                (line (svref lines (area-line-of lines (text-state-caret state)))))
           (values (text-place-caret state (if (eql key +key-left+) (car line) (cdr line))
                                     :extend shift)
                   t)))
        (t
         (multiple-value-bind (pasted command)
             (clipboard-edit state event context)
           (if command
               (values pasted t)
               (edit-text state event))))))))

(defun keep-area-caret-visible (ui area width rows)
  (let ((context (ui-context ui)))
    (when context
      (let* ((state (area-state-state area))
             (lines (area-lines context area (text-state-string state) (area-inner-width area width)))
             (height (area-line-height context))
             (inner (if (plusp (area-state-inner-height area))
                        (area-state-inner-height area)
                        (* rows height)))
             (top (* height (area-line-of lines (text-state-caret state))))
             (offset (area-state-offset area)))
        (cond ((< top offset) (setf offset top))
              ((> (+ top height) (+ offset inner)) (setf offset (- (+ top height) inner))))
        (setf (area-state-offset area)
              (max 0f0 (min offset (max 0f0 (- (* (length lines) height) inner)))))))))

(defun text-area (string
                  &key (id (required-id 'text-area)) tooltip labelled-by
                    (width 320) (rows 6) (enabled t) (flex 0))
  "An editable text of several lines showing STRING, WIDTH points wide and ROWS lines tall, whose long
lines go on in the next. Returns the text now and whether the person changed it this frame. Return
puts in a line feed; Up and Down move a line (Command with them, to the start and the end), Command
with Left and Right to the start and the end of the line; the other keys, and copy, cut and paste,
are a text field's. Pass what it returned back in the next frame; if the application passes something
else, that is what is shown."
  (let* ((ui *ui*)
         (box (open-box ui :text-area :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box-key box))
         (area (state-of ui key #'make-area-state))
         (width (float width 1f0))
         (changed nil)
         (moved nil))
    (unless (string= (text-state-string (area-state-state area)) string)
      (setf (area-state-state area) (make-text-state string)
            (area-state-offset area) 0f0
            (area-state-goal-x area) nil)
      (forget-edits area))
    (setf (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-pref-w box) width
          (box-n3 box) rows
          (box-data box) area
          (box-flex box) (float flex 1f0))
    (when enabled
      (want-cursor ui key :text)
      (let ((x (ui-pointer-x ui))
            (y (ui-pointer-y ui)))
        (when (and (= (ui-hot ui) key) (ui-pressed ui))
          (consume-press ui)
          (focus-by-click ui key)
          (setf (ui-active ui) key
                (area-state-goal-x area) nil
                (editable-run area) nil
                moved t)
          (multiple-value-bind (index character) (area-index-at ui key area x y)
            (setf (area-state-state area)
                  (press-text area (area-state-state area) index character (ui-click-count ui)))))
        (when (and (= (ui-active ui) key) (ui-down ui))
          (let ((new (multiple-value-bind (index character) (area-index-at ui key area x y)
                       (drag-text area (area-state-state area) index character))))
            (unless (eq new (area-state-state area))
              (setf (area-state-state area) new
                    moved t))))
        (when (and (= (ui-active ui) key) (ui-released ui))
          (consume-release ui)
          (setf (ui-active ui) 0))
        (when (= (ui-hot ui) key)
          (multiple-value-bind (dx dy) (consume-wheel ui)
            (declare (ignore dx))
            (when (/= dy 0f0)
              (decf (area-state-offset area) (* dy (float +points-per-line+ 1f0)))))))
      (when (= (ui-focus ui) key)
        (dolist (event (copy-list (ui-keys ui)))
          (let ((vertical (member (key-event-logical-key event) (list +key-up+ +key-down+))))
            (multiple-value-bind (new edited)
                (edit-with-history area (area-state-state area) event
                                   (lambda (text event)
                                     (declare (ignore text))
                                     (area-key ui area event width)))
              (when edited
                (consume-key ui event)
                (unless vertical
                  (setf (area-state-goal-x area) nil))
                (unless (string= (text-state-string new)
                                 (text-state-string (area-state-state area)))
                  (setf changed t))
                (setf (area-state-state area) new
                      moved t)))))))
    (if moved
        (keep-area-caret-visible ui area width rows)
        ;; Only kept inside the lines; the wheel may leave the caret out of sight.
        (let ((context (ui-context ui)))
          (when context
            (let* ((lines (area-lines context area (text-state-string (area-state-state area))
                                      (area-inner-width area width)))
                   (height (area-line-height context))
                   (inner (if (plusp (area-state-inner-height area))
                              (area-state-inner-height area)
                              (* rows height))))
              (setf (area-state-offset area)
                    (max 0f0 (min (area-state-offset area)
                                  (max 0f0 (- (* (length lines) height) inner)))))))))
    (setf (box-look box) (look-of ui key enabled)
          (box-text box) (text-state-string (area-state-state area))
          (box-n1 box) (area-state-offset area))
    (close-box ui)
    (values (text-state-string (area-state-state area)) changed)))

;;; --- a slider ---

(defun slider (value minimum maximum
               &key (id (required-id 'slider)) tooltip labelled-by (width 200) (enabled t) (flex 0))
  "A slider showing VALUE between MINIMUM and MAXIMUM. Returns the value now and whether the
person changed it this frame."
  (let* ((ui *ui*)
         (box (open-box ui :slider :id id :tooltip tooltip :labelled-by labelled-by))
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
  ;; Where each option is in the box, from layout: across, down, how wide, and how tall each is.
  (lefts #() :type simple-vector)
  (tops #() :type simple-vector)
  (widths #() :type simple-vector)
  (option-height 0f0 :type single-float))

(defun choice (options selected
               &key (id (required-id 'choice)) tooltip labelled-by (direction :horizontal) (enabled t)
                 (flex 0))
  "A choice among OPTIONS (strings), each with a round button, with the SELECTED one (an index)
marked: side by side, or with DIRECTION :vertical one under another (radio buttons, as a Mac lays
them out). Returns the index now and whether the person changed it this frame."
  (check-type direction (member :horizontal :vertical))
  (let* ((ui *ui*)
         (box (open-box ui :choice :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-choice-state))
         (new selected))
    (setf (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-data box) (cons options state)
          ;; Which way the options go, which decides the layout.
          (box-main-align box) direction
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
              (tops (choice-state-tops state))
              (widths (choice-state-widths state))
              (height (choice-state-option-height state)))
          (when (and rect (= (ui-hot ui) key))
            (dotimes (i (length lefts))
              (let ((left (+ (rect-x rect) (svref lefts i)))
                    (top (+ (rect-y rect) (svref tops i))))
                (when (and (<= left (ui-pointer-x ui) (+ left (svref widths i)))
                           (<= top (ui-pointer-y ui) (+ top height)))
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

;;; --- menus ---
;;;
;;; An open menu is a float of rows, put by the widget it belongs to, with a float under it as large
;;; as the window that nothing can be seen of (a catcher): a press anywhere but on the menu lands on
;;; the catcher, which closes the menu and goes nowhere else, as a press outside a Mac menu does.
;;; While a menu is open its keys are its own: Up and Down move the highlight, Return or the space
;;; bar choose, Escape closes. The pointer over a row moves the highlight to it.

(defconstant +menu-row-height+ 24f0)
(defconstant +menu-padding+ 4f0 "Round the rows of a menu.")
(defconstant +menu-check-width+ 22f0 "Before the words of a row, where the mark of the chosen one goes.")

(defstruct (menu-state (:constructor make-menu-state ()))
  (open nil)
  ;; The row the pointer or the arrows are on, or -1.
  (highlight -1 :type fixnum)
  ;; The point a menu opened at, for one that opens where the pointer is.
  (x 0f0 :type single-float)
  (y 0f0 :type single-float))

(defun open-menu (state highlight &optional (x 0f0) (y 0f0))
  (setf (menu-state-open state) t
        (menu-state-highlight state) highlight
        (menu-state-x state) (float x 1f0)
        (menu-state-y state) (float y 1f0))
  (setf (ui-consumed *ui*) t))

(defun menu (ui owner state items spec &key checked typing)
  "Describe the open menu of the widget with the key OWNER, whose state is STATE: ITEMS (strings) in
a float placed by SPEC, the one at CHECKED (an index) marked, over a catcher. Returns the index of the
item chosen this frame, or NIL, and true if the menu closed this frame (an item was chosen, or the
person pressed outside it or pressed Escape). With TYPING (a combo box's), the space bar is left to
the field that is being typed in."
  (let ((chosen nil)
        (closed nil)
        (count (length items))
        (rows '()))
    (multiple-value-bind (catcher open)
        (open-float ui :catcher owner :catcher (make-float-spec :placement :fill))
      (setf (box-interactive catcher) t)
      (close-float ui open)
      (when (= (ui-hot ui) (box-key catcher))
        (when (ui-pressed ui)
          (consume-press ui)
          (setf closed t))
        ;; Nothing under an open menu scrolls.
        (consume-wheel ui)))
    (multiple-value-bind (float open) (open-float ui :menu owner :menu spec)
      ;; A press between the rows is the menu's, and not a press outside it.
      (setf (box-interactive float) t)
      (when (and (= (ui-hot ui) (box-key float)) (ui-pressed ui))
        (consume-press ui))
      (loop :for item :in items
            :for i :from 0
            :do (let* ((row (open-box ui :menu-item :id i))
                       (key (box-key row)))
                  (push row rows)
                  (setf (box-text row) item
                        (box-interactive row) t
                        (box-moves row) t
                        (box-n2 row) (if (eql i checked) 1f0 0f0))
                  (when (= (ui-hot ui) key)
                    (unless (= (menu-state-highlight state) i)
                      (setf (menu-state-highlight state) i
                            (ui-consumed ui) t))
                    (when (ui-pressed ui)
                      (consume-press ui)
                      (setf (ui-active ui) key)))
                  (when (and (= (ui-active ui) key) (ui-released ui))
                    (consume-release ui)
                    (setf (ui-active ui) 0)
                    (when (= (ui-hot ui) key)
                      (setf chosen i)))
                  (close-box ui)))
      (when (or (= (ui-hot ui) (box-key float))
                (find (ui-hot ui) rows :key #'box-key))
        (consume-wheel ui))
      (close-float ui open))
    (dolist (event (copy-list (ui-keys ui)))
      (let ((k (key-event-logical-key event))
            (highlight (menu-state-highlight state)))
        (cond ((eql k +key-down+)
               (consume-key ui event)
               (setf (menu-state-highlight state) (if (< highlight 0) 0 (min (1- count) (1+ highlight)))))
              ((eql k +key-up+)
               (consume-key ui event)
               (setf (menu-state-highlight state) (if (< highlight 0) (1- count) (max 0 (1- highlight)))))
              ((and (member k '(#\Return #\Space))
                    (not (and typing (eql k #\Space))))
               (consume-key ui event)
               (when (<= 0 highlight (1- count))
                 (setf chosen highlight)))
              ((eql k #\Escape)
               (consume-key ui event)
               (setf closed t)))))
    (dolist (row rows)
      (setf (box-n3 row) (if (= (box-id row) (menu-state-highlight state)) 1 0)))
    (when chosen
      (setf closed t))
    (when closed
      (setf (menu-state-open state) nil
            (ui-consumed ui) t))
    (values chosen closed)))

;;; --- a dropdown ---

(defun dropdown (options selected
                 &key (id (required-id 'dropdown)) tooltip labelled-by width (enabled t) (flex 0))
  "A button showing the SELECTED one (an index) of OPTIONS (strings), which a press opens into a menu
of them all: a pop-up button. WIDTH is how wide it is (by default, as wide as the option it shows).
Returns the index now and whether the person changed it this frame. With the keyboard on it, the
space bar, Return and the arrows open the menu."
  (let* ((ui *ui*)
         (box (open-box ui :dropdown :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-menu-state))
         (new selected))
    (setf (box-text box) (or (and selected (nth selected options)) "")
          (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-pref-w box) (if width (float width 1f0) 0f0)
          (box-data box) options
          (box-flex box) (float flex 1f0))
    (cond
      ((not enabled) (setf (menu-state-open state) nil))
      ((not (menu-state-open state))
       ;; A press opens it (as a Mac pop-up button does, at once), and gives it the keyboard.
       (when (and (= (ui-hot ui) key) (ui-pressed ui))
         (consume-press ui)
         (focus-by-click ui key)
         (open-menu state (or selected 0)))
       (when (= (ui-focus ui) key)
         (dolist (event (copy-list (ui-keys ui)))
           (when (member (key-event-logical-key event) (list #\Space #\Return +key-down+ +key-up+))
             (consume-key ui event)
             (open-menu state (or selected 0)))))))
    (close-box ui)
    (when (menu-state-open state)
      (let ((chosen (menu ui key state options
                          (make-float-spec :anchor box :placement :below
                                           :min-width (let ((rect (rect-of ui key)))
                                                        (if rect (rect-w rect) 0f0)))
                          :checked selected)))
        (when chosen
          (setf new chosen))))
    (setf (box-n3 box) (if (menu-state-open state) 1 0)
          (box-look box) (look-of ui key enabled))
    (values new (not (eql new selected)))))

;;; --- a button with a menu of commands ---

(defun menu-button (text items &key (id (required-id 'menu-button)) tooltip (enabled t) (flex 0))
  "A button with TEXT that a press opens into a menu of ITEMS (strings): commands, of which none is
chosen beforehand. Returns the item chosen, on the frame it is chosen, and otherwise NIL. With the
keyboard on it, the space bar, Return and the arrows open the menu."
  (let* ((ui *ui*)
         (box (open-box ui :menu-button :id id :tooltip tooltip))
         (key (box-key box))
         (state (state-of ui key #'make-menu-state))
         (chosen nil))
    (setf (box-text box) text
          (box-enabled box) enabled
          (box-interactive box) enabled
          (box-focusable box) enabled
          (box-flex box) (float flex 1f0))
    (cond
      ((not enabled) (setf (menu-state-open state) nil))
      ((not (menu-state-open state))
       (when (and (= (ui-hot ui) key) (ui-pressed ui))
         (consume-press ui)
         (focus-by-click ui key)
         (open-menu state -1))
       (when (= (ui-focus ui) key)
         (dolist (event (copy-list (ui-keys ui)))
           (when (member (key-event-logical-key event) (list #\Space #\Return +key-down+ +key-up+))
             (consume-key ui event)
             (open-menu state 0))))))
    (close-box ui)
    (when (menu-state-open state)
      (let ((index (menu ui key state items
                         (make-float-spec :anchor box :placement :below
                                          :min-width (let ((rect (rect-of ui key)))
                                                       (if rect (rect-w rect) 0f0))))))
        (when index
          (setf chosen (nth index items)))))
    (setf (box-n3 box) (if (menu-state-open state) 1 0)
          (box-look box) (look-of ui key enabled))
    chosen))

;;; --- a menu on a secondary press ---
;;;
;;; What CONTEXT-MENU wraps is laid out as a column; a press of the secondary button (or of the
;;; primary with Control down) on any of it, or on what is in it, opens a menu where the pointer is.
;;; A context menu inside another's area takes the press first.

(defstruct (context-state (:include menu-state) (:constructor make-context-state ()))
  ;; The index of the first box inside the area, this build.
  (first-inside 0 :type fixnum))

(defun begin-context-menu (id)
  (let* ((ui *ui*)
         (box (open-box ui :context-area :id (or id (required-id 'context-menu))))
         (state (state-of ui (box-key box) #'make-context-state)))
    ;; It takes the pointer where nothing inside it does, so that a press there is in the area.
    (setf (box-interactive box) t
          (box-data box) state
          (context-state-first-inside state) (ui-count ui))
    box))

(defun in-area-p (ui box key)
  "True if KEY is the key of BOX or of a box made inside it this build."
  (or (= key (box-key box))
      (let ((boxes (ui-boxes ui)))
        (loop :for index :from (context-state-first-inside (box-data box)) :below (ui-count ui)
              :for inside = (svref boxes index)
                :thereis (and (= (box-key inside) key)
                              (loop :for parent = (box-parent inside) :then (box-parent parent)
                                    :while parent
                                      :thereis (eq parent box)))))))

(defun end-context-menu (box items)
  (let* ((ui *ui*)
         (key (box-key box))
         (state (box-data box))
         (chosen nil))
    (close-box ui)
    (when (and (ui-secondary-pressed ui) (in-area-p ui box (ui-hot ui)))
      (consume-secondary-press ui)
      (open-menu state -1 (ui-pointer-x ui) (ui-pointer-y ui)))
    (when (menu-state-open state)
      (let ((index (menu ui key state items
                         (make-float-spec :placement :at :x (menu-state-x state)
                                          :y (menu-state-y state)))))
        (when index
          (setf chosen (nth index items)))))
    chosen))

(defmacro context-menu ((&key id items) &body body)
  "What BODY describes, one under another, with a menu of ITEMS (strings) that a press of the
secondary button on any of it (or a Control-press) opens where the pointer is. Returns the item
chosen, on the frame it is chosen, and otherwise NIL. The :id is required."
  (let ((box (gensym "BOX")))
    `(let ((,box (begin-context-menu ,id)))
       ,@body
       (end-context-menu ,box ,items))))

;;; --- a text field with suggestions ---

(defun combo-box (string suggestions
                  &key (id (required-id 'combo-box)) tooltip labelled-by (width 180) (enabled t)
                    (flex 0))
  "A text field showing STRING with a menu of SUGGESTIONS (strings) under it, which typing opens: the
app gives the suggestions that fit what has been typed, each frame. Down and Up move into them, Return
or a press takes one, Escape closes them. Returns the text now, whether it changed this frame, and
whether it changed because a suggestion was taken."
  (let* ((ui *ui*)
         (owner (owner-key ui (or id (required-id 'combo-box))))
         (state (state-of ui owner #'make-menu-state))
         (spec (make-float-spec :placement :below))
         (taken nil))
    (when (or (not enabled) (null suggestions))
      (setf (menu-state-open state) nil))
    ;; The menu first, so that its keys (the arrows, Return on a row, Escape) are its own; it is put
    ;; under the field, which is described after it, before layout.
    (when (menu-state-open state)
      (let ((chosen (menu ui owner state suggestions spec :typing t)))
        (when chosen
          (setf taken (nth chosen suggestions)))))
    (let ((index (ui-count ui)))
      (multiple-value-bind (text changed submitted)
          (text-field (or taken string) :id id :tooltip tooltip :labelled-by labelled-by
                                        :width width :enabled enabled :flex flex)
        (declare (ignore submitted))
        (let ((field (svref (ui-boxes ui) index)))
          (setf (box-kind field) :combo-box
                (float-spec-anchor spec) field
                (float-spec-min-width spec) (box-pref-w field))
          (when (= (ui-focus ui) (box-key field))
            ;; Typing opens the suggestions (whatever the app gave for the text before: it gives those
            ;; for the new text in the next build, which closes the menu if there are none); Down
            ;; opens them too.
            (when (and changed (not taken))
              (open-menu state -1))
            (unless (menu-state-open state)
              (dolist (event (copy-list (ui-keys ui)))
                (when (and (eql (key-event-logical-key event) +key-down+) suggestions)
                  (consume-key ui event)
                  (open-menu state 0))))))
        (values text (or changed (and taken (string/= taken string))) (and taken t))))))

;;; --- the system's file panels ---
;;;
;;; Called every frame, as a widget is. While SHOW is false they do nothing; on a frame it is true
;;; (what a button returned), the host is asked for the system's panel, which it shows after the
;;; frame; the path the person chose comes back later, and the call returns it on the frame after,
;;; once. A context with no window (a test) answers through its FILE-CHOOSER at once.

(defstruct (file-dialog-state (:constructor make-file-dialog-state ()))
  (waiting nil)
  (chosen nil))

(defun file-dialog (kind widget id show types name)
  (let* ((ui *ui*)
         (context (ui-context ui))
         (state (state-of ui (owner-key ui (or id (required-id widget))) #'make-file-dialog-state)))
    (when (and show (not (file-dialog-state-waiting state)))
      (setf (file-dialog-state-waiting state) t)
      (context-choose-file context kind types name
                           (lambda (path)
                             (setf (file-dialog-state-chosen state) path
                                   (file-dialog-state-waiting state) nil)
                             (request-frame context))))
    (let ((path (file-dialog-state-chosen state)))
      (when path
        (setf (file-dialog-state-chosen state) nil
              (ui-consumed ui) t))
      path)))

(defun open-file-dialog (&key id show types)
  "On a frame SHOW is true, ask the person for a file to open with the system's Open panel, which
allows only the extensions in TYPES (strings without the dot; all if NIL). Returns the path chosen,
on the frame it comes back, and otherwise NIL (as after Cancel). The :id is required."
  (file-dialog :open 'open-file-dialog id show types nil))

(defun save-file-dialog (&key id show types name)
  "On a frame SHOW is true, ask the person where to save with the system's Save panel, offering NAME,
with TYPES the extensions allowed. Returns the path chosen, on the frame it comes back, and otherwise
NIL. The :id is required."
  (file-dialog :save 'save-file-dialog id show types name))

;;; --- a popover and a dialog ---
;;;
;;; Both float above the screen while the application says they are OPEN, and hold what their body
;;; describes, one under another. A popover is put by a widget described before it in the same
;;; container (its anchor, named by id), over a catcher as a menu is: a press outside it, or Escape,
;;; dismisses it. A dialog is modal: in the middle of the window over a scrim, which a press does not
;;; go through, with the keyboard kept inside it; Escape dismisses it. Each returns true on the frame
;;; the person dismissed it, and the application then stops saying it is open (or does not).

(define-condition missing-anchor (error)
  ((anchor :initarg :anchor :reader missing-anchor-anchor))
  (:report (lambda (condition stream)
             (format stream "A popover is put by a widget described before it in the same ~
                             container, and there is none with the id ~s."
                     (missing-anchor-anchor condition)))))

(defun owner-key (ui id)
  "The key a thing with ID would have in the open container, for a thing that has no box there."
  (let ((parent (ui-parent ui)))
    (let ((key (mix (if parent (box-key parent) +seed+) (mix (hash-of :floating) (hash-of id)))))
      (if (zerop key) 1 key))))

(defun escape-pressed-p (ui)
  "Take Escape from the frame's keys, if it is there."
  (let ((event (find #\Escape (ui-keys ui) :key #'key-event-logical-key)))
    (when event
      (consume-key ui event)
      t)))

(defun begin-popover (id open anchor placement)
  "Start the float of a popover if it is OPEN: returns its box, the box that was open, and true if a
press outside dismissed it; or NIL."
  (when open
    (let* ((ui *ui*)
           (owner (owner-key ui (or id (required-id 'popover))))
           (parent (ui-parent ui))
           (anchor-box (and parent
                            (find anchor (box-children parent) :key #'box-id :test #'equal
                                                                :from-end t)))
           (dismissed nil))
      (unless anchor-box
        (error 'missing-anchor :anchor anchor))
      (multiple-value-bind (catcher open)
          (open-float ui :catcher owner :catcher (make-float-spec :placement :fill))
        (setf (box-interactive catcher) t)
        (close-float ui open)
        (when (= (ui-hot ui) (box-key catcher))
          (when (ui-pressed ui)
            (consume-press ui)
            (setf dismissed t))
          (consume-wheel ui)))
      (multiple-value-bind (float open)
          (open-float ui :popover owner id
                      (make-float-spec :anchor anchor-box :placement placement))
        (setf (box-interactive float) t)
        (when (and (= (ui-hot ui) (box-key float)) (ui-pressed ui))
          (consume-press ui))
        (values float open dismissed)))))

(defun end-popover (float open dismissed)
  (let ((ui *ui*))
    (close-float ui open)
    (when (escape-pressed-p ui)
      (setf dismissed t))
    (when dismissed
      (setf (ui-consumed ui) t))
    (and float dismissed)))

(defmacro popover ((&key id open anchor (placement :below)) &body body)
  "While OPEN, what BODY describes, one under another, in a float put by ANCHOR (the id of a widget
described before this in the same container) :below or :right of it. Returns true on the frame the
person dismissed it, by a press outside it or Escape. The :id is required."
  (let ((float (gensym "FLOAT")) (open-box (gensym "OPEN")) (dismissed (gensym "DISMISSED")))
    `(multiple-value-bind (,float ,open-box ,dismissed) (begin-popover ,id ,open ,anchor ,placement)
       (when ,float
         ,@body
         (end-popover ,float ,open-box ,dismissed)))))

(defun begin-dialog (id open title)
  "Start the float of a dialog if it is OPEN: returns its box and the box that was open; or NIL."
  (when open
    (let* ((ui *ui*)
           (owner (owner-key ui (or id (required-id 'dialog)))))
      (multiple-value-bind (scrim open)
          (open-float ui :scrim owner :scrim (make-float-spec :placement :fill))
        ;; It takes the pointer from all under it (being over it), and does nothing with it.
        (setf (box-interactive scrim) t)
        (close-float ui open))
      (multiple-value-bind (float open)
          (open-float ui :dialog owner id (make-float-spec :placement :center :modal t))
        (setf (box-interactive float) t
              (box-text float) (or title ""))
        (when (and (= (ui-hot ui) (box-key float)) (ui-pressed ui))
          (consume-press ui))
        (when title
          (label title :id :title :size 18))
        (values float open)))))

(defun end-dialog (float open)
  (declare (ignore float))
  (let ((ui *ui*))
    (close-float ui open)
    (when (escape-pressed-p ui)
      (setf (ui-consumed ui) t)
      t)))

(defmacro dialog ((&key id open title) &body body)
  "While OPEN, a dialog in the middle of the window, over a scrim that keeps the pointer from what is
under it, with TITLE over what BODY describes; the keyboard stays inside it. Returns true on the
frame the person dismissed it with Escape (its buttons say what they do as any button does). The
:id is required."
  (let ((float (gensym "FLOAT")) (open-box (gensym "OPEN")))
    `(multiple-value-bind (,float ,open-box) (begin-dialog ,id ,open ,title)
       (when ,float
         ,@body
         (end-dialog ,float ,open-box)))))

;;; --- a box that is checked or not, with a label ---

(defun checkbox (label checked &key (id (required-id 'checkbox)) tooltip (enabled t) (flex 0))
  "A box with LABEL beside it, CHECKED or not (what the application holds). Returns the state now
and whether the person changed it this frame: the primary button went down and up over the box or
the label, or the space bar was pressed with the keyboard on it."
  (let* ((ui *ui*)
         (box (open-box ui :checkbox :id id :tooltip tooltip))
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

;;; --- a switch, on or off ---

(defun switch (label on &key (id (required-id 'switch)) tooltip (enabled t) (flex 0))
  "A switch with LABEL beside it, ON or off (what the application holds): a check box that looks
like a light switch, for a setting that takes effect at once. Returns the state now and whether the
person changed it this frame: the primary button went down and up over it, or the space bar was
pressed with the keyboard on it."
  (let* ((ui *ui*)
         (box (open-box ui :switch :id id :tooltip tooltip))
         (key (box-key box))
         (new (and on t)))
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
    (values new (not (eq new (and on t))))))

;;; --- something going on whose end is not known ---
(defconstant +spinner-steps+ 8 "How many dots go round a spinner.")

(defun spinner (&key (id (required-id 'spinner)) tooltip labelled-by (size 18) (flex 0))
  "A ring of dots that goes round while it is shown, for work whose end is not known (for work whose
end is known, a progress bar). It asks for the frames it needs to move, about ten a second: an app
with a spinner showing is not at rest."
  (let ((box (open-box *ui* :spinner :id id :tooltip tooltip :labelled-by labelled-by)))
    (setf (box-pref-w box) (float size 1f0)
          (box-pref-h box) (float size 1f0)
          ;; The step the ring is at, from the time of the frame.
          (box-n3 box) (mod (floor (now-ns) 100000000) +spinner-steps+)
          (box-flex box) (float flex 1f0))
    (request-frame (ui-context *ui*))
    (close-box *ui*)
    nil))

;;; --- a list to pick from ---

(defstruct (list-state (:constructor make-list-state ()))
  (top 0 :type fixnum)
  (remainder 0f0 :type single-float))

(defun list-box (items selected
                 &key (id (required-id 'list-box)) tooltip labelled-by
                   (rows 8) (width 200) (enabled t) (flex 0))
  "A list of ITEMS (strings), showing ROWS at a time, with the SELECTED one (an index, or
NIL) marked. Returns the index now and whether the person changed it this frame."
  (let* ((ui *ui*)
         (box (open-box ui :list-box :id id :tooltip tooltip :labelled-by labelled-by))
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
               &key (id (required-id 'canvas)) tooltip labelled-by focusable autofocus cursor
                 (flex 0))
  "An area of WIDTH by HEIGHT. PAINTER is called every frame with the list to append to, the
context, and the width and height, and draws from the top left of the area, which it clips
to; if what it draws is what it drew last frame, nothing more is done for it. Returns the
events for the canvas, oldest first: (:press x y button click-count), (:move x y), (:drag x y),
(:release x y inside-p), (:key event), (:text string), (:composition kind text selection-start
selection-length), (:wheel dx dy) and (:exit), with points in the canvas. The code that handles
them can ask CURRENT-CONTEXT for the context, which measures text. A FOCUSABLE canvas that is
also AUTOFOCUS has the keyboard when the window opens, if nothing has it by then. CURSOR is the shape
the pointer has over it (one of +CURSORS+; the arrow if not given), which the app can change from
frame to frame, as what is under the pointer changes."
  (let* ((ui *ui*)
         (box (open-box ui :canvas :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box-key box))
         (state (state-of ui key #'make-canvas-state))
         (events '())
         (rect (rect-of ui key))
         (hot (= (ui-hot ui) key)))
    (when (and autofocus focusable (not (ui-started ui)) (zerop (ui-focus ui)))
      (setf (ui-focus ui) key))
    (when cursor
      (unless (member cursor +cursors+)
        (error "~s is not a cursor; the cursors are ~{~s~^, ~}." cursor +cursors+))
      (want-cursor ui key cursor))
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

(defun scrim (&key (id (required-id 'scrim)) tooltip)
  "A translucent layer over everything before it in a stack, as large as it is allowed to be,
that takes the pointer from what is under it. Returns true when it is pressed."
  (let* ((ui *ui*)
         (box (open-box ui :scrim :id id :tooltip tooltip))
         (key (box-key box))
         (pressed nil))
    (setf (box-interactive box) t)
    (when (and (= (ui-hot ui) key) (ui-pressed ui))
      (consume-press ui)
      (setf pressed t))
    (close-box ui)
    pressed))

;;; --- a tooltip ---
;;;
;;; Any widget given :tooltip takes the pointer, and when the pointer has rested on it for
;;; +TOOLTIP-DELAY-NS+ (with no press or key since it came), its words float under it, until the
;;; pointer leaves or something is pressed. Until then the frames ask for the next, so that it comes
;;; on time.

(defconstant +tooltip-delay-ns+ 500000000 "How long the pointer rests on a widget before its tooltip.")

(defun describe-tooltip (ui)
  "Describe the tooltip of the widget the pointer is on, if it has one and its time has come. Called
after the app's function, with the build's boxes made."
  (let ((hot (ui-hot ui)))
    (unless (or (zerop hot) (ui-tooltip-dismissed ui) (ui-down ui))
      (let ((box (loop :for index :below (ui-count ui)
                       :for box = (svref (ui-boxes ui) index)
                       :when (= (box-key box) hot) :return box)))
        (when (and box (box-tooltip box))
          (if (< (- (now-ns) (ui-hot-since ui)) +tooltip-delay-ns+)
              (request-frame (ui-context ui))
              (multiple-value-bind (float open)
                  (open-float ui :tooltip hot :tooltip
                              (make-float-spec :anchor box :placement :below))
                (setf (box-text float) (box-tooltip box))
                (close-float ui open))))))))

;;; --- toasts ---
;;;
;;; A toast is a few words the app shows for a while when something happens (Saved, Sent), at the
;;; bottom of the window in the middle, the newer under the older, and that go by themselves, or when
;;; pressed. They are the ui's, not a widget's: TOAST can be called anywhere in the app's function,
;;; and the ui describes them after it. While one shows, the frames ask for the next, so that it goes
;;; on time; a screen reader reads each out as it comes.

(defun toast (words &key (seconds 3))
  "Show WORDS at the bottom of the window for SECONDS. Returns WORDS."
  (let ((ui *ui*))
    (setf (ui-toasts ui)
          (append (ui-toasts ui)
                  (list (cons words (+ (now-ns) (round (* seconds 1000000000)))))))
    words))

(defun describe-toasts (ui)
  "Describe the toasts whose time has not come, and forget the others. Called after the app's
function."
  (let ((now (now-ns)))
    (setf (ui-toasts ui) (remove-if (lambda (toast) (<= (cdr toast) now)) (ui-toasts ui))))
  (when (ui-toasts ui)
    (request-frame (ui-context ui))
    (multiple-value-bind (float open)
        (open-float ui :toasts +seed+ :toasts (make-float-spec :placement :bottom))
      (let ((pressed nil))
        (loop :for toast :in (ui-toasts ui)
              :for i :from 0
              :do (let* ((box (open-box ui :toast :id i))
                         (key (box-key box)))
                    (setf (box-text box) (car toast)
                          (box-interactive box) t)
                    (when (and (= (ui-hot ui) key) (ui-pressed ui))
                      (consume-press ui)
                      (setf pressed toast))
                    (close-box ui)))
        (close-float ui open)
        (when pressed
          (setf (ui-toasts ui) (remove pressed (ui-toasts ui))))
        float))))

;;; --- what decides size and look ---

(defun own-hashes (box)
  "The hash of what decides BOX's size and of what decides how it looks, as two values, not
counting its children."
  (let* ((layout (hash-numbers +seed+ (theme-id (box-theme box))
                               (hash-of (box-kind box)) (hash-of (box-text box))
                               (box-size box) (hash-of (box-flex box)) (hash-of (box-gap box))
                               (hash-of (box-main-align box)) (hash-of (box-cross-align box))
                               (hash-of (box-fill box)) (hash-of (box-id box))
                               (hash-of (box-interactive box)) (hash-of (box-focusable box))
                               (hash-of (box-moves box)) (hash-of (box-tooltip box))
                               (hash-of (box-pad-left box)) (hash-of (box-pad-top box))
                               (hash-of (box-pad-right box)) (hash-of (box-pad-bottom box))
                               (hash-of (box-pref-w box)) (hash-of (box-pref-h box))))
         (layout (case (box-kind box)
                   (:choice (mix layout (hash-options (car (box-data box)))))
                   ((:list-box :text-area) (mix layout (box-n3 box)))
                   (:scroll (mix layout (hash-of (box-n1 box))))
                   (:lazy-list (hash-numbers layout (hash-of (box-n1 box)) (hash-of (box-n2 box))
                                             (box-n3 box)))
                   ;; A row of a lazy list is placed by its index.
                   (:column (mix layout (box-n3 box)))
                   ;; A float is put where its spec says (its anchor's place is the screen's).
                   ((:menu :catcher :tooltip :popover :dialog :toasts)
                    (let ((spec (box-data box)))
                      (hash-numbers layout (hash-of (float-spec-placement spec))
                                    (hash-of (float-spec-x spec)) (hash-of (float-spec-y spec))
                                    (hash-of (float-spec-min-width spec)))))
                   (t layout)))
         (paint (hash-numbers layout (box-color box) (box-look box) (hash-of (box-n1 box))
                              (hash-of (box-n2 box)) (box-n3 box)
                              (hash-of (box-enabled box)))))
    (setf paint
          (case (box-kind box)
            ((:text-field :combo-box) (let ((state (box-data box)))
                           (hash-numbers paint
                                         (text-state-caret (field-state-state state))
                                         (text-state-anchor (field-state-state state))
                                         (field-state-view-start state))))
            (:text-area (let ((state (area-state-state (box-data box))))
                          (hash-numbers paint (text-state-caret state) (text-state-anchor state))))
            ((:label :paragraph)
             (let ((selection (box-data box)))
               (if (selection-state-p selection)
                   (let ((state (selection-state-state selection)))
                     (hash-numbers paint (text-state-caret state) (text-state-anchor state)))
                   paint)))
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
