(defpackage #:almighty-bolt/ui/widgets
  (:use #:cl)
  (:local-nicknames (#:hash #:almighty-bolt/ui/hash)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:text-edit #:almighty-bolt/text-edit)
                    (#:core #:almighty-bolt/internals)
                    (#:theme #:almighty-bolt/ui/theme))
  (:import-from #:almighty-bolt/internals
                #:image)
  (:export #:missing-id
           #:required-id
           #:set-size-limits
           #:look-of
           #:area-state-lines
           #:missing-id-widget
           #:label
           #:paragraph
           #:paragraph-state
           #:selection-state-p
           #:selection-state-state
           #:paragraph-state-text
           #:paragraph-state-lines
           #:paragraph-state-theme-id
           #:paragraph-state-font
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
           #:+menu-check-width+
           #:switch
           #:tabs
           #:spinner
           #:+spinner-steps+
           #:list-box
           #:progress
           #:image
           #:canvas
           #:scrim
           #:column
           #:row
           #:stack
           #:padding
           #:panel
           #:center
           #:stage
           #:scroll
           #:begin-scroll
           #:end-scroll
           #:scroll-state
           #:scroll-state-offset
           #:scroll-state-content-height
           #:scroll-state-viewport-height
           #:scroll-state-following
           #:+ring-reach+
           #:lazy-list
           #:begin-lazy-list
           #:lazy-list-rows
           #:+lazy-list-height+
           #:own-hashes
           #:describe-tooltip
           #:describe-toasts
           #:toast
           #:shortcut
           #:*shortcuts-heard*
           #:app-menu
           #:spacer
           #:grid
           #:pressable
           #:box-text-font
           #:*foreground*
           #:+tooltip-delay-ns+
           #:text-area
           #:code-view
           #:appearance
           #:area-state
           #:area-state-state
           #:area-state-offset
           #:area-lines
           #:area-line-of
           #:area-line-height
           #:area-state-generation
           #:area-state-inner-width
           #:area-state-inner-height
           #:area-state-numbered
           #:area-state-gutter
           #:area-state-marks
           #:area-left
           #:area-gutter
           #:field-state
           #:field-state-state
           #:field-state-view-start
           #:editable-marked-selection
           #:editable-marked-selection-length
           #:shown-text-state
           #:marked-range
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
           #:ui-face
           #:text-metrics
           #:*face*
           #:+thumb-size+
           #:+dot-size+
           #:+switch-width+))

(in-package #:almighty-bolt/ui/widgets)

;;; The widgets of the immediate app, which are functions the interface calls, in order,
;;; every build. A widget that can be used returns what happened to it (a button, whether it
;;; was clicked; a field, its text and whether the person changed it), and holds nothing but
;;; the little that must outlive a frame (a caret), which is kept by its key. What the
;;; application holds (the text, the number, the choice) is passed in each time and is what
;;; the widget shows, so the application is in charge.

(defconstant +thumb-size+ 16f0)
(defconstant +dot-size+ 16f0)

(defconstant +switch-width+ 32f0 "How wide the track of a switch is; it is half as tall.")

(defvar *face* nil
  "The face of the widget being described, while it takes its input (a field measures its text with
it), or NIL.")

(defun appearance ()
  "Whether the window the app is drawn in is dark or light, as the system shows it: :dark or :light.
An app that draws in a theme of its own picks its dark or its light one by it; it changes when the
system's setting does, and the app is drawn again."
  (let ((context (ui:ui-context ui:*ui*)))
    (if context (core:context-appearance context) :dark)))

(defun widget-face (font role)
  "The face a widget whose :FONT is FONT (a font designator, or NIL) and whose role is ROLE is drawn
in, in the theme it is described in."
  (theme:resolve-face font role theme:*theme*))

(defun face-font (context face)
  "The font of CONTEXT that FACE is."
  (core:context-font context (theme:face-family face) (theme:face-size face) (theme:face-weight face) (theme:face-tracking face)))

(defun ui-face (&optional box)
  "The face of BOX if it has one, or of the widget being described, or the theme's body."
  (let ((font (and box (box:box-font box))))
    (cond ((theme:face-p font) font)
          (*face*)
          (t (theme:theme-face theme:*theme* :body)))))

(defun ui-font (context &optional box)
  "The font of BOX (see UI-FACE), in CONTEXT."
  (face-font context (ui-face box)))

(defun text-metrics (context face)
  "How far apart the lines of FACE are, and how far down a line its baseline is, as two values: the
face's line height, or its font's ascent and descent; the text in the middle of the line."
  (multiple-value-bind (width ascent descent) (core:context-measure context (face-font context face) "Hg")
    (declare (ignore width))
    (let* ((natural (float (+ ascent descent) 1f0))
           (step (or (theme:face-line-height face) natural)))
      (values step (+ (float ascent 1f0) (* 0.5f0 (- step natural)))))))

(defmacro with-theme ((theme) &body body)
  "What BODY describes, drawn in THEME (a theme, or the name of one) rather than the theme it is in.
A menu, a popover or a dialog that BODY opens is in THEME too."
  `(let ((theme:*theme* (theme:find-theme ,theme)))
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
               :collect `(setf ,h (hash:mix ,h (hash:hash-of ,number))))
       ,h)))

;;; --- the look of a box from the pointer and keyboard ---

(defun look-of (ui key enabled)
  "A small number for how the box KEY looks: 0 plain, 1 hovered, 2 pressed (the pointer over
the box a press began on), 3 disabled, and 4 more if it has the keyboard."
  (+ (cond ((not enabled) 3)
           ((and (= (ui:ui-active ui) key) (= (ui:ui-hot ui) key)) 2)
           ((and (= (ui:ui-hot ui) key) (zerop (ui:ui-active ui))) 1)
           (t 0))
     (if (and enabled (= (ui:ui-focus ui) key)) 4 0)))

;;; --- containers ---

(defun set-size-limits (box
                     width height min-width min-height max-width max-height)
  "Give the container BOX the least and greatest size it was described with. WIDTH is both the least
and the greatest width, and HEIGHT the height; MIN- and MAX- give one side only. Each is within what
the container's parent allows, which wins."
  (flet ((size (value) (and value (float value 1f0))))
    (let ((min-w (size (or width min-width))) (max-w (size (or width max-width)))
          (min-h (size (or height min-height))) (max-h (size (or height max-height))))
      (setf (box:box-min-w box) (or min-w 0f0)
            (box:box-max-w box) (or max-w -1f0)
            (box:box-min-h box) (or min-h 0f0)
            (box:box-max-h box) (or max-h -1f0)))))

(defmacro with-container ((box kind &rest keys) &body body)
  `(let ((,box (ui:open-box ui:*ui* ,kind :id ,(getf keys :id))))
     ,@body
     (ui:close-box ui:*ui*)
     nil))

(defmacro column ((&key id (gap 0) (main-align :start) (cross-align :start) fill (flex 0)
                     width height min-width min-height max-width max-height)
                  &body body)
  "Children one under another."
  (let ((box (gensym "BOX")))
    `(with-container (,box :column :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-gap ,box) (float ,gap 1f0) (box:box-main-align ,box) ,main-align
             (box:box-cross-align ,box) ,cross-align (box:box-fill ,box) ,fill
             (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro row ((&key id (gap 0) (main-align :start) (cross-align :start) fill (flex 0)
                     width height min-width min-height max-width max-height)
               &body body)
  "Children side by side."
  (let ((box (gensym "BOX")))
    `(with-container (,box :row :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-gap ,box) (float ,gap 1f0) (box:box-main-align ,box) ,main-align
             (box:box-cross-align ,box) ,cross-align (box:box-fill ,box) ,fill
             (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro stack ((&key id (flex 0) width height min-width min-height max-width max-height) &body body)
  "Children on top of each other, the later on top."
  (let ((box (gensym "BOX")))
    `(with-container (,box :stack :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro padding ((&key id all left top right bottom (flex 0) width height min-width min-height max-width max-height) &body body)
  (let ((box (gensym "BOX")))
    `(with-container (,box :padding :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-pad-left ,box) (float (or ,left ,all 0) 1f0)
             (box:box-pad-top ,box) (float (or ,top ,all 0) 1f0)
             (box:box-pad-right ,box) (float (or ,right ,all 0) 1f0)
             (box:box-pad-bottom ,box) (float (or ,bottom ,all 0) 1f0)
             (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defun shadows (designator)
  "The shadows DESIGNATOR says, each (x y blur spread color) with the colour packed, in the order they
are drawn. DESIGNATOR is NIL (none), a shadow, (:x :y :blur :spread :color), each optional: moved X
across and Y down, blurred by BLUR points (0 a sharp edge), SPREAD points larger all round (smaller if
less than 0), in COLOR (a colour designator, as THEME-COLOR takes; black a quarter showing if not
given); or a list of shadows, the first drawn first, as a design tool stacks them."
  (labels ((one (plist)
             (unless (and (listp plist) (evenp (length plist)))
               (error "A shadow is (:x :y :blur :spread :color), not ~s." plist))
             (loop :for (key) :on plist :by #'cddr
                   :unless (member key '(:x :y :blur :spread :color))
                     :do (error "A shadow has no ~s: it takes :x, :y, :blur, :spread and :color."
                                key))
             (destructuring-bind (&key (x 0) (y 0) (blur 0) (spread 0) color) plist
               (check-type x real)
               (check-type y real)
               (check-type blur (real 0))
               (check-type spread real)
               (list (float x 1f0) (float y 1f0) (float blur 1f0) (float spread 1f0)
                     (if color (theme:theme-color color) #x00000040)))))
    (cond ((null designator) '())
          ((keywordp (first designator)) (list (one designator)))
          (t (mapcar #'one designator)))))

(defun border-dash (designator)
  "The dash and gap, (dash . gap) in points, of an edge whose :BORDER-DASH is DESIGNATOR: NIL for a
solid edge, a number for dashes that long with gaps as long, or (DASH GAP)."
  (etypecase designator
    (null nil)
    ((real (0)) (cons (float designator 1f0) (float designator 1f0)))
    (cons (destructuring-bind (dash gap) designator
            (check-type dash (real (0)))
            (check-type gap (real 0))
            (cons (float dash 1f0) (float gap 1f0))))))

(defun shadows-hash (shadows)
  "A fixnum for SHADOWS, every number of every one."
  (let ((hash 0))
    (dolist (shadow shadows hash)
      (dolist (number shadow)
        (setf hash (hash:mix hash (hash:hash-of number)))))))

(defmacro panel ((&key id all left top right bottom color (radius 8) (flex 0) border-color (border-width 1) border-dash shadow width height min-width min-height max-width max-height) &body body)
  "Children on a rounded background, with an edge of BORDER-COLOR, BORDER-WIDTH points wide, if given:
dashed if BORDER-DASH is a number (dashes and gaps that long) or (DASH GAP). SHADOW is a soft shadow under it, or several (see SHADOWS): it
reaches past the panel's edges, and is cut only where something it is in cuts what is in it (a
scroll container, a pane)."
  (let ((box (gensym "BOX")))
    `(with-container (,box :panel :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-pad-left ,box) (float (or ,left ,all 0) 1f0)
             (box:box-pad-top ,box) (float (or ,top ,all 0) 1f0)
             (box:box-pad-right ,box) (float (or ,right ,all 0) 1f0)
             (box:box-pad-bottom ,box) (float (or ,bottom ,all 0) 1f0)
             (box:box-color ,box) (theme:theme-color ,color)
             (box:box-border-color ,box) (theme:theme-color ,border-color)
             (box:box-border-width ,box) (float ,border-width 1f0)
             (box:box-border-dash ,box) (border-dash ,border-dash)
             (box:box-shadows ,box) (shadows ,shadow)
             (box:box-n1 ,box) (float ,radius 1f0)
             (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro grid ((&key id (columns 1) (gap 0) row-gap (column-widths :equal) (row-heights :auto)
                      (flex 0) width height min-width min-height max-width max-height)
                &body body)
  "Children in COLUMNS columns, filling each row before the next. COLUMN-WIDTHS is :EQUAL, or a
list of numbers, each column's share of the width. ROW-HEIGHTS is :AUTO (each row as tall as its
tallest child), :UNIFORM (every row as tall as the tallest child of all) or a number of points. Each
child is stretched to the cell it is in. GAP is between columns, and ROW-GAP (GAP if not given)
between rows."
  (let ((box (gensym "BOX")))
    `(with-container (,box :grid :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-n3 ,box) (max 1 ,columns)
             (box:box-gap ,box) (float ,gap 1f0)
             (box:box-n1 ,box) (float (or ,row-gap ,gap) 1f0)
             (box:box-data ,box) (cons ,column-widths ,row-heights)
             (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defvar *foreground* nil
  "The colour a label or a paragraph with no :color of its own is drawn in, or NIL for the theme's
text. A pressable that looks like a button makes it the theme's button text, for what is in it.")

(defun begin-pressable (id tooltip enabled selected look color radius left top right bottom flex)
  "Open the box of a pressable and take the pointer and the keys a button takes. Returns the box,
and true if it was clicked this frame."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :pressable :id (or id (required-id 'pressable)) :tooltip tooltip))
         (key (box:box-key box))
         (clicked nil))
    (setf (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-pad-left box) (float left 1f0)
          (box:box-pad-top box) (float top 1f0)
          (box:box-pad-right box) (float right 1f0)
          (box:box-pad-bottom box) (float bottom 1f0)
          (box:box-color box) (theme:theme-color color)
          (box:box-n1 box) (float radius 1f0)
          ;; 1 for the look of a button, 0 for a plain one.
          (box:box-n2 box) (if (eq look :button) 1f0 0f0)
          ;; 0 a button, 1 a toggle that is off, 2 one that is on.
          (box:box-n3 box) (cond ((eq selected :none) 0) (selected 2) (t 1))
          (box:box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
        (ui:consume-press ui)
        (setf (ui:ui-active ui) key))
      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0)
        (when (= (ui:ui-hot ui) key)
          (setf clicked t)))
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-keys ui)))
          (when (member (core:key-event-logical-key event) '(#\Space #\Return))
            (ui:consume-key ui event)
            (setf clicked t)))))
    (setf (box:box-look box) (look-of ui key enabled))
    (values box clicked)))

(defmacro pressable ((&key id tooltip (enabled t) (selected :none) name (look :plain) color
                        border-color (border-width 1) border-dash shadow
                        (radius 8) all left top right bottom (gap 0) (flex 0)
                        width height min-width min-height max-width max-height)
                     &body body)
  "A container that is pressed as a button is: what BODY describes, one under another (GAP apart), on
a background that answers the pointer. Returns true on the frame it is clicked (the pointer, or Space
or Return with the keyboard on it). LOOK :BUTTON draws it in the theme's button colours, and what is in
it in the button's text colour; :PLAIN (the default) on COLOR, or the theme's panel colour, a little
darker or lighter under the pointer. SELECTED, if given, makes it a toggle that is on or off. NAME is
what a screen reader calls it; by default the words of what is in it. SHADOW is a soft shadow under it,
or several, and BORDER-DASH dashes its edge, as a panel's."
  (let ((box (gensym "BOX")) (clicked (gensym "CLICKED")) (looks (gensym "LOOK")))
    `(let ((,looks ,look))
       (multiple-value-bind (,box ,clicked)
           (begin-pressable ,id ,tooltip ,enabled ,selected ,looks ,color ,radius
                            (or ,left ,all 0) (or ,top ,all 0) (or ,right ,all 0) (or ,bottom ,all 0)
                            ,flex)
         (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
         (setf (box:box-data ,box) ,name
               (box:box-border-color ,box) (theme:theme-color ,border-color)
               (box:box-border-width ,box) (float ,border-width 1f0)
               (box:box-border-dash ,box) (border-dash ,border-dash)
               (box:box-shadows ,box) (shadows ,shadow))
         (let ((*foreground* (cond ((not (eq ,looks :button)) *foreground*)
                                   ((box:box-enabled ,box) (theme:theme-button-text theme:*theme*))
                                   (t (theme:theme-dim-text theme:*theme*)))))
           (column (:gap ,gap :cross-align :stretch)
             ,@body))
         (ui:close-box ui:*ui*)
         ,clicked))))

(defun spacer (&key (flex 1))
  "Empty room that takes FLEX shares of the free space of the row or column it is in: in a row, what
is after it goes to the right edge. It holds nothing, so it needs no id, and it is not in the semantic
tree."
  (let ((box (ui:open-box ui:*ui* :spacer)))
    (setf (box:box-flex box) (float flex 1f0))
    (ui:close-box ui:*ui*)
    nil))

(defmacro center ((&key id (flex 0) width height min-width min-height max-width max-height) &body body)
  "The child in the middle of all the room there is."
  (let ((box (gensym "BOX")))
    `(with-container (,box :center :id ,id)
       (set-size-limits ,box ,width ,height ,min-width ,min-height ,max-width ,max-height)
       (setf (box:box-flex ,box) (float ,flex 1f0))
       ,@body)))

(defmacro stage ((&key id width height) &body body)
  "An area in which BODY is laid out, one under another, as in a window WIDTH by HEIGHT points,
whatever room the stage is given: given less, it shows what fits and cuts off the rest, which is
there to be moved to by making the room larger; given more, it is no larger. For a designer that
shows a view or an app at the size it is made for, which the person changes, not the window.
The :id is required."
  (let ((box (gensym "BOX")))
    `(with-container (,box :stage :id (or ,id (required-id 'stage)))
       (setf (box:box-pref-w ,box) (float ,width 1f0)
             (box:box-pref-h ,box) (float ,height 1f0)
             (box:box-cross-align ,box) :stretch)
       ,@body)))

;;; --- a scroll container ---
;;;
;;; What is inside is laid out as a column as tall as it wants to be, and the container shows a
;;; window onto it, moved by the wheel. How far down the window is (the offset, in points) is kept by
;;; the container's id, and is part of what decides the layout: the children are placed above the
;;; window by it. The wheel goes first to what is inside (a list inside scrolls its rows), and what is
;;; left of it, while the pointer is over the container or anything in it, moves the window. What
;;; scrolls takes the wheel only if it can move the way the wheel goes: a list at its top leaves a
;;; wheel going up to the container it is in, as does a container at its top to the one it is in.
;;; When the keyboard comes to something inside that is out of the window, the window moves to show it.

(defun wheel-can-move-p (dy offset most)
  "True if a wheel of DY lines can move what is at OFFSET, which goes from 0 to MOST: a positive DY
moves it toward 0, and a negative one toward MOST."
  (or (and (plusp dy) (plusp offset))
      (and (minusp dy) (< offset most))))

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
  (last-focus 0 :type fixnum)
  ;; True while it follows its end (SCROLL's :follow, and at the end): its layout keeps it there.
  (following nil))

(defun begin-scroll (id height flex)
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :scroll :id (or id (required-id 'scroll))))
         (state (ui:state-of ui (box:box-key box) #'make-scroll-state)))
    (setf (box:box-interactive box) t
          (box:box-pref-h box) (if height (float height 1f0) 0f0)
          (box:box-flex box) (float flex 1f0)
          (box:box-data box) state
          (scroll-state-first-inside state) (ui:ui-count ui))
    box))

(defun inside-scroll-p (ui scroll key)
  "True if the box KEY was made inside the scroll container SCROLL this build: after it, and with it
among its parents. (A float that a widget inside opens, a menu, is made after it too, but is in no
container, and is not inside.)"
  (let ((boxes (ui:ui-boxes ui)))
    (loop :for index :from (scroll-state-first-inside (box:box-data scroll)) :below (ui:ui-count ui)
          :for box = (svref boxes index)
            :thereis (and (= (box:box-key box) key)
                          (loop :for parent = (box:box-parent box) :then (box:box-parent parent)
                                :while parent
                                  :thereis (eq parent scroll))))))

(defun end-scroll (box &optional follow)
  "Close the scroll container BOX: the wheel over it moves it, and the keyboard come to something
inside brings that into sight. If FOLLOW, and it is at the end of what it holds, it stays at the
end when more is put there (its layout puts it there)."
  (let* ((ui ui:*ui*)
         (key (box:box-key box))
         (state (box:box-data box))
         (rect (ui:rect-of ui key))
         (offset (scroll-state-offset state))
         (viewport (scroll-state-viewport-height state))
         (most (max 0f0 (- (scroll-state-content-height state) viewport))))
    (when (and rect (wheel-can-move-p (ui:ui-wheel-dy ui) offset most)
               (or (= (ui:ui-hot ui) key) (inside-scroll-p ui box (ui:ui-hot ui))))
      (multiple-value-bind (dx dy) (ui:consume-wheel ui)
        (declare (ignore dx))
        (decf offset (* dy (float core:+points-per-line+ 1f0)))))
    ;; The keyboard came to something inside: show all of it, moving the window as little as can be.
    (let ((focus (ui:ui-focus ui)))
      (when (and rect (/= focus (scroll-state-last-focus state)) (/= focus 0)
                 (inside-scroll-p ui box focus))
        (let ((target (ui:rect-of ui focus)))
          (when target
            (let* ((top (+ (- (ui:rect-y target) (ui:rect-y rect)) offset))
                   (bottom (+ top (ui:rect-h target))))
              ;; With room for its ring: the edge of the window may be where nothing is drawn.
              (cond ((< (- top +ring-reach+) offset) (setf offset (- top +ring-reach+)))
                    ((> (+ bottom +ring-reach+) (+ offset viewport))
                     (setf offset (- (+ bottom +ring-reach+) viewport))))))))
      (setf (scroll-state-last-focus state) focus))
    (setf offset (max 0f0 (min offset most)))
    (setf (scroll-state-offset state) offset
          (box:box-n1 box) offset
          ;; Following: at the end (where the wheel left it, or where it was), so that the layout
          ;; keeps it at the end of what is inside now.
          (scroll-state-following state) (and follow (>= offset (- most 0.5f0))))
    (ui:close-box ui)
    nil))

(defmacro scroll ((&key id height (flex 0) follow) &body body)
  "What BODY describes, one under another, in a window that the wheel moves up and down. The window
is HEIGHT points tall where the container is not given a height (in a column, which gives only the
room it has), or as tall as what is inside; FLEX gives it a share of a column's free space. With
FOLLOW, the window stays at the end while it is there, as more is put at the end (a log, a REPL, a
chat); the wheel takes it back from the end, and it follows again once it is at the end again. The
:id is required: the place the window is at is kept by it."
  (let ((box (gensym "BOX")))
    `(let ((,box (begin-scroll ,id ,height ,flex)))
       ,@body
       (end-scroll ,box ,follow))))

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
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :lazy-list :id (or id (required-id 'lazy-list))))
         (state (ui:state-of ui (box:box-key box) #'make-scroll-state))
         (row-height (float row-height 1f0)))
    (setf (box:box-interactive box) t
          (box:box-pref-h box) (if height (float height 1f0) 0f0)
          (box:box-pref-w box) (if width (float width 1f0) 0f0)
          (box:box-flex box) (float flex 1f0)
          (box:box-n2 box) row-height
          (box:box-n3 box) count
          (box:box-data box) state
          (scroll-state-content-height state) (* count row-height)
          (scroll-state-first-inside state) (ui:ui-count ui))
    ;; Where the window is, kept inside what there is now (the list may have become shorter).
    (let ((viewport (lazy-list-viewport box state)))
      (setf (box:box-n1 box) (max 0f0 (min (scroll-state-offset state)
                                           (- (* count row-height) viewport)))))
    box))

(defun lazy-list-viewport (box state)
  "How tall the window of the lazy list BOX is: from the last layout, or before there is one, what it
will be given if nothing gives it a height."
  (if (plusp (scroll-state-viewport-height state))
      (scroll-state-viewport-height state)
      (if (plusp (box:box-pref-h box)) (box:box-pref-h box) +lazy-list-height+)))

(defun lazy-list-rows (box items key function)
  "Describe the rows of the lazy list BOX that show: FUNCTION is called with each of those ITEMS, in a
column of its own whose id is what KEY returns for the item (or, with no KEY, its index)."
  (let* ((ui ui:*ui*)
         (state (box:box-data box))
         (row-height (box:box-n2 box))
         (count (box:box-n3 box))
         (offset (box:box-n1 box))
         (first (min count (floor offset row-height)))
         (end (min count (ceiling (+ offset (lazy-list-viewport box state)) row-height))))
    (flet ((row (item index)
             (let ((row (ui:open-box ui :column :id (if key (funcall key item) index))))
               (setf (box:box-n3 row) index)
               (funcall function item)
               (ui:close-box ui))))
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
  (let* ((index (core:context-index-at-x context font string x)))
    (values index
            (if (and (plusp index) (< x (core:context-caret-x context font string index)))
                (1- index)
                index))))

(defun press-text (press state index character clicks)
  "The text STATE after a press at INDEX (over CHARACTER), which was the press CLICKS in a row."
  (let ((unit (case clicks (1 :char) (2 :word) (t :line))))
    (setf (text-press-unit press) unit)
    (if (eq unit :char)
        (core:text-place-caret state index)
        (multiple-value-bind (from to) (text-edit:unit-range (core:text-state-string state) character unit)
          (setf (text-press-from press) from
                (text-press-to press) to)
          (text-edit:text-select-units state character unit from to)))))

(defun drag-text (press state index character)
  "The text STATE with the button still down after a press, and the pointer at INDEX (over
CHARACTER): the selection goes from the press to there, by what the press selected by."
  (if (eq (text-press-unit press) :char)
      (core:text-place-caret state index :extend t)
      (text-edit:text-select-units state character (text-press-unit press)
                                   (text-press-from press) (text-press-to press))))

;;; --- text ---

;;; Text that is not edited can be selected and copied when the app says so (:SELECTABLE): a press
;;; gives it the keyboard, the pointer selects in it as in a text field, and Command-A and Command-C
;;; select all of it and copy. It is not in the order Tab goes round, and the selection shows only
;;; while it has the keyboard.

(defstruct (selection-state (:include text-press) (:constructor make-selection-state ()))
  (state (core:make-text-state "") :type almighty-bolt/internals:text-state))

(defun text-font (ui box)
  "The font BOX's text is drawn in, in UI's context."
  (let ((context (ui:ui-context ui)))
    (and context (box-text-font context box))))

(defun select-text (ui box selection index-at)
  "Let the pointer select in the text of BOX, whose SELECTION it is, and Command-A and Command-C
select all of it and copy. INDEX-AT is called with the pointer's x and y and gives the place in the
text nearest them and the character they are over."
  (let ((key (box:box-key box))
        (text (box:box-text box)))
    (unless (string= (core:text-state-string (selection-state-state selection)) text)
      (setf (selection-state-state selection) (core:make-text-state text :caret 0)))
    (setf (box:box-interactive box) t
          (box:box-n2 box) 1f0
          (box:box-data box) selection)
    (ui:want-cursor ui key :text)
    (let ((x (ui:ui-pointer-x ui)) (y (ui:ui-pointer-y ui)))
      (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
        (ui:consume-press ui)
        (ui:focus-by-click ui key)
        (setf (ui:ui-active ui) key)
        (multiple-value-bind (index character) (funcall index-at x y)
          (setf (selection-state-state selection)
                (press-text selection (selection-state-state selection) index character
                            (ui:ui-click-count ui)))))
      (when (and (= (ui:ui-active ui) key) (ui:ui-down ui))
        (multiple-value-bind (index character) (funcall index-at x y)
          (setf (selection-state-state selection)
                (drag-text selection (selection-state-state selection) index character))))
      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0)))
    (when (= (ui:ui-focus ui) key)
      (dolist (event (copy-list (ui:ui-keys ui)))
        (let ((character (core:key-event-logical-key event))
              (modifiers (core:key-event-modifiers event)))
          (when (and (eq (core:key-event-kind event) :down)
                     (characterp character)
                     (core:modifier-down-p modifiers core:+modifier-super+)
                     (not (core:modifier-down-p modifiers core:+modifier-control+)))
            (cond ((char-equal character #\a)
                   (ui:consume-key ui event)
                   (setf (selection-state-state selection)
                         (core:text-select-all (selection-state-state selection))))
                  ((char-equal character #\c)
                   (ui:consume-key ui event)
                   (let ((state (selection-state-state selection)))
                     (when (core:text-state-selection-p state)
                          (core:context-set-clipboard-text (ui:ui-context ui)
                                                           (core:text-state-selected-string state))))))))))
    (setf (box:box-look box) (look-of ui key t))))

(defun box-text-font (context box)
  "The font of the label or paragraph BOX (its face)."
  (ui-font context box))

(defun text-face (font size weight role)
  "The face of a label or a paragraph whose :FONT is FONT, with SIZE and WEIGHT, if given, in place of
its own."
  (theme:face-variant (widget-face font role) size weight))

(defun label (text &key (id (required-id 'label)) tooltip size color (flex 0) live selectable
                     weight font turn)
  "A line of TEXT in FONT (a family's name, a role such as :heading, or a TYPEFACE: the theme's body
font if not given), with SIZE (points) and WEIGHT (1 to 1000, or a name such as :bold or :medium) in
place of the font's if given. A LIVE label is read out by a screen reader whenever its text changes, wherever the
reader is: for a result, a count or a status that a person should hear without going to look for it.
A SELECTABLE label's text can be selected with the pointer and copied. TURN :UP turns the label a
quarter to the left, so that it reads upwards, and :DOWN a quarter to the right, so that it reads
downwards: it is then as wide as a line is tall and as tall as its text is long. A turned label
cannot be selectable."
  (check-type turn (member nil :up :down))
  (when (and turn selectable)
    (error "A turned label cannot be selectable."))
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :label :id id :tooltip tooltip)))
    (setf (box:box-text box) text
          (box:box-n1 box) (case turn (:up 1f0) (:down 2f0) (t 0f0))
          (box:box-font box) (text-face font size weight :body)
          (box:box-size box) (round (theme:face-size (box:box-font box)))
          (box:box-color box) (or (theme:theme-color color) *foreground*)
          (box:box-flex box) (float flex 1f0)
          (box:box-n3 box) (if live 1 0))
    (when selectable
      (select-text ui box (ui:state-of ui (box:box-key box) #'make-selection-state)
                   (lambda (x y)
                         (declare (ignore y))
                         (let ((rect (ui:rect-of ui (box:box-key box)))
                               (font (text-font ui box)))
                           (if (and rect font)
                               (index-at (ui:ui-context ui) font text (- x (ui:rect-x rect)))
                               (values 0 0))))))
    (ui:close-box ui)
    nil))

;;; A paragraph is text broken into lines to the width it is given, so its height depends on
;;; its width: layout works the lines out (they go in a state kept by the box's key, which is
;;; there when layout is skipped for a frame that is the same) and painting draws them.

(defstruct (paragraph-state (:include selection-state) (:constructor make-paragraph-state ()))
  (text "" :type string)
  (wrapped-at -1f0 :type single-float)
  (lines '() :type list)
  ;; The theme whose font the lines were made with, and the font.
  (theme-id 0 :type fixnum)
  (font nil)
  (generation 0 :type fixnum))

(defun set-paragraph-lines (state text wrapped-at lines)
  "Note the LINES that TEXT breaks into at WRAPPED-AT; a new generation says that the lines
are not the ones that were drawn before."
  (setf (paragraph-state-text state) text
        (paragraph-state-wrapped-at state) wrapped-at
        (paragraph-state-lines state) lines
        (paragraph-state-theme-id state) (theme:theme-id theme:*theme*))
  (incf (paragraph-state-generation state))
  state)

(defun paragraph-index-at (ui box state x y)
  "The place in the text of the paragraph BOX (with STATE) nearest X, Y, and the character there, by
the lines it was drawn in."
  (let ((rect (ui:rect-of ui (box:box-key box)))
        (font (text-font ui box))
        (lines (paragraph-state-lines state))
        (text (paragraph-state-text state)))
    (if (not (and rect font lines))
        (values 0 0)
        (let ((step (text-metrics (ui:ui-context ui) (ui-face box))))
          (let* ((row (max 0 (min (1- (length lines))
                                  (floor (- y (ui:rect-y rect)) step))))
                 (line (nth row lines)))
            (destructuring-bind (start end) line
              ;; Not after the line feed that ends a line: that is the start of the next.
              (when (and (> end start) (char= (char text (1- end)) #\Linefeed))
                (decf end))
              (multiple-value-bind (index character)
                  (index-at (ui:ui-context ui) font (subseq text start end) (- x (ui:rect-x rect)))
                (values (+ start index) (+ start character)))))))))

(defun paragraph (text &key (id (required-id 'paragraph)) tooltip size color (flex 0) live
                         selectable weight font)
  "TEXT broken into lines to the width it is given, the lines as far apart as FONT's line height (or
its ascent and descent). LIVE, SELECTABLE, SIZE, WEIGHT and FONT are as for a label."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :paragraph :id id :tooltip tooltip))
         (state (ui:state-of ui (box:box-key box) #'make-paragraph-state)))
    (setf (box:box-text box) text
          (box:box-font box) (text-face font size weight :body)
          (box:box-size box) (round (theme:face-size (box:box-font box)))
          (box:box-color box) (or (theme:theme-color color) *foreground*)
          (box:box-flex box) (float flex 1f0)
          (box:box-n3 box) (if live 1 0)
          (box:box-data box) state)
    (when selectable
      (select-text ui box state (lambda (x y) (paragraph-index-at ui box state x y))))
    (ui:close-box ui)
    nil))

(defun progress (fraction &key (id (required-id 'progress)) tooltip labelled-by (width 200) (flex 0))
  "A bar that is FRACTION (0 to 1) full."
  (let ((box (ui:open-box ui:*ui* :progress :id id :tooltip tooltip :labelled-by labelled-by)))
    (setf (box:box-n1 box) (float (max 0 (min 1 fraction)) 1f0)
          (box:box-pref-w box) (float width 1f0)
          (box:box-flex box) (float flex 1f0))
    (ui:close-box ui:*ui*)
    nil))

;;; --- an image ---

(defun image (source &key (id (required-id 'image)) label tooltip width height tint (opacity 1)
                          (flex 0))
  "SOURCE, an image from MAKE-IMAGE or LOAD-IMAGE, at its own size in points, or WIDTH by HEIGHT;
given only one of them, the other keeps its shape. With TINT (a colour, or a theme's name for one)
it is drawn as an icon is: its own colours are not used, and the tint covers it as much as its
alpha does, so that it can follow the theme. Without, it is drawn as it is, OPACITY (0 to 1)
opaque. A screen reader reads it by its LABEL; one without a label is taken to be decoration, and
is not read."
  (unless (core:image-p source)
    (error "An image widget draws an image from MAKE-IMAGE or LOAD-IMAGE, not ~s." source))
  (check-type opacity (real 0 1))
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :image :id id :tooltip tooltip))
         (natural-w (float (core:image-width source) 1f0))
         (natural-h (float (core:image-height source) 1f0))
         (w (cond (width (float width 1f0))
                  (height (* (float height 1f0) (/ natural-w natural-h)))
                  (t natural-w)))
         (h (cond (height (float height 1f0))
                  (width (* (float width 1f0) (/ natural-h natural-w)))
                  (t natural-h))))
    (setf (box:box-data box) source
          (box:box-text box) (or label "")
          (box:box-pref-w box) w
          (box:box-pref-h box) h
          (box:box-color box) (theme:theme-color tint)
          (box:box-n1 box) (float opacity 1f0)
          (box:box-flex box) (float flex 1f0))
    (ui:close-box ui)
    nil))

;;; --- a button ---

(defun button (text &key (id (required-id 'button)) tooltip (enabled t) (flex 0) font)
  "A button. Returns true when it is clicked: the primary button went down and up over it,
or the space bar or Return was pressed with the keyboard on it."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :button :id id :tooltip tooltip))
         (key (box:box-key box))
         (clicked nil))
    (setf (box:box-font box) (widget-face font :button)
          (box:box-text box) text
          (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
        (ui:consume-press ui)
        (setf (ui:ui-active ui) key))
      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0)
        (when (= (ui:ui-hot ui) key)
          (setf clicked t)))
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-keys ui)))
          (when (member (core:key-event-logical-key event) '(#\Space #\Return))
            (ui:consume-key ui event)
            (setf clicked t)))))
    (setf (box:box-look box) (look-of ui key enabled))
    (ui:close-box ui)
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
  (let ((key (core:key-event-logical-key event))
        (modifiers (core:key-event-modifiers event)))
    (if (not (and (eq (core:key-event-kind event) :down)
                  (characterp key)
                  (core:modifier-down-p modifiers core:+modifier-super+)
                  (not (core:modifier-down-p modifiers core:+modifier-control+))
                  (member key '(#\c #\x #\v) :test #'char-equal)))
        (values state nil)
        (values (cond ((char-equal key #\v)
                       (let ((text (core:context-clipboard-text context)))
                         (if text
                             (core:text-insert state (if single-line (one-line text) (newlines text)))
                             state)))
                      ((not (core:text-state-selection-p state)) state)
                      (t (core:context-set-clipboard-text context (core:text-state-selected-string state))
                         (if (char-equal key #\x) (core:text-insert state "") state)))
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
  (run nil)
  ;; What the input method is composing (MARKED), or NIL: shown at the caret, underlined, but not
  ;; in the text until it is committed. The part of it that the input method has selected (the
  ;; clause it is converting) starts at MARKED-SELECTION and is MARKED-SELECTION-LENGTH long.
  (marked nil :type (or null string))
  (marked-selection 0 :type fixnum)
  (marked-selection-length 0 :type fixnum))

(defun forget-edits (editable)
  (setf (editable-undo editable) '()
        (editable-redo editable) '()
        (editable-run editable) nil
        (editable-marked editable) nil))

;;; --- text from the input method ---
;;;
;;; While an editable text has the keyboard it says so to the context, with where its caret is in
;;; the window (SAY-TEXT-INPUT-CARET), and a window then sends what is typed through the system's
;;; input method: text comes as text events (a letter, an accent made with a dead key, a character
;;; chosen from a palette), and keys that are commands (an arrow, Return, Delete) as keys. What the
;;; input method composes (the reading of Japanese or Chinese before it is converted) comes as
;;; composition events: it is shown at the caret, underlined, and goes into the text only when it is
;;; committed, which is one edit to undo. The app is given only text that is committed.

(defun shown-text-state (editable state)
  "The text STATE of EDITABLE as it is shown: with what the input method is composing at the caret,
and the caret where the input method has it in that."
  (let ((marked (editable-marked editable)))
    (if (null marked)
        state
        (let ((at (core:text-state-caret state))
              (string (core:text-state-string state)))
          (core:make-text-state (concatenate 'string (subseq string 0 at) marked (subseq string at))
                                :caret (+ at (editable-marked-selection editable)))))))

(defun marked-range (editable state)
  "Where what the input method composes is in the text of EDITABLE as it is shown (SHOWN-TEXT-STATE
of STATE): its start and end, and the start and end of the part the input method has selected; NIL
if nothing is being composed."
  (let ((marked (editable-marked editable)))
    (when marked
      (let* ((at (core:text-state-caret state))
             (from (+ at (editable-marked-selection editable))))
        (values at (+ at (length marked))
                from (+ from (editable-marked-selection-length editable)))))))

(defun type-input (editable state event single-line)
  "What EVENT, text that the input method gives or a step of what it composes, does to the text
STATE of EDITABLE, as two values: the new state and T. Only text that is committed goes into the
state; what is composed is kept in EDITABLE until then. Line breaks are made spaces if SINGLE-LINE."
  (flet ((clean (text) (if single-line (one-line text) (newlines text))))
    (etypecase event
      (core:text-input-event
       (setf (editable-marked editable) nil)
       (values (core:text-insert state (clean (core:text-input-event-text event))) t))
      (core:composition-event
       (let ((text (core:composition-event-text event)))
         (cond ((or (eq (core:composition-event-kind event) :committed) (zerop (length text)))
                (setf (editable-marked editable) nil)
                (values (if (zerop (length text)) state (core:text-insert state (clean text))) t))
               (t
                ;; What is composed takes the place of the selection, as what is typed does.
                (let* ((state (if (core:text-state-selection-p state) (core:text-insert state "") state))
                       (start (min (length text) (core:composition-event-selection-start event)))
                       (length (min (- (length text) start)
                                    (core:composition-event-selection-length event))))
                  (setf (editable-marked editable) text
                        (editable-marked-selection editable) start
                        (editable-marked-selection-length editable) length)
                  (values state t)))))))))

(defun take-text-input (ui editable state event single-line)
  "Take EVENT, a text or composition event, for EDITABLE, whose text is STATE: the new state, as
TYPE-INPUT gives it, with the edit noted to be undone."
  (ui:consume-input ui event)
  (values (edit-with-history editable state event
                             (lambda (state event) (type-input editable state event single-line)))))

(defun say-text-input-caret (ui x y height)
  "Say that what is being described takes text, with its caret at X, Y in the window and HEIGHT
tall: the window sends what is typed through the input method, and puts its candidates there."
  (let ((context (ui:ui-context ui)))
    (when context
      (core:set-text-input-caret context (float x 1f0) (float y 1f0) 1.5f0 (float height 1f0)))))

(defun edit-kind (event)
  "What the key EVENT does to text, if it edits it: :typing, :deleting or :other. Text from the input
method is typing, and what it composes, once committed, an edit of its own."
  (typecase event
    (core:text-input-event :typing)
    (core:composition-event :other)
    (t (key-edit-kind event))))

(defun key-edit-kind (event)
  (let ((key (core:key-event-logical-key event))
        (modifiers (core:key-event-modifiers event)))
    (cond ((or (core:modifier-down-p modifiers core:+modifier-super+)
               (core:modifier-down-p modifiers core:+modifier-control+))
           :other)
          ((or (eql key #\Backspace) (eql key core:+key-delete-forward+)) :deleting)
          ((and (characterp key) (graphic-char-p key)) :typing)
          (t :other))))

(defun undo-key (event)
  "The key EVENT as :undo (Command-Z) or :redo (Command-Shift-Z), or NIL (for any other event too)."
  (let ((key (and (typep event 'core:key-event) (core:key-event-logical-key event)))
        (modifiers (if (typep event 'core:key-event) (core:key-event-modifiers event) 0)))
    (and (typep event 'core:key-event)
         (eq (core:key-event-kind event) :down)
         (characterp key)
         (char-equal key #\z)
         (core:modifier-down-p modifiers core:+modifier-super+)
         (not (core:modifier-down-p modifiers core:+modifier-control+))
         (if (core:modifier-down-p modifiers core:+modifier-shift+) :redo :undo))))

(defun edit-with-history (editable state event edit)
  "What the key EVENT does to the text STATE of EDITABLE, as two values, the new state and true if the
key was taken: Command-Z and Command-Shift-Z undo and redo, and any other key is EDIT's (a function of
the state and the event that gives the same two values). An edit that changes the text is noted so
that it can be undone."
  (let ((undo (undo-key event)))
    (cond
      ((eq undo :undo)
       (setf (editable-run editable) nil
             (editable-marked editable) nil)
       (let ((before (pop (editable-undo editable))))
         (cond (before (push state (editable-redo editable))
                       (values before t))
               (t (values state t)))))
      ((eq undo :redo)
       (setf (editable-run editable) nil
             (editable-marked editable) nil)
       (let ((after (pop (editable-redo editable))))
         (cond (after (push state (editable-undo editable))
                      (values after t))
               (t (values state t)))))
      (t
       (multiple-value-bind (new taken) (funcall edit state event)
         (when taken
           (if (string= (core:text-state-string new) (core:text-state-string state))
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
  (state (core:make-text-state "") :type almighty-bolt/internals:text-state)
  (view-start 0 :type fixnum))

(defun field-index-at (ui key state x)
  "Two values: the index in the text of the field KEY that the pointer, at X, is nearest, and the
index of the character it is over."
  (let ((rect (ui:rect-of ui key))
        (context (ui:ui-context ui)))
    (if (and rect context)
        (let* ((string (core:text-state-string (field-state-state state)))
               (start (field-state-view-start state)))
          (multiple-value-bind (index character)
              (index-at context (ui-font context) (subseq string start)
                        (- x (ui:rect-x rect) (theme:theme-field-padding-x theme:*theme*)))
            (values (+ start index) (+ start character))))
        (let ((caret (core:text-state-caret (field-state-state state))))
          (values caret caret)))))

(defun keep-caret-visible (ui state width)
  (let ((context (ui:ui-context ui)))
    (when context
      (let* ((font (ui-font context))
             (shown (shown-text-state state (field-state-state state)))
             (string (core:text-state-string shown))
             (caret (core:text-state-caret shown))
             (start (min (field-state-view-start state) caret))
             (inner (- width (* 2 (theme:theme-field-padding-x theme:*theme*)))))
        (loop :while (and (< start caret)
                          (> (core:context-caret-x context font (subseq string start) (- caret start))
                             inner))
              :do (incf start))
        (setf (field-state-view-start state) start)))))

(defun say-field-caret (ui key field)
  "Say where the caret of the text field KEY, whose state is FIELD, is in the window, from where the
field was laid out last."
  (let ((rect (ui:rect-of ui key))
        (context (ui:ui-context ui)))
    (if (not (and rect context))
        (say-text-input-caret ui 0f0 0f0 0f0)
        (let* ((shown (shown-text-state field (field-state-state field)))
               (start (field-state-view-start field))
               (pad (theme:theme-field-padding-y theme:*theme*))
               (x (core:context-caret-x context (ui-font context)
                                        (subseq (core:text-state-string shown) start)
                                        (max 0 (- (core:text-state-caret shown) start)))))
          (say-text-input-caret ui (+ (ui:rect-x rect) (theme:theme-field-padding-x theme:*theme*) x)
                                (+ (ui:rect-y rect) pad -1)
                                (max 0f0 (- (ui:rect-h rect) (* 2 (- pad 1)))))))))

(defun text-field (string
                   &key (id (required-id 'text-field)) tooltip labelled-by
                     (width 180) invalid (enabled t) (flex 0) font)
  "An editable line showing STRING, in FONT (see LABEL; the theme's body font if not given). Returns the text now, whether the person changed it this
frame, and whether Return was pressed. Command-C, Command-X and Command-V copy, cut and paste
(a line break that is pasted becomes a space). Pass what it returned back in the next frame; if the
application passes something else, that is what is shown."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :text-field :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box:box-key box))
         (*face* (setf (box:box-font box) (widget-face font :body)))
         (state (ui:state-of ui key #'make-field-state))
         (changed nil)
         (submitted nil))
    (unless (string= (core:text-state-string (field-state-state state)) string)
      (setf (field-state-state state) (core:make-text-state string)
            (field-state-view-start state) 0)
      (forget-edits state))
    (setf (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-pref-w box) (float width 1f0)
          (box:box-n3 box) (if invalid 1 0)
          (box:box-data box) state
          (box:box-flex box) (float flex 1f0))
    (when enabled
      (ui:want-cursor ui key :text)
      (let ((x (ui:ui-pointer-x ui)))
        ;; A press on the field gives it the keyboard and puts the caret there (two select a word,
        ;; three all of it).
        (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
          (ui:consume-press ui)
          (ui:focus-by-click ui key)
          (setf (ui:ui-active ui) key
                ;; What is typed after a press is an edit of its own.
                (editable-run state) nil)
          (multiple-value-bind (index character) (field-index-at ui key state x)
            (setf (field-state-state state)
                  (press-text state (field-state-state state) index character
                              (ui:ui-click-count ui)))))
        ;; Dragging with the button down extends the selection.
        (when (and (= (ui:ui-active ui) key) (ui:ui-down ui))
          (multiple-value-bind (index character) (field-index-at ui key state x)
            (setf (field-state-state state)
                  (drag-text state (field-state-state state) index character))))
        (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
          (ui:consume-release ui)
          (setf (ui:ui-active ui) 0)))
      ;; What was typed, in the order it came: keys (those another widget did not take) and text
      ;; and compositions from the input method.
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-inputs ui)))
          (cond
            ((not (typep event 'core:key-event))
             (let ((new (take-text-input ui state (field-state-state state) event t)))
               (unless (string= (core:text-state-string new)
                                (core:text-state-string (field-state-state state)))
                 (setf changed t))
               (setf (field-state-state state) new)))
            ((not (member event (ui:ui-keys ui) :test #'eq)))
            ((eql (core:key-event-logical-key event) #\Return)
             (ui:consume-key ui event)
             (setf submitted t))
            (t
             (multiple-value-bind (new edited)
                 (edit-with-history
                  state (field-state-state state) event
                  (lambda (text event)
                    (multiple-value-bind (pasted command)
                        (clipboard-edit text event (ui:ui-context ui) :single-line t)
                      (if command
                          (values pasted t)
                          (core:edit-text text event)))))
               (when edited
                 (ui:consume-key ui event)
                 (unless (string= (core:text-state-string new)
                                  (core:text-state-string (field-state-state state)))
                   (setf changed t))
                 (setf (field-state-state state) new))))))))
    ;; What is composed is the field's only while it has the keyboard.
    (unless (and enabled (= (ui:ui-focus ui) key))
      (setf (editable-marked state) nil))
    (keep-caret-visible ui state (float width 1f0))
    (when (and enabled (= (ui:ui-focus ui) key))
      (say-field-caret ui key state))
    (setf (box:box-look box) (look-of ui key enabled)
          (box:box-text box) (core:text-state-string (shown-text-state state (field-state-state state))))
    (ui:close-box ui)
    (values (core:text-state-string (field-state-state state)) changed submitted)))

;;; --- several lines of editable text ---
;;;
;;; The text is one string with line feeds in it, edited as a text field's is (EDIT-TEXT), and broken
;;; into the lines that are shown at the width of the area: a line that is too long goes on in the next.
;;; Those lines are kept with the text and the width they were made for, and made again when either
;;; changes; the keys that go up and down and the pointer find their place by them. The area shows
;;; ROWS lines and scrolls itself to keep the caret in sight.

(defstruct (area-state (:include editable) (:constructor make-area-state ()))
  (state (core:make-text-state "") :type almighty-bolt/internals:text-state)
  ;; How far down the lines are scrolled, in points.
  (offset 0f0 :type single-float)
  ;; The lines that are shown, each (START . END), END before the line feed that ends it if one does;
  ;; with the text and the width they were made for. A new generation says they are not the ones
  ;; drawn before.
  (lines #() :type simple-vector)
  (lines-text "" :type string)
  (wrapped-at -1f0 :type single-float)
  (theme-id 0 :type fixnum)
  ;; The face the lines were made in.
  (face nil)
  (generation 0 :type fixnum)
  ;; The room inside the box for lines, from the last layout (0 before there is one).
  (inner-width 0f0 :type single-float)
  (inner-height 0f0 :type single-float)
  ;; The x that Up and Down keep to, from the first of them in a row.
  (goal-x nil)
  ;; A code view's: whether its lines are numbered, how wide the column of numbers is with the
  ;; gap after it (from the last layout; 0 if they are not numbered), and the lines marked, each
  ;; (LINE COLOUR BAND NOTE) with LINE from 1, the mark's colour, the colour of the band behind
  ;; the line, and what the mark says or NIL.
  (numbered nil)
  (gutter 0f0 :type single-float)
  (marks '() :type list))

(defun area-line-height (context &optional (face (ui-face)))
  "How far apart the lines of a text area in FACE are (see TEXT-METRICS)."
  (values (text-metrics context face)))

(defun area-lines (context state string width &optional (face (ui-face)))
  "The lines that STRING is shown in at WIDTH in FACE, as a vector of (START . END), from STATE if they
were made for this string, width and face. There is always one: an empty text, and a text that ends in a line
feed, end in an empty line, where the caret can be."
  (unless (and (= (area-state-wrapped-at state) width)
               (= (area-state-theme-id state) (theme:theme-id theme:*theme*))
               (eq (area-state-face state) face)
               (string= (area-state-lines-text state) string))
    (let* ((length (length string))
           (lines (loop :for (start end) :in (if (zerop length)
                                                  '()
                                                  (core:context-break-lines context
                                                                            (face-font context face)
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
            (area-state-theme-id state) (theme:theme-id theme:*theme*)
            (area-state-face state) face)
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
      (- width (* 2 (theme:theme-field-padding-x theme:*theme*)) (area-state-gutter state))))

(defun area-left (state)
  "How far from the left of a text area its lines start: past its padding and, in a code view,
the numbers of its lines."
  (+ (theme:theme-field-padding-x theme:*theme*) (area-state-gutter state)))

(defun line-count (string)
  "How many lines STRING has: one more than its line feeds."
  (1+ (count #\Linefeed string)))

(defun area-gutter (context state string face)
  "How wide the numbers of the lines of STRING are in FACE, with the gap after them, if STATE's
lines are numbered (as wide as the widest number, and as two digits at the least); 0 if not."
  (if (not (area-state-numbered state))
      0f0
      (let ((digits (max 2 (length (princ-to-string (line-count string))))))
        (float (+ (core:context-caret-x context (face-font context face)
                                        (make-string digits :initial-element #\0) digits)
                  (theme:theme-field-padding-x theme:*theme*))
               1f0))))

(defun line-of (string index)
  "The number of the line, from 1, that INDEX in STRING is on."
  (1+ (count #\Linefeed string :end index)))

(defun area-index-at (ui key area x y)
  "Two values: the index in the text of the area KEY that the pointer at X, Y is nearest, and the
index of the character it is over."
  (let* ((rect (ui:rect-of ui key))
         (context (ui:ui-context ui))
         (state (area-state-state area))
         (string (core:text-state-string state)))
    (if (not (and rect context))
        (values (core:text-state-caret state) (core:text-state-caret state))
        (let* ((lines (area-lines context area string (area-inner-width area (ui:rect-w rect))))
               (height (area-line-height context))
               (row (max 0 (min (1- (length lines))
                                (floor (+ (- y (ui:rect-y rect) (theme:theme-field-padding-y theme:*theme*)) (area-state-offset area))
                                       height))))
               (line (svref lines row)))
          (multiple-value-bind (index character)
              (index-at context (ui-font context) (subseq string (car line) (cdr line))
                        (- x (ui:rect-x rect) (area-left area)))
            (values (+ (car line) index) (+ (car line) character)))))))

(defun area-move-vertically (context area lines down extend)
  "The text state of AREA with the caret a line up (or DOWN), at the x it was at when Up or Down
was first pressed; at the first or last line, to the start or the end."
  (let* ((state (area-state-state area))
         (string (core:text-state-string state))
         (caret (core:text-state-caret state))
         (font (ui-font context))
         (row (area-line-of lines caret))
         (line (svref lines row))
         (x (or (area-state-goal-x area)
                (setf (area-state-goal-x area)
                      (float (core:context-caret-x context font (subseq string (car line) (cdr line))
                                                   (- caret (car line)))
                             1f0))))
         (target (+ row (if down 1 -1))))
    (cond ((< target 0) (core:text-place-caret state 0 :extend extend))
          ((>= target (length lines)) (core:text-place-caret state (length string) :extend extend))
          (t (let ((line (svref lines target)))
               (core:text-place-caret state
                                      (+ (car line)
                                         (core:context-index-at-x context font
                                                                  (subseq string (car line) (cdr line)) x))
                                      :extend extend))))))

(defun area-key (ui area event width)
  "What the key EVENT does to the text of AREA, as two values: the new text state and true if the
key was one for the area."
  (let* ((context (ui:ui-context ui))
         (state (area-state-state area))
         (key (core:key-event-logical-key event))
         (modifiers (core:key-event-modifiers event))
         (shift (core:modifier-down-p modifiers core:+modifier-shift+))
         (command (core:modifier-down-p modifiers core:+modifier-super+))
         (down (eq (core:key-event-kind event) :down)))
    (flet ((lines ()
             (area-lines context area (core:text-state-string state) (area-inner-width area width))))
      (cond
        ((not down) (values state nil))
        ((eql key #\Return) (values (core:text-insert state (string #\Linefeed)) t))
        ((and (or (eql key core:+key-up+) (eql key core:+key-down+)) command)
         (values (core:text-move state (if (eql key core:+key-up+) :start :end) :extend shift) t))
        ;; The x it keeps to is kept for the next Up or Down; any other key forgets it (TEXT-AREA).
        ((or (eql key core:+key-up+) (eql key core:+key-down+))
         (values (area-move-vertically context area (lines) (eql key core:+key-down+) shift) t))
        ((and (or (eql key core:+key-left+) (eql key core:+key-right+)) command)
         ;; The start or the end of the line the caret is on.
         (let* ((lines (lines))
                (line (svref lines (area-line-of lines (core:text-state-caret state)))))
           (values (core:text-place-caret state (if (eql key core:+key-left+) (car line) (cdr line))
                                          :extend shift)
                   t)))
        (t
         (multiple-value-bind (pasted command)
             (clipboard-edit state event context)
           (if command
               (values pasted t)
               (core:edit-text state event))))))))

(defun area-most-offset (ui area width rows)
  "How far down the lines of the text AREA can be scrolled, in points, or NIL if UI has no context to
lay the lines out with."
  (let ((context (ui:ui-context ui)))
    (when context
      (let ((lines (area-lines context area
                               (core:text-state-string (shown-text-state area (area-state-state area)))
                               (area-inner-width area width)))
            (height (area-line-height context))
            (inner (if (plusp (area-state-inner-height area))
                       (area-state-inner-height area)
                       (* rows (area-line-height context)))))
        (max 0f0 (- (* (length lines) height) inner))))))

(defun keep-area-caret-visible (ui area width rows)
  (let ((context (ui:ui-context ui)))
    (when context
      (let* ((state (shown-text-state area (area-state-state area)))
             (lines (area-lines context area (core:text-state-string state) (area-inner-width area width)))
             (height (area-line-height context))
             (inner (if (plusp (area-state-inner-height area))
                        (area-state-inner-height area)
                        (* rows height)))
             (top (* height (area-line-of lines (core:text-state-caret state))))
             (offset (area-state-offset area)))
        (cond ((< top offset) (setf offset top))
              ((> (+ top height) (+ offset inner)) (setf offset (- (+ top height) inner))))
        (setf (area-state-offset area)
              (max 0f0 (min offset (max 0f0 (- (* (length lines) height) inner)))))))))

(defun say-area-caret (ui key area width)
  "Say where the caret of the text area KEY, whose state is AREA and which is WIDTH wide, is in the
window, from where the area was laid out last."
  (let ((rect (ui:rect-of ui key))
        (context (ui:ui-context ui)))
    (if (not (and rect context))
        (say-text-input-caret ui 0f0 0f0 0f0)
        (let* ((shown (shown-text-state area (area-state-state area)))
               (string (core:text-state-string shown))
               (lines (area-lines context area string (area-inner-width area width)))
               (height (area-line-height context))
               (row (area-line-of lines (core:text-state-caret shown)))
               (line (svref lines row))
               (x (core:context-caret-x context (ui-font context) (subseq string (car line) (cdr line))
                                        (- (core:text-state-caret shown) (car line)))))
          (say-text-input-caret ui (+ (ui:rect-x rect) (area-left area) x)
                                (+ (ui:rect-y rect) (theme:theme-field-padding-y theme:*theme*)
                                   (* row height) (- (area-state-offset area)) 1)
                                (- height 2))))))

(defun text-area (string
                  &key (id (required-id 'text-area)) tooltip labelled-by
                    (width 320) (rows 6) (enabled t) (flex 0) font)
  "An editable text of several lines showing STRING in FONT (see LABEL; the theme's body font if not
given), its lines as far apart as FONT's line height (or its ascent and descent), WIDTH points wide
and ROWS lines tall, whose long
lines go on in the next. Returns the text now and whether the person changed it this frame. Return
puts in a line feed; Up and Down move a line (Command with them, to the start and the end), Command
with Left and Right to the start and the end of the line; the other keys, and copy, cut and paste,
are a text field's. Pass what it returned back in the next frame; if the application passes something
else, that is what is shown."
  (multiple-value-bind (text changed)
      (lines-of-text string id tooltip labelled-by width rows enabled flex (widget-face font :body)
                     nil '())
    (values text changed)))

(defun code-view (string
                  &key (id (required-id 'code-view)) tooltip labelled-by
                    (width 480) (rows 12) (enabled t) (flex 0) font marks)
  "Code to read and edit: a text area (see TEXT-AREA) in FONT, the theme's code font if not given
(Menlo in the themes the library has), with the number of each line beside it. MARKS marks lines,
each (LINE COLOUR) or (LINE COLOUR NOTE): LINE is its number, from 1; COLOUR a colour or a colour of
the theme (as THEME-COLOR takes it, :field-invalid for an error, :accent for the line a press
called), which the line's number is drawn on and the line's band is a quarter of; NOTE what the mark
says, which a screen reader reads with the area. A long line goes on in the next, under no number.
Returns the text now, whether the person changed it this frame, and the number of the line the caret
is on."
  (lines-of-text string id tooltip labelled-by width rows enabled flex (widget-face font :code)
                 t (mapcar #'resolve-mark marks)))

(defun resolve-mark (mark)
  "A mark of a code view, (LINE COLOUR [NOTE]), as the area keeps it: (LINE COLOUR BAND NOTE)."
  (destructuring-bind (line color &optional note)
      (if (and (consp mark) (typep (first mark) '(integer 1)) (consp (rest mark))
               (or (null (cddr mark)) (and (consp (cddr mark)) (null (cdddr mark))
                                           (or (null (third mark)) (stringp (third mark))))))
          mark
          (error "A mark of a code view is (LINE COLOUR) or (LINE COLOUR NOTE), with LINE a line's ~
                  number from 1 and NOTE a string, not ~s." mark))
    (list line (theme:theme-color color) (theme:theme-color (list color :alpha 0.25)) note)))

(defun lines-of-text (string id tooltip labelled-by width rows enabled flex face numbered marks)
  "A text area or a code view (NUMBERED, with MARKS): see TEXT-AREA and CODE-VIEW."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :text-area :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box:box-key box))
         (*face* (setf (box:box-font box) face))
         (area (ui:state-of ui key #'make-area-state))
         (width (float width 1f0))
         (changed nil)
         (moved nil))
    (unless (string= (core:text-state-string (area-state-state area)) string)
      (setf (area-state-state area) (core:make-text-state string)
            (area-state-offset area) 0f0
            (area-state-goal-x area) nil)
      (forget-edits area))
    (setf (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-pref-w box) width
          (box:box-n3 box) rows
          (box:box-data box) area
          (box:box-flex box) (float flex 1f0)
          ;; Numbered or not, which the layout hash takes.
          (box:box-n2 box) (if numbered 1f0 0f0)
          (area-state-numbered area) numbered
          (area-state-marks area) marks)
    (unless numbered
      (setf (area-state-gutter area) 0f0))
    (when enabled
      (ui:want-cursor ui key :text)
      (let ((x (ui:ui-pointer-x ui))
            (y (ui:ui-pointer-y ui)))
        (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
          (ui:consume-press ui)
          (ui:focus-by-click ui key)
          (setf (ui:ui-active ui) key
                (area-state-goal-x area) nil
                (editable-run area) nil
                moved t)
          (multiple-value-bind (index character) (area-index-at ui key area x y)
            (setf (area-state-state area)
                  (press-text area (area-state-state area) index character (ui:ui-click-count ui)))))
        (when (and (= (ui:ui-active ui) key) (ui:ui-down ui))
          (let ((new (multiple-value-bind (index character) (area-index-at ui key area x y)
                       (drag-text area (area-state-state area) index character))))
            (unless (eq new (area-state-state area))
              (setf (area-state-state area) new
                    moved t))))
        (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
          (ui:consume-release ui)
          (setf (ui:ui-active ui) 0))
        ;; The wheel moves the lines if they can go its way, and is otherwise left for what the text
        ;; area is in (with no context to know how far they go, it is taken).
        (when (and (= (ui:ui-hot ui) key)
                   (let ((most (area-most-offset ui area width rows)))
                     (or (null most)
                         (wheel-can-move-p (ui:ui-wheel-dy ui) (area-state-offset area) most))))
          (multiple-value-bind (dx dy) (ui:consume-wheel ui)
            (declare (ignore dx))
            (when (/= dy 0f0)
              (decf (area-state-offset area) (* dy (float core:+points-per-line+ 1f0)))))))
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-inputs ui)))
          (cond
            ((not (typep event 'core:key-event))
             (let ((new (take-text-input ui area (area-state-state area) event nil)))
               (unless (string= (core:text-state-string new)
                                (core:text-state-string (area-state-state area)))
                 (setf changed t))
               (setf (area-state-state area) new
                     (area-state-goal-x area) nil
                     moved t)))
            ((not (member event (ui:ui-keys ui) :test #'eq)))
            (t
             (let ((vertical (member (core:key-event-logical-key event) (list core:+key-up+ core:+key-down+))))
               (multiple-value-bind (new edited)
                   (edit-with-history area (area-state-state area) event
                                      (lambda (text event)
                                        (declare (ignore text))
                                        (area-key ui area event width)))
                 (when edited
                   (ui:consume-key ui event)
                   (unless vertical
                     (setf (area-state-goal-x area) nil))
                   (unless (string= (core:text-state-string new)
                                    (core:text-state-string (area-state-state area)))
                     (setf changed t))
                   (setf (area-state-state area) new
                         moved t)))))))))
    (unless (and enabled (= (ui:ui-focus ui) key))
      (setf (editable-marked area) nil))
    (if moved
        (keep-area-caret-visible ui area width rows)
        ;; Only kept inside the lines; the wheel may leave the caret out of sight.
        (let ((most (area-most-offset ui area width rows)))
          (when most
            (setf (area-state-offset area) (max 0f0 (min (area-state-offset area) most))))))
    (when (and enabled (= (ui:ui-focus ui) key))
      (say-area-caret ui key area width))
    (setf (box:box-look box) (look-of ui key enabled)
          (box:box-text box) (core:text-state-string (shown-text-state area (area-state-state area)))
          (box:box-n1 box) (area-state-offset area))
    (ui:close-box ui)
    (let ((state (area-state-state area)))
      (values (core:text-state-string state) changed
              (line-of (core:text-state-string state) (core:text-state-caret state))))))

;;; --- a slider ---

;;; A press and a key are taken once, but a drag is where the pointer is, and it is there in every
;;; build of the frame and in the frames after it until the pointer moves. A slider slides only when
;;; the pointer is somewhere it has not slid to: the value the app keeps from a slide (rounded to a
;;; whole number, say) is the value it shows, rather than a slide again to where it was before.

(defstruct (slider-state (:constructor make-slider-state ()))
  ;; Where the pointer was when the slider last slid; NIL from a press until it slides.
  (slid-x nil :type (or null single-float)))

(defun slider (value minimum maximum
               &key (id (required-id 'slider)) tooltip labelled-by (width 200) (enabled t) (flex 0))
  "A slider showing VALUE between MINIMUM and MAXIMUM. Returns the value now and whether the
person changed it this frame."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :slider :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-slider-state))
         (new (float value 1d0))
         (range (- (float maximum 1d0) (float minimum 1d0))))
    (flet ((slide-to (x)
             (let ((rect (ui:rect-of ui key)))
               (when rect
                 (let ((travel (max 1f0 (- (ui:rect-w rect) +thumb-size+))))
                   (setf new (+ minimum
                                (* (max 0d0 (min 1d0 (/ (- x (ui:rect-x rect) (/ +thumb-size+ 2))
                                                        travel)))
                                   range))))))))
      (setf (box:box-enabled box) enabled
            (box:box-interactive box) enabled
            (box:box-focusable box) enabled
            (box:box-pref-w box) (float width 1f0)
            (box:box-flex box) (float flex 1f0))
      (when enabled
        (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
          (ui:consume-press ui)
          (ui:focus-by-click ui key)
          (setf (ui:ui-active ui) key
                (slider-state-slid-x state) nil))
        (when (= (ui:ui-active ui) key)
          (let ((x (ui:ui-pointer-x ui)))
            (when (and (ui:ui-down ui) (not (eql x (slider-state-slid-x state))))
              (setf (slider-state-slid-x state) x)
              (slide-to x)))
          (when (ui:ui-released ui)
            (ui:consume-release ui)
            (setf (ui:ui-active ui) 0)))
        (when (= (ui:ui-focus ui) key)
          (dolist (event (copy-list (ui:ui-keys ui)))
            (let* ((k (core:key-event-logical-key event))
                   (step (/ range 20))
                   (moved (cond ((or (eql k core:+key-left+) (eql k core:+key-down+)) (- new step))
                                ((or (eql k core:+key-right+) (eql k core:+key-up+)) (+ new step))
                                ((eql k core:+key-home+) (float minimum 1d0))
                                ((eql k core:+key-end+) (float maximum 1d0)))))
              (when moved
                (ui:consume-key ui event)
                (setf new (max (float minimum 1d0) (min (float maximum 1d0) moved))))))))
      (setf new (max (float minimum 1d0) (min (float maximum 1d0) new)))
      ;; A new value may change what the application shows before the slider.
      (when (/= new (float value 1d0))
        (setf (ui:ui-consumed ui) t))
      (setf (box:box-n1 box) (if (plusp range)
                                 (float (/ (- new (float minimum 1d0)) range) 1f0)
                                 0f0)
            (box:box-look box) (look-of ui key enabled))
      (let ((aux (box:box-aux-doubles box)))
        (setf (aref aux 0) new
              (aref aux 1) (float minimum 1d0)
              (aref aux 2) (float maximum 1d0)))
      (ui:close-box ui)
      (values new (/= new (float value 1d0))))))

;;; --- a choice between options ---

(defstruct (choice-state (:constructor make-choice-state ()))
  ;; Where each option is in the box, from layout: across, down, how wide, and how tall each is.
  (lefts #() :type simple-vector)
  (tops #() :type simple-vector)
  (widths #() :type simple-vector)
  (option-height 0f0 :type single-float))

(defun tabs (titles selected &key (id (required-id 'tabs)) tooltip labelled-by (enabled t) (flex 0)
                                  font)
  "A tab bar of TITLES (strings), the SELECTED one (an index) marked: a CHOICE drawn as tabs, which
the app follows by drawing the page of the tab chosen. Returns the index now and whether the person
changed it this frame."
  (choice titles selected :id id :tooltip tooltip :labelled-by labelled-by :enabled enabled
                          :flex flex :style :tabs :font font))

(defun choice (options selected
               &key (id (required-id 'choice)) tooltip labelled-by (direction :horizontal) (enabled t)
                 (flex 0) (style :radio) font)
  "A choice among OPTIONS (strings), each with a round button, with the SELECTED one (an index)
marked: side by side, or with DIRECTION :vertical one under another (radio buttons, as a Mac lays
them out). STYLE :SEGMENTED draws them as one control of joined segments, the chosen one filled,
as a Mac segmented control is (side by side, whatever DIRECTION says): for a switch between views.
STYLE :TABS draws them as the tabs of a tab bar, the chosen one marked under its words (TABS).
Returns the index now and whether the person changed it this frame."
  (check-type direction (member :horizontal :vertical))
  (check-type style (member :radio :segmented :tabs))
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :choice :id id :tooltip tooltip :labelled-by labelled-by))
         (*face* (setf (box:box-font box) (widget-face font (if (eq style :radio) :body :button))))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-choice-state))
         (new selected))
    (setf (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-data box) (cons options state)
          ;; Which way the options go, which decides the layout; :SEGMENTED for joined segments.
          (box:box-main-align box) (if (eq style :radio) direction style)
          (box:box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
        (ui:consume-press ui)
        (setf (ui:ui-active ui) key))
      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0)
        (let ((rect (ui:rect-of ui key))
              (lefts (choice-state-lefts state))
              (tops (choice-state-tops state))
              (widths (choice-state-widths state))
              (height (choice-state-option-height state)))
          (when (and rect (= (ui:ui-hot ui) key))
            (dotimes (i (length lefts))
              (let ((left (+ (ui:rect-x rect) (svref lefts i)))
                    (top (+ (ui:rect-y rect) (svref tops i))))
                (when (and (<= left (ui:ui-pointer-x ui) (+ left (svref widths i)))
                           (<= top (ui:ui-pointer-y ui) (+ top height)))
                  (setf new i)
                  (return)))))))
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-keys ui)))
          (let ((k (core:key-event-logical-key event))
                (count (length options)))
            (cond ((and (or (eql k core:+key-left+) (eql k core:+key-up+)) (plusp new))
                   (ui:consume-key ui event) (decf new))
                  ((and (or (eql k core:+key-right+) (eql k core:+key-down+)) (< new (1- count)))
                   (ui:consume-key ui event) (incf new)))))))
    (setf (box:box-n3 box) new
          (box:box-look box) (look-of ui key enabled))
    (ui:close-box ui)
    (values new (/= new selected))))

;;; --- menus ---
;;;
;;; An open menu is a float of rows, put by the widget it belongs to, with a float under it as large
;;; as the window that nothing can be seen of (a catcher): a press anywhere but on the menu lands on
;;; the catcher, which closes the menu and goes nowhere else, as a press outside a Mac menu does.
;;; While a menu is open its keys are its own: Up and Down move the highlight, Return or the space
;;; bar choose, Escape closes. The pointer over a row moves the highlight to it.

(defconstant +menu-check-width+ 22f0 "Before the words of a row, where the mark of the chosen one goes.")

(defstruct (menu-state (:constructor make-menu-state ()))
  (open nil)
  ;; The row the pointer or the arrows are on, or -1.
  (highlight -1 :type fixnum)
  ;; The point a menu opened at, for one that opens where the pointer is.
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  ;; The first row shown of a menu with more rows than fit, or -1 to show the highlighted row.
  (top -1 :type fixnum)
  ;; What has been typed to find a row by its first letters, and when, in internal time.
  (typed "" :type string)
  (typed-at 0 :type integer))

(defun open-menu (state highlight &optional (x 0f0) (y 0f0))
  (setf (menu-state-open state) t
        (menu-state-top state) -1
        (menu-state-typed state) ""
        (menu-state-highlight state) highlight
        (menu-state-x state) (float x 1f0)
        (menu-state-y state) (float y 1f0))
  (setf (ui:ui-consumed ui:*ui*) t))

(defun menu-rows-shown (ui count max-rows)
  "How many of COUNT rows a menu shows: as many as fit in the window, and no more than MAX-ROWS."
  (let* ((context (ui:ui-context ui))
         (height (if context (core:context-height context) 0f0))
         (fit (max 3 (floor (- height (* 2 (theme:theme-menu-padding theme:*theme*)) 16f0)
                            (theme:theme-row-height theme:*theme*)))))
    (max 0 (min count fit (or max-rows count)))))

(defun menu (ui owner state items spec &key checked typing max-rows)
  "Describe the open menu of the widget with the key OWNER, whose state is STATE: ITEMS (strings) in
a float placed by SPEC, the one at CHECKED (an index) marked, over a catcher. Returns the index of the
item chosen this frame, or NIL, and true if the menu closed this frame (an item was chosen, or the
person pressed outside it or pressed Escape). With TYPING (a combo box's), the space bar and letters
are left to the field that is being typed in.

A menu with more rows than fit in the window (or than MAX-ROWS) shows as many as fit, from the row
its state says, as a lazy list does: the wheel moves them, the highlighted row is kept in view, and
letters typed move the highlight to the first row they begin."
  (let* ((chosen nil)
         (closed nil)
         (count (length items))
         (shown (menu-rows-shown ui count max-rows))
         (rows '())
         (highlight-before (menu-state-highlight state)))
    (labels ((reveal (index)
               ;; Move the first row shown as little as shows the row INDEX; a menu just opened shows
               ;; it in the middle.
               (let ((top (menu-state-top state)))
                 (setf (menu-state-top state)
                       (cond ((< index 0) (max 0 top))
                             ((< top 0) (- index (floor shown 2)))
                             ((< index top) index)
                             ((>= index (+ top shown)) (1+ (- index shown)))
                             (t top)))
                 (clamp-top)))
             (clamp-top ()
               (setf (menu-state-top state)
                     (max 0 (min (menu-state-top state) (- count shown))))))
      (when (< (menu-state-top state) 0)
        (reveal highlight-before))
      (clamp-top)
      (let ((top (menu-state-top state)))
        (multiple-value-bind (catcher open)
            (ui:open-float ui :catcher owner :catcher (ui:make-float-spec :placement :fill))
          (setf (box:box-interactive catcher) t)
          (ui:close-float ui open)
          (when (= (ui:ui-hot ui) (box:box-key catcher))
            (when (ui:ui-pressed ui)
              (ui:consume-press ui)
              (setf closed t))
            ;; Nothing under an open menu scrolls.
            (ui:consume-wheel ui)))
        (multiple-value-bind (float open) (ui:open-float ui :menu owner :menu spec)
          ;; A press between the rows is the menu's, and not a press outside it.
          (setf (box:box-interactive float) t)
          (when (and (= (ui:ui-hot ui) (box:box-key float)) (ui:ui-pressed ui))
            (ui:consume-press ui))
          (loop :for item :in (nthcdr top items)
                :for i :from top :below (+ top shown)
                :do (let* ((row (ui:open-box ui :menu-item :id i))
                           (key (box:box-key row)))
                      (push row rows)
                      (setf (box:box-text row) item
                            (box:box-interactive row) t
                            (box:box-moves row) t
                            (box:box-n2 row) (if (eql i checked) 1f0 0f0))
                      (when (= (ui:ui-hot ui) key)
                        ;; The row the pointer moves onto; rows that the wheel or the keys move
                        ;; under a pointer at rest do not take the highlight from the keys.
                        (when (and (ui:ui-moved ui) (/= (menu-state-highlight state) i))
                          (setf (menu-state-highlight state) i
                                (ui:ui-consumed ui) t))
                        (when (ui:ui-pressed ui)
                          (ui:consume-press ui)
                          (setf (ui:ui-active ui) key)))
                      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
                        (ui:consume-release ui)
                        (setf (ui:ui-active ui) 0)
                        (when (= (ui:ui-hot ui) key)
                          (setf chosen i)))
                      (ui:close-box ui)))
          ;; The wheel over the menu moves its rows (and nothing under it).
          (when (or (= (ui:ui-hot ui) (box:box-key float))
                    (find (ui:ui-hot ui) rows :key #'box:box-key))
            (multiple-value-bind (dx dy) (ui:consume-wheel ui)
              (declare (ignore dx))
              (let ((whole (truncate (- dy))))
                (unless (zerop whole)
                  (setf (menu-state-top state) (+ top whole)
                        (ui:ui-consumed ui) t)
                  (clamp-top)))))
          ;; For the painter: where the rows shown are among them all.
          (setf (box:box-n3 float) count
                (box:box-n1 float) (float top 1f0)
                (box:box-n2 float) (float shown 1f0))
          (ui:close-float ui open)))
      (dolist (event (copy-list (ui:ui-keys ui)))
        (let ((k (core:key-event-logical-key event))
              (highlight (menu-state-highlight state)))
          (cond ((eql k core:+key-down+)
                 (ui:consume-key ui event)
                 (setf (menu-state-highlight state)
                       (if (< highlight 0) 0 (min (1- count) (1+ highlight)))))
                ((eql k core:+key-up+)
                 (ui:consume-key ui event)
                 (setf (menu-state-highlight state)
                       (if (< highlight 0) (1- count) (max 0 (1- highlight)))))
                ((and (member k '(#\Return #\Space))
                      (not (and typing (eql k #\Space))))
                 (ui:consume-key ui event)
                 (when (<= 0 highlight (1- count))
                   (setf chosen highlight)))
                ((eql k #\Escape)
                 (ui:consume-key ui event)
                 (setf closed t))
                ;; Letters find the first row that begins with what was typed in the last second.
                ((and (not typing) (characterp k) (graphic-char-p k) (not (eql k #\Space)))
                 (ui:consume-key ui event)
                 (let ((now (get-internal-real-time)))
                   (when (> (- now (menu-state-typed-at state)) internal-time-units-per-second)
                     (setf (menu-state-typed state) ""))
                   (setf (menu-state-typed state)
                         (concatenate 'string (menu-state-typed state) (string k))
                         (menu-state-typed-at state) now))
                 (let* ((typed (menu-state-typed state))
                        (found (position-if (lambda (item)
                                              (and (>= (length item) (length typed))
                                                   (string-equal typed item :end2 (length typed))))
                                            items)))
                   (when found
                     (setf (menu-state-highlight state) found)))))))
      ;; A highlight the keys moved is kept in view, from the next frame.
      (unless (= highlight-before (menu-state-highlight state))
        (reveal (menu-state-highlight state))
        (setf (ui:ui-consumed ui) t)))
    (dolist (row rows)
      (setf (box:box-n3 row) (if (= (box:box-id row) (menu-state-highlight state)) 1 0)))
    (when chosen
      (setf closed t))
    (when closed
      (setf (menu-state-open state) nil
            (ui:ui-consumed ui) t))
    (values chosen closed)))

;;; --- a dropdown ---

(defun dropdown (options selected
                 &key (id (required-id 'dropdown)) tooltip labelled-by width (enabled t) (flex 0)
                   max-rows font)
  "A button showing the SELECTED one (an index) of OPTIONS (strings), which a press opens into a menu
of them all: a pop-up button. WIDTH is how wide it is (by default, as wide as the option it shows).
Its menu shows as many options as fit in the window, or MAX-ROWS, and scrolls.
Returns the index now and whether the person changed it this frame. With the keyboard on it, the
space bar, Return and the arrows open the menu."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :dropdown :id id :tooltip tooltip :labelled-by labelled-by))
         (*face* (setf (box:box-font box) (widget-face font :body)))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-menu-state))
         (new selected))
    (setf (box:box-text box) (or (and selected (nth selected options)) "")
          (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-pref-w box) (if width (float width 1f0) 0f0)
          (box:box-data box) options
          (box:box-flex box) (float flex 1f0))
    (cond
      ((not enabled) (setf (menu-state-open state) nil))
      ((not (menu-state-open state))
       ;; A press opens it (as a Mac pop-up button does, at once), and gives it the keyboard.
       (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
         (ui:consume-press ui)
         (ui:focus-by-click ui key)
         (open-menu state (or selected 0)))
       (when (= (ui:ui-focus ui) key)
         (dolist (event (copy-list (ui:ui-keys ui)))
           (when (member (core:key-event-logical-key event) (list #\Space #\Return core:+key-down+ core:+key-up+))
             (ui:consume-key ui event)
             (open-menu state (or selected 0)))))))
    (ui:close-box ui)
    (when (menu-state-open state)
      (let ((chosen (menu ui key state options
                          (ui:make-float-spec :anchor box :placement :below
                                              :min-width (let ((rect (ui:rect-of ui key)))
                                                             (if rect (ui:rect-w rect) 0f0)))
                          :checked selected :max-rows max-rows)))
        (when chosen
          (setf new chosen))))
    (setf (box:box-n3 box) (if (menu-state-open state) 1 0)
          (box:box-look box) (look-of ui key enabled))
    (values new (not (eql new selected)))))

;;; --- a button with a menu of commands ---

(defun menu-button (text items &key (id (required-id 'menu-button)) tooltip (enabled t) (flex 0)
                                     max-rows font)
  "A button with TEXT that a press opens into a menu of ITEMS (strings): commands, of which none is
chosen beforehand. Returns the item chosen, on the frame it is chosen, and otherwise NIL. With the
keyboard on it, the space bar, Return and the arrows open the menu."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :menu-button :id id :tooltip tooltip))
         (*face* (setf (box:box-font box) (widget-face font :button)))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-menu-state))
         (chosen nil))
    (setf (box:box-text box) text
          (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-flex box) (float flex 1f0))
    (cond
      ((not enabled) (setf (menu-state-open state) nil))
      ((not (menu-state-open state))
       (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
         (ui:consume-press ui)
         (ui:focus-by-click ui key)
         (open-menu state -1))
       (when (= (ui:ui-focus ui) key)
         (dolist (event (copy-list (ui:ui-keys ui)))
           (when (member (core:key-event-logical-key event) (list #\Space #\Return core:+key-down+ core:+key-up+))
             (ui:consume-key ui event)
             (open-menu state 0))))))
    (ui:close-box ui)
    (when (menu-state-open state)
      (let ((index (menu ui key state items
                         (ui:make-float-spec :anchor box :placement :below
                                             :min-width (let ((rect (ui:rect-of ui key)))
                                                            (if rect (ui:rect-w rect) 0f0)))
                         :max-rows max-rows)))
        (when index
          (setf chosen (nth index items)))))
    (setf (box:box-n3 box) (if (menu-state-open state) 1 0)
          (box:box-look box) (look-of ui key enabled))
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
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :context-area :id (or id (required-id 'context-menu))))
         (state (ui:state-of ui (box:box-key box) #'make-context-state)))
    ;; It takes the pointer where nothing inside it does, so that a press there is in the area.
    (setf (box:box-interactive box) t
          (box:box-data box) state
          (context-state-first-inside state) (ui:ui-count ui))
    box))

(defun in-area-p (ui box key)
  "True if KEY is the key of BOX or of a box made inside it this build."
  (or (= key (box:box-key box))
      (let ((boxes (ui:ui-boxes ui)))
        (loop :for index :from (context-state-first-inside (box:box-data box)) :below (ui:ui-count ui)
              :for inside = (svref boxes index)
                :thereis (and (= (box:box-key inside) key)
                              (loop :for parent = (box:box-parent inside) :then (box:box-parent parent)
                                    :while parent
                                      :thereis (eq parent box)))))))

(defun end-context-menu (box items)
  (let* ((ui ui:*ui*)
         (key (box:box-key box))
         (state (box:box-data box))
         (chosen nil))
    (ui:close-box ui)
    (when (and (ui:ui-secondary-pressed ui) (in-area-p ui box (ui:ui-hot ui)))
      (ui:consume-secondary-press ui)
      (open-menu state -1 (ui:ui-pointer-x ui) (ui:ui-pointer-y ui)))
    (when (menu-state-open state)
      (let ((index (menu ui key state items
                         (ui:make-float-spec :placement :at :x (menu-state-x state)
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
                    (flex 0) font)
  "A text field showing STRING with a menu of SUGGESTIONS (strings) under it, which typing opens: the
app gives the suggestions that fit what has been typed, each frame. Down and Up move into them, Return
or a press takes one, Escape closes them. Returns the text now, whether it changed this frame, and
whether it changed because a suggestion was taken."
  (let* ((ui ui:*ui*)
         (owner (owner-key ui (or id (required-id 'combo-box))))
         (state (ui:state-of ui owner #'make-menu-state))
         (spec (ui:make-float-spec :placement :below))
         (taken nil))
    (when (or (not enabled) (null suggestions))
      (setf (menu-state-open state) nil))
    ;; The menu first, so that its keys (the arrows, Return on a row, Escape) are its own; it is put
    ;; under the field, which is described after it, before layout.
    (when (menu-state-open state)
      (let ((chosen (menu ui owner state suggestions spec :typing t)))
        (when chosen
          (setf taken (nth chosen suggestions)))))
    (let ((index (ui:ui-count ui)))
      (multiple-value-bind (text changed submitted)
          (text-field (or taken string) :id id :tooltip tooltip :labelled-by labelled-by
                                        :width width :enabled enabled :flex flex :font font)
        (declare (ignore submitted))
        (let ((field (svref (ui:ui-boxes ui) index)))
          (setf (box:box-kind field) :combo-box
                (ui:float-spec-anchor spec) field
                (ui:float-spec-min-width spec) (box:box-pref-w field))
          (when (= (ui:ui-focus ui) (box:box-key field))
            ;; Typing opens the suggestions (whatever the app gave for the text before: it gives those
            ;; for the new text in the next build, which closes the menu if there are none); Down
            ;; opens them too.
            (when (and changed (not taken))
              (open-menu state -1))
            (unless (menu-state-open state)
              (dolist (event (copy-list (ui:ui-keys ui)))
                (when (and (eql (core:key-event-logical-key event) core:+key-down+) suggestions)
                  (ui:consume-key ui event)
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
  (let* ((ui ui:*ui*)
         (context (ui:ui-context ui))
         (state (ui:state-of ui (owner-key ui (or id (required-id widget))) #'make-file-dialog-state)))
    (when (and show (not (file-dialog-state-waiting state)))
      (setf (file-dialog-state-waiting state) t)
      (core:context-choose-file context kind types name
                                (lambda (path)
                                (setf (file-dialog-state-chosen state) path
                                      (file-dialog-state-waiting state) nil)
                                (core:request-frame context))))
    (let ((path (file-dialog-state-chosen state)))
      (when path
        (setf (file-dialog-state-chosen state) nil
              (ui:ui-consumed ui) t))
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
  (let ((parent (ui:ui-parent ui)))
    (let ((key (hash:mix (if parent (box:box-key parent) hash:+seed+) (hash:mix (hash:hash-of :floating) (hash:hash-of id)))))
      (if (zerop key) 1 key))))

(defun escape-pressed-p (ui)
  "Take Escape from the frame's keys, if it is there."
  (let ((event (find #\Escape (ui:ui-keys ui) :key #'core:key-event-logical-key)))
    (when event
      (ui:consume-key ui event)
      t)))

(defun begin-popover (id open anchor placement)
  "Start the float of a popover if it is OPEN: returns its box, the box that was open, and true if a
press outside dismissed it; or NIL."
  (when open
    (let* ((ui ui:*ui*)
           (owner (owner-key ui (or id (required-id 'popover))))
           (parent (ui:ui-parent ui))
           (anchor-box (and parent
                            (find anchor (box:box-children parent) :key #'box:box-id :test #'equal
                                                                :from-end t)))
           (dismissed nil))
      (unless anchor-box
        (error 'missing-anchor :anchor anchor))
      (multiple-value-bind (catcher open)
          (ui:open-float ui :catcher owner :catcher (ui:make-float-spec :placement :fill))
        (setf (box:box-interactive catcher) t)
        (ui:close-float ui open)
        (when (= (ui:ui-hot ui) (box:box-key catcher))
          (when (ui:ui-pressed ui)
            (ui:consume-press ui)
            (setf dismissed t))
          (ui:consume-wheel ui)))
      (multiple-value-bind (float open)
          (ui:open-float ui :popover owner id
                         (ui:make-float-spec :anchor anchor-box :placement placement))
        (setf (box:box-interactive float) t)
        (when (and (= (ui:ui-hot ui) (box:box-key float)) (ui:ui-pressed ui))
          (ui:consume-press ui))
        (values float open dismissed)))))

(defun end-popover (float open dismissed)
  (let ((ui ui:*ui*))
    (ui:close-float ui open)
    (when (escape-pressed-p ui)
      (setf dismissed t))
    (when dismissed
      (setf (ui:ui-consumed ui) t))
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

(defun begin-dialog (id open title &optional font)
  "Start the float of a dialog if it is OPEN: returns its box and the box that was open; or NIL."
  (when open
    (let* ((ui ui:*ui*)
           (owner (owner-key ui (or id (required-id 'dialog)))))
      (multiple-value-bind (scrim open)
          (ui:open-float ui :scrim owner :scrim (ui:make-float-spec :placement :fill))
        ;; It takes the pointer from all under it (being over it), and does nothing with it.
        (setf (box:box-interactive scrim) t)
        (ui:close-float ui open))
      (multiple-value-bind (float open)
          (ui:open-float ui :dialog owner id (ui:make-float-spec :placement :center :modal t))
        (setf (box:box-interactive float) t
              (box:box-font float) (widget-face font :body)
              (box:box-text float) (or title ""))
        (when (and (= (ui:ui-hot ui) (box:box-key float)) (ui:ui-pressed ui))
          (ui:consume-press ui))
        (when title
          (label title :id :title :size 18))
        (values float open)))))

(defun end-dialog (float open)
  (declare (ignore float))
  (let ((ui ui:*ui*))
    (ui:close-float ui open)
    (when (escape-pressed-p ui)
      (setf (ui:ui-consumed ui) t)
      t)))

(defmacro dialog ((&key id open title font) &body body)
  "While OPEN, a dialog in the middle of the window, over a scrim that keeps the pointer from what is
under it, with TITLE (in FONT, see LABEL) over what BODY describes; the keyboard stays inside it. Returns true on the
frame the person dismissed it with Escape (its buttons say what they do as any button does). The
:id is required."
  (let ((float (gensym "FLOAT")) (open-box (gensym "OPEN")))
    `(multiple-value-bind (,float ,open-box) (begin-dialog ,id ,open ,title ,font)
       (when ,float
         ,@body
         (end-dialog ,float ,open-box)))))

;;; --- a box that is checked or not, with a label ---

(defun checkbox (label checked &key (id (required-id 'checkbox)) tooltip (enabled t) (flex 0) font)
  "A box with LABEL beside it, CHECKED or not (what the application holds). Returns the state now
and whether the person changed it this frame: the primary button went down and up over the box or
the label, or the space bar was pressed with the keyboard on it."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :checkbox :id id :tooltip tooltip))
         (*face* (setf (box:box-font box) (widget-face font :body)))
         (key (box:box-key box))
         (new (and checked t)))
    (setf (box:box-text box) label
          (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
        (ui:consume-press ui)
        (setf (ui:ui-active ui) key))
      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0)
        (when (= (ui:ui-hot ui) key)
          (setf new (not new))))
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-keys ui)))
          (when (eql (core:key-event-logical-key event) #\Space)
            (ui:consume-key ui event)
            (setf new (not new))))))
    (setf (box:box-n3 box) (if new 1 0)
          (box:box-look box) (look-of ui key enabled))
    (ui:close-box ui)
    (values new (not (eq new (and checked t))))))

;;; --- a switch, on or off ---

(defun switch (label on &key (id (required-id 'switch)) tooltip (enabled t) (flex 0) font)
  "A switch with LABEL beside it, ON or off (what the application holds): a check box that looks
like a light switch, for a setting that takes effect at once. Returns the state now and whether the
person changed it this frame: the primary button went down and up over it, or the space bar was
pressed with the keyboard on it."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :switch :id id :tooltip tooltip))
         (*face* (setf (box:box-font box) (widget-face font :body)))
         (key (box:box-key box))
         (new (and on t)))
    (setf (box:box-text box) label
          (box:box-enabled box) enabled
          (box:box-interactive box) enabled
          (box:box-focusable box) enabled
          (box:box-flex box) (float flex 1f0))
    (when enabled
      (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
        (ui:consume-press ui)
        (setf (ui:ui-active ui) key))
      (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
        (ui:consume-release ui)
        (setf (ui:ui-active ui) 0)
        (when (= (ui:ui-hot ui) key)
          (setf new (not new))))
      (when (= (ui:ui-focus ui) key)
        (dolist (event (copy-list (ui:ui-keys ui)))
          (when (eql (core:key-event-logical-key event) #\Space)
            (ui:consume-key ui event)
            (setf new (not new))))))
    (setf (box:box-n3 box) (if new 1 0)
          (box:box-look box) (look-of ui key enabled))
    (ui:close-box ui)
    (values new (not (eq new (and on t))))))

;;; --- something going on whose end is not known ---
(defconstant +spinner-steps+ 8 "How many dots go round a spinner.")

(defun spinner (&key (id (required-id 'spinner)) tooltip labelled-by (size 18) (flex 0))
  "A ring of dots that goes round while it is shown, for work whose end is not known (for work whose
end is known, a progress bar). It asks for the frames it needs to move, about ten a second: an app
with a spinner showing is not at rest."
  (let ((box (ui:open-box ui:*ui* :spinner :id id :tooltip tooltip :labelled-by labelled-by)))
    (setf (box:box-pref-w box) (float size 1f0)
          (box:box-pref-h box) (float size 1f0)
          ;; The step the ring is at, from the time of the frame.
          (box:box-n3 box) (mod (floor (core:now-ns) 100000000) +spinner-steps+)
          (box:box-flex box) (float flex 1f0))
    (core:request-frame (ui:ui-context ui:*ui*))
    (ui:close-box ui:*ui*)
    nil))

;;; --- a list to pick from ---

(defstruct (list-state (:constructor make-list-state ()))
  (top 0 :type fixnum)
  (remainder 0f0 :type single-float)
  ;; The index the app last gave as the selected one, or :NONE before the first frame.
  (given :none))

(defun list-box (items selected
                 &key (id (required-id 'list-box)) tooltip labelled-by
                   (rows 8) (width 200) (enabled t) (flex 0) font)
  "A list of ITEMS (strings), showing ROWS at a time, with the SELECTED one (an index, or
NIL) marked. Returns the index now and whether the person changed it this frame."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :list-box :id id :tooltip tooltip :labelled-by labelled-by))
         (*face* (setf (box:box-font box) (widget-face font :body)))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-list-state))
         (count (length items))
         (new selected))
    (labels ((reveal (index)
               ;; Scroll as little as shows the row INDEX.
               (cond ((< index (list-state-top state)) (setf (list-state-top state) index))
                     ((>= index (+ (list-state-top state) rows))
                      (setf (list-state-top state) (1+ (- index rows))))))
             (pick (index)
               (when (and (<= 0 index) (< index count))
                 (setf new index)
                 (reveal index))))
      ;; A selected row the app gives, on the first frame or changed since, is scrolled into
      ;; view; one that stays the same is left where the wheel put it.
      (unless (eql selected (list-state-given state))
        (setf (list-state-given state) selected)
        (when (and selected (< -1 selected count))
          (reveal selected)))
      (setf (box:box-enabled box) enabled
            (box:box-interactive box) enabled
            (box:box-focusable box) enabled
            (box:box-pref-w box) (float width 1f0)
            (box:box-n3 box) rows
            (box:box-flex box) (float flex 1f0))
      (when enabled
        (let ((rect (ui:rect-of ui key)))
          (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
            (ui:consume-press ui)
            (ui:focus-by-click ui key)
            (setf (ui:ui-active ui) key)
            (when rect
              (let ((index (+ (list-state-top state)
                              (floor (- (ui:ui-pointer-y ui) (ui:rect-y rect) 1) (theme:theme-row-height theme:*theme*)))))
                (when (and (< index count) (not (eql index selected)))
                  (pick index)))))
          (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
            (ui:consume-release ui)
            (setf (ui:ui-active ui) 0)))
        ;; The wheel moves the rows if they can go its way, and is otherwise left for what the list
        ;; is in.
        (when (and (= (ui:ui-hot ui) key)
                   (wheel-can-move-p (ui:ui-wheel-dy ui) (list-state-top state) (max 0 (- count rows))))
          (multiple-value-bind (dx dy) (ui:consume-wheel ui)
            (declare (ignore dx))
            (let* ((amount (+ (list-state-remainder state) (- dy)))
                   (whole (truncate amount))
                   (most (max 0 (- count rows)))
                   (top (+ (list-state-top state) whole)))
              ;; What is left of a line is kept for the next turn, but not past an end, where it
              ;; would hold back the first turn of the wheel the other way.
              (setf (list-state-remainder state) (if (<= 0 top most) (- amount whole) 0f0)
                    (list-state-top state) (max 0 (min most top))))))
        (when (= (ui:ui-focus ui) key)
          (dolist (event (copy-list (ui:ui-keys ui)))
            (let ((k (core:key-event-logical-key event)))
              (cond ((eql k core:+key-down+)
                     (ui:consume-key ui event)
                     (pick (if new (min (1- count) (1+ new)) 0)))
                    ((eql k core:+key-up+)
                     (ui:consume-key ui event)
                     (pick (if new (max 0 (1- new)) 0))))))))
      (setf (list-state-top state) (max 0 (min (list-state-top state) (max 0 (- count rows)))))
      (setf (box:box-data box) (cons items state)
            (box:box-n2 box) (float (or new -1) 1f0)
            (box:box-look box) (look-of ui key enabled))
      (ui:close-box ui)
      (values new (not (eql new selected))))))

;;; --- a canvas the application draws on ---

(defstruct (canvas-state (:constructor make-canvas-state ()))
  (fragment (core:make-display-list :capacity 256))
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
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :canvas :id id :tooltip tooltip :labelled-by labelled-by))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-canvas-state))
         (events '())
         (rect (ui:rect-of ui key))
         (hot (= (ui:ui-hot ui) key)))
    ;; Not in a design surface, where nothing takes the keyboard.
    (when (and autofocus focusable (not (ui:ui-started ui)) (zerop (ui:ui-focus ui)) (zerop (ui:ui-inert ui)))
      (setf (ui:ui-focus ui) key))
    (when cursor
      (unless (member cursor core:+cursors+)
        (error "~s is not a cursor; the cursors are ~{~s~^, ~}." cursor core:+cursors+))
      (ui:want-cursor ui key cursor))
    (setf (box:box-interactive box) t
          (box:box-focusable box) focusable
          (box:box-moves box) t
          (box:box-pref-w box) (float width 1f0)
          (box:box-pref-h box) (float height 1f0)
          (box:box-flex box) (float flex 1f0))
    (when rect
      (let ((x (- (ui:ui-pointer-x ui) (ui:rect-x rect)))
            (y (- (ui:ui-pointer-y ui) (ui:rect-y rect))))
        (when (and hot (ui:ui-pressed ui))
          (ui:consume-press ui)
          (setf (ui:ui-active ui) key)
          (when focusable (ui:focus-by-click ui key))
          (push (list :press x y 0 (ui:ui-click-count ui)) events))
        (when (and hot (ui:consume-secondary-press ui))
          (push (list :press x y 1 1) events))
        (when (and (ui:ui-moved ui) (or hot (= (ui:ui-active ui) key)))
          (setf (ui:ui-moved ui) nil
                (ui:ui-consumed ui) t)
          (push (list (if (and (= (ui:ui-active ui) key) (ui:ui-down ui)) :drag :move) x y) events))
        (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
          (ui:consume-release ui)
          (setf (ui:ui-active ui) 0)
          (push (list :release x y hot) events))
        (when hot
          (multiple-value-bind (dx dy) (ui:consume-wheel ui)
            (when (or (/= dx 0f0) (/= dy 0f0))
              (push (list :wheel dx dy) events))))))
    (when (and (canvas-state-was-hot state) (not hot))
      (setf (ui:ui-consumed ui) t)
      (push (list :exit) events))
    (setf (canvas-state-was-hot state) hot)
    (when (and focusable (= (ui:ui-focus ui) key))
      (dolist (event (copy-list (ui:ui-inputs ui)))
        (typecase event
          (almighty-bolt/internals:key-event
           ;; A key that a widget took is gone from the keys, and is not the canvas's.
           (when (member event (ui:ui-keys ui) :test #'eq)
             (ui:consume-key ui event)
             (ui:consume-input ui event)
             (push (list :key event) events)))
          (almighty-bolt/internals:text-input-event
           (ui:consume-input ui event)
           (push (list :text (almighty-bolt/internals:text-input-event-text event)) events))
          (almighty-bolt/internals:composition-event
           (ui:consume-input ui event)
           (push (list :composition (almighty-bolt/internals:composition-event-kind event)
                       (almighty-bolt/internals:composition-event-text event)
                       (almighty-bolt/internals:composition-event-selection-start event)
                       (almighty-bolt/internals:composition-event-selection-length event))
                 events)))))
    ;; What it draws now, compared with what it drew before.
    (let ((fragment (canvas-state-fragment state)))
      (loop
        (core:clear-display-list fragment)
        (handler-case (progn (core:with-painting ((ui:ui-context ui) id)
                                  (funcall painter fragment (ui:ui-context ui) (float width 1f0)
                                           (float height 1f0)))
                             (return))
          (almighty-bolt/internals:display-list-full ()
            (setf fragment (core:make-display-list
                            :capacity (* 2 (almighty-bolt/internals:display-list-capacity fragment)))
                  (canvas-state-fragment state) fragment))))
      (setf (box:box-data box) fragment
            (box:box-n3 box) (core:display-list-hash fragment)))
    (setf (box:box-look box) (look-of ui key t))
    (ui:close-box ui)
    (nreverse events)))

;;; --- a veil that takes the pointer ---

(defun scrim (&key (id (required-id 'scrim)) tooltip)
  "A translucent layer over everything before it in a stack, as large as it is allowed to be,
that takes the pointer from what is under it. Returns true when it is pressed."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :scrim :id id :tooltip tooltip))
         (key (box:box-key box))
         (pressed nil))
    (setf (box:box-interactive box) t)
    (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
      (ui:consume-press ui)
      (setf pressed t))
    (ui:close-box ui)
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
  (let ((hot (ui:ui-hot ui)))
    (unless (or (zerop hot) (ui:ui-tooltip-dismissed ui) (ui:ui-down ui))
      (let ((box (loop :for index :below (ui:ui-count ui)
                       :for box = (svref (ui:ui-boxes ui) index)
                       :when (= (box:box-key box) hot) :return box)))
        (when (and box (box:box-tooltip box))
          (if (< (- (core:now-ns) (ui:ui-hot-since ui)) +tooltip-delay-ns+)
              (core:request-frame (ui:ui-context ui))
              (multiple-value-bind (float open)
                  (ui:open-float ui :tooltip hot :tooltip
                                 (ui:make-float-spec :anchor box :placement :below))
                (setf (box:box-text float) (box:box-tooltip box))
                (ui:close-float ui open))))))))

;;; --- toasts ---
;;;
;;; A toast is a few words the app shows for a while when something happens (Saved, Sent), at the
;;; bottom of the window in the middle, the newer under the older, and that go by themselves, or when
;;; pressed. They are the ui's, not a widget's: TOAST can be called anywhere in the app's function,
;;; and the ui describes them after it. While one shows, the frames ask for the next, so that it goes
;;; on time; a screen reader reads each out as it comes.

;;; --- shortcuts ---

(defun app-menu (title items)
  "A menu of the app's own in the menu bar, called TITLE, of ITEMS: each a string, :SEPARATOR, or
(string &key key shift option) for one with a shortcut, Command and KEY (a character), with Shift
and Option if they are true. Described in every frame, as a widget is; the menu bar has the menus
of the window that has the keyboard, after the application's menu and before Edit and Window.
Returns the item chosen (its string), on the frame it is chosen by the menu or its shortcut, and
otherwise NIL."
  (let ((ui ui:*ui*))
    (vector-push-extend title (ui:ui-menus ui))
    (vector-push-extend items (ui:ui-menus ui))
    (let ((chosen (ui:ui-menu-chosen ui)))
      (when (and chosen (string= (car chosen) title))
        (setf (ui:ui-menu-chosen ui) nil
              (ui:ui-consumed ui) t)
        (cdr chosen)))))

(defvar *shortcuts-heard* t
  "True where a SHORTCUT described is heard: false in a pane of a dock that is not the active one,
so that two editors side by side do not both undo.")

(defun shortcut (key &rest modifiers)
  "True in the frame KEY (a character, or one of the +KEY-...+ codes) is pressed with exactly
MODIFIERS held: :command, :shift, :option and :control. The key is taken, so nothing described after
this sees it. A widget described before it that has the keyboard takes the keys it uses first (a text
field takes Command-A, -C, -V, -X and -Z). The menu bar's keys (Command-H, -M, -Q and -W) never
reach the app. In a pane of a dock, it is heard only while the pane is the active one: the one last
pressed in, or that has the keyboard."
  (let ((ui ui:*ui*)
        (want (loop :for modifier :in modifiers
                    :sum (ecase modifier
                           (:command core:+modifier-super+)
                           (:shift core:+modifier-shift+)
                           (:option core:+modifier-alt+)
                           (:control core:+modifier-control+))))
        (held (logior core:+modifier-super+ core:+modifier-shift+ core:+modifier-alt+ core:+modifier-control+)))
    (dolist (event (and *shortcuts-heard* (copy-list (ui:ui-keys ui))) nil)
      (let ((pressed (core:key-event-logical-key event)))
        (when (and (eq (core:key-event-kind event) :down)
                   pressed
                   (if (and (characterp key) (alpha-char-p key))
                       (char-equal pressed key)
                       (eql pressed key))
                   (= (logand (core:key-event-modifiers event) held) want))
          (ui:consume-key ui event)
          (return t))))))

(defun toast (words &key (seconds 3))
  "Show WORDS at the bottom of the window for SECONDS. Returns WORDS."
  (let ((ui ui:*ui*))
    (setf (ui:ui-toasts ui)
          (append (ui:ui-toasts ui)
                  (list (cons words (+ (core:now-ns) (round (* seconds 1000000000)))))))
    words))

(defun describe-toasts (ui)
  "Describe the toasts whose time has not come, and forget the others. Called after the app's
function."
  (let ((now (core:now-ns)))
    (setf (ui:ui-toasts ui) (remove-if (lambda (toast) (<= (cdr toast) now)) (ui:ui-toasts ui))))
  (when (ui:ui-toasts ui)
    (core:request-frame (ui:ui-context ui))
    (multiple-value-bind (float open)
        (ui:open-float ui :toasts hash:+seed+ :toasts (ui:make-float-spec :placement :bottom))
      (let ((pressed nil))
        (loop :for toast :in (ui:ui-toasts ui)
              :for i :from 0
              :do (let* ((box (ui:open-box ui :toast :id i))
                         (key (box:box-key box)))
                       (setf (box:box-text box) (car toast)
                             (box:box-interactive box) t)
                       (when (and (= (ui:ui-hot ui) key) (ui:ui-pressed ui))
                           (ui:consume-press ui)
                           (setf pressed toast))
                       (ui:close-box ui)))
        (ui:close-float ui open)
        (when pressed
          (setf (ui:ui-toasts ui) (remove pressed (ui:ui-toasts ui))))
        float))))

;;; --- what decides size and look ---

(defun own-hashes (box)
  "The hash of what decides BOX's size and of what decides how it looks, as two values, not
counting its children."
  (let* ((layout (hash-numbers hash:+seed+ (theme:theme-id (box:box-theme box))
                               (hash:hash-of (box:box-kind box)) (hash:hash-of (box:box-text box))
                               (box:box-size box) (hash:hash-of (box:box-flex box)) (hash:hash-of (box:box-gap box))
                               (hash:hash-of (box:box-main-align box)) (hash:hash-of (box:box-cross-align box))
                               (hash:hash-of (box:box-fill box)) (hash:hash-of (box:box-id box))
                               (hash:hash-of (box:box-interactive box)) (hash:hash-of (box:box-focusable box))
                               (hash:hash-of (box:box-moves box)) (hash:hash-of (box:box-tooltip box))
                               (hash:hash-of (box:box-pad-left box)) (hash:hash-of (box:box-pad-top box))
                               (hash:hash-of (box:box-pad-right box)) (hash:hash-of (box:box-pad-bottom box))
                               (hash:hash-of (box:box-pref-w box)) (hash:hash-of (box:box-pref-h box))
                               (hash:hash-of (box:box-min-w box)) (hash:hash-of (box:box-max-w box))
                               (hash:hash-of (box:box-min-h box)) (hash:hash-of (box:box-max-h box))
                               ;; The font it is drawn in, by its face's hash.
                               (let ((font (box:box-font box)))
                                 (if (theme:face-p font) (theme:face-id font) (hash:hash-of font)))))
         (layout (case (box:box-kind box)
                   (:choice (hash:mix layout (hash-options (car (box:box-data box)))))
                   (:list-box (hash:mix layout (box:box-n3 box)))
                   ;; Its rows, and whether its lines are numbered (which takes room from them).
                   (:text-area (hash:mix (hash:mix layout (box:box-n3 box)) (hash:hash-of (box:box-n2 box))))
                   ;; Turned, or not.
                   (:label (hash:mix layout (hash:hash-of (box:box-n1 box))))
                   ;; Which image it is.
                   (:image (hash:mix layout (core:image-id (box:box-data box))))
                   (:scroll (hash:mix layout (hash:hash-of (box:box-n1 box))))
                   ;; Side by side or one over the other.
                   (:split (hash:mix layout (box:box-n3 box)))
                   (:lazy-list (hash-numbers layout (hash:hash-of (box:box-n1 box)) (hash:hash-of (box:box-n2 box))
                                             (box:box-n3 box)))
                   ;; A row of a lazy list is placed by its index.
                   (:column (hash:mix layout (box:box-n3 box)))
                   (:grid (hash-numbers layout (box:box-n3 box) (hash:hash-of (box:box-n1 box))
                                        (hash:hash-of (car (box:box-data box))) (hash:hash-of (cdr (box:box-data box)))))
                   ;; A float is put where its spec says, by its anchor: which box that is (its
                   ;; place is the screen's), so that a float put by another box (an outline of
                   ;; what is selected, when the selection changes) is laid out again.
                   ((:menu :catcher :tooltip :popover :dialog :toasts :outline)
                    (let* ((spec (box:box-data box))
                           (anchor (ui:float-spec-anchor spec)))
                      (hash-numbers layout (hash:hash-of (ui:float-spec-placement spec))
                                    (hash:hash-of (ui:float-spec-x spec)) (hash:hash-of (ui:float-spec-y spec))
                                    (hash:hash-of (ui:float-spec-min-width spec))
                                    (if anchor (box:box-key anchor) 0))))
                   (t layout)))
         ;; One more than the colour, so that NIL (the theme's) and 0 (transparent) differ.
         (paint (hash-numbers layout (if (box:box-color box) (1+ (box:box-color box)) 0) (box:box-look box)
                              (if (box:box-border-color box) (1+ (box:box-border-color box)) 0)
                              (hash:hash-of (box:box-border-width box)) (shadows-hash (box:box-shadows box))
                              (let ((dash (box:box-border-dash box)))
                                (if dash (hash:mix (hash:hash-of (car dash)) (hash:hash-of (cdr dash))) 0))
                              (hash:hash-of (box:box-n1 box))
                              (hash:hash-of (box:box-n2 box)) (box:box-n3 box)
                              (hash:hash-of (box:box-enabled box)))))
    (setf paint
          (case (box:box-kind box)
            ;; The input method can move the part of what it composes that it has selected without
            ;; changing the text.
            ((:text-field :combo-box) (let ((state (box:box-data box)))
                           (hash-numbers paint
                                         (core:text-state-caret (field-state-state state))
                                         (core:text-state-anchor (field-state-state state))
                                         (field-state-view-start state)
                                         (editable-marked-selection state)
                                         (editable-marked-selection-length state))))
            (:text-area (let* ((area (box:box-data box))
                               (state (area-state-state area)))
                          (hash-numbers (marks-hash paint (area-state-marks area))
                                        (core:text-state-caret state) (core:text-state-anchor state)
                                        (editable-marked-selection area)
                                        (editable-marked-selection-length area))))
            ((:label :paragraph)
             (let ((selection (box:box-data box)))
               (if (selection-state-p selection)
                   (let ((state (selection-state-state selection)))
                     (hash-numbers paint (core:text-state-caret state) (core:text-state-anchor state)))
                   paint)))
            (:list-box (destructuring-bind (items . state) (box:box-data box)
                         (hash-numbers paint (length items) (list-state-top state)
                                       (visible-items-hash items (list-state-top state)
                                                           (box:box-n3 box)))))
            (t paint)))
    (values layout paint)))

(defun marks-hash (hash marks)
  "HASH with the marks of a code view folded in: the lines and their colours."
  (dolist (mark marks hash)
    (setf hash (hash-numbers hash (first mark) (second mark) (third mark)))))

(defun hash-options (options)
  (let ((hash hash:+seed+))
    (dolist (option options hash)
      (setf hash (hash:mix hash (hash:hash-of option))))))

(defun visible-items-hash (items top rows)
  (let ((hash hash:+seed+))
    (loop :for item :in (nthcdr top items)
          :repeat rows
          :do (setf hash (hash:mix hash (hash:hash-of item))))
    hash))
