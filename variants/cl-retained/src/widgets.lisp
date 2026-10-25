(defpackage #:almighty-bolt-variant-retained/widgets
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-push-text
                #:push-rect
                #:push-rounded-rect
                #:layout-padding
                #:layout-flex
                #:layout-align
                #:constrain
                #:key-event-kind
                #:key-event-logical-key)
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+text-color+
                #:+dim-text-color+
                #:+button-text-color+
                #:+accent-color+
                #:+panel-color+
                #:+track-color+
                #:+thumb-color+
                #:+scrim-color+
                #:+font-name+
                #:+control-radius+
                #:button-color)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node
                #:node-id
                #:node-children
                #:node-flex
                #:node-x
                #:node-y
                #:node-w
                #:node-h
                #:node-enabled
                #:node-hovered
                #:node-pressed
                #:node-focused
                #:adopt
                #:touch
                #:set-size
                #:set-place
                #:layout-node
                #:paint-node
                #:node-interactive-p
                #:node-focusable-p
                #:activate
                #:pointer-press
                #:pointer-release
                #:key-input)
  (:export #:label
           #:label-p
           #:label-text
           #:label-color
           #:label-size
           #:button
           #:button-p
           #:button-label
           #:button-text
           #:button-state
           #:button-focused
           #:button-on-click
           #:padding
           #:panel
           #:center
           #:stack
           #:scrim
           #:progress
           #:progress-fraction
           #:row
           #:column
           #:paint-children
           #:split-arguments))

(in-package #:almighty-bolt-variant-retained/widgets)

;;; The first widgets of the retained variant: text, a button, space around a child,
;;; and a row and a column. Each is a structure that includes NODE, and the layout
;;; and painting are generic functions specialized on them. A tree is built once
;;; with these constructors and then changed with setters, which mark the tree and
;;; leave the work to the next frame.

;;; --- constructors take their children first and their options after ---

(defun split-arguments (arguments)
  "The nodes at the front of ARGUMENTS, and the keyword options after them, as two
values. Nodes are never keywords, so the first keyword starts the options."
  (let ((split (or (position-if #'keywordp arguments) (length arguments))))
    (values (subseq arguments 0 split) (nthcdr split arguments))))

(defun float-or-nil (number)
  (and number (float number 1f0)))

;;; --- a line of text ---

(defstruct (label (:include node) (:constructor %make-label))
  (%text "" :type string)
  ;; Read only: the font and the metrics are found from it and kept, and nothing
  ;; would find them again if it changed.
  (size 15 :type real :read-only t)
  (%color +text-color+)
  ;; The font and the metrics of the text in it, found the first time they are
  ;; needed and kept until the text or the size changes.
  (font nil)
  (measured nil :type boolean)
  (text-width 0f0 :type single-float)
  (ascent 0f0 :type single-float)
  (descent 0f0 :type single-float))

(defun label (text &key id (size 15) (color +text-color+) (flex 0) (cacheable t))
  "A line of TEXT, SIZE points high. A CACHEABLE label keeps its glyph commands."
  (%make-label :%text text :id id :size size :%color color :flex (float flex 1f0)
               :cacheable cacheable))

(defun label-text (label)
  (label-%text label))

(defun (setf label-text) (text label)
  (setf (label-%text label) text
        (label-measured label) nil)
  (touch label t)
  text)

(defun label-color (label)
  (label-%color label))

(defun (setf label-color) (color label)
  (setf (label-%color label) color)
  (touch label nil)
  color)

(defun measure-label (label context)
  "Make sure LABEL has its font and the size of its text."
  (unless (label-measured label)
    (unless (label-font label)
      (setf (label-font label) (context-font context +font-name+ (label-size label))))
    (multiple-value-bind (width ascent descent)
        (context-measure context (label-font label) (label-%text label))
      (setf (label-text-width label) (float width 1f0)
            (label-ascent label) (float ascent 1f0)
            (label-descent label) (float descent 1f0)
            (label-measured label) t))))

(defmethod layout-node ((label label) context min-w max-w min-h max-h)
  (measure-label label context)
  (set-size label
            (constrain (label-text-width label) min-w max-w)
            (constrain (+ (label-ascent label) (label-descent label)) min-h max-h))
  (values (node-w label) (node-h label)))

(defmethod paint-node ((label label) context list x y)
  (measure-label label context)
  (context-push-text context list (label-font label) (label-%text label)
                     x (+ y (label-ascent label)) (label-%color label))
  nil)

;;; --- space around a child ---

(defstruct (padding (:include node) (:constructor %make-padding))
  (left 0f0 :type single-float)
  (top 0f0 :type single-float)
  (right 0f0 :type single-float)
  (bottom 0f0 :type single-float))

(defun padding (child &key id all left top right bottom (flex 0))
  "CHILD with space around it: ALL on every side, or LEFT, TOP, RIGHT and BOTTOM."
  (adopt (%make-padding :id id :flex (float flex 1f0)
                        :left (float (or left all 0) 1f0)
                        :top (float (or top all 0) 1f0)
                        :right (float (or right all 0) 1f0)
                        :bottom (float (or bottom all 0) 1f0))
         (list child)))

(defmethod layout-node ((padding padding) context min-w max-w min-h max-h)
  (let ((child (svref (node-children padding) 0)))
    (flet ((measure (a b c d)
             (layout-node child context a b c d)))
      (declare (dynamic-extent #'measure))
      (multiple-value-bind (w h x y)
          (layout-padding min-w max-w min-h max-h #'measure
                          :left (padding-left padding) :top (padding-top padding)
                          :right (padding-right padding) :bottom (padding-bottom padding))
        (set-place child x y)
        (set-size padding w h)
        (values w h)))))

(defun paint-children (node context list x y)
  (loop :for child :across (node-children node)
        :do (paint-node child context list (+ x (node-x child)) (+ y (node-y child)))))

(defmethod paint-node ((padding padding) context list x y)
  (paint-children padding context list x y))

;;; --- a row or a column ---

(defstruct (flex-box (:include node) (:constructor %make-flex-box))
  (axis :horizontal :type (member :horizontal :vertical))
  (gap 0f0 :type single-float)
  (main-align :start :type keyword)
  (cross-align :start :type keyword)
  (fill nil))

(defun make-flex-box (axis arguments)
  (multiple-value-bind (children options) (split-arguments arguments)
    (destructuring-bind (&key id (gap 0) (main-align :start) (cross-align :start) fill
                           (flex 0))
        options
      (adopt (%make-flex-box :axis axis :id id :gap (float gap 1f0)
                             :main-align main-align :cross-align cross-align
                             :fill fill :flex (float flex 1f0))
             children))))

(defun row (&rest arguments)
  "Children side by side, followed by options: :GAP between them, :MAIN-ALIGN and
:CROSS-ALIGN as for LAYOUT-FLEX, :FILL to take all the width, :ID and :FLEX."
  (make-flex-box :horizontal arguments))

(defun column (&rest arguments)
  "Children one under another; the options are those of ROW."
  (make-flex-box :vertical arguments))

(defmethod layout-node ((box flex-box) context min-w max-w min-h max-h)
  (let ((children (node-children box)))
    (flet ((measure (i a b c d)
             (layout-node (svref children i) context a b c d))
           (place (i x y)
             (set-place (svref children i) x y))
           (flex-of (i)
             (values (node-flex (svref children i)) t)))
      (declare (dynamic-extent #'measure #'place #'flex-of))
      (multiple-value-bind (w h)
          (layout-flex (flex-box-axis box) min-w max-w min-h max-h (length children)
                       #'measure #'place
                       :flex-of #'flex-of
                       :gap (flex-box-gap box)
                       :main-align (flex-box-main-align box)
                       :cross-align (flex-box-cross-align box)
                       :main-size (if (flex-box-fill box) :max :min))
        (set-size box w h)
        (values w h)))))

(defmethod paint-node ((box flex-box) context list x y)
  (paint-children box context list x y))

;;; --- a button ---

(defconstant +button-padding-x+ 14f0)
(defconstant +button-padding-y+ 7f0)

(defstruct (button (:include node) (:constructor %make-button))
  (label nil)
  (on-click nil :type (or null function)))

(defun button (text &key id on-click (flex 0))
  "A button that calls ON-CLICK, with no arguments, when it is clicked."
  ;; The button keeps the commands of its label with its own, so the label does not.
  (let ((label (label text :color +button-text-color+ :cacheable nil)))
    (adopt (%make-button :id id :label label :on-click on-click
                         :flex (float flex 1f0) :cacheable t)
           (list label))))

(defun button-text (button)
  (label-text (button-label button)))

(defun (setf button-text) (text button)
  (setf (label-text (button-label button)) text))

(defun button-state (button)
  "How the button looks: :DISABLED, :PRESSED (pressed and the pointer over it), :HOVER or
:NORMAL."
  (cond ((not (node-enabled button)) :disabled)
        ((and (node-pressed button) (node-hovered button)) :pressed)
        ((node-hovered button) :hover)
        (t :normal)))

(defmethod node-interactive-p ((button button))
  t)

(defmethod node-focusable-p ((button button))
  t)

(defmethod activate ((button button))
  (when (button-on-click button)
    (funcall (button-on-click button)))
  (touch button nil))

(defmethod pointer-release ((button button) session x y inside-p)
  (declare (ignore session x y))
  (when inside-p
    (activate button)))

(defmethod key-input ((button button) session event)
  (declare (ignore session))
  (when (and (eq (key-event-kind event) :down)
             (member (key-event-logical-key event) '(#\Space #\Return)))
    (activate button)
    t))

(defmethod layout-node ((button button) context min-w max-w min-h max-h)
  (let ((label (button-label button)))
    (flet ((measure (a b c d)
             (layout-node label context a b c d)))
      (declare (dynamic-extent #'measure))
      (multiple-value-bind (w h x y)
          (layout-padding min-w max-w min-h max-h #'measure
                          :left +button-padding-x+ :top +button-padding-y+
                          :right +button-padding-x+ :bottom +button-padding-y+)
        (set-place label x y)
        (set-size button w h)
        (values w h)))))

(defmethod paint-node ((button button) context list x y)
  (let ((w (node-w button))
        (h (node-h button)))
    (when (node-focused button)
      (push-rounded-rect list (- x 2) (- y 2) (+ w 4) (+ h 4) (+ +control-radius+ 2)
                         +accent-color+))
    (push-rounded-rect list x y w h +control-radius+ (button-color (button-state button)))
    (paint-children button context list x y)))

;;; --- a panel: padding with a background ---

(defstruct (panel (:include padding) (:constructor %make-panel))
  (color +panel-color+)
  (radius 8 :type real))

(defun panel (child &key id all left top right bottom (color +panel-color+) (radius 8)
                      (flex 0))
  "CHILD on a rounded background of COLOR, with space round it as for PADDING."
  (adopt (%make-panel :id id :flex (float flex 1f0) :color color :radius radius
                      :left (float (or left all 0) 1f0)
                      :top (float (or top all 0) 1f0)
                      :right (float (or right all 0) 1f0)
                      :bottom (float (or bottom all 0) 1f0))
         (list child)))

(defmethod paint-node ((panel panel) context list x y)
  (push-rounded-rect list x y (node-w panel) (node-h panel) (panel-radius panel)
                     (panel-color panel))
  (paint-children panel context list x y))

;;; --- centring a child in all the room there is ---

(defstruct (center (:include node) (:constructor %make-center)))

(defun center (child &key id (flex 0))
  (adopt (%make-center :id id :flex (float flex 1f0)) (list child)))

(defmethod layout-node ((center center) context min-w max-w min-h max-h)
  (let ((child (svref (node-children center) 0)))
    (flet ((measure (a b c d)
             (layout-node child context a b c d)))
      (declare (dynamic-extent #'measure))
      (multiple-value-bind (w h x y) (layout-align min-w max-w min-h max-h #'measure)
        (set-place child x y)
        (set-size center w h)
        (values w h)))))

(defmethod paint-node ((center center) context list x y)
  (paint-children center context list x y))

;;; --- children on top of each other ---

(defstruct (stack (:include node) (:constructor %make-stack)))

(defun stack (&rest arguments)
  "Children drawn one over another, the later on top, followed by the options :ID and
:FLEX. The stack is as large as its largest child, and every child is at its corner."
  (multiple-value-bind (children options) (split-arguments arguments)
    (destructuring-bind (&key id (flex 0)) options
      (adopt (%make-stack :id id :flex (float flex 1f0)) children))))

(defmethod layout-node ((stack stack) context min-w max-w min-h max-h)
  (let ((width 0f0)
        (height 0f0))
    (declare (type single-float width height))
    (loop :for child :across (node-children stack)
          :do (multiple-value-bind (w h) (layout-node child context 0f0 max-w 0f0 max-h)
                (declare (type single-float w h))
                (set-place child 0f0 0f0)
                (setf width (max width w)
                      height (max height h))))
    (multiple-value-bind (w h) (values (constrain width min-w max-w) (constrain height min-h max-h))
      (set-size stack w h)
      (values w h))))

(defmethod paint-node ((stack stack) context list x y)
  (paint-children stack context list x y))

;;; --- a veil that takes the pointer, to put under a dialog ---

(defstruct (scrim (:include node) (:constructor %make-scrim))
  (on-press nil :type (or null function)))

(defun scrim (&key id on-press)
  "A translucent layer over everything under it in a stack, as large as it is allowed
to be, that takes the pointer so that nothing under it does. ON-PRESS is called with no
arguments when it is pressed."
  (%make-scrim :id id :on-press on-press))

(defmethod node-interactive-p ((scrim scrim))
  t)

(defmethod layout-node ((scrim scrim) context min-w max-w min-h max-h)
  (declare (ignore context))
  (let ((w (if (< max-w most-positive-single-float) max-w min-w))
        (h (if (< max-h most-positive-single-float) max-h min-h)))
    (set-size scrim w h)
    (values w h)))

(defmethod paint-node ((scrim scrim) context list x y)
  (declare (ignore context))
  (push-rect list x y (node-w scrim) (node-h scrim) +scrim-color+))

(defmethod pointer-press ((scrim scrim) session x y button)
  (declare (ignore session x y button))
  (when (scrim-on-press scrim)
    (funcall (scrim-on-press scrim)))
  t)

;;; --- a gauge ---

(defstruct (progress (:include node) (:constructor %make-progress))
  (%fraction 0d0 :type double-float)
  (width 200f0 :type single-float))

(defun progress (&key id (fraction 0) (width 200) (flex 0))
  "A bar that is FRACTION (0 to 1) full."
  (%make-progress :id id :%fraction (float fraction 1d0) :width (float width 1f0)
                  :flex (float flex 1f0) :cacheable t))

(defun progress-fraction (progress)
  (progress-%fraction progress))

(defun (setf progress-fraction) (fraction progress)
  (let ((fraction (float (max 0 (min 1 fraction)) 1d0)))
    (unless (= fraction (progress-%fraction progress))
      (setf (progress-%fraction progress) fraction)
      (touch progress nil))
    fraction))

(defmethod layout-node ((progress progress) context min-w max-w min-h max-h)
  (declare (ignore context))
  (set-size progress (constrain (progress-width progress) min-w max-w) (constrain 14f0 min-h max-h))
  (values (node-w progress) (node-h progress)))

(defmethod paint-node ((progress progress) context list x y)
  (declare (ignore context))
  (let* ((w (node-w progress))
         (h (node-h progress))
         (filled (* w (float (progress-%fraction progress) 1f0))))
    (push-rounded-rect list x y w h (* 0.5f0 h) +track-color+)
    (when (> filled 0f0)
      (push-rounded-rect list x y (max filled h) h (* 0.5f0 h) +thumb-color+))))
