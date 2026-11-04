(defpackage #:almighty-bolt/ui/semantics
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:accessibility-tree
                #:text-state-string
                #:text-state-caret
                #:text-state-selection-p
                #:text-state-selection-start
                #:text-state-selection-end)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
                #:box-kind
                #:box-children
                #:box-text
                #:box-enabled
                #:box-data
                #:box-n1
                #:box-n2
                #:box-n3
                #:box-aux
                #:box-labelled-by
                #:box-parent
                #:box-interactive
                #:box-tooltip
                #:box-main-align
                #:box-theme
                #:box-x
                #:box-y
                #:box-w
                #:box-h)
  (:import-from #:almighty-bolt/ui/ui
                #:ui-root
                #:ui-last-layout-hash
                #:ui-last-width
                #:ui-last-height
                #:ui-last-scale
                #:ui-floats
                #:ui-boxes
                #:ui-count
                #:ui-focus
                #:ui-hot)
  (:import-from #:almighty-bolt/ui/widgets
                #:field-state-state
                #:area-state-state
                #:area-state-offset
                #:choice-state-lefts
                #:choice-state-widths
                #:choice-state-tops
                #:choice-state-option-height
                #:list-state-top
                #:scroll-state-offset
                #:scroll-state-content-height)
  (:import-from #:almighty-bolt/ui/theme
                #:theme-row-height)
  (:import-from #:almighty-bolt/ui/immediate
                #:immediate-app
                #:immediate-app-ui
                #:immediate-app-failure
                #:immediate-app-tree
                #:immediate-app-tree-frame)
  (:export #:node-id
           #:node-role
           #:node-name
           #:node-value
           #:node-state
           #:node-bounds
           #:node-children
           #:node-help
           #:node-property
           #:find-nodes
           #:walk-nodes
           #:box-facts))

(in-package #:almighty-bolt/ui/semantics)

;;; What the screen means, for a person who cannot see it and for a program that is not one: the
;;; tree of what was last presented, with for every thing its role, its name, its value, its state,
;;; where it is and what is inside it. It is made when it is asked for, from the boxes of the
;;; frame that was last built; a frame does nothing for it. The caller makes sure no frame is being
;;; built (the machine interface asks on the main thread, between frames).
;;;
;;; A node is a property list, so that it can be printed, compared and sent as it is:
;;;   (:id ID :role ROLE :name NAME :value VALUE :state (...) :bounds (X Y WIDTH HEIGHT)
;;;    :children (...))
;;; with ID, NAME, VALUE and the state keys left out when there is nothing to say. The bounds are in
;;; points from the top left of the window.

(defun node-id (node) (getf node :id))
(defun node-role (node) (getf node :role))
(defun node-name (node) (getf node :name))
(defun node-value (node) (getf node :value))
(defun node-state (node) (getf node :state))
(defun node-bounds (node) (getf node :bounds))
(defun node-children (node) (getf node :children))
(defun node-help (node) (getf node :help))
(defun node-property (node key) (getf (getf node :state) key))

(defun role-of (box)
  (ecase (box-kind box)
    (:label :static-text)
    (:paragraph :paragraph)
    (:progress :progress-bar)
    (:button :button)
    (:text-field :text-field)
    (:combo-box :combo-box)
    (:text-area :text-area)
    (:slider :slider)
    (:choice :radio-group)
    (:checkbox :checkbox)
    (:switch :switch)
    (:spinner :busy-indicator)
    (:dropdown :pop-up-button)
    (:menu-button :menu-button)
    (:menu :menu)
    (:menu-item :menu-item)
    (:tooltip :tooltip)
    (:popover :popover)
    (:dialog :dialog)
    (:toasts :group)
    (:toast :toast)
    ((:catcher :spacer :outline) :none)
    (:list-box :list)
    (:canvas :canvas)
    (:scrim :overlay)
    ((:scroll :lazy-list) :scroll-area)
    ((:column :row :stack :padding :panel :center :context-area :surface :pane :split) :group)
    (:splitter :splitter)
    (:grid :grid)
    (:pressable (if (zerop (box-n3 box)) :button :toggle-button))
    (:drag-source :button)))

(defun find-box (ui id)
  "The box of the frame with the id ID, or NIL."
  (loop :for index :below (ui-count ui)
        :do (let ((box (aref (ui-boxes ui) index)))
              (when (and (box-id box) (equal (box-id box) id))
                (return box)))))

(defun find-labelling-box (ui box id)
  "The box with the id ID nearest BOX: among BOX's parent's children and what is inside them, then
its grandparent's, and so on out. Ids need be unique only within a container, so two panels can each
have a label :title and a field labelled by it; each field is named by the label in its own panel.
The whole frame is searched last, for a label in a float (a dialog's title) or elsewhere."
  (labels ((inside (container skip)
             (loop :for child :across (box-children container)
                   :unless (eq child skip)
                     :do (when (equal (box-id child) id)
                           (return-from inside child))
                         (let ((found (inside child nil)))
                           (when found (return-from inside found))))
             nil))
    (loop :for from = box :then container
          :for container = (box-parent box) :then (box-parent container)
          :while container
          :do (when (equal (box-id container) id)
                (return-from find-labelling-box container))
              (let ((found (inside container from)))
                (when found (return-from find-labelling-box found))))
    (find-box ui id)))

(defun words-inside (box)
  "The words of the labels and paragraphs inside BOX, in order, joined by commas, or NIL."
  (let ((words '()))
    (labels ((walk (box)
               (loop :for child :across (box-children box)
                     :do (when (and (member (box-kind child) '(:label :paragraph))
                                    (plusp (length (box-text child))))
                           (push (box-text child) words))
                         (walk child))))
      (walk box))
    (and words (format nil "~{~a~^, ~}" (nreverse words)))))

(defun name-of (ui box)
  "What the thing is called: its own words, or the words of the widget that its :labelled-by names."
  (case (box-kind box)
    (:pressable (or (box-data box) (words-inside box)))
    (:drag-source (words-inside box))
    ((:label :paragraph :button :checkbox :switch :menu-item :menu-button :tooltip :dialog
      :toast)
     (box-text box))
    (t (let ((label (and (box-labelled-by box) (find-labelling-box ui box (box-labelled-by box)))))
         (and label (box-text label))))))

(defun value-of (box)
  (case (box-kind box)
    ((:text-field :combo-box :text-area :dropdown) (box-text box))
    (:slider (let ((aux (box-aux box)))
               (and aux (aref aux 0))))
    (:progress (float (box-n1 box) 1d0))
    ;; How wide (or tall) the pane before it is.
    (:splitter (float (box-n1 box) 1d0))
    ((:checkbox :switch) (= (box-n3 box) 1))
    (:choice (let ((options (car (box-data box))))
               (nth (box-n3 box) options)))
    (:list-box (let ((items (car (box-data box))) (selected (box-n2 box)))
                 (and (>= selected 0) (nth (round selected) items))))
    (t nil)))

(defun state-of (ui box x y hidden)
  (let ((state '()))
    (flet ((note (key value) (setf state (append state (list key value)))))
      (when hidden (note :hidden t))
      (unless (box-enabled box) (note :disabled t))
      (when (and (plusp (ui-focus ui)) (= (box-key box) (ui-focus ui))) (note :focused t))
      (when (and (plusp (ui-hot ui)) (= (box-key box) (ui-hot ui))) (note :hovered t))
      ;; A cell of a grid says where it is in it, from 0.
      (let ((parent (box-parent box)))
        (when (and parent (eq (box-kind parent) :grid))
          (let ((index (position box (box-children parent)))
                (columns (max 1 (box-n3 parent))))
            (note :row (floor index columns))
            (note :column (mod index columns)))))
      (case (box-kind box)
        ((:label :paragraph)
         (when (= (box-n3 box) 1) (note :live t)))
        (:pressable
         (unless (zerop (box-n3 box)) (note :selected (= (box-n3 box) 2))))
        ;; Read out as it comes.
        (:toast (note :announce t))
        ((:text-field :combo-box :text-area)
         (let ((text (if (member (box-kind box) '(:text-field :combo-box))
                         (field-state-state (box-data box))
                         (area-state-state (box-data box)))))
           (when (and (not (eq (box-kind box) :text-area)) (= (box-n3 box) 1)) (note :invalid t))
           (when (eq (box-kind box) :text-area)
             (note :offset (area-state-offset (box-data box))))
           (note :caret (text-state-caret text))
           (when (text-state-selection-p text)
             (note :selection (list (text-state-selection-start text)
                                    (text-state-selection-end text))))))
        (:slider
         (let ((aux (box-aux box)))
           (when aux
             (note :minimum (aref aux 1))
             (note :maximum (aref aux 2)))))
        ((:checkbox :switch) (note :checked (= (box-n3 box) 1)))
        (:menu-button (note :expanded (= (box-n3 box) 1)))
        (:dropdown
         (note :expanded (= (box-n3 box) 1))
         (note :options (box-data box)))
        (:menu-item
         (when (= (box-n3 box) 1) (note :highlighted t))
         (when (= (box-n2 box) 1f0) (note :checked t)))
        ((:scroll :lazy-list)
         (note :offset (scroll-state-offset (box-data box)))
         (note :content-height (scroll-state-content-height (box-data box)))
         (when (eq (box-kind box) :lazy-list)
           ;; How many rows there are, and which of them were built (the ones the tree holds).
           (note :row-count (box-n3 box))
           (let ((rows (box-children box)))
             (when (plusp (length rows))
               (note :first-row (box-n3 (aref rows 0)))))))
        (:choice
         (note :selected (box-n3 box))
         (note :options (car (box-data box)))
         (if (eq (box-main-align box) :segmented)
             (progn (note :direction :horizontal) (note :style :segmented))
             (note :direction (box-main-align box)))
         ;; Where each option is, so that it can be pressed.
         (let ((state (cdr (box-data box))))
           (note :option-bounds
                 (loop :for left :across (choice-state-lefts state)
                       :for top :across (choice-state-tops state)
                       :for width :across (choice-state-widths state)
                       :collect (list (+ x left) (+ y top) width
                                      (choice-state-option-height state))))))
        (:list-box
         (note :selected (let ((selected (box-n2 box)))
                           (and (>= selected 0) (round selected))))
         (note :items (car (box-data box)))
         ;; The rows that show: the first one's index, and where each is.
         (let* ((state (cdr (box-data box)))
                (top (list-state-top state))
                (rows (box-n3 box))
                (count (length (car (box-data box))))
                (row-height (theme-row-height (box-theme box))))
           (note :first-visible top)
           (note :item-bounds
                 (loop :for index :from top :below (min count (+ top rows))
                       :for row :from 0
                       :collect (list x (+ y 1 (* row row-height)) (box-w box) row-height)))))))
    state))

(defun node-of (ui box x y left top right bottom)
  "The node of BOX, whose parent is at X, Y, inside the part of the window from LEFT, TOP to RIGHT,
BOTTOM that shows (a scroll container's window, or all of it). Its bounds are the part of it that
shows; one that is all out of sight is :hidden, with no width or height."
  (let* ((x (+ x (box-x box)))
         (y (+ y (box-y box)))
         (shown-left (max x left))
         (shown-top (max y top))
         (shown-w (max 0f0 (- (min (+ x (box-w box)) right) shown-left)))
         (shown-h (max 0f0 (- (min (+ y (box-h box)) bottom) shown-top)))
         ;; A box that has an area and none of it shows. (One with no area, an empty label, is where
         ;; it is.)
         (hidden (and (plusp (box-w box)) (plusp (box-h box))
                      (or (zerop shown-w) (zerop shown-h))))
         (id (box-id box))
         (name (name-of ui box))
         (value (value-of box))
         (state (state-of ui box x y hidden))
         (children (cond
                     ;; A pressable is one thing to press, named by its words: what is in it is left
                     ;; out, but for what can be pressed or typed into in its own right.
                     ((eq (box-kind box) :pressable)
                      (labels ((pressed-inside (box x y)
                                 (loop :for child :across (box-children box)
                                       :append (if (box-interactive child)
                                                   (list (node-of ui child x y left top right bottom))
                                                   (pressed-inside child (+ x (box-x child))
                                                                   (+ y (box-y child)))))))
                        (pressed-inside box x y)))
                     ((member (box-kind box) '(:scroll :lazy-list))
                      (loop :for child :across (box-children box)
                            :collect (node-of ui child x y shown-left shown-top
                                              (+ shown-left shown-w) (+ shown-top shown-h))))
                     ;; A spacer is room and nothing more, and says nothing to anyone.
                     (t (loop :for child :across (box-children box)
                              :unless (eq (box-kind child) :spacer)
                                :collect (node-of ui child x y left top right bottom))))))
    (append (and id (list :id id))
            (list :role (role-of box))
            (and name (list :name name))
            ;; A check box that is not checked has the value NIL, which is a value.
            (and (or value (member (box-kind box) '(:checkbox :switch))) (list :value value))
            (and state (list :state state))
            ;; What it says when the pointer rests on it.
            (and (box-tooltip box) (list :help (box-tooltip box)))
            (list :bounds (if hidden
                              (list (min (max x left) right) (min (max y top) bottom) 0f0 0f0)
                              (list shown-left shown-top shown-w shown-h)))
            (and children (list :children children)))))

(defun shown-frame (ui)
  "What says where things are in the frame the window shows: the hash of what decides its layout,
and its size. (Not the hash of its look: a frame that differs from another only in how something
looks, a button held down, has everything where it was.)"
  (list (ui-last-layout-hash ui) (ui-last-width ui) (ui-last-height ui) (ui-last-scale ui)))

(defmethod accessibility-tree ((instance immediate-app))
  "The semantic tree of the frame the window shows, a node (see above) for the whole window, or NIL
if no frame has been built. While frames fail, the window shows the last that did not (see
SHOW-FAILURE), and the boxes are those of the build that failed, which has no places: the tree is
then the one last asked for, if that was of a frame laid out as the one shown (a state that only
changes how something looks, a toggle that is on, can be a frame behind), and otherwise a window
that says what failed. Kept only as it is asked for, so that a frame allocates nothing for it."
  (let ((ui (immediate-app-ui instance)))
    (if (immediate-app-failure instance)
        (if (equal (immediate-app-tree-frame instance) (shown-frame ui))
            (immediate-app-tree instance)
            (let ((w (ui-last-width ui)) (h (ui-last-height ui)))
              (list :role :group
                    :bounds (list 0f0 0f0 w h)
                    :children (list (list :role :static-text
                                          :name (format nil "The app failed in this frame: ~a"
                                                        (immediate-app-failure instance))
                                          :bounds (list 0f0 (- h 36f0) w 36f0))))))
        (let ((tree (frame-tree ui)))
          (setf (immediate-app-tree instance) tree
                (immediate-app-tree-frame instance) (shown-frame ui))
          tree))))

(defun frame-tree (ui)
  "The semantic tree of the frame last built in UI."
  (let ((root (ui-root ui)))
    (and root
         (let ((node (node-of ui root 0f0 0f0 -1f30 -1f30 1f30 1f30))
               ;; What floats above the screen (a menu) is in the window too, after the rest; the
               ;; catcher under a menu, which shows nothing, is not, nor the inspector's outlines, which
               ;; are the library's and not the app's.
               (floats (loop :for float :across (ui-floats ui)
                             :unless (or (eq (box-kind float) :catcher)
                                         (eq (box-kind float) :outline))
                               :collect (node-of ui float 0f0 0f0 -1f30 -1f30 1f30 1f30))))
           (if floats
               (append (loop :for (key value) :on node :by #'cddr
                             :unless (eq key :children) :append (list key value))
                       (list :children (append (getf node :children) floats)))
               node)))))

(defun walk-nodes (function node)
  "Call FUNCTION on NODE and, depth first, on every node inside it."
  (funcall function node)
  (dolist (child (node-children node))
    (walk-nodes function child)))

(defun find-nodes (tree &key id role name)
  "The nodes of TREE with the given ID, ROLE and NAME (each only if given), in depth-first order."
  (let ((found '()))
    (walk-nodes (lambda (node)
                  (when (and (or (null id) (equal (node-id node) id))
                             (or (null role) (eq (node-role node) role))
                             (or (null name) (equal (node-name node) name)))
                    (push node found)))
                tree)
    (nreverse found)))

(defun box-facts (ui box x y)
  "What the semantic tree says of BOX, a box of UI's last frame at X, Y in the window, as a property
list (:role, :name, :value, :state): what the inspector shows of a widget it picked."
  (let ((name (name-of ui box))
        (value (value-of box)))
    (append (list :role (role-of box))
            (and name (list :name name))
            (and (or value (member (box-kind box) '(:checkbox :switch))) (list :value value))
            (list :state (state-of ui box x y nil)))))
