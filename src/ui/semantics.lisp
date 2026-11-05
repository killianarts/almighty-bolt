(defpackage #:almighty-bolt/ui/semantics
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:widgets #:almighty-bolt/ui/widgets)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:app-pane #:almighty-bolt/ui/app-pane)
                    (#:immediate #:almighty-bolt/ui/immediate))
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
  (ecase (box:box-kind box)
    (:label :static-text)
    (:paragraph :paragraph)
    (:progress :progress-bar)
    ;; An image without a label is decoration, and is not read.
    (:image (if (plusp (length (box:box-text box))) :image :none))
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
    ((:column :row :stack :padding :panel :center :context-area :surface :pane :split :stage)
     :group)
    (:splitter :splitter)
    (:tree :outline)
    (:tree-row :row)
    (:grid :grid)
    (:pressable (if (zerop (box:box-n3 box)) :button :toggle-button))
    (:drag-source :button)))

(defun find-box (ui id)
  "The box of the frame with the id ID, or NIL."
  (loop :for index :below (ui:ui-count ui)
        :do (let ((box (aref (ui:ui-boxes ui) index)))
              (when (and (box:box-id box) (equal (box:box-id box) id))
                (return box)))))

(defun find-labelling-box (ui box id)
  "The box with the id ID nearest BOX: among BOX's parent's children and what is inside them, then
its grandparent's, and so on out. Ids need be unique only within a container, so two panels can each
have a label :title and a field labelled by it; each field is named by the label in its own panel.
The whole frame is searched last, for a label in a float (a dialog's title) or elsewhere."
  (labels ((inside (container skip)
             (loop :for child :across (box:box-children container)
                   :unless (eq child skip)
                     :do (when (equal (box:box-id child) id)
                           (return-from inside child))
                         (let ((found (inside child nil)))
                           (when found (return-from inside found))))
             nil))
    (loop :for from = box :then container
          :for container = (box:box-parent box) :then (box:box-parent container)
          :while container
          :do (when (equal (box:box-id container) id)
                (return-from find-labelling-box container))
              (let ((found (inside container from)))
                (when found (return-from find-labelling-box found))))
    (find-box ui id)))

(defun words-inside (box)
  "The words of the labels, paragraphs and labelled images inside BOX, in order, joined by commas,
or NIL: what a pressable with no name of its own is called, so that one that is only an icon is
called what the icon's label says."
  (let ((words '()))
    (labels ((walk (box)
               (loop :for child :across (box:box-children box)
                     :do (when (and (member (box:box-kind child) '(:label :paragraph :image))
                                    (plusp (length (box:box-text child))))
                           (push (box:box-text child) words))
                         (walk child))))
      (walk box))
    (and words (format nil "~{~a~^, ~}" (nreverse words)))))

(defun name-of (ui box)
  "What the thing is called: its own words, or the words of the widget that its :labelled-by names."
  (case (box:box-kind box)
    (:pressable (or (box:box-data box) (words-inside box)))
    ((:drag-source :tree-row) (words-inside box))
    ((:label :paragraph :image :button :checkbox :switch :menu-item :menu-button :tooltip :dialog
      :toast)
     (box:box-text box))
    (t (let ((label (and (box:box-labelled-by box) (find-labelling-box ui box (box:box-labelled-by box)))))
         (and label (box:box-text label))))))

(defun help-of (box)
  "What BOX says when the pointer rests on it, and for a code view what its marks say, line by line:
what a screen reader gives as its help."
  (let ((notes (and (eq (box:box-kind box) :text-area)
                    (widgets:area-state-numbered (box:box-data box))
                    (loop :for (line nil nil words) :in (widgets:area-state-marks (box:box-data box))
                          :when words
                            :collect (format nil "Line ~d: ~a" line words)))))
    (if notes
        (format nil "~@[~a ~]~{~a.~^ ~}" (box:box-tooltip box) notes)
        (box:box-tooltip box))))

(defun value-of (box)
  (case (box:box-kind box)
    ;; What is typed, without what the input method is still composing (which is shown).
    ((:text-field :combo-box) (core:text-state-string (widgets:field-state-state (box:box-data box))))
    (:text-area (core:text-state-string (widgets:area-state-state (box:box-data box))))
    (:dropdown (box:box-text box))
    (:slider (let ((aux (box:box-aux box)))
               (and aux (aref aux 0))))
    (:progress (float (box:box-n1 box) 1d0))
    ;; How wide (or tall) the pane before it is.
    (:splitter (float (box:box-n1 box) 1d0))
    ((:checkbox :switch) (= (box:box-n3 box) 1))
    (:choice (let ((options (car (box:box-data box))))
               (nth (box:box-n3 box) options)))
    (:list-box (let ((items (car (box:box-data box))) (selected (box:box-n2 box)))
                 (and (>= selected 0) (nth (round selected) items))))
    (t nil)))

(defun state-of (ui box x y hidden)
  (let ((state '()))
    (flet ((note (key value) (setf state (append state (list key value)))))
      (when hidden (note :hidden t))
      (unless (box:box-enabled box) (note :disabled t))
      (when (and (plusp (ui:ui-focus ui)) (= (box:box-key box) (ui:ui-focus ui))) (note :focused t))
      (when (and (plusp (ui:ui-hot ui)) (= (box:box-key box) (ui:ui-hot ui))) (note :hovered t))
      ;; A thing that a secondary press on opens a menu: it is in a context menu's area.
      (when (and (box:box-interactive box)
                 (loop :for parent = (box:box-parent box) :then (box:box-parent parent)
                       :while parent
                         :thereis (eq (box:box-kind parent) :context-area)))
        (note :menu t))
      ;; A cell of a grid says where it is in it, from 0.
      (let ((parent (box:box-parent box)))
        (when (and parent (eq (box:box-kind parent) :grid))
          (let ((index (position box (box:box-children parent)))
                (columns (max 1 (box:box-n3 parent))))
            (note :row (floor index columns))
            (note :column (mod index columns)))))
      (case (box:box-kind box)
        ((:label :paragraph)
         (when (= (box:box-n3 box) 1) (note :live t)))
        (:pressable
         (unless (zerop (box:box-n3 box)) (note :selected (= (box:box-n3 box) 2))))
        ;; How deep it is, from 0, whether it is selected, and whether what is in it is shown.
        (:tree-row
         (note :level (round (box:box-n1 box)))
         (note :selected (= (box:box-n3 box) 1))
         (unless (zerop (box:box-n2 box)) (note :expanded (= (box:box-n2 box) 2))))
        ;; Read out as it comes.
        (:toast (note :announce t))
        ((:text-field :combo-box :text-area)
         (let ((text (if (member (box:box-kind box) '(:text-field :combo-box))
                         (widgets:field-state-state (box:box-data box))
                         (widgets:area-state-state (box:box-data box)))))
           (when (and (not (eq (box:box-kind box) :text-area)) (= (box:box-n3 box) 1)) (note :invalid t))
           (when (eq (box:box-kind box) :text-area)
             (note :offset (widgets:area-state-offset (box:box-data box)))
             ;; A code view's: its lines are numbered, and which are marked, with what each says.
             (let ((area (box:box-data box)))
               (when (widgets:area-state-numbered area)
                 (note :line-numbers t)
                 (when (widgets:area-state-marks area)
                   (note :marks (loop :for (line nil nil words) :in (widgets:area-state-marks area)
                                      :collect (list line words)))))))
           (note :caret (core:text-state-caret text))
           (when (core:text-state-selection-p text)
             (note :selection (list (core:text-state-selection-start text)
                                    (core:text-state-selection-end text))))))
        (:slider
         (let ((aux (box:box-aux box)))
           (when aux
             (note :minimum (aref aux 1))
             (note :maximum (aref aux 2)))))
        ((:checkbox :switch) (note :checked (= (box:box-n3 box) 1)))
        (:menu-button (note :expanded (= (box:box-n3 box) 1)))
        (:dropdown
         (note :expanded (= (box:box-n3 box) 1))
         (note :options (box:box-data box)))
        (:menu-item
         (when (= (box:box-n3 box) 1) (note :highlighted t))
         (when (= (box:box-n2 box) 1f0) (note :checked t)))
        ((:scroll :lazy-list)
         (note :offset (widgets:scroll-state-offset (box:box-data box)))
         (note :content-height (widgets:scroll-state-content-height (box:box-data box)))
         (when (eq (box:box-kind box) :lazy-list)
           ;; How many rows there are, and which of them were built (the ones the tree holds).
           (note :row-count (box:box-n3 box))
           (let ((rows (box:box-children box)))
             (when (plusp (length rows))
               (note :first-row (box:box-n3 (aref rows 0)))))))
        (:choice
         (note :selected (box:box-n3 box))
         (note :options (car (box:box-data box)))
         (if (member (box:box-main-align box) '(:segmented :tabs))
             (progn (note :direction :horizontal) (note :style (box:box-main-align box)))
             (note :direction (box:box-main-align box)))
         ;; Where each option is, so that it can be pressed.
         (let ((state (cdr (box:box-data box))))
           (note :option-bounds
                 (loop :for left :across (widgets:choice-state-lefts state)
                       :for top :across (widgets:choice-state-tops state)
                       :for width :across (widgets:choice-state-widths state)
                       :collect (list (+ x left) (+ y top) width
                                      (widgets:choice-state-option-height state))))))
        (:list-box
         (note :selected (let ((selected (box:box-n2 box)))
                           (and (>= selected 0) (round selected))))
         (note :items (car (box:box-data box)))
         ;; The rows that show: the first one's index, and where each is.
         (let* ((state (cdr (box:box-data box)))
                (top (widgets:list-state-top state))
                (rows (box:box-n3 box))
                (count (length (car (box:box-data box))))
                (row-height (theme:theme-row-height (box:box-theme box))))
           (note :first-visible top)
           (note :item-bounds
                 (loop :for index :from top :below (min count (+ top rows))
                       :for row :from 0
                       :collect (list x (+ y 1 (* row row-height)) (box:box-w box) row-height)))))))
    state))

(defun node-of (ui box x y left top right bottom)
  "The node of BOX, whose parent is at X, Y, inside the part of the window from LEFT, TOP to RIGHT,
BOTTOM that shows (a scroll container's window, or all of it). Its bounds are the part of it that
shows; one that is all out of sight is :hidden, with no width or height."
  (let* ((x (+ x (box:box-x box)))
         (y (+ y (box:box-y box)))
         (shown-left (max x left))
         (shown-top (max y top))
         (shown-w (max 0f0 (- (min (+ x (box:box-w box)) right) shown-left)))
         (shown-h (max 0f0 (- (min (+ y (box:box-h box)) bottom) shown-top)))
         ;; A box that has an area and none of it shows. (One with no area, an empty label, is where
         ;; it is.)
         (hidden (and (plusp (box:box-w box)) (plusp (box:box-h box))
                      (or (zerop shown-w) (zerop shown-h))))
         (id (box:box-id box))
         (name (name-of ui box))
         (value (value-of box))
         (state (state-of ui box x y hidden))
         (children (cond
                         ;; A pressable is one thing to press, named by its words: what is in it is left
                         ;; out, but for what can be pressed or typed into in its own right.
                         ((eq (box:box-kind box) :pressable)
                          (labels ((pressed-inside (box x y)
                                 (loop :for child :across (box:box-children box)
                                       :append (if (box:box-interactive child)
                                                   (list (node-of ui child x y left top right bottom))
                                                   (pressed-inside child (+ x (box:box-x child))
                                                                   (+ y (box:box-y child)))))))
                        (pressed-inside box x y)))
                         ;; An app in a pane: its own tree, where the pane shows it.
                         ((and (eq (box:box-kind box) :canvas) (app-pane:app-pane-inside ui (box:box-key box)))
                          (multiple-value-bind (instance zoom focused) (app-pane:app-pane-inside ui (box:box-key box))
                        (let ((inner (core:accessibility-tree instance)))
                              (and inner
                                   (list (graft inner x y zoom shown-left shown-top
                                                (+ shown-left shown-w) (+ shown-top shown-h)
                                                focused))))))
                         ;; What shows of it: a stage cuts off what is laid out past its edges.
                         ((member (box:box-kind box) '(:scroll :lazy-list :stage))
                          (loop :for child :across (box:box-children box)
                                :collect (node-of ui child x y shown-left shown-top
                                                  (+ shown-left shown-w) (+ shown-top shown-h))))
                         ;; A spacer is room and nothing more, and says nothing to anyone.
                         (t (loop :for child :across (box:box-children box)
                                  :unless (eq (box:box-kind child) :spacer)
                                    :collect (node-of ui child x y left top right bottom))))))
    (append (and id (list :id id))
            (list :role (role-of box))
            (and name (list :name name))
            ;; A check box that is not checked has the value NIL, which is a value.
            (and (or value (member (box:box-kind box) '(:checkbox :switch))) (list :value value))
            (and state (list :state state))
            ;; What it says when the pointer rests on it.
            (let ((help (help-of box)))
              (and help (list :help help)))
            (list :bounds (if hidden
                              (list (min (max x left) right) (min (max y top) bottom) 0f0 0f0)
                              (list shown-left shown-top shown-w shown-h)))
            (and children (list :children children)))))

(defun graft (node px py zoom left top right bottom focused)
  "NODE, of the tree of an app in a pane at PX, PY in the window, at ZOOM, as the window has it: its
bounds and the places in its state in the window's points, and only the part of it inside LEFT, TOP,
RIGHT, BOTTOM (what shows of the pane) shown; one that is all out of sight is :hidden. A thing in it
has the keyboard only if the pane has it (FOCUSED). A slider says how wide its thumb is shown."
  (flet ((place (bounds)
           (destructuring-bind (x y w h) bounds
             (list (+ px (* zoom x)) (+ py (* zoom y)) (* zoom w) (* zoom h)))))
    (destructuring-bind (x y w h) (place (node-bounds node))
      (let* ((shown-left (max x left))
             (shown-top (max y top))
             (shown-w (max 0f0 (- (min (+ x w) right) shown-left)))
             (shown-h (max 0f0 (- (min (+ y h) bottom) shown-top)))
             (hidden (or (node-property node :hidden)
                         (and (plusp w) (plusp h) (or (zerop shown-w) (zerop shown-h)))))
             (state (append
                     (and hidden (list :hidden t))
                     (loop :for (key value) :on (node-state node) :by #'cddr
                           :append (case key
                                     (:hidden '())
                                     (:focused (and focused (list key value)))
                                     ((:option-bounds :item-bounds)
                                      (list key (mapcar #'place value)))
                                     (t (list key value))))
                     (and (eq (node-role node) :slider) (/= zoom 1f0)
                          (list :thumb-width (* zoom widgets:+thumb-size+))))))
        (loop :for (key value) :on node :by #'cddr
              :append (case key
                        (:state '())
                        (:bounds (append (and state (list :state state))
                                         (list :bounds
                                               (if hidden
                                                   (list (min (max x left) right)
                                                         (min (max y top) bottom) 0f0 0f0)
                                                   (list shown-left shown-top shown-w shown-h)))))
                        (:children
                         (list :children
                               (loop :for child :in value
                                     :collect (graft child px py zoom left top right bottom
                                                     focused))))
                        (t (list key value))))))))

(defun shown-frame (ui)
  "What says where things are in the frame the window shows: the hash of what decides its layout,
and its size. (Not the hash of its look: a frame that differs from another only in how something
looks, a button held down, has everything where it was.)"
  (list (ui:ui-last-layout-hash ui) (ui:ui-last-width ui) (ui:ui-last-height ui) (ui:ui-last-scale ui)))

(defmethod core:accessibility-tree ((instance immediate:immediate-app))
  "The semantic tree of the frame the window shows, a node (see above) for the whole window, or NIL
if no frame has been built. While frames fail, the window shows the last that did not (see
SHOW-FAILURE), and the boxes are those of the build that failed, which has no places: the tree is
then the one last asked for, if that was of a frame laid out as the one shown (a state that only
changes how something looks, a toggle that is on, can be a frame behind), and otherwise a window
that says what failed. Kept only as it is asked for, so that a frame allocates nothing for it."
  (let ((ui (immediate:immediate-app-ui instance)))
    (if (immediate:immediate-app-failure instance)
        (if (equal (immediate:immediate-app-tree-frame instance) (shown-frame ui))
            (immediate:immediate-app-tree instance)
            (let ((w (ui:ui-last-width ui)) (h (ui:ui-last-height ui)))
              (list :role :group
                    :bounds (list 0f0 0f0 w h)
                    :children (list (list :role :static-text
                                          :name (format nil "The app failed in this frame: ~a"
                                                        (immediate:immediate-app-failure instance))
                                          :bounds (list 0f0 (- h 36f0) w 36f0))))))
        (let ((tree (frame-tree ui)))
          (setf (immediate:immediate-app-tree instance) tree
                (immediate:immediate-app-tree-frame instance) (shown-frame ui))
          tree))))

(defun frame-tree (ui)
  "The semantic tree of the frame last built in UI."
  (let ((root (ui:ui-root ui)))
    (and root
         (let ((node (node-of ui root 0f0 0f0 -1f30 -1f30 1f30 1f30))
               ;; What floats above the screen (a menu) is in the window too, after the rest; the
               ;; catcher under a menu, which shows nothing, is not, nor the inspector's outlines, which
               ;; are the library's and not the app's.
               (floats (loop :for float :across (ui:ui-floats ui)
                             :unless (or (eq (box:box-kind float) :catcher)
                                         (eq (box:box-kind float) :outline))
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
            (and (or value (member (box:box-kind box) '(:checkbox :switch))) (list :value value))
            (list :state (state-of ui box x y nil)))))
