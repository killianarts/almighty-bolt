(defpackage #:almighty-bolt/ui/inspector
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/hash
                #:+seed+)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-id
                #:box-kind
                #:box-text
                #:box-theme
                #:box-enabled
                #:box-color
                #:box-data
                #:box-children
                #:box-pref-w
                #:box-pref-h
                #:box-pad-left
                #:box-pad-top
                #:box-pad-right
                #:box-pad-bottom
                #:box-n1
                #:box-x
                #:box-y
                #:box-w
                #:box-h)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:ui-root
                #:ui-floats
                #:ui-inspector
                #:ui-inspector-keyboard
                #:ui-focus
                #:ui-hits
                #:ui-hit-count
                #:hit-key
                #:hit-layer
                #:+inspector-layer+
                #:ui-pressed
                #:ui-pointer-x
                #:ui-pointer-y
                #:consume-press
                #:make-float-spec
                #:float-spec-inspector
                #:open-float
                #:close-float)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:theme-name
                #:theme-panel
                #:theme-ring)
  (:import-from #:almighty-bolt/ui/widgets
                #:with-theme
                #:column
                #:scroll
                #:label
                #:paragraph
                #:button
                #:text-field
                #:row
                #:take-shortcut)
  (:export #:+inspector-width+
           #:inspector-open-p
           #:inspector-take-input
           #:describe-inspector
           #:inspector-float-p
           #:*box-facts*
           #:*inspector-runner*
           #:run-inspector-actions
           #:inspector-say))

(in-package #:almighty-bolt/ui/inspector)

;;; The inspector: Command-Option-I in any app turns it on and off. It is a panel down the right of
;;; the window, which the app is laid out beside, and its Pick button makes the next press on the
;;; app pick the widget under the pointer instead of pressing it; the panel says what the picked one
;;; is, and an outline shows where. It is the library's, not the app's: it is taken before the app's
;;; function is called (its key, and the press that picks), described after everything else (above
;;; a dialog, which would otherwise keep the pointer and the keyboard from it), drawn in the theme
;;; the window had when it was opened (so that editing that theme does not change the inspector),
;;; and kept out of the semantic tree, so that an agent or a screen reader sees the app alone.
;;;
;;; What it says of a widget is from the last frame that was laid out: the boxes are still those of
;;; that frame when the input of a new one is taken, and they are where they were drawn. The pick is
;;; kept by the box's key, which is the same from frame to frame while the widget is described in
;;; the same place, and so survives the app being made again by new code.
;;;
;;; It changes a widget as a person would, by the machine interface's actions (a click, SET-VALUE),
;;; whose events go to the app as a hand's do: the app stays in charge of what it holds. An action
;;; cannot be done inside the frame that asks for it (some wait for frames between their steps: a
;;; dropdown's menu opens before its row is pressed), so the panel notes it, as data, and after the
;;; frame hands what it noted to *INSPECTOR-RUNNER*, which the machine interface sets; in a window it
;;; runs them on a thread of its own, through a session on the window, as an agent's would be.

(defconstant +inspector-width+ 300f0 "How wide the inspector's panel is.")

(defvar *box-facts* nil
  "A function of the ui, a box and where the box is in the window, that returns what the semantic
tree says of the box as a property list (:role, :name, :value, :state); set by the semantics, which
are loaded after the inspector.")

(defvar *inspector-runner* nil
  "A function of the variant and a list of actions that does them after the frame, or NIL. An action
is a property list: :op (:click or :set-value), :value for :set-value, and :id, :role and :name, which
say which widget, as the machine interface's FIND-NODE takes them (the inspector gives the id).")

(defstruct (inspector (:constructor make-inspector ()))
  (open nil)
  ;; True while the next press on the app picks.
  (picking nil)
  ;; The key of the box picked, or 0.
  (picked 0 :type fixnum)
  ;; The theme the window had when the inspector was opened, which it is drawn in.
  (theme nil)
  ;; Where the box under the pointer is while picking, and where the picked one is, in the window,
  ;; as lists of x, y, width and height (NIL if there is none); and what is known of the picked one.
  (hover nil)
  (shown nil)
  (facts nil)
  ;; What the panel's field holds, and the key of the box it was filled for; the actions noted and not
  ;; yet handed on; and what went wrong with the last of them, if anything.
  (draft "" :type string)
  (draft-for 0 :type fixnum)
  (pending '())
  (message nil))

(defun inspector-open-p (ui)
  (let ((inspector (ui-inspector ui)))
    (and inspector (inspector-open inspector))))

(defun inspector-float-p (box)
  "True if BOX, a float, is the inspector's."
  (float-spec-inspector (box-data box)))

;;; --- finding boxes in the frame before ---

(defun walk-frame (ui function)
  "Call FUNCTION with each box of the last frame laid out, and where it is in the window (x and y),
the part of the window it shows in (left, top, right and bottom: a scroll container's window, or
all of it), in the order they were described: the screen, then the app's floats."
  (labels ((walk (box x y left top right bottom)
             (let ((x (+ x (box-x box))) (y (+ y (box-y box))))
               (funcall function box x y left top right bottom)
               (if (member (box-kind box) '(:scroll :lazy-list))
                   (let ((left (max x left)) (top (max y top))
                         (right (min (+ x (box-w box)) right))
                         (bottom (min (+ y (box-h box)) bottom)))
                     (loop :for child :across (box-children box)
                           :do (walk child x y left top right bottom)))
                   (loop :for child :across (box-children box)
                         :do (walk child x y left top right bottom))))))
    (when (ui-root ui)
      (walk (ui-root ui) 0f0 0f0 -1f30 -1f30 1f30 1f30))
    (loop :for float :across (ui-floats ui)
          :unless (inspector-float-p float)
            :do (walk float 0f0 0f0 -1f30 -1f30 1f30 1f30))))

(defun box-at (ui px py)
  "The box of the last frame that is under the point PX, PY and shows there, the last described (so
the innermost, and a float before the screen under it), and where it is: three values, or NIL."
  (let ((found nil) (found-x 0f0) (found-y 0f0))
    (walk-frame ui (lambda (box x y left top right bottom)
                     (when (and (<= (max x left) px) (< px (min (+ x (box-w box)) right))
                                (<= (max y top) py) (< py (min (+ y (box-h box)) bottom)))
                       (setf found box found-x x found-y y))))
    (values found found-x found-y)))

(defun box-by-key (ui key)
  "The box of the last frame with KEY, and where it is: three values, or NIL."
  (walk-frame ui (lambda (box x y left top right bottom)
                   (declare (ignore left top right bottom))
                   (when (= (box-key box) key)
                     (return-from box-by-key (values box x y)))))
  nil)

(defun facts-of (ui box x y)
  "What the panel says of BOX, which is at X, Y in the window, as a property list."
  (append (list :kind (box-kind box) :id (box-id box) :enabled (box-enabled box)
                :theme (theme-name (box-theme box)) :x x :y y :w (box-w box) :h (box-h box))
          (and *box-facts* (funcall *box-facts* ui box x y))))

;;; --- what it takes before the app ---

(defun inspector-take-input (ui window-theme width)
  "Take from the frame's input, before the app's function, what is the inspector's: Command-Option-I,
which opens and closes it, Escape while it picks, and the press that picks; then note what it is to
show of the frame before, whose boxes the ui still has. WINDOW-THEME is the theme of the window, and
WIDTH how wide it is. Called on the first build of a frame."
  (let ((*ui* ui))
    (when (take-shortcut #\i :command :option)
      (let ((inspector (or (ui-inspector ui) (setf (ui-inspector ui) (make-inspector)))))
        (setf (inspector-open inspector) (not (inspector-open inspector))
              (inspector-picking inspector) nil
              (inspector-theme inspector) window-theme)))
    (let ((inspector (ui-inspector ui)))
      (when (and inspector (inspector-open inspector))
        (when (and (inspector-picking inspector) (take-shortcut #\Escape))
          (setf (inspector-picking inspector) nil))
        (setf (inspector-hover inspector) nil)
        (when (inspector-picking inspector)
          (let* ((px (ui-pointer-x ui)) (py (ui-pointer-y ui))
                 (on-app (and (<= 0f0 px) (< px (- width +inspector-width+)))))
            (when on-app
              (multiple-value-bind (box x y) (box-at ui px py)
                (when box
                  (setf (inspector-hover inspector) (list x y (box-w box) (box-h box)))
                  ;; The press picks, and the app does not have it.
                  (when (ui-pressed ui)
                    (consume-press ui)
                    (setf (inspector-picked inspector) (box-key box)
                          (inspector-picking inspector) nil
                          (inspector-hover inspector) nil)))))))
        (multiple-value-bind (box x y) (if (zerop (inspector-picked inspector))
                                           nil
                                           (box-by-key ui (inspector-picked inspector)))
          (setf (inspector-shown inspector) (and box (list x y (box-w box) (box-h box)))
                (inspector-facts inspector) (and box (facts-of ui box x y))))
        ;; A new pick fills the field with its value, and forgets what went wrong before.
        (unless (= (inspector-draft-for inspector) (inspector-picked inspector))
          (setf (inspector-draft-for inspector) (inspector-picked inspector)
                (inspector-draft inspector) (draft-of (getf (inspector-facts inspector) :value))
                (inspector-message inspector) nil))))
    ;; While the keyboard is in the panel, the app's shortcuts leave the keys to it.
    (setf (ui-inspector-keyboard ui)
          (and (inspector-open-p ui)
               (loop :for i :below (ui-hit-count ui)
                     :thereis (let ((hit (aref (ui-hits ui) i)))
                                (and (= (hit-key hit) (ui-focus ui))
                                     (= (hit-layer hit) +inspector-layer+))))))))

(defun draft-of (value)
  "VALUE as the panel's field shows it to be changed: a number as a person writes it."
  (typecase value
    (string value)
    (real (format nil "~f" value))
    (t "")))

(defun number-in (string)
  "The number STRING is, or NIL."
  (let ((value (let ((*read-eval* nil))
                 (ignore-errors (read-from-string string)))))
    (and (realp value) value)))

;;; --- what it changes ---

(defun note-action (inspector op &rest value)
  "Note that the picked widget is to have OP done to it (with VALUE, if given), after the frame. It
is found again by its id, which every widget that can be changed has."
  (setf (inspector-message inspector) nil
        (inspector-pending inspector)
        (append (inspector-pending inspector)
                (list (append (list :op op) (and value (list :value (first value)))
                              (list :id (getf (inspector-facts inspector) :id)))))))

(defun run-inspector-actions (ui variant)
  "Hand what the panel noted in this frame to *INSPECTOR-RUNNER*. Called after the frame."
  (let ((inspector (ui-inspector ui)))
    (when (and inspector (inspector-pending inspector))
      (let ((actions (inspector-pending inspector)))
        (setf (inspector-pending inspector) '())
        (if *inspector-runner*
            (funcall *inspector-runner* variant actions)
            (setf (inspector-message inspector) "Nothing here does what the inspector asks."))))))

(defun inspector-say (ui message)
  "Show MESSAGE in the panel, as what happened to the last thing it asked for."
  (let ((inspector (ui-inspector ui)))
    (when inspector
      (setf (inspector-message inspector) message))))

(defun describe-changes (inspector)
  "The panel's controls for changing the picked widget, by its role."
  (let* ((facts (inspector-facts inspector))
         (role (getf facts :role))
         (state (getf facts :state)))
    (when (member role '(:button :menu-button :checkbox :switch :pop-up-button))
      (when (button "Press" :id :inspector-press)
        (note-action inspector :click)))
    (when (member role '(:text-field :combo-box :text-area :slider))
      (row (:id :inspector-set-row :gap 8)
        (setf (inspector-draft inspector)
              (text-field (inspector-draft inspector) :id :inspector-draft :width 180))
        (when (button "Set" :id :inspector-set)
          (if (eq role :slider)
              (let ((number (number-in (inspector-draft inspector))))
                (if number
                    (note-action inspector :set-value number)
                    (setf (inspector-message inspector)
                          (format nil "~s is not a number." (inspector-draft inspector)))))
              (note-action inspector :set-value (inspector-draft inspector))))))
    ;; A button for each of the options of a choice or a dropdown, or the items of a list.
    (let ((options (or (getf state :options) (getf state :items))))
      (when (member role '(:radio-group :pop-up-button :list))
        (column (:id :inspector-options :gap 4)
          (loop :for option :in options
                :for i :from 0
                :do (when (button (format nil "Choose ~a" option) :id i)
                      ;; A dropdown is set by the words of its row, a choice or a list by the index.
                      (note-action inspector :set-value (if (eq role :pop-up-button) option i)))))))
    (when (inspector-message inspector)
      (paragraph (inspector-message inspector) :id :inspector-message))))

;;; --- what it describes after the app ---

(defun outline (ui id rect color)
  "A frame of COLOR round RECT (x, y, width, height in the window)."
  (when rect
    (destructuring-bind (x y w h) rect
      (multiple-value-bind (float open)
          (open-float ui :outline +seed+ id
                      (make-float-spec :placement :at :x (float x 1f0) :y (float y 1f0)
                                       :inspector t))
        (setf (box-pref-w float) (float w 1f0)
              (box-pref-h float) (float h 1f0)
              (box-color float) color)
        (close-float ui open)))))

(defun shown (value)
  "VALUE as Lisp writes it, symbols in lower case."
  (let ((*print-case* :downcase))
    (prin1-to-string value)))

(defun fact-lines (facts)
  "The lines the panel shows for FACTS."
  (destructuring-bind (&key kind id enabled theme x y w h role name (value nil value-p) state
                       &allow-other-keys)
      facts
    (append (list (format nil "Kind: ~(~a~)" kind))
            (and role (list (format nil "Role: ~(~a~)" role)))
            (list (if id (format nil "Id: ~a" (shown id)) "Id: none"))
            (and name (list (format nil "Name: ~a" name)))
            (and value-p (list (format nil "Value: ~a" (shown value))))
            (list (format nil "At: ~d, ~d" (round x) (round y))
                  (format nil "Size: ~d by ~d" (round w) (round h))
                  (if theme (format nil "Theme: ~a" (shown theme)) "Theme: one with no name")
                  (if enabled "Enabled" "Not enabled"))
            ;; Not whether the pointer is over it or it has the keyboard, which the inspector's own
            ;; pointer and keyboard change, and which would move what is under them.
            (loop :for (key value) :on state :by #'cddr
                  :unless (member key '(:hovered :focused))
                    :collect (format nil "~(~a~): ~a" key (shown value))))))

(defun describe-inspector (ui)
  "Describe the inspector, if it is open: the outlines and the panel, above everything else. Called
after the app's function, the toasts and the tooltip."
  (let ((inspector (ui-inspector ui)))
    (when (and inspector (inspector-open inspector))
      (with-theme ((inspector-theme inspector))
        (let ((color (theme-ring *theme*)))
          (outline ui :inspector-hover (inspector-hover inspector) color)
          (outline ui :inspector-picked (inspector-shown inspector) color))
        (multiple-value-bind (float open)
            (open-float ui :inspector +seed+ :inspector
                        (make-float-spec :placement :side :min-width +inspector-width+
                                         :inspector t))
          (setf (box-pad-left float) 16f0 (box-pad-top float) 16f0
                (box-pad-right float) 16f0 (box-pad-bottom float) 16f0
                (box-color float) (theme-panel *theme*)
                (box-n1 float) 0f0)
          ;; Taller than the window, it scrolls.
          (scroll (:id :inspector-scroll)
          (column (:id :inspector-column :gap 10)
            (label "Inspector" :id :inspector-title :size 18)
            (when (button (if (inspector-picking inspector) "Stop picking" "Pick")
                          :id :inspector-pick)
              (setf (inspector-picking inspector) (not (inspector-picking inspector))))
            (cond ((inspector-picking inspector)
                   (paragraph "Press a widget to pick it. Escape stops." :id :inspector-hint))
                  ((zerop (inspector-picked inspector))
                   (paragraph "Press Pick, then a widget." :id :inspector-hint))
                  ((null (inspector-facts inspector))
                   (paragraph "What was picked is not on the screen now." :id :inspector-hint))
                  (t
                   (column (:id :inspector-facts :gap 4)
                     (loop :for line :in (fact-lines (inspector-facts inspector))
                           :for i :from 0
                           :do (paragraph line :id i)))
                   (describe-changes inspector)))))
          (close-float ui open))))))
