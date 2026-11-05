(defpackage #:almighty-bolt-examples/gallery
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:export #:make-gallery))

(in-package #:almighty-bolt-examples/gallery)

;;; Every widget and container of the library on one page, each with what it returns shown under
;;; it and how it is called (its arguments and options, as its lambda list has them), for trying
;;; them out and for showing them. It uses the public package only. A widget that is added to the
;;; library is added here too, with its call: the tests check that the gallery has one of each, and
;;; that each call it shows is the one the library has.
;;;
;;;   (bolt:open-app 'make-gallery :title "Gallery" :width 960 :height 760)

(defstruct (task (:constructor make-task (id text)))
  (id 0 :read-only t)
  (text "" :read-only t)
  (done nil))

(defparameter +fruits+
  '("Apple" "Banana" "Cherry" "Damson" "Elderberry" "Fig" "Grape" "Huckleberry"))

(defparameter +sizes+ '("Small" "Medium" "Large"))

(defparameter +deliveries+ '("Standard" "Express" "Collect in person"))

(defparameter +colours+ '("Red" "Orange" "Yellow" "Green" "Blue" "Indigo" "Violet"))

(defparameter +views+ '("List" "Columns" "Gallery"))

(defparameter +canvas-color+ (bolt:pack-color 30 33 46))
(defparameter +dot-color+ (bolt:pack-color 150 200 255))
(defparameter +note-color+ (bolt:pack-color 150 156 175))
(defparameter +block-color+ (bolt:pack-color 60 66 92))
(defparameter +call-color+ (bolt:pack-color 175 205 150))

(defmacro section ((title id) &body body)
  "A panel with TITLE over what BODY describes, taking a share of the width of the row it is in."
  `(bolt:panel (:id ,id :all 16 :flex 1)
     (bolt:column (:gap 10)
       (bolt:label ,title :id :title :size 18)
       ,@body)))

(defun note (text id &key live)
  "A line in a dimmer colour, for what a widget returned. LIVE is as for a label."
  (bolt:label text :id id :size 13 :color +note-color+ :live live))

(defun call (text id)
  "How the widget above it is called, TEXT, in a fixed-width face. It can be selected and copied."
  (bolt:paragraph text :id id :size 12 :font "Menlo" :color +call-color+ :selectable t))

(defun make-tasks (count)
  (let ((tasks (make-array count)))
    (dotimes (i count tasks)
      (setf (aref tasks i) (make-task i (format nil "Task ~d" (1+ i)))))))

(defun make-gallery ()
  (bolt:state-let ((presses 0)
                   (name "Ada")
                   (fruit-search "")
                   (notes (format nil "Several lines of text.~%Return starts another, and a long line goes on in the next."))
                   (subscribed t)
                   (size 1)
                   (delivery 0)
                   (colour 0)
                   (view 0)
                   (starred nil)
                   (busy nil)
                   (fruit 2)
                   (volume 40d0)
                   (dots '())
                   (tasks (make-tasks 10000))
                   (overlay nil)
                   (popover nil)
                   (toasts 0)
                   (file-note "The system's file panels: nothing chosen yet.")
                   (asking nil)
                   (answer "Not asked yet."))
    (lambda ()
      (bolt:stack (:id :gallery)
        (bolt:scroll (:id :page)
          (bolt:padding (:all 24)
            (bolt:column (:gap 20)
              (bolt:label "Widget gallery" :id :heading :size 28)
              (bolt:paragraph "Every widget and container of almighty-bolt, each with what it gives back under it, and how it is called: its arguments and options, with their defaults. Every widget needs its :id. The calls can be selected and copied. The page is a scroll container; turn the wheel to see the rest."
                              :id :intro :color +note-color+)

              (bolt:row (:id :first :gap 20 :fill t)
                (section ("Text" :text)
                  (bolt:label "A label is one line." :id :plain)
                  (bolt:label "Labels come in sizes." :id :big :size 22)
                  (bolt:row (:id :turned :gap 8)
                    (bolt:label "Turned up" :id :turned-up :turn :up)
                    (bolt:label "and down" :id :turned-down :turn :down))
                  (call "(bolt:label text &key id tooltip size color (flex 0) live selectable weight font turn)"
                        :label-call)
                  (bolt:paragraph "A paragraph is broken into lines to the width it is given, so its height follows its width. Make the window narrower and it takes more lines."
                                  :id :paragraph)
                  (call "(bolt:paragraph text &key id tooltip size color (flex 0) live selectable weight font)"
                        :paragraph-call))
                (section ("Buttons" :buttons)
                  (bolt:row (:gap 8)
                    (when (bolt:button "Press me" :id :press
                                         :tooltip "Rest the pointer on a widget to see its tooltip")
                      (incf presses))
                    (when (bolt:button "Reset" :id :reset :enabled (plusp presses))
                      (setf presses 0))
                    (bolt:button "Disabled" :id :disabled :enabled nil
                                 :tooltip "A disabled button still has a tooltip"))
                  ;; Live: a screen reader reads the count out when it changes.
                  (note (format nil "Pressed ~d time~:p." presses) :presses :live t)
                  (call "(bolt:button text &key id tooltip (enabled t) (flex 0) font)" :button-call)))

              (bolt:row (:id :second :gap 20 :fill t)
                (section ("Text fields" :fields)
                  (bolt:label "Name" :id :name-label)
                  (multiple-value-bind (text changed)
                      (bolt:text-field name :id :name :labelled-by :name-label :width 220
                                            :invalid (zerop (length name)))
                    (when changed (setf name text)))
                  (note (if (zerop (length name))
                            "An empty name is shown as invalid."
                            (format nil "Hello, ~a." name))
                        :greeting)
                  (bolt:text-field "Not enabled" :id :locked :width 220 :enabled nil)
                  (call "(bolt:text-field string &key id tooltip labelled-by (width 180) invalid (enabled t) (flex 0) font)"
                        :text-field-call)
                  (bolt:label "Fruit, with suggestions" :id :fruit-search-label)
                  (multiple-value-bind (text changed)
                      (bolt:combo-box fruit-search
                                      (and (plusp (length fruit-search))
                                           (remove-if-not (lambda (f) (search fruit-search f
                                                                              :test #'char-equal))
                                                          +fruits+))
                                      :id :fruit-search :labelled-by :fruit-search-label
                                      :width 220)
                    (when changed (setf fruit-search text)))
                  (call "(bolt:combo-box string suggestions &key id tooltip labelled-by (width 180) (enabled t) (flex 0) font)"
                        :combo-box-call))
                (section ("Text area" :area)
                  (bolt:label "Notes" :id :notes-label)
                  (multiple-value-bind (text changed)
                      (bolt:text-area notes :id :notes :labelled-by :notes-label :width 300 :rows 4)
                    (when changed (setf notes text)))
                  (note (format nil "~d character~:p on ~d line~:p."
                                (length notes) (1+ (count #\Newline notes)))
                        :notes-count)
                  (call "(bolt:text-area string &key id tooltip labelled-by (width 320) (rows 6) (enabled t) (flex 0) font)"
                        :text-area-call)))

              (bolt:row (:id :third :gap 20 :fill t)
                (section ("Choices" :choices)
                  (multiple-value-bind (checked changed)
                      (bolt:checkbox "Send me the newsletter" subscribed :id :subscribe)
                    (when changed (setf subscribed checked)))
                  (call "(bolt:checkbox label checked &key id tooltip (enabled t) (flex 0) font)"
                        :checkbox-call)
                  (bolt:label "Size" :id :size-label)
                  (multiple-value-bind (index changed)
                      (bolt:choice +sizes+ size :id :size :labelled-by :size-label)
                    (when changed (setf size index)))
                  (note (format nil "~:[No newsletter~;Newsletter~], ~(~a~)."
                                subscribed (nth size +sizes+))
                        :choices-note)
                  (bolt:label "Delivery" :id :delivery-label)
                  (multiple-value-bind (index changed)
                      (bolt:choice +deliveries+ delivery :id :delivery :labelled-by :delivery-label
                                                         :direction :vertical)
                    (when changed (setf delivery index)))
                  (note (format nil "~a delivery." (nth delivery +deliveries+)) :delivery-note)
                  (bolt:label "View, segmented" :id :view-label)
                  (multiple-value-bind (index changed)
                      (bolt:choice +views+ view :id :view :labelled-by :view-label :style :segmented)
                    (when changed (setf view index)))
                  (note (format nil "Shown as ~(~a~)." (nth view +views+)) :view-note)
                  (call "(bolt:choice options selected &key id tooltip labelled-by (direction :horizontal) (enabled t) (flex 0) (style :radio) font)"
                        :choice-call)
                  (bolt:label "Colour" :id :colour-label)
                  (multiple-value-bind (index changed)
                      (bolt:dropdown +colours+ colour :id :colour :labelled-by :colour-label)
                    (when changed (setf colour index)))
                  (note (format nil "~a it is." (nth colour +colours+)) :colour-note)
                  (call "(bolt:dropdown options selected &key id tooltip labelled-by width (enabled t) (flex 0) max-rows font)"
                        :dropdown-call))
                (section ("List and menus" :list)
                  (bolt:label "Fruit (right-click it for a menu)" :id :fruit-label)
                  (let ((command
                          (bolt:context-menu (:id :fruit-menu
                                              :items '("Pick the first" "Pick the last"))
                            (multiple-value-bind (index changed)
                                (bolt:list-box +fruits+ fruit :id :fruit :labelled-by :fruit-label
                                                              :rows 4)
                              (when changed (setf fruit index))))))
                    (cond ((equal command "Pick the first") (setf fruit 0))
                          ((equal command "Pick the last") (setf fruit (1- (length +fruits+))))))
                  (note (format nil "Picked: ~a." (nth fruit +fruits+)) :fruit-note)
                  (call "(bolt:list-box items selected &key id tooltip labelled-by (rows 8) (width 200) (enabled t) (flex 0) font)"
                        :list-box-call)
                  (call "(bolt:context-menu (&key id items) &body body)" :context-menu-call)
                  (let ((command (bolt:menu-button "Fruit" '("Next" "Previous") :id :fruit-actions)))
                    (cond ((equal command "Next")
                           (setf fruit (mod (1+ fruit) (length +fruits+))))
                          ((equal command "Previous")
                           (setf fruit (mod (1- fruit) (length +fruits+))))))
                  (call "(bolt:menu-button text items &key id tooltip (enabled t) (flex 0) max-rows font)"
                        :menu-button-call)))

              (bolt:row (:id :fourth :gap 20 :fill t)
                (section ("Slider, progress bar, switch and spinner" :ranges)
                  (bolt:label "Volume" :id :volume-label)
                  (multiple-value-bind (value changed)
                      (bolt:slider volume 0 100 :id :volume :labelled-by :volume-label :width 260
                                             :tooltip "Drag, or use the arrow keys")
                    (when changed (setf volume value)))
                  (call "(bolt:slider value minimum maximum &key id tooltip labelled-by (width 200) (enabled t) (flex 0))"
                        :slider-call)
                  (bolt:label "Loudness" :id :loudness-label)
                  (bolt:progress (/ volume 100) :id :loudness :labelled-by :loudness-label
                                                :width 260)
                  (note (format nil "~d%" (round volume)) :volume-note)
                  (call "(bolt:progress fraction &key id tooltip labelled-by (width 200) (flex 0))"
                        :progress-call)
                  (bolt:row (:gap 12 :cross-align :center)
                    (multiple-value-bind (on changed) (bolt:switch "Busy" busy :id :busy)
                      (when changed (setf busy on)))
                    ;; Only while it is wanted: a spinner asks for frames as long as it shows.
                    (when busy
                      (bolt:spinner :id :working :labelled-by :busy)))
                  (call "(bolt:switch label on &key id tooltip (enabled t) (flex 0) font)" :switch-call)
                  (call "(bolt:spinner &key id tooltip labelled-by (size 18) (flex 0))" :spinner-call))
                (section ("Canvas" :drawing)
                  (bolt:label "Click to put a dot" :id :canvas-label)
                  (let ((shown dots))
                    (dolist (event (bolt:canvas 280 120
                                                (lambda (list context width height)
                                                  (declare (ignore context))
                                                  (bolt:push-rounded-rect list 0 0 width height 8
                                                                          +canvas-color+)
                                                  (dolist (dot shown)
                                                    (bolt:push-rounded-rect list (- (car dot) 6)
                                                                            (- (cdr dot) 6) 12 12 6
                                                                            +dot-color+)))
                                                :id :canvas :labelled-by :canvas-label))
                      (when (eq (first event) :press)
                        (push (cons (second event) (third event)) dots))))
                  (bolt:row (:gap 8 :cross-align :center)
                    (when (bolt:button "Clear" :id :clear :enabled (and dots t))
                      (setf dots '()))
                    (note (format nil "~d dot~:p." (length dots)) :dots))
                  (call "(bolt:canvas width height painter &key id tooltip labelled-by focusable autofocus cursor (flex 0))"
                        :canvas-call)))

              (bolt:row (:id :fifth :gap 20 :fill t)
                (section ("Lazy list" :lazy)
                  (bolt:paragraph "Ten thousand rows, of which only the ones in the window are built."
                                  :id :lazy-about :size 13 :color +note-color+)
                  (bolt:lazy-list (task tasks :id :tasks :key #'task-id :row-height 28 :height 168)
                    (multiple-value-bind (checked changed)
                        (bolt:checkbox (task-text task) (task-done task) :id :done)
                      (when changed (setf (task-done task) checked))))
                  (note (format nil "~d done." (count-if #'task-done tasks)) :done-count)
                  (call "(bolt:lazy-list (var items &key id key (row-height 24) height width (flex 0)) &body body)"
                        :lazy-list-call))
                (section ("Scroll container" :scrolling)
                  (bolt:paragraph "Anything at all in a window that the wheel moves. This one is inside the page, which scrolls too."
                                  :id :scroll-about :size 13 :color +note-color+)
                  (bolt:scroll (:id :inner :height 150)
                    (bolt:column (:gap 6)
                      (dotimes (i 20)
                        (bolt:label (format nil "Line ~d of twenty" (1+ i)) :id i))))
                  (call "(bolt:scroll (&key id height (flex 0)) &body body)" :scroll-call)))

              (bolt:row (:id :sixth :gap 20 :fill t)
                (section ("Containers" :containers)
                  (bolt:paragraph "Rows, columns, padding, panels and center lay things out; a stack puts them on top of each other."
                                  :id :containers-about :size 13 :color +note-color+)
                  (bolt:stack (:id :stacked)
                    (bolt:panel (:id :under :all 0 :color +block-color+)
                      (bolt:padding (:left 220 :top 60)))
                    (bolt:padding (:id :over :left 16 :top 20)
                      (bolt:label "On top of the panel" :id :on-top)))
                  (bolt:panel (:id :raised :all 16 :color :background
                               :shadow '((:y 1 :blur 2 :color ("#000000" :alpha 0.12))
                                         (:y 8 :blur 24 :color ("#000000" :alpha 0.16))))
                    (bolt:label "A panel with a shadow" :id :raised-words))
                  (bolt:panel (:id :dashed :all 16 :color :background :border-color :dim-text
                               :border-dash '(6 4))
                    (bolt:label "A dashed edge" :id :dashed-words))
                  (call "(bolt:row (&key id (gap 0) (main-align :start) (cross-align :start) fill (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :row-call)
                  (call "(bolt:column (&key id (gap 0) (main-align :start) (cross-align :start) fill (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :column-call)
                  (call "(bolt:stack (&key id (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :stack-call)
                  (call "(bolt:padding (&key id all left top right bottom (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :padding-call)
                  (call "(bolt:panel (&key id all left top right bottom color (radius 8) (flex 0) border-color (border-width 1) border-dash shadow width height min-width min-height max-width max-height) &body body)"
                        :panel-call)
                  (call "(bolt:center (&key id (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :center-call))
                (section ("Overlays" :overlays)
                  (bolt:paragraph "A popover floats by the widget it belongs to; a dialog keeps the pointer and the keyboard until it is answered; a scrim covers what is under it; a toast says something for a while; and the system's own panels choose files."
                                  :id :overlay-about :size 13 :color +note-color+)
                  (bolt:column (:id :overlay-buttons :gap 8)
                    (when (bolt:button "Show a popover" :id :show-popover)
                      (setf popover t))
                    (when (bolt:popover (:id :popover :open popover :anchor :show-popover)
                            (bolt:label "A popover" :id :popover-title :size 15)
                            (bolt:label "Press outside it, or Escape, to close it." :id :popover-text
                                                                                    :size 13))
                      (setf popover nil))
                    (when (bolt:button "Ask in a dialog" :id :show-dialog)
                      (setf asking t))
                    (when (bolt:button "Show a scrim" :id :show-overlay)
                      (setf overlay t))
                    (when (bolt:button "Make a toast" :id :show-toast)
                      (bolt:toast (format nil "Toast number ~d" (incf toasts)))))
                  (note answer :dialog-answer)
                  (bolt:row (:id :file-buttons :gap 8)
                    (let ((path (bolt:open-file-dialog
                                 :id :open-file
                                 :show (bolt:button "Open a file..." :id :open-file-button))))
                      (when path (setf file-note (format nil "Chose ~a" path))))
                    (let ((path (bolt:save-file-dialog
                                 :id :save-file :name "Untitled.txt" :types '("txt")
                                 :show (bolt:button "Save as..." :id :save-file-button))))
                      (when path (setf file-note (format nil "Would save to ~a" path)))))
                  (note file-note :file-note)
                  (call "(bolt:popover (&key id open anchor (placement :below)) &body body)"
                        :popover-call)
                  (call "(bolt:dialog (&key id open title font) &body body)" :dialog-call)
                  (call "(bolt:scrim &key id tooltip)" :scrim-call)
                  (call "(bolt:toast words &key (seconds 3))" :toast-call)
                  (call "(bolt:open-file-dialog &key id show types)" :open-file-dialog-call)
                  (call "(bolt:save-file-dialog &key id show types name)" :save-file-dialog-call)))

              (bolt:row (:id :seventh :gap 20 :fill t)
                (section ("Grid" :grids)
                  (bolt:paragraph "Children in columns, filling each row before the next."
                                  :id :grid-about :size 13 :color +note-color+)
                  (bolt:grid (:id :cells :columns 3 :gap 6)
                    (dotimes (i 6)
                      (bolt:panel (:id i :all 8 :color +block-color+)
                        (bolt:label (format nil "Cell ~d" (1+ i)) :id :cell))))
                  (call "(bolt:grid (&key id (columns 1) (gap 0) row-gap (column-widths :equal) (row-heights :auto) (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :grid-call))
                (section ("Pressable and spacer" :pressables)
                  (when (bolt:pressable (:id :star :all 10 :selected starred)
                          (bolt:label (if starred "Starred" "Not starred") :id :star-text)
                          (bolt:label "Anything at all, pressed as a button is; given :selected, a toggle."
                                      :id :star-about :size 12))
                    (setf starred (not starred)))
                  (note (format nil "~:[Not starred~;Starred~]." starred) :star-note)
                  (call "(bolt:pressable (&key id tooltip (enabled t) selected name (look :plain) color border-color (border-width 1) border-dash shadow (radius 8) all left top right bottom (gap 0) (flex 0) width height min-width min-height max-width max-height) &body body)"
                        :pressable-call)
                  (bolt:row (:id :spaced :fill t)
                    (bolt:label "Left" :id :left)
                    (bolt:spacer)
                    (bolt:label "Right, pushed by a spacer" :id :right))
                  (call "(bolt:spacer &key (flex 1))" :spacer-call))))))
        (when (bolt:dialog (:id :question :open asking :title "Keep the changes?")
                (bolt:label "A dialog keeps the keyboard; Tab goes round its buttons." :id :question-text)
                (bolt:row (:gap 8)
                  (when (bolt:button "Keep" :id :keep)
                    (setf asking nil answer "Kept."))
                  (when (bolt:button "Throw away" :id :throw-away)
                    (setf asking nil answer "Thrown away."))))
          (setf asking nil answer "Escape: not answered."))
        (when overlay
          (when (bolt:scrim :id :veil)
            (setf overlay nil))
          (bolt:center (:id :dialog)
            (bolt:panel (:all 24)
              (bolt:column (:gap 12)
                (bolt:label "An overlay" :id :dialog-title :size 18)
                (bolt:paragraph "Press Close, or anywhere outside this panel." :id :dialog-text)
                (when (bolt:button "Close" :id :close)
                  (setf overlay nil))))))))))
