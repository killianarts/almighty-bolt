(defpackage #:almighty-bolt-examples/gallery
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:export #:make-gallery))

(in-package #:almighty-bolt-examples/gallery)

;;; Every widget and container of the library on one page, each with what it returns shown under
;;; it, for trying them out and for showing them. It uses the public package only. A widget that is
;;; added to the library is added here too: the tests check that the gallery has one of each.
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

(defparameter +canvas-color+ (bolt:pack-color 30 33 46))
(defparameter +dot-color+ (bolt:pack-color 150 200 255))
(defparameter +note-color+ (bolt:pack-color 150 156 175))
(defparameter +block-color+ (bolt:pack-color 60 66 92))

(defmacro section ((title id) &body body)
  "A panel with TITLE over what BODY describes, taking a share of the width of the row it is in."
  `(bolt:panel (:id ,id :all 16 :flex 1)
     (bolt:column (:gap 10)
       (bolt:label ,title :id :title :size 18)
       ,@body)))

(defun note (text id &key live)
  "A line in a dimmer colour, for what a widget returned. LIVE is as for a label."
  (bolt:label text :id id :size 13 :color +note-color+ :live live))

(defun make-tasks (count)
  (let ((tasks (make-array count)))
    (dotimes (i count tasks)
      (setf (aref tasks i) (make-task i (format nil "Task ~d" (1+ i)))))))

(defun make-gallery ()
  (bolt:state-let ((presses 0)
                   (name "Ada")
                   (notes (format nil "Several lines of text.~%Return starts another, and a long line goes on in the next."))
                   (subscribed t)
                   (size 1)
                   (delivery 0)
                   (colour 0)
                   (busy nil)
                   (fruit 2)
                   (volume 40d0)
                   (dots '())
                   (tasks (make-tasks 10000))
                   (overlay nil)
                   (popover nil)
                   (toasts 0)
                   (asking nil)
                   (answer "Not asked yet."))
    (lambda ()
      (bolt:stack (:id :gallery)
        (bolt:scroll (:id :page)
          (bolt:padding (:all 24)
            (bolt:column (:gap 20)
              (bolt:label "Widget gallery" :id :heading :size 28)
              (bolt:paragraph "Every widget and container of almighty-bolt, each with what it gives back under it. The page is a scroll container; turn the wheel to see the rest."
                              :id :intro :color +note-color+)

              (bolt:row (:id :first :gap 20 :fill t)
                (section ("Text" :text)
                  (bolt:label "A label is one line." :id :plain)
                  (bolt:label "Labels come in sizes." :id :big :size 22)
                  (bolt:paragraph "A paragraph is broken into lines to the width it is given, so its height follows its width. Make the window narrower and it takes more lines."
                                  :id :paragraph))
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
                  (note (format nil "Pressed ~d time~:p." presses) :presses :live t)))

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
                  (bolt:text-field "Not enabled" :id :locked :width 220 :enabled nil))
                (section ("Text area" :area)
                  (bolt:label "Notes" :id :notes-label)
                  (multiple-value-bind (text changed)
                      (bolt:text-area notes :id :notes :labelled-by :notes-label :width 300 :rows 4)
                    (when changed (setf notes text)))
                  (note (format nil "~d character~:p on ~d line~:p."
                                (length notes) (1+ (count #\Newline notes)))
                        :notes-count)))

              (bolt:row (:id :third :gap 20 :fill t)
                (section ("Choices" :choices)
                  (multiple-value-bind (checked changed)
                      (bolt:checkbox "Send me the newsletter" subscribed :id :subscribe)
                    (when changed (setf subscribed checked)))
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
                  (bolt:label "Colour" :id :colour-label)
                  (multiple-value-bind (index changed)
                      (bolt:dropdown +colours+ colour :id :colour :labelled-by :colour-label)
                    (when changed (setf colour index)))
                  (note (format nil "~a it is." (nth colour +colours+)) :colour-note))
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
                  (let ((command (bolt:menu-button "Fruit" '("Next" "Previous") :id :fruit-actions)))
                    (cond ((equal command "Next")
                           (setf fruit (mod (1+ fruit) (length +fruits+))))
                          ((equal command "Previous")
                           (setf fruit (mod (1- fruit) (length +fruits+))))))))

              (bolt:row (:id :fourth :gap 20 :fill t)
                (section ("Slider, progress bar, switch and spinner" :ranges)
                  (bolt:label "Volume" :id :volume-label)
                  (multiple-value-bind (value changed)
                      (bolt:slider volume 0 100 :id :volume :labelled-by :volume-label :width 260
                                             :tooltip "Drag, or use the arrow keys")
                    (when changed (setf volume value)))
                  (bolt:label "Loudness" :id :loudness-label)
                  (bolt:progress (/ volume 100) :id :loudness :labelled-by :loudness-label
                                                :width 260)
                  (note (format nil "~d%" (round volume)) :volume-note)
                  (bolt:row (:gap 12 :cross-align :center)
                    (multiple-value-bind (on changed) (bolt:switch "Busy" busy :id :busy)
                      (when changed (setf busy on)))
                    ;; Only while it is wanted: a spinner asks for frames as long as it shows.
                    (when busy
                      (bolt:spinner :id :working :labelled-by :busy))))
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
                    (note (format nil "~d dot~:p." (length dots)) :dots))))

              (bolt:row (:id :fifth :gap 20 :fill t)
                (section ("Lazy list" :lazy)
                  (bolt:paragraph "Ten thousand rows, of which only the ones in the window are built."
                                  :id :lazy-about :size 13 :color +note-color+)
                  (bolt:lazy-list (task tasks :id :tasks :key #'task-id :row-height 28 :height 168)
                    (multiple-value-bind (checked changed)
                        (bolt:checkbox (task-text task) (task-done task) :id :done)
                      (when changed (setf (task-done task) checked))))
                  (note (format nil "~d done." (count-if #'task-done tasks)) :done-count))
                (section ("Scroll container" :scrolling)
                  (bolt:paragraph "Anything at all in a window that the wheel moves. This one is inside the page, which scrolls too."
                                  :id :scroll-about :size 13 :color +note-color+)
                  (bolt:scroll (:id :inner :height 150)
                    (bolt:column (:gap 6)
                      (dotimes (i 20)
                        (bolt:label (format nil "Line ~d of twenty" (1+ i)) :id i))))))

              (bolt:row (:id :sixth :gap 20 :fill t)
                (section ("Containers" :containers)
                  (bolt:paragraph "Rows, columns, padding, panels and center lay things out; a stack puts them on top of each other."
                                  :id :containers-about :size 13 :color +note-color+)
                  (bolt:stack (:id :stacked)
                    (bolt:panel (:id :under :all 0 :color +block-color+)
                      (bolt:padding (:left 220 :top 60)))
                    (bolt:padding (:id :over :left 16 :top 20)
                      (bolt:label "On top of the panel" :id :on-top))))
                (section ("Overlays" :overlays)
                  (bolt:paragraph "A popover floats by the widget it belongs to; a dialog keeps the pointer and the keyboard until it is answered; a scrim covers what is under it; a toast says something for a while."
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
                  (note answer :dialog-answer))))))
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
