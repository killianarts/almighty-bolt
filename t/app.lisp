(defpackage #:almighty-bolt/t/app
  ;; This package uses the public one, whole: a symbol that the public package exports and that
  ;; Common Lisp or the test framework also has would be a conflict, and fail here.
  (:use #:cl #:lisp-unit2 #:almighty-bolt)
  (:import-from #:sb-introspect #:function-lambda-list)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:m #:almighty-bolt/machine)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:counter #:almighty-bolt-examples/counter)
                    (#:todo #:almighty-bolt-examples/todo)
                    (#:gallery #:almighty-bolt-examples/gallery)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:host #:almighty-bolt/platform/macos/host)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:target #:almighty-bolt/render/metal/target)
                    (#:renderer #:almighty-bolt/render/metal/renderer)
                    (#:atlas #:almighty-bolt/render/metal/glyph-atlas)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:window #:almighty-bolt/platform/macos/window)
                    (#:view #:almighty-bolt/platform/macos/view)
                    (#:a11y #:almighty-bolt/platform/macos/accessibility)))

(in-package #:almighty-bolt/t/app)

;;; A person's first hour with the library: the example apps, which use only the public package, are
;;; made as the host makes them, shown, clicked, typed into and made again. Events in, what is shown
;;; out (the text is read back from the display list, in which the fake context draws a character as
;;; a command whose parameter is its code).

(defstruct (screen (:constructor %make-screen (app context list state)))
  app context list state
  ;; What the host does after it has presented a frame (tell a screen reader), if anything.
  (after-draw nil))

(defun show (constructor)
  "Make the app of CONSTRUCTOR with a store for its state, as OPEN-APP does, and draw its first
frame."
  (let* ((state (core:make-kept-state))
         (instance (core:with-kept-state (state) (app:make-app-instance constructor)))
         (screen (%make-screen instance (fake:make-fake-context :width 360f0 :height 320f0)
                               (core:make-display-list :capacity 8192) state)))
    (draw screen)
    screen))

(defun draw (screen)
  (core:settle-frame (screen-app screen) (screen-context screen) (screen-list screen))
  (when (screen-after-draw screen)
    (funcall (screen-after-draw screen))))

(defun pointer (screen kind x y &key button (buttons 0) (clicks 1))
  "An event of the pointer at X, Y; CLICKS is which press in a row a press is (2 for the second of a
double-click)."
  (core:handle-event (screen-app screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button button :buttons buttons
                                              :click-count (if (eq kind :move) 0 clicks)))
  (draw screen))

(defun click (screen id)
  (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) id)
    (assert-true x id)
    (let ((px (+ x (/ width 2))) (py (+ y (/ height 2))))
      (pointer screen :move px py)
      (pointer screen :down px py :button 0 :buttons 1)
      (pointer screen :up px py :button 0))))

(defun type-text (screen string)
  (loop :for character :across string
        :do (core:handle-event (screen-app screen)
                               (core:make-key-event :timestamp 0 :kind :down
                                                    :logical-key character :modifiers 0))
            (draw screen)))

(defun text-in (screen id)
  "The characters drawn inside the rectangle of the control ID."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) id)
    (let ((list (screen-list screen)))
      (coerce (loop :for index :below (core:display-list-count list)
                    :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                               (<= left (core:display-list-x list index) (+ left width -1))
                               (<= top (core:display-list-y list index) (+ top height -1)))
                      :collect (code-char (round (core:display-list-parameter list index))))
              'string))))

(define-test the-counter-shows-its-count-and-counts-clicks ()
  (let ((screen (show 'counter:make-counter)))
    (assert-equal "0" (text-in screen :count))
    (click screen :count-button)
    (click screen :count-button)
    (assert-equal "2" (text-in screen :count))))

(define-test the-counter-made-again-by-new-code-keeps-its-count ()
  (let ((screen (show 'counter:make-counter)))
    (click screen :count-button)
    (click screen :count-button)
    (click screen :count-button)
    (let ((new (core:with-kept-state ((screen-state screen))
                 (app:make-app-instance 'counter:make-counter))))
      (core:inherit-state new (screen-app screen))
      (setf (screen-app screen) new)
      (draw screen)
      (assert-equal "3" (text-in screen :count)))))

(define-test the-list-adds-a-thing-written-in-its-field-and-counts-what-is-left ()
  (let ((screen (show 'todo:make-todo)))
    (assert-equal "2 left" (text-in screen :left))
    (click screen :draft)
    (type-text screen "Water the plants")
    (click screen :add)
    (assert-equal "3 left" (text-in screen :left))
    (assert-equal "Water the plants" (text-in screen 4))))

(define-test return-in-the-field-adds-the-thing-too ()
  (let ((screen (show 'todo:make-todo)))
    (click screen :draft)
    (type-text screen "Feed the cat")
    (type-text screen (string #\Return))
    (assert-equal "3 left" (text-in screen :left))
    (assert-equal "Feed the cat" (text-in screen 4))
    (assert-equal "" (text-in screen :draft))))

(define-test checking-a-thing-off-leaves-one-less ()
  (let ((screen (show 'todo:make-todo)))
    (click screen 2)
    (assert-equal "1 left" (text-in screen :left))
    (click screen 1)
    (assert-equal "2 left" (text-in screen :left))))

(defun screen-session (screen)
  "The machine interface's session on SCREEN, a stand-in for a window."
  (m:make-session :app (lambda () (screen-app screen))
                  :deliver (lambda (event)
                             (core:handle-event (screen-app screen) event)
                             (draw screen))
                  :settle (lambda () (dotimes (i 4) (draw screen)))
                  :on-main #'funcall))

(define-test an-agent-checks-things-off-and-adds-one-with-the-keyboard ()
  (let* ((screen (show 'todo:make-todo))
         (session (screen-session screen)))
    (assert-equal "2 left" (text-in screen :left))
    (m:set-value session t :role :checkbox :name "Open a window from the REPL")
    (assert-equal "1 left" (text-in screen :left))
    (m:set-value session t :role :checkbox :name "Open a window from the REPL")
    (assert-equal "1 left" (text-in screen :left))     ; already as wanted: no press
    (m:set-value session nil :role :checkbox :name "Read the README")
    (assert-equal "2 left" (text-in screen :left))
    (m:type-text session "Feed the cat" :id :draft)
    (m:press-key session :return)
    (assert-equal "3 left" (text-in screen :left))
    (assert-equal "Feed the cat" (text-in screen 4))
    (assert-equal "" (s:node-value (m:find-node session :id :draft)))))

;;; --- a switch, choices one under another, a spinner ---

(defun make-settings ()
  (state-let ((wifi nil) (delivery 0) (busy t))
    (lambda ()
      (column (:gap 8)
        (multiple-value-bind (on changed) (switch "Wi-Fi" wifi :id :wifi)
          (when changed (setf wifi on)))
        (label (if wifi "On" "Off") :id :wifi-note)
        (label "Delivery" :id :delivery-label)
        (multiple-value-bind (index changed)
            (choice '("Standard" "Express" "Collect") delivery :id :delivery
                    :labelled-by :delivery-label :direction :vertical)
          (when changed (setf delivery index)))
        (when busy
          (spinner :id :working))))))

(define-test a-switch-turns-on-and-off-by-the-pointer-and-the-space-bar ()
  (let* ((screen (show 'make-settings))
         (session (screen-session screen)))
    (assert-eq :switch (s:node-role (node-of screen :wifi)))
    (assert-equal "Wi-Fi" (s:node-name (node-of screen :wifi)))
    (assert-eq nil (s:node-value (node-of screen :wifi)))
    (click screen :wifi)
    (assert-equal "On" (text-in screen :wifi-note))
    (assert-true (s:node-property (node-of screen :wifi) :checked))
    (m:press-key session :tab)
    (m:press-key session :space)
    (assert-equal "Off" (text-in screen :wifi-note))
    (m:set-value session t :id :wifi)
    (assert-equal "On" (text-in screen :wifi-note))))

(define-test a-choice-that-goes-down-has-its-options-one-under-another ()
  (let* ((screen (show 'make-settings))
         (session (screen-session screen))
         (bounds (s:node-property (node-of screen :delivery) :option-bounds)))
    (assert-eq :vertical (s:node-property (node-of screen :delivery) :direction))
    ;; One left edge, each lower than the one before.
    (assert-equal 1 (length (remove-duplicates (mapcar #'first bounds))))
    (assert-true (apply #'< (mapcar #'second bounds)))
    (m:set-value session "Collect" :id :delivery)
    (assert-equal "Collect" (s:node-value (node-of screen :delivery)))
    ;; The switch, then the choice. (A press does not give a choice the keyboard.)
    (m:press-key session :tab)
    (m:press-key session :tab)
    (m:press-key session :up)
    (assert-equal "Express" (s:node-value (node-of screen :delivery)))))

(defun spinner-colors (screen)
  "The colours of what is drawn in the spinner's place, in order."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) :working)
    (let ((list (screen-list screen)))
      (loop :for index :below (core:display-list-count list)
            :when (and (<= left (core:display-list-x list index) (+ left width))
                       (<= top (core:display-list-y list index) (+ top height)))
              :collect (core:display-list-color list index)))))

(define-test a-spinner-goes-round-as-time-passes-and-asks-for-the-frames-it-needs ()
  (let ((clock (core:make-virtual-clock 0)))
    (core:with-time-source ((core:virtual-clock-source clock))
      (let* ((screen (show 'make-settings))
             (before (spinner-colors screen)))
        (assert-eq :busy-indicator (s:node-role (node-of screen :working)))
        (assert-true (core:take-frame-request (screen-context screen)))
        (assert-equal 8 (length before))
        ;; A tenth of a second is a step round.
        (core:advance-virtual-clock clock 100000000)
        (draw screen)
        (let ((after (spinner-colors screen)))
          (assert-false (equal before after))
          (assert-equal (append (last before) (butlast before)) after))))))

;;; --- a dropdown, and the layer that its menu floats on ---

(defun make-picker ()
  "A dropdown of sizes, what it says, and a button that counts its presses."
  (state-let ((size 1) (presses 0))
    (lambda ()
      (column (:gap 8)
        ;; Above the dropdown, so that its menu (below it) is not over the button.
        (when (button "Press" :id :press)
          (incf presses))
        (label (format nil "~d presses" presses) :id :presses)
        (label "Size" :id :size-label)
        (multiple-value-bind (index changed)
            (dropdown '("Small" "Medium" "Large") size :id :size :labelled-by :size-label)
          (when changed (setf size index)))))))

(defun menu-of (screen)
  (first (s:find-nodes (core:accessibility-tree (screen-app screen)) :role :menu)))

(defun menu-rows (screen)
  (let ((menu (menu-of screen)))
    (and menu (mapcar #'s:node-name (s:node-children menu)))))

(define-test a-dropdown-opens-a-menu-under-it-and-a-row-of-it-chooses ()
  (let* ((screen (show 'make-picker))
         (session (screen-session screen)))
    (assert-eq :pop-up-button (s:node-role (node-of screen :size)))
    (assert-equal "Medium" (s:node-value (node-of screen :size)))
    (assert-eq nil (menu-of screen))
    (m:click session :id :size)
    (assert-true (s:node-property (node-of screen :size) :expanded))
    (assert-equal '("Small" "Medium" "Large") (menu-rows screen))
    ;; The one it shows is marked, and the menu is below the dropdown.
    (assert-equal '("Medium")
                  (mapcar #'s:node-name
                          (remove-if-not (lambda (row) (s:node-property row :checked))
                                         (s:node-children (menu-of screen)))))
    (destructuring-bind (x y w h) (s:node-bounds (node-of screen :size))
      (declare (ignore x w))
      (assert-true (>= (second (s:node-bounds (menu-of screen))) (+ y h))))
    (m:click session :role :menu-item :name "Large")
    (assert-equal "Large" (s:node-value (node-of screen :size)))
    (assert-eq nil (menu-of screen))
    (m:set-value session "Small" :id :size)
    (assert-equal "Small" (s:node-value (node-of screen :size)))))

(define-test a-press-outside-an-open-menu-closes-it-and-goes-nowhere-else ()
  (let* ((screen (show 'make-picker))
         (session (screen-session screen)))
    (m:click session :id :size)
    (assert-true (menu-of screen))
    (click screen :press)
    (assert-eq nil (menu-of screen))
    (assert-equal "0 presses" (s:node-name (node-of screen :presses)))
    (assert-equal "Medium" (s:node-value (node-of screen :size)))
    (click screen :press)
    (assert-equal "1 presses" (s:node-name (node-of screen :presses)))))

(define-test the-keys-open-a-dropdown-move-in-its-menu-choose-and-close-it ()
  (let* ((screen (show 'make-picker))
         (session (screen-session screen)))
    ;; The button, then the dropdown.
    (m:press-key session :tab)
    (m:press-key session :tab)
    (m:press-key session :space)
    (assert-true (menu-of screen))
    (m:press-key session :escape)
    (assert-eq nil (menu-of screen))
    (assert-equal "Medium" (s:node-value (node-of screen :size)))
    (m:press-key session :down)
    (m:press-key session :down)
    (assert-equal '("Large")
                  (mapcar #'s:node-name
                          (remove-if-not (lambda (row) (s:node-property row :highlighted))
                                         (s:node-children (menu-of screen)))))
    (m:press-key session :return)
    (assert-eq nil (menu-of screen))
    (assert-equal "Large" (s:node-value (node-of screen :size)))))

(define-test what-an-open-menu-draws-changes-with-its-highlight-and-goes-when-it-closes ()
  (let* ((screen (show 'make-picker))
         (session (screen-session screen)))
    (flet ((drawn () (core:display-list-hash (screen-list screen))))
      ;; The keyboard on the dropdown, its menu closed.
      (m:press-key session :tab)
      (m:press-key session :tab)
      (let ((closed (drawn)))
        (m:press-key session :space)
        (let ((open (drawn)))
          (assert-false (eql closed open))
          ;; Only the highlight moves, a row down.
          (m:press-key session :down)
          (let ((moved (drawn)))
            (assert-false (eql open moved))
            (m:press-key session :up)
            (assert-eql open (drawn)))
          (m:press-key session :escape)
          (assert-eql closed (drawn))
          (m:press-key session :space)
          (assert-eql open (drawn)))))))

(defun make-picker-in-a-scroll ()
  (lambda ()
    (scroll (:id :page :height 200)
      (column (:gap 8)
        (dropdown '("Small" "Medium" "Large") 0 :id :size)
        (dotimes (i 30) (label (format nil "Line ~d" i) :id i))))))

(define-test the-wheel-over-an-open-menu-does-not-scroll-what-is-under-it ()
  (let* ((screen (show 'make-picker-in-a-scroll))
         (session (screen-session screen)))
    (m:click session :id :size)
    (m:scroll session :role :menu-item :name "Medium" :dy -3)
    (assert-eql 0f0 (offset-of screen :page))
    (assert-true (menu-of screen))))

(defun make-pickers-in-rows ()
  "Three rows, each with a dropdown called :size: each has a menu of its own."
  (state-let ((sizes (list 0 0 0)))
    (lambda ()
      (column (:gap 4)
        (dotimes (i 3)
          (row (:id i :gap 8)
            (multiple-value-bind (index changed) (dropdown '("Small" "Large") (nth i sizes) :id :size)
              (when changed (setf (nth i sizes) index)))))))))

(define-test dropdowns-with-one-id-in-different-rows-each-have-their-own-menu ()
  (let* ((screen (show 'make-pickers-in-rows))
         (session (screen-session screen))
         (dropdowns (s:find-nodes (core:accessibility-tree (screen-app screen)) :role :pop-up-button)))
    (multiple-value-bind (x y) (destructuring-bind (x y w h) (s:node-bounds (second dropdowns))
                                 (values (+ x (/ w 2)) (+ y (/ h 2))))
      (pointer screen :move x y)
      (pointer screen :down x y :button 0 :buttons 1)
      (pointer screen :up x y :button 0))
    (m:click session :role :menu-item :name "Large")
    (assert-equal '("Small" "Large" "Small")
                  (mapcar #'s:node-value
                          (s:find-nodes (core:accessibility-tree (screen-app screen))
                                        :role :pop-up-button)))))

;;; --- a menu button and a context menu ---

(defun make-actions ()
  "A menu button of commands, an area with a context menu holding a label and a button that counts
its presses, a label outside the area, and what was chosen last."
  (state-let ((last "Nothing") (presses 0))
    (lambda ()
      (column (:gap 8)
        (let ((chosen (menu-button "Actions" '("Rename" "Duplicate" "Delete") :id :actions)))
          (when chosen (setf last chosen)))
        (let ((chosen (context-menu (:id :area :items '("Cut" "Copy" "Paste"))
                        (label "Right-click here" :id :inside)
                        (when (button (format nil "Pressed ~d" presses) :id :press)
                          (incf presses)))))
          (when chosen (setf last chosen)))
        (label "Not in the area" :id :outside)
        (label last :id :last)))))

(define-test a-menu-button-opens-a-menu-of-commands-and-says-which-was-chosen ()
  (let* ((screen (show 'make-actions))
         (session (screen-session screen)))
    (assert-eq :menu-button (s:node-role (node-of screen :actions)))
    (m:click session :id :actions)
    (assert-true (s:node-property (node-of screen :actions) :expanded))
    (assert-equal '("Rename" "Duplicate" "Delete") (menu-rows screen))
    ;; Commands: none is marked as chosen.
    (assert-false (some (lambda (row) (s:node-property row :checked))
                        (s:node-children (menu-of screen))))
    (m:click session :role :menu-item :name "Delete")
    (assert-equal "Delete" (s:node-name (node-of screen :last)))
    (assert-eq nil (menu-of screen))))

(define-test a-secondary-press-in-the-area-opens-its-menu-where-the-pointer-is ()
  (let* ((screen (show 'make-actions))
         (session (screen-session screen)))
    (m:secondary-click session :id :outside)
    (assert-eq nil (menu-of screen))
    (m:secondary-click session :id :inside)
    (assert-equal '("Cut" "Copy" "Paste") (menu-rows screen))
    ;; Its top left is where the pointer was, the middle of the label.
    (destructuring-bind (x y w h) (s:node-bounds (node-of screen :inside))
      (assert-equal (list (+ x (/ w 2)) (+ y (/ h 2)))
                    (subseq (s:node-bounds (menu-of screen)) 0 2)))
    (m:click session :role :menu-item :name "Copy")
    (assert-equal "Copy" (s:node-name (node-of screen :last)))))

(define-test a-control-press-on-a-button-in-the-area-opens-the-menu-and-does-not-press-it ()
  (let ((screen (show 'make-actions)))
    (destructuring-bind (x y w h) (s:node-bounds (node-of screen :press))
      (let ((px (+ x (/ w 2))) (py (+ y (/ h 2))))
        (pointer screen :move px py)
        (dolist (kind '(:down :up))
          (core:handle-event (screen-app screen)
                             (core:make-pointer-event :timestamp 0 :kind kind :x (float px 1d0)
                                                      :y (float py 1d0) :button 0
                                                      :buttons (if (eq kind :down) 1 0)
                                                      :modifiers core:+modifier-control+
                                                      :click-count 1))
          (draw screen))))
    (assert-equal '("Cut" "Copy" "Paste") (menu-rows screen))
    (assert-equal "Pressed 0" (s:node-name (node-of screen :press)))))

;;; --- tooltips ---

(defun make-tips ()
  (state-let ((saves 0))
    (lambda ()
      (column (:gap 8)
        (when (button "Save" :id :save :tooltip "Save the document")
          (incf saves))
        (label "Plain words" :id :plain :tooltip "A label can have one too")
        (button "Locked" :id :locked :enabled nil :tooltip "Not now")
        (button "No tip" :id :no-tip)
        (label (format nil "~d saves" saves) :id :saves)))))

(defun tooltip-of (screen)
  (first (s:find-nodes (core:accessibility-tree (screen-app screen)) :role :tooltip)))

(defun rest-on (screen id)
  "Move the pointer to the middle of ID."
  (destructuring-bind (x y w h) (s:node-bounds (node-of screen id))
    (pointer screen :move (+ x (/ w 2)) (+ y (/ h 2)))))

(defmacro with-clock ((clock) &body body)
  `(let ((,clock (core:make-virtual-clock 1000000000)))
     (core:with-time-source ((core:virtual-clock-source ,clock))
       ,@body)))

(define-test a-tooltip-comes-when-the-pointer-has-rested-on-its-widget ()
  (with-clock (clock)
    (let ((screen (show 'make-tips)))
      (assert-equal "Save the document" (s:node-help (node-of screen :save)))
      (rest-on screen :save)
      (assert-eq nil (tooltip-of screen))
      ;; It asks for the frames that bring it on time.
      (assert-true (core:take-frame-request (screen-context screen)))
      (core:advance-virtual-clock clock 300000000)
      (draw screen)
      (assert-eq nil (tooltip-of screen))
      (core:advance-virtual-clock clock 300000000)
      (draw screen)
      (assert-equal "Save the document" (s:node-name (tooltip-of screen)))
      ;; Under the button.
      (destructuring-bind (x y w h) (s:node-bounds (node-of screen :save))
        (declare (ignore x w))
        (assert-true (>= (second (s:node-bounds (tooltip-of screen))) (+ y h))))
      ;; Moved onto a widget with none, it goes.
      (rest-on screen :no-tip)
      (core:advance-virtual-clock clock 600000000)
      (draw screen)
      (assert-eq nil (tooltip-of screen)))))

(define-test a-press-puts-a-tooltip-away-until-the-pointer-comes-again ()
  (with-clock (clock)
    (let ((screen (show 'make-tips)))
      (rest-on screen :save)
      (core:advance-virtual-clock clock 600000000)
      (draw screen)
      (assert-true (tooltip-of screen))
      (click screen :save)
      (assert-eq nil (tooltip-of screen))
      (assert-equal "1 saves" (s:node-name (node-of screen :saves)))
      (core:advance-virtual-clock clock 600000000)
      (draw screen)
      (assert-eq nil (tooltip-of screen))
      (rest-on screen :no-tip)
      (rest-on screen :save)
      (core:advance-virtual-clock clock 600000000)
      (draw screen)
      (assert-true (tooltip-of screen)))))

(define-test a-label-and-a-disabled-button-have-tooltips-too ()
  (with-clock (clock)
    (let ((screen (show 'make-tips)))
      (dolist (each '((:plain "A label can have one too") (:locked "Not now")))
        (rest-on screen (first each))
        (core:advance-virtual-clock clock 600000000)
        (draw screen)
        (assert-equal (second each) (s:node-name (tooltip-of screen)))))))

;;; --- popovers and dialogs ---

(defun make-popovers ()
  "A button that opens a popover with a button in it, a label with a popover to its right, both open
at the start, and a button under where the popovers' catchers are that counts its presses."
  (state-let ((details t) (info t) (inner 0) (under 0))
    (lambda ()
      (column (:gap 8)
        ;; Above the popovers, which open below the bar, so that they are not over it.
        (label (format nil "~d under" under) :id :under-count)
        (when (button "Under" :id :under)
          (incf under))
        (row (:id :bar :gap 8)
          (when (button "Details" :id :details)
            (setf details t))
          (when (popover (:id :details-popover :open details :anchor :details)
                  (label "More about it" :id :about)
                  (when (button (format nil "Inner ~d" inner) :id :inner)
                    (incf inner)))
            (setf details nil))
          (label "Info" :id :info)
          (when (popover (:id :info-popover :open info :anchor :info :placement :right)
                  (label "Beside it" :id :beside))
            (setf info nil)))))))

(defun popover-ids (screen)
  (mapcar #'s:node-id (s:find-nodes (core:accessibility-tree (screen-app screen))
                                    :role :popover)))

(define-test two-popovers-can-be-open-at-once-and-each-is-dismissed-by-itself ()
  (let* ((screen (show 'make-popovers))
         (session (screen-session screen)))
    (assert-equal '(:details-popover :info-popover) (popover-ids screen))
    ;; Escape goes to the first; a press outside to the one on top, the last.
    (m:press-key session :escape)
    (assert-equal '(:info-popover) (popover-ids screen))
    (click screen :under)
    (assert-equal '() (popover-ids screen))
    (assert-equal "0 under" (s:node-name (node-of screen :under-count)))))

(define-test a-popover-floats-by-its-anchor-holds-widgets-and-a-press-outside-dismisses-it ()
  (let* ((screen (show 'make-popovers))
         (session (screen-session screen)))
    (m:press-key session :escape)
    (m:press-key session :escape)
    (assert-equal '() (popover-ids screen))
    (m:click session :id :details)
    (assert-equal '(:details-popover) (popover-ids screen))
    (destructuring-bind (x y w h) (s:node-bounds (node-of screen :details))
      (declare (ignore w))
      (let ((bounds (s:node-bounds (node-of screen :details-popover))))
        (assert-equal x (first bounds))
        (assert-true (>= (second bounds) (+ y h)))))
    (m:click session :id :inner)
    (assert-equal "Inner 1" (s:node-name (node-of screen :inner)))
    ;; A press outside, on a button: the popover goes, and the button is not pressed.
    (click screen :under)
    (assert-equal '() (popover-ids screen))
    (assert-equal "0 under" (s:node-name (node-of screen :under-count)))
    (click screen :under)
    (assert-equal "1 under" (s:node-name (node-of screen :under-count)))))

(define-test a-popover-put-to-the-right-is-beside-its-anchor ()
  (let ((screen (show 'make-popovers)))
    (destructuring-bind (x y w h) (s:node-bounds (node-of screen :info))
      (declare (ignore h))
      (assert-equal (list (+ x w) y)
                    (subseq (s:node-bounds (node-of screen :info-popover)) 0 2)))))

(defun make-popover-in-a-scroll ()
  (lambda ()
    (scroll (:id :page :height 200)
      (column (:gap 8)
        (label "Anchor" :id :anchor)
        (popover (:id :tip :open t :anchor :anchor)
          (label "Words" :id :words))
        (dotimes (i 30) (label (format nil "Line ~d" i) :id i))))))

(define-test the-wheel-over-a-popover-in-a-scroll-does-not-scroll-it ()
  (let* ((screen (show 'make-popover-in-a-scroll))
         (session (screen-session screen)))
    (m:scroll session :id :tip :dy -3)
    (assert-eql 0f0 (offset-of screen :page))))

(defun make-confirm ()
  "A field and a button behind a dialog that has two buttons."
  (state-let ((asking t) (answer "none") (behind 0))
    (lambda ()
      (column (:gap 8)
        (text-field "" :id :field)
        (when (button "Behind" :id :behind)
          (incf behind))
        (label (format nil "~a, ~d behind" answer behind) :id :answer)
        (when (dialog (:id :confirm :open asking :title "Delete the file?")
                (label "It cannot be brought back." :id :warning)
                (row (:gap 8)
                  (when (button "Cancel" :id :cancel)
                    (setf asking nil answer "cancel"))
                  (when (button "Delete" :id :delete)
                    (setf asking nil answer "delete"))))
          (setf asking nil answer "escape"))))))

(define-test a-dialog-keeps-the-pointer-and-the-keyboard-and-its-buttons-answer ()
  (let* ((screen (show 'make-confirm))
         (session (screen-session screen)))
    (draw screen)
    (let ((dialog (first (s:find-nodes (core:accessibility-tree (screen-app screen))
                                       :role :dialog))))
      (assert-equal "Delete the file?" (s:node-name dialog))
      ;; In the middle of the window (360 by 320).
      (destructuring-bind (x y w h) (s:node-bounds dialog)
        (assert-true (< (abs (- (+ x (/ w 2)) 180)) 1))
        (assert-true (< (abs (- (+ y (/ h 2)) 160)) 1))))
    ;; The keyboard is in it, and Tab goes round its two buttons only.
    (assert-true (s:node-property (node-of screen :cancel) :focused))
    (m:press-key session :tab)
    (assert-true (s:node-property (node-of screen :delete) :focused))
    (m:press-key session :tab)
    (assert-true (s:node-property (node-of screen :cancel) :focused))
    ;; A press on what is behind goes nowhere.
    (click screen :behind)
    (assert-equal "none, 0 behind" (s:node-name (node-of screen :answer)))
    (m:click session :id :delete)
    (assert-equal "delete, 0 behind" (s:node-name (node-of screen :answer)))
    (assert-eq nil (s:find-nodes (core:accessibility-tree (screen-app screen)) :role :dialog))))

(define-test escape-dismisses-a-dialog ()
  (let* ((screen (show 'make-confirm))
         (session (screen-session screen)))
    (draw screen)
    (m:press-key session :escape)
    (assert-equal "escape, 0 behind" (s:node-name (node-of screen :answer)))))

;;; --- a combo box ---

(defparameter +cities+ '("Amsterdam" "Berlin" "London" "Long Beach" "Lyon" "Madrid"))

(defun make-city ()
  (state-let ((city "") (picked nil))
    (lambda ()
      (column (:gap 8)
        (multiple-value-bind (text changed taken)
            (combo-box city (if (zerop (length city))
                                '()
                                (remove-if-not (lambda (c) (search city c :test #'char-equal))
                                               +cities+))
                       :id :city :width 200)
          (when changed (setf city text picked taken)))
        (label (format nil "~a~:[~;, picked~]" city picked) :id :said)))))

(define-test typing-in-a-combo-box-opens-the-suggestions-that-fit ()
  (let* ((screen (show 'make-city))
         (session (screen-session screen)))
    (assert-eq :combo-box (s:node-role (node-of screen :city)))
    (assert-eq nil (menu-of screen))
    ;; The first letter opens them (the app had none for the empty text).
    (m:type-text session "l" :id :city)
    (assert-equal '("Berlin" "London" "Long Beach" "Lyon") (menu-rows screen))
    (m:type-text session "o")
    (assert-equal '("London" "Long Beach") (menu-rows screen))
    ;; The menu is under the field.
    (destructuring-bind (x y w h) (s:node-bounds (node-of screen :city))
      (declare (ignore w))
      (assert-equal x (first (s:node-bounds (menu-of screen))))
      (assert-true (>= (second (s:node-bounds (menu-of screen))) (+ y h))))
    (m:type-text session "n")
    (assert-equal '("London" "Long Beach") (menu-rows screen))
    (m:type-text session "d")
    (assert-equal '("London") (menu-rows screen))
    (assert-equal "lond" (s:node-name (node-of screen :said)))))

(define-test the-arrows-and-return-or-a-press-take-a-suggestion ()
  (let* ((screen (show 'make-city))
         (session (screen-session screen)))
    (m:type-text session "lo" :id :city)
    (m:press-key session :down)
    (m:press-key session :down)
    (m:press-key session :return)
    (assert-equal "Long Beach, picked" (s:node-name (node-of screen :said)))
    (assert-equal "Long Beach" (s:node-value (node-of screen :city)))
    (assert-eq nil (menu-of screen))
    (m:set-value session "a" :id :city)
    (assert-equal '("Amsterdam" "Long Beach" "Madrid") (menu-rows screen))
    (m:click session :role :menu-item :name "Madrid")
    (assert-equal "Madrid, picked" (s:node-name (node-of screen :said)))))

(define-test escape-closes-the-suggestions-and-space-and-return-still-type ()
  (let* ((screen (show 'make-city))
         (session (screen-session screen)))
    (m:type-text session "l" :id :city)
    (assert-true (menu-of screen))
    (m:press-key session :escape)
    (assert-eq nil (menu-of screen))
    (assert-equal "l" (s:node-value (node-of screen :city)))
    ;; Down opens them again; Return with no row highlighted is the field's, and closes nothing.
    (m:press-key session :down)
    (assert-true (menu-of screen))
    (m:press-key session :up)
    (m:press-key session :up)
    (m:type-text session "o")
    (assert-true (menu-of screen))
    (m:type-text session "ng b")
    (assert-equal "long b" (s:node-value (node-of screen :city)))
    (assert-equal '("Long Beach") (menu-rows screen))
    (m:press-key session :return)
    (assert-equal "long b" (s:node-value (node-of screen :city)))))

;;; --- the system's file panels ---

(defun make-documents ()
  (state-let ((opened "nothing") (saved "nowhere") (opens 0))
    (lambda ()
      (column (:gap 8)
        (let ((path (open-file-dialog :id :open :types '("txt" "md")
                                      :show (button "Open..." :id :open-button))))
          (when path (setf opened path) (incf opens)))
        (label (format nil "~d opens" opens) :id :opens)
        (let ((path (save-file-dialog :id :save :types '("txt") :name "Untitled.txt"
                                      :show (button "Save as..." :id :save-button))))
          (when path (setf saved path)))
        (label (format nil "Opened ~a, saved ~a" opened saved) :id :said)))))

(define-test the-file-panels-are-asked-for-on-a-press-and-give-the-path-back ()
  (let* ((screen (show 'make-documents))
         (asked '()))
    (setf (core:context-file-chooser (screen-context screen))
          (lambda (kind types name)
            (push (list kind types name) asked)
            (ecase kind (:open "/notes/a.txt") (:save "/notes/b.txt"))))
    (assert-eq nil asked)
    (click screen :open-button)
    (assert-equal '((:open ("txt" "md") nil)) asked)
    (assert-equal "Opened /notes/a.txt, saved nowhere" (s:node-name (node-of screen :said)))
    (click screen :save-button)
    (assert-equal '(:save ("txt") "Untitled.txt") (first asked))
    (assert-equal "Opened /notes/a.txt, saved /notes/b.txt" (s:node-name (node-of screen :said)))
    ;; Asked once a press, and the path given back once, however many frames there are.
    (dotimes (i 3) (draw screen))
    (assert-equal 2 (length asked))
    (assert-equal "1 opens" (s:node-name (node-of screen :opens)))))

(define-test a-cancelled-file-panel-gives-nothing-back ()
  (let ((screen (show 'make-documents)))
    ;; With no chooser, a context with no window cancels.
    (click screen :open-button)
    (assert-equal "Opened nothing, saved nowhere" (s:node-name (node-of screen :said)))))

;;; --- toasts ---

(defun make-saver ()
  (state-let ((saves 0))
    (lambda ()
      (column (:gap 8)
        (when (button "Save" :id :save)
          (incf saves)
          (toast (format nil "Saved ~d" saves)))))))

(defun toasts-of (screen)
  (s:find-nodes (core:accessibility-tree (screen-app screen)) :role :toast))

(define-test a-toast-shows-at-the-bottom-for-a-while-and-goes ()
  (with-clock (clock)
    (let ((screen (show 'make-saver)))
      (assert-eq nil (toasts-of screen))
      (click screen :save)
      (let ((toast (first (toasts-of screen))))
        (assert-equal "Saved 1" (s:node-name toast))
        ;; In the middle, 24 points above the bottom of the 360 by 320 window.
        (destructuring-bind (x y w h) (s:node-bounds toast)
          (assert-true (< (abs (- (+ x (/ w 2)) 180)) 1))
          (assert-true (< (abs (- (+ y h) 296)) 1))))
      (assert-true (core:take-frame-request (screen-context screen)))
      (core:advance-virtual-clock clock 2900000000)
      (draw screen)
      (assert-equal 1 (length (toasts-of screen)))
      (assert-true (core:take-frame-request (screen-context screen)))
      (core:advance-virtual-clock clock 200000000)
      (draw screen)
      (assert-eq nil (toasts-of screen))
      (assert-false (core:take-frame-request (screen-context screen))))))

(define-test toasts-stack-the-newer-under-and-a-press-puts-one-away ()
  (with-clock (clock)
    (let* ((screen (show 'make-saver))
           (session (screen-session screen)))
      (click screen :save)
      (core:advance-virtual-clock clock 100000000)
      (click screen :save)
      (let ((toasts (toasts-of screen)))
        (assert-equal '("Saved 1" "Saved 2") (mapcar #'s:node-name toasts))
        (assert-true (< (second (s:node-bounds (first toasts)))
                        (second (s:node-bounds (second toasts))))))
      (m:click session :role :toast :name "Saved 1")
      (assert-equal '("Saved 2") (mapcar #'s:node-name (toasts-of screen))))))

;;; --- the gallery of every widget ---

(defparameter +every-widget+
  ;; What each widget and container that is not only layout is in the semantic tree. A widget added
  ;; to the library is added here and to the gallery (examples/gallery.lisp).
  '((label . :static-text) (paragraph . :paragraph) (button . :button) (text-field . :text-field)
    (text-area . :text-area) (slider . :slider) (choice . :radio-group) (checkbox . :checkbox)
    (list-box . :list) (progress . :progress-bar) (canvas . :canvas) (scrim . :overlay)
    (switch . :switch) (spinner . :busy-indicator) (dropdown . :pop-up-button)
    (menu-button . :menu-button) (popover . :popover) (dialog . :dialog) (toast . :toast)
    (combo-box . :combo-box) (grid . :grid) (pressable . :toggle-button)
    (scroll . :scroll-area) (lazy-list . :scroll-area)))

(defun roles-in (screen)
  (let ((roles '()))
    (s:walk-nodes (lambda (node) (pushnew (s:node-role node) roles))
                  (core:accessibility-tree (screen-app screen)))
    roles))

(defun scroll-into-sight (session screen container id)
  "Turn the wheel over CONTAINER until the thing ID is not hidden."
  (loop :repeat 100
        :while (s:node-property (node-of screen id) :hidden)
        :do (m:scroll session :id container :dy -2)))

(define-test the-gallery-has-one-of-every-widget ()
  (let* ((screen (show 'gallery:make-gallery))
         (session (screen-session screen)))
    ;; The spinner is there while Busy is on, and the scrim while the overlay shows.
    (scroll-into-sight session screen :page :busy)
    (m:set-value session t :id :busy)
    (scroll-into-sight session screen :page :show-toast)
    (let ((roles '()))
      (flet ((note-roles () (setf roles (union roles (roles-in screen)))))
        (m:click session :id :show-popover)
        (note-roles)
        (m:press-key session :escape)
        (m:click session :id :show-dialog)
        (note-roles)
        (m:click session :id :keep)
        (assert-equal "Kept." (s:node-name (node-of screen :dialog-answer)))
        (m:click session :id :show-toast)
        (note-roles)
        (m:click session :id :show-overlay)
        (note-roles))
      (loop :for (widget . role) :in +every-widget+
            :do (assert-true (member role roles) widget role)))
    (assert-eql 10000 (s:node-property (node-of screen :tasks) :row-count))
    (assert-eq :scroll-area (s:node-role (node-of screen :inner)))
    (m:click session :id :close)
    (assert-false (member :overlay (roles-in screen)))))

(defparameter +every-call+
  ;; Each widget, container and function of the library whose call the gallery shows, under it.
  '(label paragraph button menu-button text-field combo-box text-area slider choice dropdown checkbox
    switch spinner list-box progress canvas scrim spacer row column stack padding panel center grid
    pressable scroll lazy-list context-menu popover dialog toast open-file-dialog save-file-dialog))

(defun call-of (operator)
  "How OPERATOR is called, as the gallery writes it: its lambda list, without the defaults that only
mean nothing was given (the id that is required, and a pressable that is not a toggle)."
  (labels ((tidy (parameter)
             (if (and (consp parameter)
                      (or (eq (second parameter) :none)
                          (and (consp (second parameter))
                               (string= (first (second parameter)) "REQUIRED-ID"))))
                 (first parameter)
                 parameter)))
    (let* ((list (function-lambda-list (or (macro-function operator) (fdefinition operator))))
           (list (if (consp (first list))
                     (cons (mapcar #'tidy (first list)) (rest list))
                     (mapcar #'tidy list)))
           (*package* (symbol-package operator))
           (*print-pretty* nil))
      (format nil "~(bolt:~a~{ ~s~}~)" operator list))))

(define-test the-gallery-shows-how-each-widget-is-called ()
  (let ((screen (show 'gallery:make-gallery))
        (shown '()))
    (s:walk-nodes (lambda (node)
                    (let ((name (s:node-name node)))
                      (when (and (stringp name) (eql 0 (search "(bolt:" name)))
                        (push name shown))))
                  (core:accessibility-tree (screen-app screen)))
    (dolist (operator +every-call+)
      (assert-eql 1 (count (format nil "(~a)" (call-of operator)) shown :test #'string=)
                  operator))
    ;; And no call of something that is not in the list, or that is not called so any more.
    (assert-eql (length +every-call+) (length shown))))

(define-test what-the-gallerys-widgets-return-is-shown-under-them ()
  (let* ((screen (show 'gallery:make-gallery))
         (session (screen-session screen)))
    (flet ((says (id) (s:node-name (node-of screen id))))
      (scroll-into-sight session screen :page :presses)
      (m:click session :id :press)
      (m:click session :id :press)
      (assert-equal "Pressed 2 times." (says :presses))
      ;; What is out of sight is not typed into.
      (handler-case (progn (m:set-value session "" :id :notes)
                           (assert-true nil "a hidden text area was typed into"))
        (m:cannot () t))
      (scroll-into-sight session screen :page :greeting)
      (m:set-value session "" :id :name)
      (assert-true (s:node-property (node-of screen :name) :invalid))
      (m:set-value session "Grace" :id :name)
      (assert-equal "Hello, Grace." (says :greeting))
      (scroll-into-sight session screen :page :volume-note)
      (m:set-value session 75 :id :volume)
      (assert-equal "75%" (says :volume-note))
      (assert-true (< 0.7d0 (s:node-value (node-of screen :loudness)) 0.8d0))
      (scroll-into-sight session screen :page :dots)
      (m:click session :id :canvas)
      (assert-equal "1 dot." (says :dots))
      (scroll-into-sight session screen :page :done-count)
      (m:click session :role :checkbox :name "Task 1")
      (assert-equal "1 done." (says :done-count)))))

;;; --- copy, cut and paste ---

(defun clipboard (screen)
  "What the screen's context holds as the clipboard: a context made without a window keeps it, and
never touches the person's."
  (core:context-clipboard-text (screen-context screen)))

(define-test a-field-copies-cuts-and-pastes-with-the-command-keys ()
  (let* ((screen (show 'todo:make-todo))
         (session (screen-session screen)))
    (m:type-text session "Water" :id :draft)
    ;; Nothing is selected, so there is nothing to copy.
    (m:press-key session :c :modifiers '(:super))
    (assert-eq nil (clipboard screen))
    (m:press-key session :a :modifiers '(:super))
    (m:press-key session :c :modifiers '(:super))
    (assert-equal "Water" (clipboard screen))
    (assert-equal "Water" (text-in screen :draft))
    (m:press-key session :right)
    (m:press-key session :v :modifiers '(:super))
    (assert-equal "WaterWater" (text-in screen :draft))
    (m:press-key session :a :modifiers '(:super))
    (m:press-key session :x :modifiers '(:super))
    (assert-equal "" (text-in screen :draft))
    (assert-equal "WaterWater" (clipboard screen))
    ;; What was cut is the app's to see, as typing is.
    (assert-equal "" (s:node-value (m:find-node session :id :draft)))
    (m:press-key session :v :modifiers '(:super))
    (m:press-key session :return)
    (assert-equal "WaterWater" (text-in screen 4))))

(define-test a-line-break-pasted-into-a-field-is-a-space ()
  (let* ((screen (show 'todo:make-todo))
         (session (screen-session screen)))
    (core:context-set-clipboard-text (screen-context screen)
                                     (format nil "Feed~%the~c~ccat" #\Return #\Linefeed))
    (m:focus session :id :draft)
    (m:press-key session :v :modifiers '(:super))
    (m:press-key session :return)
    (assert-equal "Feed the cat" (text-in screen 4))))

(define-test the-list-is-a-tree-of-check-boxes-named-by-what-is-to-be-done ()
  (let* ((screen (show 'todo:make-todo))
         (boxes (s:find-nodes (core:accessibility-tree (screen-app screen)) :role :checkbox)))
    (assert-equal '("Read the README" "Open a window from the REPL" "Change a label and compile it")
                  (mapcar #'s:node-name boxes))
    (assert-equal '(t nil nil) (mapcar #'s:node-value boxes))
    (click screen 2)
    (assert-equal '(t t nil)
                  (mapcar #'s:node-value
                          (s:find-nodes (core:accessibility-tree (screen-app screen))
                                        :role :checkbox)))
    (let ((field (first (s:find-nodes (core:accessibility-tree (screen-app screen))
                                      :role :text-field))))
      (assert-eq :draft (s:node-id field))
      (assert-equal "" (s:node-value field)))))

(define-test everything-the-public-package-exports-is-something ()
  (let ((nothing '()))
    (do-external-symbols (symbol :almighty-bolt)
      (unless (or (fboundp symbol) (boundp symbol) (find-class symbol nil))
        (push symbol nothing)))
    (assert-equal '() nothing)))

;;; --- a scroll container ---

(defun make-shelf ()
  "A button, a scroll container 100 points tall with ten buttons of 28 inside, and a button below
that leaves two of them. Each button counts its presses."
  (state-let ((presses (make-list 10 :initial-element 0)) (above 0) (count 10))
    (lambda ()
      (column ()
        (when (button (format nil "Above ~d" above) :id :above)
          (incf above))
        (scroll (:id :shelf :height 100)
          (dotimes (i count)
            (when (button (format nil "B~d ~d" i (nth i presses)) :id i)
              (incf (nth i presses)))))
        (when (button "Fewer" :id :fewer)
          (setf count 2))))))

(defun node-of (screen id)
  (first (s:find-nodes (core:accessibility-tree (screen-app screen)) :id id)))

(defun offset-of (screen id)
  (s:node-property (node-of screen id) :offset))

(define-test the-wheel-moves-a-scroll-container-and-what-is-out-of-its-window-is-hidden ()
  (let* ((screen (show 'make-shelf))
         (session (screen-session screen)))
    (assert-eq :scroll-area (s:node-role (node-of screen :shelf)))
    ;; As wide as the column lets it be (the screen), not as its buttons: its bar is beside them.
    (assert-eql 360f0 (third (s:node-bounds (node-of screen :shelf))))
    (assert-eql 0f0 (offset-of screen :shelf))
    (assert-true (s:node-property (node-of screen 5) :hidden))
    (assert-eq nil (core:lookup-target (screen-app screen) 5))
    ;; Three lines down, 24 points each: the sixth button, 140 points into the shelf, now shows.
    (m:scroll session :id :shelf :dy -3)
    (assert-eql 72f0 (offset-of screen :shelf))
    (assert-false (s:node-property (node-of screen 5) :hidden))
    (click screen 5)
    (assert-equal "B5 1" (text-in screen 5))
    ;; No further than the end: ten buttons of 28 in a window of 100.
    (m:scroll session :id :shelf :dy -100)
    (assert-eql 180f0 (offset-of screen :shelf))
    (m:scroll session :id :shelf :dy 100)
    (assert-eql 0f0 (offset-of screen :shelf))))

(define-test what-is-scrolled-out-of-the-window-takes-no-press ()
  (let* ((screen (show 'make-shelf))
         (session (screen-session screen)))
    (m:scroll session :id :shelf :dy -3)
    ;; The second button is now above the window, where the first button of the screen is; a press
    ;; there is that button's.
    (click screen :above)
    ;; (By name: the glyphs of what is out of the window are in the display list, under its clip.)
    (assert-equal "Above 1" (s:node-name (node-of screen :above)))
    (assert-true (s:node-property (node-of screen 1) :hidden))
    (assert-equal "B1 0" (s:node-name (node-of screen 1)))
    (handler-case (progn (m:click session :id 1)
                         (assert-true nil "a hidden button was clicked"))
      (m:cannot (condition)
        (assert-true (search "scrolled out of sight" (princ-to-string condition)))))))

(define-test the-keyboard-coming-to-something-out-of-the-window-scrolls-it-into-view ()
  (let* ((screen (show 'make-shelf))
         (session (screen-session screen)))
    ;; The button above, then the first six in the shelf. (A press does not give a button the keyboard.)
    (dotimes (i 7) (m:press-key session :tab))
    (assert-true (s:node-property (node-of screen 5) :focused))
    ;; As little as shows all of it and its ring: its bottom, 168 points in, 2 points above the
    ;; bottom of the window.
    (assert-eql 70f0 (offset-of screen :shelf))
    (assert-false (s:node-property (node-of screen 5) :hidden))
    ;; Back up to the second: its top, 28 points in, 2 points below the top of the window.
    (dotimes (i 4) (m:press-key session :tab :modifiers '(:shift)))
    (assert-eql 26f0 (offset-of screen :shelf))
    ;; And the first: the top of the window, as there is nothing above it.
    (m:press-key session :tab :modifiers '(:shift))
    (assert-eql 0f0 (offset-of screen :shelf))))

(define-test a-scroll-container-whose-contents-shrink-goes-back-to-the-top ()
  (let* ((screen (show 'make-shelf))
         (session (screen-session screen)))
    (m:scroll session :id :shelf :dy -100)
    (assert-eql 180f0 (offset-of screen :shelf))
    ;; In the frame of the press itself, not one after: what is left shows at once.
    (click screen :fewer)
    (assert-eql 0f0 (offset-of screen :shelf))
    (assert-false (s:node-property (node-of screen 1) :hidden))
    (assert-equal "B1 0" (text-in screen 1))))

(defun make-nested-scroll ()
  (state-let ((picked 0))
    (lambda ()
      (scroll (:id :outer :height 80)
        (multiple-value-bind (index changed)
            (list-box '("One" "Two" "Three" "Four" "Five") picked :id :inner :rows 2)
          (when changed (setf picked index)))
        (dotimes (i 4)
          (button (format nil "B~d" i) :id i))))))

(define-test the-wheel-goes-to-a-list-inside-a-scroll-container-first ()
  (let* ((screen (show 'make-nested-scroll))
         (session (screen-session screen)))
    (m:scroll session :id :inner :dy -2)
    (assert-eql 2 (s:node-property (node-of screen :inner) :first-visible))
    (assert-eql 0f0 (offset-of screen :outer))
    ;; Over a button inside, the container takes it.
    (m:scroll session :id 0 :dy -1)
    (assert-eql 24f0 (offset-of screen :outer))
    (assert-eql 2 (s:node-property (node-of screen :inner) :first-visible))))

(define-test the-wheel-over-a-list-at-an-end-moves-the-container-it-is-in ()
  (let* ((screen (show 'make-nested-scroll))
         (session (screen-session screen)))
    (m:scroll session :id 0 :dy -1)
    (assert-eql 24f0 (offset-of screen :outer))
    ;; Up over the list at its top: the container goes up.
    (m:scroll session :id :inner :dy 1)
    (assert-eql 0f0 (offset-of screen :outer))
    (assert-eql 0 (s:node-property (node-of screen :inner) :first-visible))
    ;; Down over the list at its bottom: the container goes down.
    (m:scroll session :id :inner :dy -3)
    (assert-eql 3 (s:node-property (node-of screen :inner) :first-visible))
    (assert-eql 0f0 (offset-of screen :outer))
    (m:scroll session :id :inner :dy -1)
    (assert-eql 24f0 (offset-of screen :outer))
    (assert-eql 3 (s:node-property (node-of screen :inner) :first-visible))))

(defun make-scroll-in-a-scroll ()
  (lambda ()
    (scroll (:id :page :height 100)
      (scroll (:id :box :height 50)
        (dotimes (i 3)
          (button (format nil "In ~d" i) :id (+ 100 i))))
      (dotimes (i 6)
        (button (format nil "B~d" i) :id i)))))

(define-test the-wheel-over-a-scroll-container-at-an-end-moves-the-one-it-is-in ()
  (let* ((screen (show 'make-scroll-in-a-scroll))
         (session (screen-session screen)))
    (m:scroll session :id :box :dy -100)
    (assert-true (plusp (offset-of screen :box)))
    (assert-eql 0f0 (offset-of screen :page))
    (let ((bottom (offset-of screen :box)))
      (m:scroll session :id :box :dy -1)
      (assert-eql 24f0 (offset-of screen :page))
      (assert-eql bottom (offset-of screen :box))
      ;; Back up over the inner one, which can go up: it does, and the outer one stays.
      (m:scroll session :id :box :dy 1)
      (assert-true (< (offset-of screen :box) bottom))
      (assert-eql 24f0 (offset-of screen :page)))))

(defun make-text-area-in-a-scroll ()
  (state-let ((notes "Short"))
    (lambda ()
      (scroll (:id :page :height 80)
        (multiple-value-bind (new changed) (text-area notes :id :notes :width 200 :rows 2)
          (when changed (setf notes new)))
        (dotimes (i 4)
          (button (format nil "B~d" i) :id i))))))

(define-test the-wheel-over-a-text-area-whose-lines-all-show-moves-the-container-it-is-in ()
  (let* ((screen (show 'make-text-area-in-a-scroll))
         (session (screen-session screen)))
    (m:scroll session :id :notes :dy -1)
    (assert-eql 24f0 (offset-of screen :page))
    (assert-eql 0f0 (offset-of screen :notes))))

;;; --- a list that builds only the rows that show ---

(defstruct (entry (:constructor make-entry (id text))) id text (presses 0) (note ""))

(defun make-long-list ()
  "A hundred thousand rows of 28 points, five of which show, each a button that counts its presses
and says its entry's number, and a field for a note; Return in a note puts a new entry at the top."
  (state-let ((entries (let ((vector (make-array 100000)))
                         (dotimes (i 100000 vector)
                           (setf (aref vector i) (make-entry i (format nil "E~d" i))))))
              (next 100000))
    (lambda ()
      (lazy-list (entry entries :id :entries :key #'entry-id :row-height 28 :height 140)
        (row (:gap 4)
          (when (button (format nil "~a ~d" (entry-text entry) (entry-presses entry)) :id :press)
            (incf (entry-presses entry)))
          (multiple-value-bind (text changed submitted)
              (text-field (entry-note entry) :id :note :width 60)
            (when changed (setf (entry-note entry) text))
            (when submitted
              (setf entries (concatenate 'vector (vector (make-entry next "New")) entries))
              (incf next))))))))

(defun built-rows (screen)
  "The ids of the rows of the long list that were described, in order."
  (mapcar #'s:node-id (s:node-children (node-of screen :entries))))

(defun row-node (screen row id)
  (first (s:find-nodes (first (s:find-nodes (node-of screen :entries) :id row)) :id id)))

(define-test a-lazy-list-describes-only-the-rows-that-show ()
  (let* ((screen (show 'make-long-list))
         (session (screen-session screen)))
    (assert-equal '(0 1 2 3 4) (built-rows screen))
    (assert-eql 100000 (s:node-property (node-of screen :entries) :row-count))
    (assert-eql 2800000f0 (s:node-property (node-of screen :entries) :content-height))
    ;; A thousand lines of 24 points: 24000 points, the row at 24000 / 28 = 857 and a bit at the top.
    (m:scroll session :id :entries :dy -1000)
    (assert-eql 24000f0 (offset-of screen :entries))
    (assert-equal '(857 858 859 860 861 862) (built-rows screen))
    (assert-eql 857 (s:node-property (node-of screen :entries) :first-row))
    (m:click session :id :press :name "E859 0")
    (assert-true (s:find-nodes (node-of screen :entries) :name "E859 1"))
    ;; The end: the last five rows fill the window.
    (m:scroll session :id :entries :dy -1000000)
    (assert-eql (- 2800000f0 140f0) (offset-of screen :entries))
    (assert-equal '(99995 99996 99997 99998 99999) (built-rows screen))))

;;; --- the ring round what has the keyboard, at the edge of a scroll ---

(defun make-framed (kind)
  "Twenty points in from the window's edges, a lazy list (KIND :lazy-list) or a scroll container
(:scroll) 140 points tall of fifty buttons 28 points tall, the first called :press."
  (let ((items (loop :for i :below 50 :collect i)))
    (lambda ()
      (padding (:all 20)
        (ecase kind
          (:lazy-list
           (lazy-list (i items :id :frame :row-height 28 :height 140)
             (button (format nil "Task ~d" i) :id (if (zerop i) :press i))))
          (:scroll
           (scroll (:id :frame :height 140)
             (dolist (i items)
               (button (format nil "Task ~d" i) :id (if (zerop i) :press i))))))))))

(defun color-at (screen x y)
  "The colour of the last rectangle on SCREEN's display list that covers the point X, Y and is drawn
inside every clip it is in. (A point away from the corners of a rounded one.)"
  (let ((list (screen-list screen)) (clips '()) (color nil))
    (flet ((over (index)
             (let ((left (core:display-list-x list index)) (top (core:display-list-y list index)))
               (and (<= left x) (< x (+ left (core:display-list-width list index)))
                    (<= top y) (< y (+ top (core:display-list-height list index)))))))
      (dotimes (index (core:display-list-count list) color)
        (let ((kind (core:display-list-kind list index)))
          (cond ((= kind core:+command-clip+) (push index clips))
                ((= kind core:+command-unclip+) (pop clips))
                ((and (or (= kind core:+command-rect+) (= kind core:+command-rounded-rect+))
                      (over index) (every #'over clips))
                 (setf color (core:display-list-color list index)))))))))

(define-test the-ring-of-what-has-the-keyboard-shows-past-the-edge-of-a-scroll-or-a-lazy-list ()
  ;; The ring is drawn 2 points outside what has the keyboard, and the container clips what is in
  ;; it to its edges; the first button is at its top left, with nothing between.
  (dolist (kind '(:lazy-list :scroll))
    (let* ((screen (show (lambda () (make-framed kind))))
           (session (screen-session screen))
           (accent (theme:theme-ring (theme:find-theme :dark))))
      (m:press-key session :tab)
      (assert-true (s:node-property (node-of screen :press) :focused) kind)
      (destructuring-bind (fx fy fw fh) (s:node-bounds (node-of screen :frame))
        (declare (ignore fw fh))
        (destructuring-bind (x y w h) (s:node-bounds (node-of screen :press))
          (assert-equal (list fx fy) (list x y) kind)
          ;; Its ring, 1 point to the left of the container and 1 above it.
          (assert-eql accent (color-at screen (- fx 1) (+ y (/ h 2))) kind)
          (assert-eql accent (color-at screen (+ x (/ w 2)) (- fy 1)) kind)
          ;; Half a line (12 points) further down, it is partly out of the window: nothing of it is
          ;; above the container (what is there is the window's background, as at its corner), and
          ;; the ring is at the left of what shows.
          (m:scroll session :id :frame :dy -0.5)
          (assert-eql 12f0 (offset-of screen :frame) kind)
          (assert-true (s:node-property (node-of screen :press) :focused) kind)
          (assert-eql (color-at screen 5 5) (color-at screen (+ x (/ w 2)) (- fy 1)) kind)
          (assert-eql accent (color-at screen (- fx 1) (+ fy 8)) kind))))))

(define-test what-is-kept-for-a-row-stays-with-its-item-when-the-items-move ()
  (let* ((screen (show 'make-long-list))
         (session (screen-session screen)))
    ;; (Every row has a field called :note, so the first row's is pressed where it is.)
    (destructuring-bind (x y width height) (s:node-bounds (row-node screen 0 :note))
      (let ((px (+ x (/ width 2))) (py (+ y (/ height 2))))
        (pointer screen :move px py)
        (pointer screen :down px py :button 0 :buttons 1)
        (pointer screen :up px py :button 0)))
    (assert-true (s:node-property (row-node screen 0 :note) :focused))
    (m:type-text session "abc")
    (m:press-key session :left)
    (m:press-key session :return)
    ;; A new entry is at the top; the field that has the keyboard, and its caret, are the first
    ;; entry's still, one row down.
    (assert-equal '(100000 0 1 2 3) (built-rows screen))
    (assert-false (s:node-property (row-node screen 100000 :note) :focused))
    (assert-true (s:node-property (row-node screen 0 :note) :focused))
    (assert-equal "abc" (s:node-value (row-node screen 0 :note)))
    (assert-eql 2 (s:node-property (row-node screen 0 :note) :caret))))

(defun make-flexible-list ()
  (lambda ()
    (column (:fill t)
      (label "Top" :id :top)
      (lazy-list (n (loop :for i :below 50 :collect i) :id :numbers :row-height 20 :height 100
                      :flex 1)
        (label (princ-to-string n) :id :n)))))

(define-test a-lazy-list-taller-than-it-guessed-asks-for-the-frame-that-fills-it ()
  ;; Before its first layout the list takes itself to be its :height, 100, and describes five rows;
  ;; the column gives it the rest of the screen, 320 - 14 = 306 points, room for 16 rows of 20.
  (let ((screen (show 'make-flexible-list)))
    (assert-eql 5 (length (built-rows-of screen :numbers)))
    (assert-true (core:take-frame-request (screen-context screen)))
    (draw screen)
    (assert-eql 16 (length (built-rows-of screen :numbers)))
    (assert-false (core:take-frame-request (screen-context screen)))))

(defun built-rows-of (screen id)
  (mapcar #'s:node-id (s:node-children (node-of screen id))))

;;; --- several lines of text ---

(defun make-notes (&optional (text ""))
  "A text area 200 points wide (23 characters of the test context's 8 points, inside its padding)
and three lines of 14 tall, and a count of its lines."
  (state-let ((notes text))
    (lambda ()
      (column (:gap 4)
        (multiple-value-bind (new changed) (text-area notes :id :notes :width 200 :rows 3)
          (when changed (setf notes new)))
        (label (format nil "~d lines" (1+ (count #\Linefeed notes))) :id :count)))))

(defun notes (screen) (node-of screen :notes))

(defun caret-of (screen) (s:node-property (notes screen) :caret))

(define-test return-in-a-text-area-starts-a-new-line-and-the-app-has-the-text ()
  (let* ((screen (show 'make-notes))
         (session (screen-session screen)))
    (assert-eq :text-area (s:node-role (notes screen)))
    (m:type-text session (format nil "one~%two") :id :notes)
    (assert-equal (format nil "one~%two") (s:node-value (notes screen)))
    (assert-equal "2 lines" (text-in screen :count))
    (m:set-value session (format nil "a~%b~%c") :id :notes)
    (assert-equal "3 lines" (text-in screen :count))))

(define-test up-and-down-move-a-line-and-keep-to-where-they-started ()
  (let* ((screen (show 'make-notes))
         (session (screen-session screen)))
    (m:type-text session (format nil "abcdef~%ab~%abcdef") :id :notes)
    (assert-eql 16 (caret-of screen))
    ;; Up to the short line, at its end; up again to the first, where the caret was across.
    (m:press-key session :up)
    (assert-eql 9 (caret-of screen))
    (m:press-key session :up)
    (assert-eql 6 (caret-of screen))
    (m:press-key session :down)
    (m:press-key session :down)
    (assert-eql 16 (caret-of screen))
    ;; Up from the first line goes to the start, and Command with Left and Right to the ends of a line.
    (m:press-key session :up :modifiers '(:super))
    (assert-eql 0 (caret-of screen))
    (m:press-key session :down)
    (m:press-key session :right :modifiers '(:super))
    (assert-eql 9 (caret-of screen))
    (m:press-key session :left :modifiers '(:super))
    (assert-eql 7 (caret-of screen))
    (m:press-key session :up :modifiers '(:shift))
    (assert-equal '(0 7) (s:node-property (notes screen) :selection))))

(define-test a-long-line-goes-on-in-the-next ()
  (let* ((screen (show 'make-notes))
         (session (screen-session screen)))
    (m:type-text session (make-string 30 :initial-element #\a) :id :notes)
    (m:press-key session :up :modifiers '(:super))
    (m:press-key session :down)
    ;; 23 characters to a line: down from the first is the 24th, which starts the second line; down
    ;; from there is the end.
    (assert-eql 23 (caret-of screen))
    (m:press-key session :down)
    (assert-eql 30 (caret-of screen))
    (assert-equal "1 lines" (text-in screen :count))
    ;; And it is drawn so: all thirty inside the area, on two lines; from the first frame, too.
    (assert-equal (make-string 30 :initial-element #\a) (text-in screen :notes))
    (let ((screen (show (lambda () (make-notes (make-string 30 :initial-element #\b))))))
      (assert-equal (make-string 30 :initial-element #\b) (text-in screen :notes)))))

(define-test a-press-puts-the-caret-on-the-line-it-is-on ()
  (let ((screen (show (lambda () (make-notes (format nil "abcdef~%ab~%abcdef"))))))
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :notes)
      ;; The second line is 6 + 14 points down; a character in from the padding of 8.
      (let ((px (+ x 8 8)) (py (+ y 6 14 7)))
        (pointer screen :move px py)
        (pointer screen :down px py :button 0 :buttons 1)
        (pointer screen :up px py :button 0)))
    (assert-true (s:node-property (notes screen) :focused))
    (assert-eql 8 (caret-of screen))))

(define-test a-text-area-scrolls-to-keep-the-caret-in-sight-and-the-wheel-scrolls-it ()
  (let* ((screen (show 'make-notes))
         (session (screen-session screen)))
    (m:type-text session (format nil "1~%2~%3~%4~%5") :id :notes)
    ;; Five lines of 14 in a window of three: the last at the bottom.
    (assert-eql 28f0 (s:node-property (notes screen) :offset))
    (m:scroll session :id :notes :dy 1)
    (assert-eql 4f0 (s:node-property (notes screen) :offset))
    (assert-eql 9 (caret-of screen))
    (m:press-key session :up :modifiers '(:super))
    (assert-eql 0f0 (s:node-property (notes screen) :offset))))

(define-test pasting-into-a-text-area-keeps-the-line-breaks ()
  (let* ((screen (show 'make-notes))
         (session (screen-session screen)))
    (core:context-set-clipboard-text (screen-context screen)
                                     (format nil "a~c~cb~cc" #\Return #\Linefeed #\Return))
    (m:focus session :id :notes)
    (m:press-key session :v :modifiers '(:super))
    (assert-equal (format nil "a~%b~%c") (s:node-value (notes screen)))
    (m:press-key session :a :modifiers '(:super))
    (m:press-key session :x :modifiers '(:super))
    (assert-equal (format nil "a~%b~%c") (clipboard screen))
    (assert-equal "" (s:node-value (notes screen)))))

;;; --- what a screen reader reads ---
;;;
;;; The app is drawn as above, and a real NSView in a real NSWindow (not shown) is given its tree and
;;; the means to press, as the host gives them. Everything is then asked as AppKit asks it of the
;;; view: for its children, and of each child for its role, name, value, place and what it does when
;;; it is pressed. What VoiceOver does with the answers is for a person to try.

(defstruct (reader (:constructor make-reader (view window bridge screen)))
  view window bridge screen
  ;; The notifications that the bridge was asked to post, newest first: (element kind), or (element kind
  ;; elements) for a layout change that names the elements that moved.
  (told '())
  ;; The texts that it was asked to have read out, newest first.
  (spoken '()))

(defun call-with-reader (screen function)
  "Call FUNCTION with a reader of SCREEN, whose bridge says that a screen reader is running and
notes what it is asked to post. After each frame the reader's bridge is told, as the host does."
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication")
      (let* ((bridge nil)
             (view (view:make-metal-view
                    :width 360 :height 320
                    :accessibility-tree (lambda () (core:accessibility-tree (screen-app screen)))
                    :accessibility-press (lambda (x y)
                                           (dolist (spec '((:move nil 0 0) (:down 0 1 1) (:up 0 0 1)))
                                             (destructuring-bind (kind button buttons clicks) spec
                                               (core:handle-event
                                                (screen-app screen)
                                                (core:make-pointer-event
                                                 :timestamp 0 :kind kind :x (float x 1d0)
                                                 :y (float y 1d0) :button button :buttons buttons
                                                 :click-count clicks))))
                                           (dotimes (i 4) (draw screen)))))
             (window (window:make-window :title "reader" :width 360 :height 320))
             (reader (make-reader (view:metal-view-object view) (window:window-object window) nil screen)))
        (setf bridge (view:metal-view-bridge view)
              (reader-bridge reader) bridge
              (a11y:bridge-assistive bridge) (constantly t)
              (a11y:bridge-poster bridge) (lambda (element kind &optional elements)
                                            (push (if elements
                                                      (list element kind elements)
                                                      (list element kind))
                                                  (reader-told reader)))
              (a11y:bridge-announcer bridge) (lambda (view text)
                                               (declare (ignore view))
                                               (push text (reader-spoken reader)))
              (screen-after-draw screen) (lambda () (a11y:frame-presented bridge)))
        (window:set-content-view window (reader-view reader))
        (unwind-protect (funcall function reader)
          (setf (screen-after-draw screen) nil)
          (a11y:release-bridge bridge))))))

(defmacro with-reader ((reader screen) &body body)
  `(call-with-reader ,screen (lambda (,reader) ,@body)))

(defun ns-list (array)
  (if (zerop array)
      '()
      (loop :for index :below (objc:msg-send :ulong array "count")
            :collect (objc:msg-send :id array "objectAtIndex:" (:ulong index)))))

(defun ns-value (object)
  "What an NSString (a Lisp string) or an NSNumber (a double) holds, or NIL for nothing."
  (cond ((zerop object) nil)
        ((objc:msg-send :bool object "isKindOfClass:" (:id (objc:objc-class "NSString")))
         (objc:lisp-string object))
        ((objc:msg-send :bool object "isKindOfClass:" (:id (objc:objc-class "NSNumber")))
         (objc:msg-send :double object "doubleValue"))
        (t :something-else)))

(defun elements (reader)
  "What the view says its accessibility children are."
  (ns-list (objc:msg-send :id (reader-view reader) "accessibilityChildren")))

(defun element-named (reader id)
  "The child of the view with the accessibility identifier of ID (an id of the app)."
  (let ((name (string-downcase (princ-to-string id))))
    (find name (elements reader)
          :key (lambda (element)
                 (ns-value (objc:msg-send :id element "accessibilityIdentifier")))
          :test #'equal)))

(defun says (element what)
  (ns-value (objc:msg-send :id element (ecase what
                                         (:role "accessibilityRole")
                                         (:label "accessibilityLabel")
                                         (:value "accessibilityValue")))))

(defun enabled-p (element)
  (objc:msg-send :bool element "isAccessibilityEnabled"))

(defun press (element)
  (objc:msg-send :bool element "accessibilityPerformPress"))

(defun told (reader)
  "What was posted since this was last asked, oldest first."
  (reverse (prog1 (reader-told reader) (setf (reader-told reader) '()))))

(defun spoken (reader)
  "What was asked to be read out since this was last asked, oldest first."
  (reverse (prog1 (reader-spoken reader) (setf (reader-spoken reader) '()))))

(define-test a-screen-reader-reads-the-counter-as-a-number-and-a-button ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (assert-equal 2 (length (elements reader)))
      (let ((count (element-named reader :count))
            (button (element-named reader :count-button)))
        (assert-equal "AXStaticText" (says count :role))
        (assert-equal "0" (says count :value))
        (assert-equal "AXButton" (says button :role))
        (assert-equal "Count" (says button :label))
        (assert-true (enabled-p button))))))

(define-test the-window-reaches-the-elements-through-appkit ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let* ((children (ns-list (objc:msg-send :id (reader-window reader)
                                               "accessibilityAttributeValue:"
                                               (:id (objc:ns-string "AXChildren")))))
             (identifiers (mapcar (lambda (element)
                                    (and (objc:msg-send :bool element "respondsToSelector:"
                                                        (:id (objc:register-selector
                                                              "accessibilityIdentifier")))
                                         (ns-value (objc:msg-send :id element
                                                                  "accessibilityIdentifier"))))
                                  children)))
        (assert-true (member "count" identifiers :test #'equal))
        (assert-true (member "count-button" identifiers :test #'equal))))))

(define-test pressing-the-button-as-a-screen-reader-does-counts ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((button (element-named reader :count-button)))
        (assert-true (press button))
        (assert-equal "1" (says (element-named reader :count) :value))
        (assert-true (press button))
        (assert-true (press button))
        (assert-equal "3" (says (element-named reader :count) :value))
        (assert-equal "3" (text-in screen :count))))))

(define-test the-elements-are-where-the-widgets-are-on-the-screen ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((view (reader-view reader))
            (window (reader-window reader)))
        ;; Where the whole view is on the screen, from AppKit, and then where a widget in it is,
        ;; worked out here: the view is flipped (its y runs down from the top), the screen's runs up.
        (multiple-value-bind (bx by bw bh) (objc:msg-send :rect view "bounds")
          (multiple-value-bind (wx wy ww wh) (objc:msg-send :rect view "convertRect:toView:"
                                                            (:rect bx by bw bh) (:id 0))
            (multiple-value-bind (vx vy vw vh) (objc:msg-send :rect window "convertRectToScreen:"
                                                              (:rect wx wy ww wh))
              (declare (ignore vw))
              (dolist (id '(:count :count-button))
                (destructuring-bind (x y width height)
                    (s:node-bounds (first (s:find-nodes
                                           (core:accessibility-tree (screen-app screen))
                                           :id id)))
                  (multiple-value-bind (ex ey ew eh)
                      (objc:msg-send :rect (element-named reader id) "accessibilityFrame")
                    (let ((expected (list (+ vx x) (+ vy (- vh y height)) width height)))
                      (assert-true (every (lambda (want got) (< (abs (- want got)) 1d-4))
                                          expected (list ex ey ew eh))
                                   id expected (list ex ey ew eh)))))))))))))

(define-test a-point-on-the-screen-finds-the-element-under-it ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((button (element-named reader :count-button))
            (view (reader-view reader)))
        (multiple-value-bind (x y width height) (objc:msg-send :rect button "accessibilityFrame")
          (assert-eql button (objc:msg-send :id view "accessibilityHitTest:"
                                            (:point (+ x (/ width 2)) (+ y (/ height 2)))))
          (assert-eql view (objc:msg-send :id view "accessibilityHitTest:"
                                          (:point (- x 5) (- y 5)))))))))

(define-test a-screen-reader-keeps-the-same-elements-from-frame-to-frame-and-reload-to-reload ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((count (element-named reader :count))
            (button (element-named reader :count-button)))
        (press button)
        (assert-eql count (element-named reader :count))
        (assert-eql button (element-named reader :count-button))
        ;; The app is made again, as compiling it does.
        (let ((new (core:with-kept-state ((screen-state screen))
                     (app:make-app-instance 'counter:make-counter))))
          (core:inherit-state new (screen-app screen))
          (setf (screen-app screen) new)
          (draw screen))
        (assert-eql count (element-named reader :count))
        (assert-equal "1" (says count :value))
        (assert-eql button (element-named reader :count-button))))))

(defun make-disabled ()
  (lambda ()
    (row (:gap 8)
      (label "Nothing to do" :id :note)
      (button "Nope" :id :nope :enabled nil))))

(define-test a-button-that-is-not-enabled-says-so-and-refuses-to-be-pressed ()
  (let ((screen (show 'make-disabled)))
    (with-reader (reader screen)
      (let ((button (element-named reader :nope)))
        (assert-false (enabled-p button))
        (assert-false (press button))
        (assert-equal '() (told reader))))))

(defun make-vanishing ()
  "An app whose first button is replaced by another and then by a great many, so that the things a
screen reader was given are taken away and their places are used again."
  (state-let ((stage 0))
    (lambda ()
      (row (:gap 2)
        (case stage
          (0 (button "Here" :id :here))
          (1 (button "There" :id :there))
          (t (dotimes (index 80)
               (button (format nil "Another ~d" index) :id (list :another index)))))
        (when (button "Next" :id :next)
          (incf stage))))))

(define-test an-element-whose-widget-went-away-answers-with-nothing-and-cannot-be-pressed ()
  (let ((screen (show 'make-vanishing)))
    (with-reader (reader screen)
      (let ((here (element-named reader :here)))
        ;; A screen reader holds on to what it was given, and so does this.
        (objc:retain here)
        (unwind-protect
             (progn
               (assert-equal "Here" (says here :label))
               (assert-true (press (element-named reader :next)))
               (assert-false (element-named reader :here))
               (assert-eq nil (says here :label))
               (assert-eq nil (says here :role))
               (assert-false (press here))
               ;; Eighty things come, and take the places of the ones that went. What was taken
               ;; away does not speak for them.
               (assert-true (press (element-named reader :next)))
               (assert-true (element-named reader '(:another 0)))
               (assert-eq nil (says here :label))
               (assert-false (press here)))
          (objc:release here))))))

(define-test the-list-is-read-as-a-field-and-check-boxes-and-what-is-typed-is-read-back ()
  (let ((screen (show 'todo:make-todo)))
    (with-reader (reader screen)
      (let ((field (element-named reader :draft))
            (read-me (element-named reader 1))
            (open-window (element-named reader 2)))
        (assert-equal "AXTextField" (says field :role))
        (assert-equal "" (says field :value))
        (assert-equal "AXCheckBox" (says read-me :role))
        (assert-equal "Read the README" (says read-me :label))
        (assert-equal 1d0 (says read-me :value))
        (assert-equal 0d0 (says open-window :value))
        (click screen :draft)
        (type-text screen "Feed the cat")
        (assert-equal "Feed the cat" (says field :value))
        (assert-true (press open-window))
        (assert-equal 1d0 (says open-window :value))
        (assert-equal "1 left" (text-in screen :left))))))

(define-test a-screen-reader-is-told-what-changed-and-nothing-else ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((count (element-named reader :count))
            (button (element-named reader :count-button)))
        (told reader)
        (press button)
        (let ((told (told reader)))
          (assert-true (member (list count :value-changed) told :test #'equal))
          (assert-false (member :layout-changed told :key #'second)))
        ;; With no screen reader running, and nothing having asked, nothing is told.
        (setf (a11y:bridge-assistive (reader-bridge reader)) (constantly nil)
              (a11y:bridge-watched (reader-bridge reader)) nil)
        (press button)
        (assert-equal '() (told reader))
        (assert-equal "2" (says (element-named reader :count) :value))))))

(define-test a-screen-reader-is-told-when-things-come-and-when-the-keyboard-moves ()
  (let ((screen (show 'todo:make-todo)))
    (with-reader (reader screen)
      (element-named reader :draft)
      (told reader)
      (click screen :draft)
      (let ((field (element-named reader :draft)))
        (assert-true (member (list field :focused-changed) (told reader) :test #'equal))
        (assert-eql field (objc:msg-send :id (reader-view reader) "accessibilityFocusedUIElement"))
        (type-text screen "Water the plants")
        (assert-true (member (list field :value-changed) (told reader) :test #'equal))
        (click screen :add)
        (assert-true (member (list (reader-view reader) :layout-changed) (told reader)
                             :test #'equal))
        (assert-equal 4 (length (remove-if-not (lambda (element)
                                                 (equal (says element :role) "AXCheckBox"))
                                               (elements reader))))))))

(define-test with-nothing-holding-the-keyboard-the-view-names-nothing-as-focused ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      ;; Not the view itself, which is no element: VoiceOver, put there, said "unknown" and was stuck.
      (assert-eql 0 (objc:msg-send :id (reader-view reader) "accessibilityFocusedUIElement")))))

(define-test a-screen-reader-that-asks-for-the-focus-of-a-field-gives-it-the-keyboard ()
  (let ((screen (show 'todo:make-todo)))
    (with-reader (reader screen)
      (let ((field (element-named reader :draft)))
        (assert-false (eql field (objc:msg-send :id (reader-view reader)
                                                "accessibilityFocusedUIElement")))
        (objc:msg-send :void field "setAccessibilityFocused:" (:bool t))
        (assert-eql field (objc:msg-send :id (reader-view reader) "accessibilityFocusedUIElement"))
        (type-text screen "Hello")
        (assert-equal "Hello" (says field :value))))))

(define-test a-screen-reader-reads-a-text-area-and-can-give-it-the-keyboard ()
  (let ((screen (show (lambda () (make-notes (format nil "one~%two"))))))
    (with-reader (reader screen)
      (let ((notes (element-named reader :notes)))
        (assert-equal "AXTextArea" (says notes :role))
        (assert-equal (format nil "one~%two") (says notes :value))
        (objc:msg-send :void notes "setAccessibilityFocused:" (:bool t))
        (assert-eql notes (objc:msg-send :id (reader-view reader) "accessibilityFocusedUIElement"))
        (type-text screen "!")
        (assert-equal (format nil "one~%two!") (says notes :value))))))

(define-test a-screen-reader-reads-a-switch-as-a-check-box-and-a-spinner-as-busy ()
  (let ((screen (show 'make-settings)))
    (with-reader (reader screen)
      (let ((wifi (element-named reader :wifi))
            (working (element-named reader :working)))
        (assert-equal "AXCheckBox" (says wifi :role))
        (assert-equal "Wi-Fi" (says wifi :label))
        (assert-equal 0d0 (says wifi :value))
        (press wifi)
        (assert-equal 1d0 (says wifi :value))
        (assert-equal "AXBusyIndicator" (says working :role))))))

(define-test a-screen-reader-reads-a-dropdown-and-the-rows-of-its-menu-and-presses-one ()
  (let ((screen (show 'make-picker)))
    (with-reader (reader screen)
      (let ((size (element-named reader :size)))
        (assert-equal "AXPopUpButton" (says size :role))
        (assert-equal "Size" (says size :label))
        (assert-equal "Medium" (says size :value))
        (press size)
        (let ((rows (remove "AXMenuItem" (elements reader)
                            :key (lambda (e) (says e :role)) :test-not #'equal)))
          (assert-equal '("Small" "Medium" "Large") (mapcar (lambda (e) (says e :label)) rows))
          (press (third rows))
          (assert-equal "Large" (says (element-named reader :size) :value)))))))

(define-test a-screen-reader-reads-a-menu-button-and-presses-it-open ()
  (let ((screen (show 'make-actions)))
    (with-reader (reader screen)
      (let ((actions (element-named reader :actions)))
        (assert-equal "AXMenuButton" (says actions :role))
        (assert-equal "Actions" (says actions :label))
        (press actions)
        (assert-equal '("Rename" "Duplicate" "Delete")
                      (mapcar (lambda (e) (says e :label))
                              (remove "AXMenuItem" (elements reader)
                                      :key (lambda (e) (says e :role)) :test-not #'equal)))))))

(define-test a-screen-reader-gives-a-tooltip-as-the-help-of-its-widget ()
  (let ((screen (show 'make-tips)))
    (with-reader (reader screen)
      (assert-equal "Save the document"
                    (ns-value (objc:msg-send :id (element-named reader :save) "accessibilityHelp"))))))

(define-test a-screen-reader-reads-a-toast-out-as-it-comes ()
  (let ((screen (show 'make-saver)))
    (with-reader (reader screen)
      (spoken reader)
      (press (element-named reader :save))
      (assert-true (member "Saved 1" (spoken reader) :test #'equal)))))

(define-test a-screen-reader-reads-a-combo-box-and-can-give-it-the-keyboard ()
  (let ((screen (show 'make-city)))
    (with-reader (reader screen)
      (let ((city (element-named reader :city)))
        (assert-equal "AXComboBox" (says city :role))
        (objc:msg-send :void city "setAccessibilityFocused:" (:bool t))
        (type-text screen "Ly")
        (assert-equal "Ly" (says (element-named reader :city) :value))))))

(define-test a-window-that-was-closed-has-nothing-to-read ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((button (element-named reader :count-button)))
        (objc:retain button)
        (unwind-protect
             (progn
               (a11y:release-bridge (reader-bridge reader))
               (assert-equal '() (elements reader))
               ;; A frame that is presented after that does not bring them back.
               (draw screen)
               (assert-equal '() (elements reader))
               (assert-eq nil (says button :label))
               (assert-false (press button)))
          (objc:release button))))))

(define-test what-goes-wrong-in-telling-a-screen-reader-does-not-stop-the-frame ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (element-named reader :count)
      (setf (a11y:bridge-tree (reader-bridge reader)) (lambda () (error "the tree is broken")))
      (let ((complaint (with-output-to-string (*error-output*)
                         (click screen :count-button))))
        ;; The click was counted and drawn, and the complaint was made.
        (assert-equal "1" (text-in screen :count))
        (assert-true (search "the tree is broken" complaint)))
      (setf (a11y:bridge-tree (reader-bridge reader))
            (lambda () (core:accessibility-tree (screen-app screen))))
      (click screen :count-button)
      (assert-equal "2" (says (element-named reader :count) :value)))))

;;; --- the other widgets ---

(defun make-gallery ()
  "One of each of the widgets that the counter and the list do not have."
  (state-let ((volume 30d0) (flavour 1) (picked 2))
    (lambda ()
      (column (:gap 4)
        (label "Volume" :id :volume-label)
        (multiple-value-bind (value changed)
            (slider volume 0 100 :id :volume :labelled-by :volume-label)
          (when changed (setf volume value)))
        (progress 0.25d0 :id :progress)
        (label "Flavour" :id :flavour-label)
        (multiple-value-bind (index changed)
            (choice '("Plain" "Salted" "Sweet") flavour :id :flavour :labelled-by :flavour-label)
          (when changed (setf flavour index)))
        (multiple-value-bind (index changed)
            (list-box '("One" "Two" "Three" "Four" "Five" "Six" "Seven") picked
                      :id :numbers :rows 3)
          (when changed (setf picked index)))
        (canvas 40 20 (lambda (list context width height)
                        (declare (ignore list context width height))
                        nil)
                :id :drawing)
        (stack ()
          (label "Under" :id :under)
          (scrim :id :veil))))))

(defun make-locked ()
  (lambda ()
    (column (:gap 4)
      (slider 10 0 100 :id :volume :enabled nil)
      (choice '("A" "B") 0 :id :pair :enabled nil))))

(defun parts-of (element)
  (ns-list (objc:msg-send :id element "accessibilityChildren")))

(defun chosen-p (element)
  (objc:msg-send :bool element "isAccessibilitySelected"))

(defun end-of (element which)
  (ns-value (objc:msg-send :id element (ecase which
                                         (:minimum "accessibilityMinValue")
                                         (:maximum "accessibilityMaxValue")))))

(defun chosen-parts (element)
  (ns-list (objc:msg-send :id element "accessibilitySelectedChildren")))

(defun increment (element)
  (objc:msg-send :bool element "accessibilityPerformIncrement"))

(defun decrement (element)
  (objc:msg-send :bool element "accessibilityPerformDecrement"))

(defun frame-of (element)
  (multiple-value-list (objc:msg-send :rect element "accessibilityFrame")))

(define-test a-screen-reader-reads-sliders-bars-choices-lists-and-drawings ()
  (let ((screen (show 'make-gallery)))
    (with-reader (reader screen)
      (let ((volume (element-named reader :volume))
            (progress (element-named reader :progress))
            (flavour (element-named reader :flavour))
            (numbers (element-named reader :numbers)))
        (assert-equal "AXSlider" (says volume :role))
        (assert-equal "Volume" (says volume :label))
        (assert-equal (list 30d0 0d0 100d0)
                      (list (says volume :value) (end-of volume :minimum) (end-of volume :maximum)))
        (assert-equal "AXProgressIndicator" (says progress :role))
        (assert-equal (list 0.25d0 0d0 1d0)
                      (list (says progress :value) (end-of progress :minimum)
                            (end-of progress :maximum)))
        (assert-equal "AXRadioGroup" (says flavour :role))
        (assert-equal "Flavour" (says flavour :label))
        (let ((options (parts-of flavour)))
          (assert-equal '("Plain" "Salted" "Sweet") (mapcar (lambda (e) (says e :label)) options))
          (assert-equal '("AXRadioButton" "AXRadioButton" "AXRadioButton")
                        (mapcar (lambda (e) (says e :role)) options))
          (assert-equal '(0d0 1d0 0d0) (mapcar (lambda (e) (says e :value)) options))
          (assert-equal '(nil t nil) (mapcar #'chosen-p options))
          (assert-equal (list (second options)) (chosen-parts flavour)))
        (assert-equal "AXList" (says numbers :role))
        (let ((rows (parts-of numbers)))
          (assert-equal '("One" "Two" "Three") (mapcar (lambda (e) (says e :label)) rows))
          (assert-equal '("AXRow" "AXRow" "AXRow") (mapcar (lambda (e) (says e :role)) rows))
          (assert-equal '(nil nil t) (mapcar #'chosen-p rows))
          (assert-equal (list (third rows)) (chosen-parts numbers)))
        (assert-equal "AXGroup" (says (element-named reader :drawing) :role))
        (assert-equal "AXGroup" (says (element-named reader :veil) :role))))))

(defun near (want got)
  "True if GOT is WANT, to within what a press on a slider (a point in single precision) can say."
  (< (abs (- want got)) 1d-3))

(define-test a-screen-reader-moves-a-slider-up-and-down-a-step-at-a-time ()
  (let ((screen (show 'make-gallery)))
    (with-reader (reader screen)
      (let ((volume (element-named reader :volume)))
        (assert-true (increment volume))
        (assert-true (near 35 (says volume :value)) (says volume :value))
        (assert-true (near 35 (s:node-value (node-of screen :volume))))
        (assert-true (decrement volume))
        (assert-true (decrement volume))
        (assert-true (near 25 (says volume :value)) (says volume :value))
        ;; A press leaves the value a hair off, which does not matter to a step.
        (dotimes (i 15) (increment volume))
        (assert-equal 100d0 (says volume :value))
        (assert-false (increment volume))
        (assert-equal 100d0 (says volume :value))
        (dotimes (i 4) (decrement volume))
        (assert-true (near 80 (says volume :value)) (says volume :value))
        (dotimes (i 25) (decrement volume))
        (assert-equal 0d0 (says volume :value))
        (assert-false (decrement volume))))))

(define-test a-screen-reader-picks-an-option-and-a-row-by-pressing-them ()
  (let ((screen (show 'make-gallery)))
    (with-reader (reader screen)
      (let* ((flavour (element-named reader :flavour))
             (numbers (element-named reader :numbers))
             (sweet (third (parts-of flavour)))
             (two (second (parts-of numbers))))
        (assert-true (press sweet))
        (assert-equal "Sweet" (s:node-value (node-of screen :flavour)))
        (assert-equal '(0d0 0d0 1d0) (mapcar (lambda (e) (says e :value)) (parts-of flavour)))
        (assert-equal (list sweet) (chosen-parts flavour))
        (assert-true (press two))
        (assert-equal "Two" (s:node-value (node-of screen :numbers)))
        (assert-equal (list two) (chosen-parts numbers))
        (assert-equal '(nil t nil) (mapcar #'chosen-p (parts-of numbers)))))))

(define-test a-list-shows-a-screen-reader-the-rows-that-show-and-keeps-them-when-it-scrolls ()
  (let ((screen (show 'make-gallery)))
    (with-reader (reader screen)
      (let* ((numbers (element-named reader :numbers))
             (rows (parts-of numbers))
             (one (first rows))
             (three (third rows)))
        (objc:retain one)
        (unwind-protect
             (progn
               (told reader)
               (m:scroll (screen-session screen) :id :numbers :dy -2)
               (let ((now (parts-of numbers)))
                 (assert-equal '("Three" "Four" "Five") (mapcar (lambda (e) (says e :label)) now))
                 (assert-eql three (first now))
                 (assert-true (chosen-p (first now)))
                 (assert-equal '("numbers.2" "numbers.3" "numbers.4")
                               (mapcar (lambda (e) (ns-value (objc:msg-send :id e
                                                                            "accessibilityIdentifier")))
                                       now)))
               (let ((told (told reader)))
                 (assert-true (member (list (reader-view reader) :layout-changed) told
                                      :test #'equal))
                 ;; A row that is kept is told that it moved, as the cursor of a screen reader that
                 ;; is on it goes where it is.
                 (assert-true (member (list three :moved) told :test #'equal)))
               ;; What scrolled out of sight is gone, and does not speak for what came in.
               (assert-eq nil (says one :label)))
          (objc:release one))))))

(define-test a-screen-reader-is-told-when-a-slider-moves-and-when-a-choice-is-made ()
  (let ((screen (show 'make-gallery)))
    (with-reader (reader screen)
      (let* ((volume (element-named reader :volume))
             (flavour (element-named reader :flavour))
             (numbers (element-named reader :numbers))
             (options (parts-of flavour))
             (rows (parts-of numbers)))
        (told reader)
        (increment volume)
        (assert-true (member (list volume :value-changed) (told reader) :test #'equal))
        (press (third options))
        (let ((told (told reader)))
          (assert-true (member (list (second options) :value-changed) told :test #'equal))
          (assert-true (member (list (third options) :value-changed) told :test #'equal)))
        (press (second rows))
        (let ((told (told reader)))
          (assert-true (member (list numbers :selection-changed) told :test #'equal))
          (assert-eql 1 (count-if (lambda (each) (eq (second each) :selection-changed)) told)))))))

(define-test the-parts-are-inside-what-they-are-parts-of-and-a-point-finds-the-part ()
  (let ((screen (show 'make-gallery)))
    (with-reader (reader screen)
      (let* ((flavour (element-named reader :flavour))
             (options (parts-of flavour))
             (view (reader-view reader)))
        (destructuring-bind (gx gy gw gh) (frame-of flavour)
          (dolist (option options)
            (destructuring-bind (x y width height) (frame-of option)
              (assert-true (and (<= (- gx 1d-6) x) (<= (- gy 1d-6) y)
                                (<= (+ x width) (+ gx gw 1d-6)) (<= (+ y height) (+ gy gh 1d-6)))
                           (frame-of option) (frame-of flavour)))))
        (destructuring-bind (x y width height) (frame-of (third options))
          (assert-eql (third options)
                      (objc:msg-send :id view "accessibilityHitTest:"
                                     (:point (+ x (/ width 2)) (+ y (/ height 2))))))
        (let ((row (first (parts-of (element-named reader :numbers)))))
          (destructuring-bind (x y width height) (frame-of row)
            (assert-eql row
                        (objc:msg-send :id view "accessibilityHitTest:"
                                       (:point (+ x (/ width 2)) (+ y (/ height 2)))))))))))

(define-test a-slider-and-a-choice-that-are-not-enabled-say-so-and-refuse ()
  (let ((screen (show 'make-locked)))
    (with-reader (reader screen)
      (let* ((volume (element-named reader :volume))
             (pair (element-named reader :pair))
             (options (parts-of pair)))
        (assert-false (enabled-p volume))
        (assert-false (increment volume))
        (assert-false (decrement volume))
        (assert-equal 10d0 (says volume :value))
        (assert-equal '(nil nil) (mapcar #'enabled-p options))
        (assert-false (press (second options)))
        (assert-equal '(1d0 0d0) (mapcar (lambda (e) (says e :value)) options))))))

(defvar *slides* 0 "How many times the slider in MAKE-WHOLE-SLIDER has said it was moved.")

(defun make-whole-slider ()
  "A slider whose value the app keeps as a whole number, as a font size or a count is."
  (state-let ((size 5))
    (lambda ()
      (column (:gap 8)
        (multiple-value-bind (value moved) (slider size 0 10 :id :whole)
          (when moved
            (incf *slides*)
            (setf size (round value))))
        (label (format nil "~d" size) :id :whole-value)))))

(define-test a-slider-held-still-is-not-moved-again-when-the-app-rounds-its-value ()
  (let ((*slides* 0)
        (warned nil))
    (handler-bind ((core:settle-cap-reached (lambda (warning)
                                              (setf warned t)
                                              (muffle-warning warning))))
      (let ((screen (show 'make-whole-slider)))
        (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) :whole)
          ;; The thumb's middle travels the width less the thumb, 16: a press at 7.3 of the 10.
          (let ((px (+ x 8 (* 0.73 (- width 16)))) (py (+ y (/ height 2))))
            (pointer screen :move px py)
            (pointer screen :down px py :button 0 :buttons 1)
            ;; It moved once, and the app holds 7, which is what it shows, though the pointer is
            ;; still at 7.3.
            (assert-eql 1 *slides*)
            (assert-equal "7" (text-in screen :whole-value))
            (assert-eql 7d0 (s:node-value (node-of screen :whole)))
            ;; Held there for a frame more, it is not moved again.
            (draw screen)
            (assert-eql 1 *slides*)
            ;; Dragged on, it is.
            (pointer screen :move (+ px 37) py :buttons 1)
            (assert-eql 2 *slides*)
            (assert-equal "9" (text-in screen :whole-value))
            (pointer screen :up (+ px 37) py :button 0)
            ;; A key moves it on, and a press where the pointer last was slides it back there.
            (key screen core:+key-right+)
            (assert-equal "10" (text-in screen :whole-value))
            (pointer screen :down (+ px 37) py :button 0 :buttons 1)
            (pointer screen :up (+ px 37) py :button 0)
            (assert-equal "9" (text-in screen :whole-value))))
        (assert-false warned)))))

(define-test the-system-is-told-once-something-has-asked-for-the-screen-and-not-before ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((bridge (reader-bridge reader)))
        ;; The system does not say that a screen reader runs (it was found to say so wrongly), and
        ;; nothing has asked what the window shows: nothing is worked out or told.
        (setf (a11y:bridge-assistive bridge) (constantly nil))
        (click screen :count-button)
        (assert-false (a11y:bridge-watched bridge))
        (assert-equal '() (told reader))
        ;; Something asks, and from then on what changes is told.
        (let ((count (element-named reader :count)))
          (assert-true (a11y:bridge-watched bridge))
          (told reader)
          (click screen :count-button)
          (assert-true (member (list count :value-changed) (told reader) :test #'equal))
          (assert-equal "2" (says count :value)))))))

(defun make-notices ()
  "A live label and a live paragraph beside a label that is not live, all three changed by one button,
and another live label that comes with the first press."
  (state-let ((presses 0))
    (lambda ()
      (column (:gap 4)
        (label (format nil "Saved ~d" presses) :id :saved :live t)
        (paragraph (format nil "Message ~d" presses) :id :message :live t)
        (label (format nil "Quiet ~d" presses) :id :quiet)
        (when (plusp presses)
          (label "Late" :id :late :live t))
        (when (button "Press" :id :press)
          (incf presses))))))

(define-test a-live-text-is-read-out-when-it-changes-and-nothing-else-is ()
  (let ((screen (show 'make-notices)))
    (with-reader (reader screen)
      ;; Something asks what the window shows, and the texts it shows are not read out for that.
      (element-named reader :saved)
      (draw screen)
      (assert-equal '() (spoken reader))
      (assert-true (s:node-property (node-of screen :saved) :live))
      (assert-true (s:node-property (node-of screen :message) :live))
      (assert-false (s:node-property (node-of screen :quiet) :live))
      ;; The live ones are read out when they change, the other is not, and one that does not change
      ;; is not read again.
      (click screen :press)
      (assert-equal '("Saved 1" "Message 1") (spoken reader))
      ;; A live text that has come is there to be read, and is not read out for coming.
      (assert-true (element-named reader :late))
      (draw screen)
      (assert-equal '() (spoken reader))
      (click screen :press)
      (assert-equal '("Saved 2" "Message 2") (spoken reader)))))

(define-test the-count-is-read-out-when-a-screen-reader-presses-the-button ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((button (element-named reader :count-button)))
        (assert-equal '() (spoken reader))
        (press button)
        (press button)
        (assert-equal '("1" "2") (spoken reader))))))

(define-test the-system-takes-every-kind-of-notification-for-an-element ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      ;; The way the bridge tells the system when no test stands in for it: each kind is made into the
      ;; system's own notification, which fails here if the system has no such thing.
      (let ((post (a11y:bridge-poster (a11y:make-bridge)))
            (count (element-named reader :count)))
        (dolist (kind '(:layout-changed :value-changed :selection-changed :focused-changed
                        :moved :resized))
          (funcall post count kind))
        (funcall post (reader-view reader) :layout-changed (list count))
        (funcall post 0 :moved)
        (press (element-named reader :count-button))
        (assert-equal "1" (says count :value))))))

(define-test the-system-takes-an-announcement-through-the-window ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      ;; The way the bridge has text read out when no test stands in for it. The system has no
      ;; screen reader here to hear it, so what is checked is that asking goes through, for a view
      ;; in a window, for words that are none, and for no view at all, and that the app goes on.
      (let ((speak (a11y:bridge-announcer (a11y:make-bridge))))
        (funcall speak (reader-view reader) "Counted")
        (funcall speak (reader-view reader) "")
        (funcall speak 0 "Nowhere")
        (press (element-named reader :count-button))
        (assert-equal "1" (says (element-named reader :count) :value))))))

(define-test a-screen-reader-is-told-where-things-went-when-the-layout-shifts ()
  (let ((screen (show 'counter:make-counter)))
    (with-reader (reader screen)
      (let ((count (element-named reader :count))
            (button (element-named reader :count-button)))
        (told reader)
        ;; While the count has one digit nothing moves, and nothing is told of it.
        (dotimes (index 9)
          (press button))
        (let ((told (told reader)))
          (assert-false (member :moved told :key #'second))
          (assert-false (member :resized told :key #'second))
          (assert-false (find-if #'third told)))
        ;; With the tenth press the count is wider and the button is pushed along: the cursor of a
        ;; screen reader that is on the button would stay where the button was.
        (press button)
        (assert-equal "10" (says count :value))
        (let ((told (told reader)))
          (assert-true (member (list count :resized) told :test #'equal))
          (assert-true (member (list button :moved) told :test #'equal))
          (assert-false (member (list button :resized) told :test #'equal))
          (assert-false (member (list count :moved) told :test #'equal))
          ;; And a layout change of the view that names both, which is what VoiceOver draws its cursor
          ;; again for.
          (let ((layout (find-if #'third told)))
            (assert-true layout)
            (assert-eql (reader-view reader) (first layout))
            (assert-eq :layout-changed (second layout))
            (assert-equal 2 (length (third layout)))
            (assert-true (member button (third layout)))
            (assert-true (member count (third layout)))))))))

(define-test a-thing-that-comes-is-not-told-to-have-moved-and-the-ones-it-pushes-are ()
  (let ((screen (show 'make-notices)))
    (with-reader (reader screen)
      (let ((button (element-named reader :press)))
        (told reader)
        (press button)
        (let ((late (element-named reader :late))
              (told (told reader)))
          (assert-true late)
          (assert-true (member (list button :moved) told :test #'equal))
          (assert-false (member (list late :moved) told :test #'equal))
          (assert-false (member (list late :resized) told :test #'equal)))))))

;;; --- themes ---

(defun middle-color (screen id)
  "The colour drawn at the middle of the control ID (the colour of a button, under its words)."
  (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) id)
    (color-at screen (+ x (/ width 2)) (+ y (/ height 2)))))

(defun same-theme-p (a b)
  (every (lambda (setting) (equal (theme-setting a setting) (theme-setting b setting)))
         +theme-settings+))

(define-test the-window-is-dark-or-light-as-the-system-is ()
  (let ((screen (show 'counter:make-counter)))
    (assert-eql (theme-setting (find-theme :dark) :background) (color-at screen 1 1))
    (assert-eql (theme-setting (find-theme :dark) :button) (middle-color screen :count-button))
    ;; The system goes light, and nothing else changes: the next frame is drawn light.
    (setf (core:context-appearance (screen-context screen)) :light)
    (draw screen)
    (assert-eql (theme-setting (find-theme :light) :background) (color-at screen 1 1))
    (assert-eql (theme-setting (find-theme :light) :button) (middle-color screen :count-button))))

(defun make-themed ()
  "A button in the window's theme, and one in the theme :test-sea."
  (lambda ()
    (column (:gap 10)
      (button "Plain" :id :plain)
      (with-theme (:test-sea)
        (button "Sea" :id :sea)))))

(define-test a-part-of-the-screen-is-drawn-in-the-theme-it-is-described-in ()
  (define-theme :test-sea () :accent "#20a090")
  (let ((screen (show 'make-themed))
        (dark (theme-setting (find-theme :dark) :button)))
    (assert-eql dark (middle-color screen :plain))
    (assert-eql (parse-color "#20a090") (middle-color screen :sea))
    ;; Defined again (as an edit in the editor does), it is drawn in the new colour, though the app
    ;; described nothing new.
    (define-theme :test-sea () :accent "#c06030")
    (draw screen)
    (assert-eql (parse-color "#c06030") (middle-color screen :sea))
    (assert-eql dark (middle-color screen :plain))))

;;; The colours the words in the control ID are drawn in, top to bottom, each run of one colour once.
(defun word-colors (screen id)
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) id)
    (let* ((list (screen-list screen))
           (glyphs (loop :for index :below (core:display-list-count list)
                         :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                                    (< left (core:display-list-x list index) (+ left width))
                                    (< top (core:display-list-y list index) (+ top height)))
                           :collect (cons (core:display-list-y list index)
                                          (core:display-list-color list index))))
           (colors '()))
      (dolist (glyph (stable-sort glyphs #'< :key #'car) (nreverse colors))
        (unless (eql (cdr glyph) (first colors))
          (push (cdr glyph) colors))))))

(defun make-fruit-list ()
  (state-let ((picked 1))
    (lambda ()
      (multiple-value-bind (index changed)
          (list-box '("Apple" "Banana" "Cherry" "Damson") picked :id :fruit :rows 4)
        (when changed (setf picked index))))))

(define-test the-words-of-a-chosen-row-are-readable-on-its-highlight-in-dark-and-light ()
  (let ((screen (show 'make-fruit-list)))
    (dolist (appearance '(:dark :light))
      (setf (core:context-appearance (screen-context screen)) appearance)
      (draw screen)
      (let ((theme (find-theme appearance))
            (colors (word-colors screen :fruit)))
        ;; Apple in the text's colour, Banana on the highlight in the button's words' colour, and
        ;; Cherry and Damson in the text's again.
        (assert-equal (list (theme-setting theme :text) (theme-setting theme :button-text)
                            (theme-setting theme :text))
                      colors appearance)
        (assert-true (>= (contrast-ratio (second colors) (theme-setting theme :row-selected)) 4.5)
                     appearance)))))

(define-test a-theme-given-an-accent-works-out-what-comes-from-it-and-keeps-the-rest ()
  (let ((dark (find-theme :dark))
        (sea (make-theme :accent "#20a090")))
    (assert-eql (parse-color "#20a090") (theme-setting sea :button))
    (assert-eql (parse-color "#20a090") (theme-setting sea :row-selected))
    ;; Worked out from the accent: not dark's, and not the accent itself.
    (dolist (setting '(:button-hover :button-pressed :thumb :ring))
      (assert-false (eql (theme-setting dark setting) (theme-setting sea setting)) setting)
      (assert-false (eql (parse-color "#20a090") (theme-setting sea setting)) setting))
    ;; Not from the accent: dark's, as they were.
    (dolist (setting '(:background :text :panel :field-border :field-fill :dim-text :row-hover))
      (assert-eql (theme-setting dark setting) (theme-setting sea setting) setting))
    ;; Given as well as the accent, it is what was given.
    (assert-eql (parse-color "#ff0000")
                (theme-setting (make-theme :accent "#20a090" :button-hover "#ff0000")
                               :button-hover))))

(define-test a-theme-written-down-defines-the-same-theme-again ()
  (let* ((theme (make-theme :name :test-written :base :light :accent "#20a090" :ring "#ffeedd"
                            :font-size 17 :radius 9))
         (form (theme-form theme)))
    (assert-eq 'define-theme (first form))
    (eval form)
    (assert-true (same-theme-p theme (find-theme :test-written)))
    ;; And not merely any theme: the light one is not it.
    (assert-false (same-theme-p theme (find-theme :light)))))

(defparameter *label-theme* :dark)

(define-test a-theme-with-another-font-lays-its-text-out-again ()
  ;; With the real text engine (the fake one measures every font alike): only the font's name
  ;; changes, so nothing but the theme says that the label is not as wide as it was.
  (define-theme :test-courier () :font-name "Courier")
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let* ((host (host:make-host (make-instance 'core:app-instance) (device:default-device)
                                   target:+pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
             (context (host:host-context host))
             (instance (core:with-kept-state ((core:make-kept-state))
                        (app:make-app-instance
                         (lambda ()
                           (lambda ()
                             (with-theme (*label-theme*)
                               (label "iiiiiiii" :id :word)))))))
             (list (core:make-display-list :capacity 4096)))
        (setf (core:context-width context) 360f0
              (core:context-height context) 320f0)
        (flet ((width ()
                 (core:settle-frame instance context list)
                 (nth-value 2 (core:lookup-target instance :word))))
          (let* ((*label-theme* :dark) (narrow (width))
                 (*label-theme* :test-courier) (wide (width)))
            (assert-true (> wide (* 1.5 narrow)) narrow wide)))))))

;;; --- spacing in a theme ---

(define-test a-spacing-works-out-the-sizes-and-a-size-given-is-what-was-given ()
  (let ((dark (find-theme :dark))
        (roomy (make-theme :spacing 1.5))
        (rows (make-theme :spacing 1.5 :row-height 30)))
    ;; At 1, the sizes the widgets have always had.
    (assert-eql 14f0 (theme-setting dark :button-padding-x))
    (assert-eql 24f0 (theme-setting dark :row-height))
    ;; Half as much again, to the nearest point: 7 is 10.5, which goes up.
    (assert-eql 21f0 (theme-setting roomy :button-padding-x))
    (assert-eql 11f0 (theme-setting roomy :button-padding-y))
    (assert-eql 36f0 (theme-setting roomy :row-height))
    ;; Given as well as the spacing, it is what was given; the others are still worked out.
    (assert-eql 30f0 (theme-setting rows :row-height))
    (assert-eql 21f0 (theme-setting rows :button-padding-x))
    ;; A theme that does not give the spacing keeps its base's sizes, and its colours are no
    ;; reason to work them out again.
    (let ((sea (make-theme :base rows :accent "#20a090")))
      (assert-eql 30f0 (theme-setting sea :row-height))
      (assert-eql 21f0 (theme-setting sea :button-padding-x)))
    ;; A size is a number of points, and the spacing more than nothing.
    (assert-error 'error (make-theme :row-height "#ffffff"))
    (assert-error 'error (make-theme :row-height -1))
    (assert-error 'error (make-theme :spacing 0))))

(define-test a-theme-with-spacing-written-down-defines-the-same-theme-again ()
  (let* ((theme (make-theme :name :test-roomy-written :spacing 1.25 :row-height 28))
         (form (theme-form theme)))
    ;; (DEFINE-THEME name (:base base) settings...): the spacing and the size, as they were given.
    (assert-eql 1.25 (getf (cdddr form) :spacing) form)
    (assert-eql 28 (getf (cdddr form) :row-height) form)
    (eval form)
    (assert-true (same-theme-p theme (find-theme :test-roomy-written)))
    (assert-false (same-theme-p theme (find-theme :dark)))))

(defun make-two-buttons ()
  "The same button, dropdown and menu button in the window's theme and in :test-roomy."
  (lambda ()
    (column (:gap 10)
      (button "Same" :id :plain)
      (dropdown '("Same") 0 :id :plain-drop)
      (menu-button "Same" '("One") :id :plain-menu)
      (with-theme (:test-roomy)
        (column (:gap 10)
          (button "Same" :id :roomy)
          (dropdown '("Same") 0 :id :roomy-drop)
          (menu-button "Same" '("One") :id :roomy-menu))))))

(defun size-of (screen id)
  (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) id)
    (declare (ignore x y))
    (values width height)))

(define-test a-button-in-a-roomier-theme-is-bigger-by-its-padding-and-the-one-beside-it-is-not ()
  (define-theme :test-roomy () :spacing 1.5)
  (let ((screen (show 'make-two-buttons)))
    (loop :for (plain roomy) :in '((:plain :roomy) (:plain-drop :roomy-drop)
                                   (:plain-menu :roomy-menu))
          :do (multiple-value-bind (plain-w plain-h) (size-of screen plain)
                (multiple-value-bind (roomy-w roomy-h) (size-of screen roomy)
                  ;; The words are the same; the padding is 21 and 11 at the sides and top for 14
                  ;; and 7.
                  (assert-eql 14f0 (- roomy-w plain-w) roomy)
                  (assert-eql 8f0 (- roomy-h plain-h) roomy))))
    ;; Defined again with more room, it is laid out again, though the app described nothing new.
    (define-theme :test-roomy () :spacing 2)
    (draw screen)
    (multiple-value-bind (plain-w) (size-of screen :plain)
      (multiple-value-bind (roomy-w) (size-of screen :roomy)
        (assert-eql 28f0 (- roomy-w plain-w))))))

(defun make-two-fruit-lists ()
  "The same list in the window's theme and, beside it, in :test-tall."
  (state-let ((picked 0) (tall-picked 0))
    (lambda ()
      (row (:gap 10)
        (multiple-value-bind (index changed)
            (list-box '("Apple" "Banana" "Cherry" "Damson") picked :id :plain-fruit :rows 4
                                                                   :width 120)
          (when changed (setf picked index)))
        (with-theme (:test-tall)
          (multiple-value-bind (index changed)
              (list-box '("Apple" "Banana" "Cherry" "Damson") tall-picked :id :tall-fruit
                                                                          :rows 4 :width 120)
            (when changed (setf tall-picked index))))))))

(define-test a-list-with-taller-rows-picks-the-row-it-draws-where-it-draws-it ()
  (define-theme :test-tall () :row-height 40)
  (let ((screen (show 'make-two-fruit-lists)))
    (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) :tall-fruit)
      ;; Four rows of 40, inside its edge; the list beside it has rows of 24.
      (assert-eql (+ 2f0 (* 4 40f0)) height)
      (assert-eql (+ 2f0 (* 4 24f0)) (nth-value 1 (size-of screen :plain-fruit)))
      ;; A screen reader is told where the rows are.
      (assert-equal (list x (+ y 1 40) width 40f0)
                    (second (s:node-property (node-of screen :tall-fruit) :item-bounds)))
      ;; A press in the middle of the third row picks Cherry, and draws its highlight there.
      (let ((px (+ x (/ width 2))) (py (+ y 1 (* 2 40) 20)))
        (pointer screen :move px py)
        (pointer screen :down px py :button 0 :buttons 1)
        (pointer screen :up px py :button 0)
        (assert-equal "Cherry" (s:node-value (node-of screen :tall-fruit)))
        (assert-eql (theme-setting (find-theme :test-tall) :row-selected)
                    (color-at screen (+ x 20) (- py 15)))))))

(defun make-roomy-fields ()
  "A field and an area in the window's theme, and the same in :test-roomy-field."
  (state-let ((text "abc") (notes "abcdefghijklmnopqrstuvwxyz"))
    (lambda ()
      (row (:gap 10)
        (column (:gap 10)
          (text-field "abc" :id :plain-field :width 120)
          (text-area "abc" :id :plain-area :width 120 :rows 2))
        (with-theme (:test-roomy-field)
          (column (:gap 10)
            (setf text (text-field text :id :roomy-field :width 120))
            (setf notes (text-area notes :id :roomy-area :width 120 :rows 2))))))))

(defun glyph-place (screen id)
  "Where the first character drawn in the control ID is, from the top left of the control."
  (multiple-value-bind (x y) (core:lookup-target (screen-app screen) id)
    (multiple-value-bind (gx gy) (glyph-point screen id 0 0)
      (values (- gx x) (- gy y)))))

(defun press-into (screen id n)
  "A press a quarter of the way into the Nth character drawn in ID."
  (multiple-value-bind (px py) (glyph-point screen id n 0.25)
    (pointer screen :move px py)
    (pointer screen :down px py :button 0 :buttons 1)
    (pointer screen :up px py :button 0)))

(define-test a-field-in-a-roomier-theme-has-its-words-further-in-and-a-press-finds-them ()
  (define-theme :test-roomy-field () :spacing 2)
  (let ((screen (show 'make-roomy-fields)))
    (dolist (pair '((:plain-field :roomy-field) (:plain-area :roomy-area)))
      (destructuring-bind (plain roomy) pair
        (multiple-value-bind (plain-x plain-y) (glyph-place screen plain)
          (multiple-value-bind (roomy-x roomy-y) (glyph-place screen roomy)
            ;; 16 points in for 8 at the sides, and 12 for 6 at the top.
            (assert-eql 8f0 (- roomy-x plain-x) roomy)
            (assert-eql 6f0 (- roomy-y plain-y) roomy)))
        ;; As tall as its lines and 12 above and below them, for 6.
        (assert-eql 12f0 (- (nth-value 1 (size-of screen roomy)) (nth-value 1 (size-of screen plain)))
                    roomy)))
    ;; A press a quarter of the way into the b puts the caret before it.
    (press-into screen :roomy-field 1)
    (type-text screen "X")
    (assert-equal "aXbc" (text-in screen :roomy-field))
    ;; Typed on past its end (120 wide with 16 at each side: eleven characters of 8), the field
    ;; shows the end, where the caret is.
    (key screen core:+key-end+)
    (type-text screen "defghijklmnopqrstu")
    (let ((shown (text-in screen :roomy-field)))
      (assert-eql 11 (length shown) shown)
      (assert-equal "klmnopqrstu" shown))
    ;; In the area, 120 wide with 16 at each side, eleven characters of 8 make a line, so the l
    ;; begins the second line, as far in as the a.
    (assert-eql (values (glyph-point screen :roomy-area 0 0))
                (values (glyph-point screen :roomy-area 11 0)))
    ;; A press a quarter of the way into the b, 2 points above the bottom of its line (14 points
    ;; tall, 12 under the top of the area), puts the caret before it.
    (let ((y (nth-value 1 (core:lookup-target (screen-app screen) :roomy-area))))
      (let ((px (glyph-point screen :roomy-area 1 0.25)) (py (+ y 12 14 -2)))
        (pointer screen :move px py)
        (pointer screen :down px py :button 0 :buttons 1)
        (pointer screen :up px py :button 0)))
    (type-text screen "Y")
    (assert-true (search "aYbcdefghij" (text-in screen :roomy-area)) (text-in screen :roomy-area))
    ;; Down to the third line: the two lines the area shows (28 points inside its padding) are the
    ;; second and the third, scrolled one line of 14.
    (key screen core:+key-down+)
    (key screen core:+key-down+)
    (assert-eql 14f0 (s:node-property (node-of screen :roomy-area) :offset))))

(defun first-glyph-in (screen bounds words)
  "Where the first character of WORDS is, from the top left of BOUNDS (x y width height): of the
characters drawn inside them, the last as many as WORDS has (a tooltip is drawn over what is under
it, after it)."
  (destructuring-bind (left top width height) bounds
    (let* ((list (screen-list screen))
           (inside (loop :for index :below (core:display-list-count list)
                         :for x = (core:display-list-x list index)
                         :for y = (core:display-list-y list index)
                         :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                                    (<= left x (+ left width -1)) (<= top y (+ top height -1)))
                           :collect (cons x y)))
           (first (nth (- (length inside) (length words)) inside)))
      (values (- (car first) left) (- (cdr first) top)))))

(defun toast-and-tooltip (spacing)
  "The bounds of a toast and of a tooltip, where their words begin in them, the gap between two
toasts and the bottom of the lower, with the dark theme at SPACING."
  (let ((dark (find-theme :dark)))
    (unwind-protect
         (progn
           (define-theme :dark (:base dark) :spacing spacing)
           (with-clock (clock)
             (let ((screen (show 'make-tips)))
               (rest-on screen :save)
               (core:advance-virtual-clock clock 600000000)
               (draw screen)
               (let* ((tip (s:node-bounds (tooltip-of screen)))
                      (tip-words (multiple-value-list (first-glyph-in screen tip "Save the document"))))
                 (let ((screen (show 'make-saver)))
                   (click screen :save)
                   (let ((toast (s:node-bounds (first (toasts-of screen)))))
                     (core:advance-virtual-clock clock 100000000)
                     (click screen :save)
                     ;; Between the first toast and the second, under it.
                     (destructuring-bind (first second) (mapcar #'s:node-bounds (toasts-of screen))
                       (list toast (multiple-value-list (first-glyph-in screen toast "Saved 1"))
                             tip tip-words
                             (- (second second) (+ (second first) (fourth first)))
                             ;; The bottom of the lower one.
                             (+ (second second) (fourth second))))))))))
      (setf (gethash :dark theme::*themes*) dark))))

(define-test a-toast-and-a-tooltip-are-roomier-in-a-roomier-window ()
  (destructuring-bind (toast toast-words tip tip-words gap bottom) (toast-and-tooltip 1)
    (destructuring-bind (roomy-toast roomy-words roomy-tip roomy-tip-words roomy-gap roomy-bottom)
        (toast-and-tooltip 2)
      ;; 16 points between toasts for 8, and the lower one where it was, 24 above the bottom of the
      ;; window.
      (assert-eql 8f0 (- roomy-gap gap))
      (assert-eql 296f0 bottom)
      (assert-eql 296f0 roomy-bottom)
      ;; 32 points at the sides of a toast's words for 16, and 20 above and below for 10.
      (assert-eql 32f0 (- (third roomy-toast) (third toast)))
      (assert-eql 20f0 (- (fourth roomy-toast) (fourth toast)))
      (assert-eql 16f0 (- (first roomy-words) (first toast-words)))
      (assert-eql 10f0 (- (second roomy-words) (second toast-words)))
      ;; A tooltip's: 16 for 8, and 8 for 4.
      (assert-eql 16f0 (- (third roomy-tip) (third tip)))
      (assert-eql 8f0 (- (fourth roomy-tip) (fourth tip)))
      (assert-eql 8f0 (- (first roomy-tip-words) (first tip-words)))
      (assert-eql 4f0 (- (second roomy-tip-words) (second tip-words))))))

(defparameter +odd-sizes+
  '(:control-height 30 :slider-height 31 :progress-height 17 :field-padding-y 9 :option-gap 33
    :option-row-gap 11 :popover-padding 13 :popover-gap 5 :dialog-padding 27 :dialog-gap 15
    :menu-padding 7 :row-height 26)
  "A size for each that is none of the sizes the widgets have otherwise, so that each one seen is
the theme's.")

(defun make-odd-controls ()
  (lambda ()
    (row (:gap 10)
      (text-field "" :id :plain-field :width 60)
      (with-theme (:test-odd)
        (column (:gap 4)
          (checkbox "C" nil :id :odd-check)
          (switch "S" nil :id :odd-switch)
          (slider 1 0 2 :id :odd-slider :width 60)
          (progress 0.5 :id :odd-progress :width 60)
          (text-field "" :id :odd-field :width 60)
          (choice '("A" "B") 0 :id :odd-across)
          (choice '("A" "B") 0 :id :odd-down :direction :vertical))))))

(define-test the-controls-are-as-tall-and-as-far-apart-as-their-theme-says ()
  (eval `(define-theme :test-odd () ,@+odd-sizes+))
  (let ((screen (show 'make-odd-controls)))
    (flet ((height (id) (nth-value 1 (size-of screen id)))
           (options (id) (s:node-property (node-of screen id) :option-bounds)))
      (assert-eql 30f0 (height :odd-check))
      (assert-eql 30f0 (height :odd-switch))
      (assert-eql 31f0 (height :odd-slider))
      (assert-eql 17f0 (height :odd-progress))
      ;; 9 above and below the line, for 6.
      (assert-eql 6f0 (- (height :odd-field) (height :plain-field)))
      ;; Each option as tall as a checkbox, 33 between them side by side and 11 one under another.
      (destructuring-bind ((x1 y1 w1 h1) (x2 y2 w2 h2)) (options :odd-across)
        (declare (ignore y1 y2))
        (assert-eql 30f0 h1)
        (assert-eql 30f0 h2)
        (assert-eql 33f0 (- x2 (+ x1 w1)))
        (assert-eql (- (+ x2 w2) x1) (size-of screen :odd-across)))
      (destructuring-bind ((x1 y1 w1 h1) (x2 y2 w2 h2)) (options :odd-down)
        (declare (ignore x1 x2 w1 w2))
        (assert-eql 11f0 (- y2 (+ y1 h1)))
        (assert-eql (- (+ y2 h2) y1) (nth-value 1 (size-of screen :odd-down)))))))

(defun make-odd-popover ()
  (lambda ()
    (with-theme (:test-odd)
      (column (:gap 8)
        (label "Anchor" :id :odd-anchor)
        (popover (:id :odd-popover :open t :anchor :odd-anchor)
          (label "One" :id :pop-one)
          (label "Two" :id :pop-two))))))

(defun make-odd-menu ()
  (state-let ((size 0))
    (lambda ()
      (with-theme (:test-odd)
        (multiple-value-bind (index changed) (dropdown '("Small" "Large") size :id :odd-size)
          (when changed (setf size index)))))))

(defun make-odd-dialog ()
  (lambda ()
    (with-theme (:test-odd)
      (dialog (:id :odd-dialog :open t :title "Sure?")
        (label "One" :id :dialog-one)
        (label "Two" :id :dialog-two)))))

(defun bounds-of (screen id)
  (multiple-value-list (core:lookup-target (screen-app screen) id)))

(define-test popovers-dialogs-and-menus-have-the-room-their-theme-says ()
  (eval `(define-theme :test-odd () ,@+odd-sizes+))
  (let ((screen (show 'make-odd-popover)))
    (destructuring-bind (px py pw ph) (bounds-of screen :odd-popover)
      (declare (ignore pw ph))
      (destructuring-bind (x1 y1 w1 h1) (bounds-of screen :pop-one)
        (declare (ignore w1))
        (assert-eql 13f0 (- x1 px))
        (assert-eql 13f0 (- y1 py))
        (assert-eql 5f0 (- (second (bounds-of screen :pop-two)) (+ y1 h1))))))
  (let* ((screen (show 'make-odd-menu))
         (session (screen-session screen)))
    ;; The menu of the dropdown: rows 26 tall, 7 inside its edge.
    (m:click session :id :odd-size)
    (destructuring-bind (mx my mw mh) (s:node-bounds (menu-of screen))
      (declare (ignore mw))
      (let ((rows (mapcar #'s:node-bounds (s:node-children (menu-of screen)))))
        (assert-eql 7f0 (- (second (first rows)) my))
        (assert-eql 7f0 (- (first (first rows)) mx))
        (assert-eql 26f0 (fourth (first rows)))
        (assert-eql 26f0 (- (second (second rows)) (second (first rows))))
        (assert-eql (+ 7f0 26f0 26f0 7f0) mh))))
  (let ((screen (show 'make-odd-dialog)))
    (destructuring-bind (dx dy dw dh) (bounds-of screen :odd-dialog)
      (declare (ignore dy dw dh))
      (destructuring-bind (x1 y1 w1 h1) (bounds-of screen :dialog-one)
        (declare (ignore w1 y1))
        (assert-eql 27f0 (- x1 dx))
        (assert-eql 15f0 (- (second (bounds-of screen :dialog-two))
                            (+ (second (bounds-of screen :dialog-one)) h1)))))))

;;; --- the glyph atlas fills up ---

(defparameter *typed-size* 15
  "The size of the words that change in MAKE-MANY-SIZES, as a font size typed in a designer would.")

(defun make-many-sizes ()
  "Words that stay as they are, in the window's theme, and words in a theme whose font size changes."
  (lambda ()
    (column (:gap 4)
      (label "These words stay as they are, the same in every frame." :id :steady)
      (with-theme ((make-theme :font-size *typed-size*))
        (paragraph "The quick brown fox jumps over the lazy dog, 0123456789." :id :typed)))))

(defun draw-on-host (host instance)
  "Build a frame of INSTANCE on HOST and draw it into a texture of its own; its pixels."
  (let ((context (host:host-context host))
        (list (host:host-display-list host)))
    (setf (core:context-width context) 360f0
          (core:context-height context) 240f0
          (core:context-scale context) 2f0)
    (core:settle-frame instance context list)
    (let ((target (target:make-offscreen-target (device:default-device) 720 480)))
      (renderer:render-display-list (host:host-renderer host) list
                                    (target:offscreen-target-descriptor target) 0 360f0 240f0 2f0)
      (renderer:wait-for-last-frame (host:host-renderer host))
      (target:read-pixels target))))

(define-test an-atlas-that-fills-up-starts-again-and-draws-the-same ()
  ;; A small atlas, so that a few sizes fill it, as each letter of a long font name typed into the
  ;; theme designer did: every one is a new font, whose glyphs are rasterized again.
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (flet ((app ()
               (core:with-kept-state ((core:make-kept-state))
                 (app:make-app-instance 'make-many-sizes)))
             (host (atlas-size)
               (host:make-host (make-instance 'core:app-instance) (device:default-device)
                               target:+pixel-format-bgra8-unorm+ :capacity 8192 :scale 2
                                                                 :atlas-size atlas-size)))
        (let* ((small (host 384))
               (glyphs (renderer:renderer-atlas (host:host-renderer small)))
               (instance (app))
               (emptied 0))
          (loop :for size :from 8 :to 26
                :do (let ((before (atlas:glyph-atlas-count glyphs))
                          (*typed-size* size))
                      (draw-on-host small instance)
                      (when (< (atlas:glyph-atlas-count glyphs) before)
                        (incf emptied))))
          ;; It filled up and started again, more than once; the steady words, painted in the first
          ;; frame and kept, were painted again each time, with the glyphs as they are now.
          (assert-true (>= emptied 2) emptied)
          (let ((*typed-size* 18))
            (let ((after (draw-on-host small instance))
                  (fresh (draw-on-host (host 1024) (app))))
              (assert-true (some #'plusp after))
              (assert-true (equalp fresh after)))))))))

;;; --- selecting text with the pointer ---

(defun make-selecting ()
  (state-let ((line "hello brave world")
              (notes (format nil "one two~%three four~%five")))
    (lambda ()
      (column (:gap 10)
        (setf line (text-field line :id :line :width 300))
        (setf notes (text-area notes :id :notes))
        (when (button "Reset" :id :reset)
          (setf line "reset"))))))

(defun glyph-point (screen id n &optional (across 0.5))
  "A point over the Nth character drawn in the control ID, ACROSS of the way from its left edge."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) id)
    (let ((list (screen-list screen)) (seen 0))
      (dotimes (index (core:display-list-count list) (error "No glyph ~d in ~s." n id))
        (when (and (= (core:display-list-kind list index) core:+command-glyph+)
                   (<= left (core:display-list-x list index) (+ left width -1))
                   (<= top (core:display-list-y list index) (+ top height -1)))
          (when (= seen n)
            (return (values (+ (core:display-list-x list index)
                               (* across (core:display-list-width list index)))
                            (+ (core:display-list-y list index)
                               (/ (core:display-list-height list index) 2)))))
          (incf seen))))))

(defun presses (screen x y count &key (release t))
  "COUNT presses in a row at X, Y, as a double-click (2) or a triple-click (3) is; the last is not
released unless RELEASE."
  (pointer screen :move x y)
  (loop :for clicks :from 1 :to count
        :do (pointer screen :down x y :button 0 :buttons 1 :clicks clicks)
            (when (or release (< clicks count))
              (pointer screen :up x y :button 0))))

(defun copied (screen)
  "Command-C, and what is on the clipboard then."
  (core:handle-event (screen-app screen)
                     (core:make-key-event :timestamp 0 :kind :down :logical-key #\c
                                          :modifiers core:+modifier-super+))
  (draw screen)
  (core:context-clipboard-text (screen-context screen)))

(define-test a-double-click-selects-the-word-and-a-triple-click-all-of-a-field ()
  (let ((screen (show 'make-selecting)))
    ;; On the right half of the last letter of "brave", which is nearer the place after it.
    (multiple-value-bind (x y) (glyph-point screen :line 10 0.8)
      (presses screen x y 2)
      (assert-equal "brave" (copied screen)))
    (multiple-value-bind (x y) (glyph-point screen :line 7)
      (presses screen x y 3)
      (assert-equal "hello brave world" (copied screen)))
    ;; One press puts the caret back, with nothing selected.
    (multiple-value-bind (x y) (glyph-point screen :line 7)
      (presses screen x y 1)
      (core:context-set-clipboard-text (screen-context screen) "before")
      (assert-equal "before" (copied screen)))))

(define-test a-drag-after-a-double-click-selects-whole-words-either-way ()
  (let ((screen (show 'make-selecting)))
    (multiple-value-bind (x y) (glyph-point screen :line 7)
      (presses screen x y 2 :release nil)
      ;; Into "world", and back past the start into "hello".
      (multiple-value-bind (x y) (glyph-point screen :line 13)
        (pointer screen :move x y :buttons 1))
      (assert-equal "brave world" (copied screen))
      (multiple-value-bind (x y) (glyph-point screen :line 1)
        (pointer screen :move x y :buttons 1)
        (pointer screen :up x y :button 0))
      (assert-equal "hello brave" (copied screen)))))

(define-test in-a-text-area-a-double-click-selects-a-word-and-a-triple-click-its-paragraph ()
  (let ((screen (show 'make-selecting)))
    ;; The glyphs drawn are "one two", "three four" and "five": the 14th is the "o" of "four".
    (multiple-value-bind (x y) (glyph-point screen :notes 14)
      (presses screen x y 2)
      (assert-equal "four" (copied screen))
      (presses screen x y 3)
      (assert-equal (format nil "three four~%") (copied screen)))))

;;; --- text that is not edited, selected and copied ---

(defun make-readable ()
  (state-let ((typed ""))
    (lambda ()
      (column (:gap 10)
        (label "Order 4417 shipped" :id :order :selectable t)
        (label "Not for copying" :id :plain)
        (paragraph (format nil "first line~%second line") :id :notes :selectable t)
        (setf typed (text-field typed :id :field))))))

(defun drag-over (screen id from to)
  "Press on the left of the FROM-th character drawn in ID and drag to the right of the TO-th."
  (multiple-value-bind (x y) (glyph-point screen id from 0.1)
    (presses screen x y 1 :release nil))
  (multiple-value-bind (x y) (glyph-point screen id to 0.9)
    (pointer screen :move x y :buttons 1)
    (pointer screen :up x y :button 0)))

(defun selection-shown-p (screen id)
  "True if the colour of a selection is drawn in the control ID."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) id)
    (let ((list (screen-list screen))
          (color (theme-setting (find-theme :dark) :selection)))
      (loop :for index :below (core:display-list-count list)
              :thereis (and (= (core:display-list-color list index) color)
                            (<= left (core:display-list-x list index) (+ left width))
                            (<= top (core:display-list-y list index) (+ top height)))))))

(defun selection-left (screen id)
  "The left of the colour of a selection drawn highest in the control ID (on its first line)."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) id)
    (let ((list (screen-list screen))
          (color (theme-setting (find-theme :dark) :selection))
          (best nil))
      (dotimes (index (core:display-list-count list) (and best (car best)))
        (let ((x (core:display-list-x list index)) (y (core:display-list-y list index)))
          (when (and (= (core:display-list-color list index) color)
                     (<= left x (+ left width)) (<= top y (+ top height))
                     (or (null best) (< y (cdr best))))
            (setf best (cons x y))))))))

(define-test a-selectable-label-is-selected-with-the-pointer-and-copied ()
  (let ((screen (show 'make-readable)))
    (drag-over screen :order 6 9)
    (assert-equal "4417" (copied screen))
    (multiple-value-bind (x y) (glyph-point screen :order 1)
      (presses screen x y 2)
      (assert-equal "Order" (copied screen)))
    (assert-true (selection-shown-p screen :order))
    ;; Somewhere else has the keyboard: the selection is not shown, and Command-C is the field's.
    (click screen :field)
    (assert-false (selection-shown-p screen :order))
    (core:context-set-clipboard-text (screen-context screen) "before")
    (assert-equal "before" (copied screen))
    ;; A label that is not selectable is not selected by a drag.
    (drag-over screen :plain 0 6)
    (assert-false (selection-shown-p screen :plain))
    (assert-equal "before" (copied screen))))

(define-test a-selectable-paragraph-is-selected-across-its-lines-and-all-with-command-a ()
  (let ((screen (show 'make-readable)))
    ;; The glyphs drawn are "first line" with its line feed and "second line": from the "l" of the
    ;; first to the "d".
    (drag-over screen :notes 6 16)
    (assert-equal (format nil "line~%second") (copied screen))
    (assert-eql (values (glyph-point screen :notes 6 0)) (selection-left screen :notes))
    ;; Command-A selects all, and it is shown so, from the first character.
    (core:handle-event (screen-app screen)
                       (core:make-key-event :timestamp 0 :kind :down :logical-key #\a
                                            :modifiers core:+modifier-super+))
    (draw screen)
    (assert-equal (format nil "first line~%second line") (copied screen))
    (assert-eql (values (glyph-point screen :notes 0 0)) (selection-left screen :notes))
    ;; A drag to the right of the end of the first line stops at its end, before the line feed.
    (multiple-value-bind (x y) (glyph-point screen :notes 0 0.1)
      (presses screen x y 1 :release nil)
      (multiple-value-bind (left top width) (core:lookup-target (screen-app screen) :notes)
        (declare (ignore left top))
        (pointer screen :move (+ x width -2) y :buttons 1)
        (pointer screen :up (+ x width -2) y :button 0)))
    (assert-equal "first line" (copied screen))
    ;; Text that is selected only takes the keyboard from a press: Tab goes past it.
    (core:handle-event (screen-app screen)
                       (core:make-key-event :timestamp 0 :kind :down :logical-key #\Tab
                                            :modifiers 0))
    (draw screen)
    (assert-true (s:node-property (node-of screen :field) :focused))))

(defun make-dialog-with-a-number ()
  (lambda ()
    (column ()
      (label "Signing in" :id :behind)
      (dialog (:id :code :open t :title "Your code")
        (label "Use 735-921 to sign in." :id :code-text :selectable t)
        (button "Done" :id :done)))))

(define-test a-selectable-label-in-a-dialog-keeps-the-keyboard-to-be-copied ()
  (let ((screen (show 'make-dialog-with-a-number)))
    (drag-over screen :code-text 4 10)
    (assert-true (selection-shown-p screen :code-text))
    (assert-equal "735-921" (copied screen))))

;;; --- undo ---

(defun key (screen key &rest modifiers)
  "Press KEY (a character or a key code) with MODIFIERS (:command, :shift)."
  (core:handle-event (screen-app screen)
                     (core:make-key-event :timestamp 0 :kind :down :logical-key key
                                          :modifiers (logior (if (member :command modifiers)
                                                                 core:+modifier-super+ 0)
                                                             (if (member :shift modifiers)
                                                                 core:+modifier-shift+ 0))))
  (draw screen))

(defun value-of (screen id)
  (s:node-value (node-of screen id)))

(define-test command-z-undoes-a-run-of-typing-a-run-of-deleting-and-a-paste-each-as-one ()
  (let ((screen (show 'make-selecting)))
    (click screen :line)
    (key screen core:+key-end+)
    (type-text screen " now")
    (key screen #\Backspace)
    (key screen #\Backspace)
    (core:context-set-clipboard-text (screen-context screen) "!")
    (key screen #\v :command)
    (assert-equal "hello brave world n!" (value-of screen :line))
    (key screen #\z :command)
    (assert-equal "hello brave world n" (value-of screen :line))
    (key screen #\z :command)
    (assert-equal "hello brave world now" (value-of screen :line))
    (key screen #\z :command)
    (assert-equal "hello brave world" (value-of screen :line))
    ;; Nothing more to undo.
    (key screen #\z :command)
    (assert-equal "hello brave world" (value-of screen :line))
    ;; Redone, one at a time, in order.
    (key screen #\z :command :shift)
    (assert-equal "hello brave world now" (value-of screen :line))
    (key screen #\z :command :shift)
    (assert-equal "hello brave world n" (value-of screen :line))
    ;; A new edit forgets what was undone.
    (type-text screen "o")
    (key screen #\z :command :shift)
    (assert-equal "hello brave world no" (value-of screen :line))))

(define-test moving-the-caret-ends-a-run-of-typing ()
  (let ((screen (show 'make-selecting)))
    (click screen :line)
    (key screen core:+key-end+)
    (type-text screen "ab")
    (key screen core:+key-left+)
    (type-text screen "c")
    (assert-equal "hello brave worldacb" (value-of screen :line))
    (key screen #\z :command)
    (assert-equal "hello brave worldab" (value-of screen :line))
    (key screen #\z :command)
    (assert-equal "hello brave world" (value-of screen :line))
    ;; And so does a press in the field.
    (type-text screen "d")
    (multiple-value-bind (x y) (glyph-point screen :line 0 0.1)
      (presses screen x y 1))
    (type-text screen "e")
    (assert-equal "ehello brave worldd" (value-of screen :line))
    (key screen #\z :command)
    (assert-equal "hello brave worldd" (value-of screen :line))))

(define-test in-a-text-area-return-is-an-edit-of-its-own-and-text-the-app-gives-starts-again ()
  (let ((screen (show 'make-selecting)))
    (click screen :notes)
    (key screen core:+key-end+ :command)
    (type-text screen "six")
    (key screen #\Return)
    (type-text screen "seven")
    (key screen #\z :command)
    (assert-equal (format nil "one two~%three four~%fivesix~%") (value-of screen :notes))
    (key screen #\z :command)
    (assert-equal (format nil "one two~%three four~%fivesix") (value-of screen :notes))
    ;; The app puts text of its own in the field: what was typed before it is not undone into it.
    (click screen :line)
    (type-text screen "x")
    (click screen :reset)
    (click screen :line)
    (key screen #\z :command)
    (assert-equal "reset" (value-of screen :line))))

(defun make-only-a-dialog ()
  (lambda ()
    (dialog (:id :code :open t :title "Your code")
      (label "Use 735-921 to sign in." :id :code-text :selectable t)
      (button "Done" :id :done))))

(define-test an-app-that-is-only-a-dialog-shows-it ()
  (let ((screen (show 'make-only-a-dialog)))
    (assert-equal "Use 735-921 to sign in." (text-in screen :code-text))
    (assert-eq :dialog (s:node-role (node-of screen :code)))
    (drag-over screen :code-text 4 10)
    (assert-equal "735-921" (copied screen))))

;;; --- the shape of the pointer ---

(defun make-pointing ()
  (state-let ((typed "") (notes ""))
    (lambda ()
      (column (:gap 10)
        (setf typed (text-field typed :id :field))
        (text-field "off" :id :off :enabled nil)
        (setf notes (text-area notes :id :notes :rows 2))
        (label "Copy me" :id :copyable :selectable t)
        (label "Not me" :id :plain)
        (button "Press" :id :press)
        (canvas 100 40 (lambda (list context width height)
                         (declare (ignore list context width height)))
                :id :sketch :cursor :crosshair)))))

(defun cursor-over (screen id)
  "The shape of the pointer with it over the middle of the control ID."
  (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) id)
    (pointer screen :move (+ x (/ width 2)) (+ y (/ height 2)))
    (core:context-cursor (screen-context screen))))

(define-test the-pointer-is-a-text-cursor-over-text-and-what-a-canvas-asks-for ()
  (let ((screen (show 'make-pointing)))
    (assert-eq :text (cursor-over screen :field))
    (assert-eq :arrow (cursor-over screen :off))
    (assert-eq :text (cursor-over screen :notes))
    (assert-eq :text (cursor-over screen :copyable))
    (assert-eq :arrow (cursor-over screen :plain))
    (assert-eq :arrow (cursor-over screen :press))
    (assert-eq :crosshair (cursor-over screen :sketch))
    ;; A selection dragged out of a field keeps the text cursor until the button comes up.
    (cursor-over screen :field)
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :field)
      (pointer screen :down (+ x 4) (+ y 4) :button 0 :buttons 1))
    (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) :press)
      (pointer screen :move (+ x (/ width 2)) (+ y (/ height 2)) :buttons 1)
      (assert-eq :text (core:context-cursor (screen-context screen)))
      (pointer screen :up (+ x (/ width 2)) (+ y (/ height 2)) :button 0))
    (assert-eq :arrow (core:context-cursor (screen-context screen)))))

(define-test a-canvas-given-a-shape-that-is-not-a-cursor-says-so ()
  (assert-error 'error
                (show (lambda ()
                        (lambda ()
                          (canvas 10 10 (lambda (&rest arguments) (declare (ignore arguments)))
                                  :id :bad :cursor :hourglass))))))

(define-test the-view-shows-each-cursor-the-system-has ()
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication")
      (let ((view (view:make-metal-view :width 100 :height 100)))
        (dolist (shape core:+cursors+)
          (assert-true (plusp (view:cursor-object shape)) shape))
        (assert-eql (objc:msg-send :id (objc:objc-class "NSCursor") "IBeamCursor")
                    (view:cursor-object :text))
        ;; The pointer comes in (AppKit asks the view for its cursor), and the frame says :text.
        (objc:msg-send :void (view:metal-view-object view) "cursorUpdate:" (:id 0))
        (view:set-view-cursor view :text)
        (assert-eq :text (view:metal-view-cursor view))
        (assert-eql (objc:msg-send :id (objc:objc-class "NSCursor") "IBeamCursor")
                    (objc:msg-send :id (objc:objc-class "NSCursor") "currentCursor"))
        (view:set-view-cursor view :arrow)
        (assert-eql (objc:msg-send :id (objc:objc-class "NSCursor") "arrowCursor")
                    (objc:msg-send :id (objc:objc-class "NSCursor") "currentCursor"))))))

;;; --- shortcuts and the menu bar ---

(defun make-saving ()
  (state-let ((typed "") (saves 0))
    (lambda ()
      (column (:gap 10)
        (setf typed (text-field typed :id :field))
        (when (shortcut #\s :command)
          (incf saves))
        ;; Taken by the one above: never true.
        (when (shortcut #\s :command)
          (setf saves 100))
        (when (shortcut #\z :command)
          (setf typed "the app's undo"))
        (label (format nil "~d saves" saves) :id :saves)))))

(define-test a-shortcut-is-true-when-its-keys-are-pressed-and-a-field-keeps-its-own ()
  (let ((screen (show 'make-saving)))
    (key screen #\s :command)
    (assert-equal "1 saves" (text-in screen :saves))
    ;; Not with another modifier, nor without Command: typed in the field, an s is a letter.
    (key screen #\s :command :shift)
    (assert-equal "1 saves" (text-in screen :saves))
    (click screen :field)
    (type-text screen "as")
    (assert-equal "1 saves" (text-in screen :saves))
    (assert-equal "as" (value-of screen :field))
    (key screen #\s :command)
    (assert-equal "2 saves" (text-in screen :saves))
    ;; Command-Z is the field's, which has the keyboard and is described first.
    (key screen #\z :command)
    (assert-equal "" (value-of screen :field))))

(define-test the-edit-menu-gives-the-app-the-keys-of-its-commands ()
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let* ((app (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication"))
             (keys '())
             (view (view:make-metal-view
                    :width 100 :height 100
                    :event-handler (lambda (event)
                                     (when (typep event 'core:key-event)
                                       (push (list (core:key-event-logical-key event)
                                                   (core:key-event-modifiers event))
                                             keys))))))
        (let ((almighty-bolt/platform/macos/app::*ends-process* nil))
          (almighty-bolt/platform/macos/app::install-menu app))
        (flet ((item (menu title)
                 (let ((menu (objc:msg-send :id (objc:msg-send :id (objc:msg-send :id app "mainMenu")
                                                               "itemWithTitle:"
                                                               (:id (objc:ns-string menu)))
                                            "submenu")))
                   (objc:msg-send :id menu "itemWithTitle:" (:id (objc:ns-string title))))))
          ;; Each command of the Edit menu, sent to the view as the menu sends it, is its key.
          (loop :for (title action character modifiers)
                  :in `(("Undo" "undo:" #\z ,core:+modifier-super+)
                        ("Redo" "redo:" #\z ,(logior core:+modifier-super+ core:+modifier-shift+))
                        ("Cut" "cut:" #\x ,core:+modifier-super+)
                        ("Copy" "copy:" #\c ,core:+modifier-super+)
                        ("Paste" "paste:" #\v ,core:+modifier-super+)
                        ("Select All" "selectAll:" #\a ,core:+modifier-super+))
                :do (let ((item (item "Edit" title)))
                      (assert-true (plusp item) title)
                      (assert-eql (objc:register-selector action) (objc:msg-send :ulong item "action")
                                  title)
                      (objc:msg-send :void (view:metal-view-object view) action (:id 0))
                      (assert-equal (list character modifiers) (first keys) title)))
          ;; Redo is Command-Shift-Z, not Undo's key again.
          (assert-eql (logior (ash 1 20) (ash 1 17))
                      (objc:msg-send :ulong (item "Edit" "Redo") "keyEquivalentModifierMask"))
          ;; Quit asks the application to end, which in a Lisp that a person works in it does not:
          ;; it closes the windows instead (the smoke check sees them close), and the Lisp stays.
          (assert-eql (objc:register-selector "terminate:")
                      (objc:msg-send :ulong (item "Application" "Quit") "action"))
          (let ((delegate (objc:msg-send :id (objc:msg-send
                                              :id almighty-bolt/platform/macos/app::*app-delegate-class*
                                              "alloc")
                                         "init")))
            (let ((almighty-bolt/platform/macos/app::*ends-process* nil))
              (assert-eql 0 (objc:msg-send :ulong delegate "applicationShouldTerminate:" (:id app))))
            (let ((almighty-bolt/platform/macos/app::*ends-process* t))
              (assert-eql 1 (objc:msg-send :ulong delegate "applicationShouldTerminate:" (:id app)))))
          (assert-true (plusp (item "Window" "Minimize"))))))))

;;; --- an id used in two containers ---
;;;
;;; Ids need be unique only within a container, so two panels can each have a label :title and a
;;; field labelled by it. Each field is named by the label in its own panel, and the machine
;;; interface tells the two apart by the container they are in.

(defun make-two-searches ()
  (state-let ((texts (list :hebrew "" :gregorian "")))
    (lambda ()
      (row (:id :searches :gap 8)
        (dolist (calendar '(:hebrew :gregorian))
          (panel (:id calendar :all 4)
            (column (:gap 4)
              (label (if (eq calendar :hebrew) "Hebrew date" "Gregorian date") :id :title)
              (multiple-value-bind (text changed)
                  (text-field (getf texts calendar) :id :field :labelled-by :title :width 80)
                (when changed (setf (getf texts calendar) text)))
              (button "Show" :id :show))))))))

(define-test each-field-is-named-by-the-label-in-its-own-container ()
  (let* ((screen (show 'make-two-searches))
         (fields (s:find-nodes (m:see (screen-session screen)) :role :text-field)))
    (assert-equal '("Hebrew date" "Gregorian date") (mapcar #'s:node-name fields))))

(define-test within-says-which-container-a-thing-with-a-shared-id-is-in ()
  (let* ((screen (show 'make-two-searches))
         (session (screen-session screen)))
    (let ((message (handler-case (progn (m:find-node session :id :show) nil)
                     (m:ambiguous-node (condition) (princ-to-string condition)))))
      (assert-true (search ":within" message) message))
    (let ((gregorian (m:find-node session :id :show :within :gregorian))
          (hebrew (m:find-node session :id :show :within :hebrew)))
      (assert-true (> (first (s:node-bounds gregorian)) (first (s:node-bounds hebrew)))))
    (m:set-value session "7 Nov" :id :field :within :gregorian)
    (assert-equal "7 Nov" (s:node-value (m:find-node session :id :field :within :gregorian)))
    (assert-equal "" (s:node-value (m:find-node session :id :field :within :hebrew)))))

;;; --- a panel with a colour of its own ---

(defun make-clear-panel ()
  (state-let ((clear nil))
    (lambda ()
      (column (:gap 8)
        ;; A shortcut and not a button, so that the panel's colour is all that changes.
        (when (shortcut #\k :command) (setf clear t))
        (label "Command-K clears it" :id :how)
        (panel (:id :pane :all 20 :color (and clear (pack-color 0 0 0 0)))
          (label "x" :id :inside))))))

(define-test a-panel-whose-colour-is-transparent-draws-no-background ()
  (let ((screen (show 'make-clear-panel)))
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :pane)
      ;; Without a colour, the theme's panel colour.
      (assert-eql (theme:theme-panel (theme:find-theme :dark)) (color-at screen (+ x 4) (+ y 4)))
      ;; Black with nothing of it showing, and not the theme's colour again, from the frame it is
      ;; given.
      (core:handle-event (screen-app screen)
                         (core:make-key-event :timestamp 0 :kind :down :logical-key #\k
                                              :modifiers core:+modifier-super+))
      (draw screen)
      (assert-eql 0 (color-at screen (+ x 4) (+ y 4))))))

;;; --- a spacer ---

(defun make-toolbar ()
  (lambda ()
    (column (:gap 8)
      (row (:id :bar :fill t)
        (button "Left" :id :left)
        (spacer)
        (button "Right" :id :right))
      (row (:id :shares :fill t)
        (label "a" :id :a)
        (spacer)
        (label "b" :id :b)
        (spacer :flex 2)
        (label "c" :id :c)))))

(define-test a-spacer-takes-the-free-room-and-is-not-in-the-tree ()
  (let* ((screen (show 'make-toolbar))
         (tree (m:see (screen-session screen))))
    (destructuring-bind (x y width height) (s:node-bounds (first (s:find-nodes tree :id :right)))
      (declare (ignore y height))
      ;; The button after the spacer is at the right edge of the window.
      (assert-true (< (abs (- 360 (+ x width))) 0.5) (list x width)))
    ;; The spacers say nothing: the rows hold only what was put in them besides.
    (assert-equal '(:left :right)
                  (mapcar #'s:node-id (s:node-children (first (s:find-nodes tree :id :bar)))))
    ;; A spacer of flex 2 takes twice what one of flex 1 takes.
    (flet ((left-of (id) (first (s:node-bounds (first (s:find-nodes tree :id id)))))
           (right-of (id) (destructuring-bind (x y w h) (s:node-bounds (first (s:find-nodes tree :id id)))
                            (declare (ignore y h))
                            (+ x w))))
      (let ((one (- (left-of :b) (right-of :a)))
            (two (- (left-of :c) (right-of :b))))
        (assert-true (< (abs (- two (* 2 one))) 1) (list one two))))))

;;; --- a list that shows what is chosen ---

(defun make-long-pick ()
  (state-let ((chosen 40))
    (lambda ()
      (column (:gap 8)
        (when (button "Five" :id :five) (setf chosen 5))
        (multiple-value-bind (index changed)
            (list-box (loop :for i :below 60 :collect (format nil "Item ~d" i)) chosen
                      :id :items :rows 8)
          (when changed (setf chosen index)))))))

(define-test a-list-shows-the-row-the-app-chose-and-leaves-the-wheel-alone ()
  (let* ((screen (show 'make-long-pick))
         (session (screen-session screen)))
    (flet ((first-visible () (s:node-property (node-of screen :items) :first-visible)))
      ;; On its first frame, the chosen row shows.
      (assert-true (<= (first-visible) 40 (+ (first-visible) 7)) (first-visible))
      ;; The wheel moves it away, and while the choice is the same it stays where it was moved.
      (m:scroll session :id :items :dy 20)
      (let ((moved (first-visible)))
        (assert-true (< moved 33) moved)
        (draw screen)
        (assert-eql moved (first-visible)))
      ;; A row the app chooses is brought into view.
      (m:scroll session :id :items :dy -100)
      (click screen :five)
      (assert-true (<= (first-visible) 5 (+ (first-visible) 7)) (first-visible)))))

;;; --- containers given a size ---

(defun make-sized ()
  (state-let ((taller nil))
    (lambda ()
      (column (:gap 8)
        ;; A shortcut, so that the panel's height is all that changes.
        (when (shortcut #\t :command) (setf taller t))
        (column (:id :least :min-height 60)
          (label "x" :id :x))
        (column (:id :narrow :max-width 120)
          (paragraph "A paragraph of words that goes on well past a hundred and twenty points"
                     :id :words))
        (panel (:id :fixed :height (if taller 90 40) :all 4)
          (column ()
            (dotimes (i 6)
              (label (format nil "Line ~d" i) :id i))))))))

(defun clip-of (screen x y)
  "The clip rectangle on SCREEN's display list that starts at X, Y, as a list, or NIL."
  (let ((list (screen-list screen)))
    (dotimes (index (core:display-list-count list))
      (when (and (= (core:display-list-kind list index) core:+command-clip+)
                 (= x (core:display-list-x list index)) (= y (core:display-list-y list index)))
        (return (list (core:display-list-width list index) (core:display-list-height list index)))))))

(define-test a-container-takes-the-size-it-is-given ()
  (let ((screen (show 'make-sized)))
    ;; At least 60 tall, though a label is less.
    (assert-eql 60f0 (fourth (bounds-of screen :least)))
    ;; No wider than 120, and the paragraph in it is broken to that width.
    (assert-true (<= (third (bounds-of screen :narrow)) 120) (bounds-of screen :narrow))
    (assert-true (<= (third (bounds-of screen :words)) 120) (bounds-of screen :words))
    (assert-true (> (fourth (bounds-of screen :words)) (* 2 (fourth (bounds-of screen :x)))))
    ;; Exactly 40 tall, with six lines in it that are taller than that: they are cut off at its
    ;; edges.
    (destructuring-bind (x y width height) (bounds-of screen :fixed)
      (assert-eql 40f0 height)
      (assert-equal (list width height) (clip-of screen x y)))
    ;; Given another height, it is that height from the next frame.
    (core:handle-event (screen-app screen)
                       (core:make-key-event :timestamp 0 :kind :down :logical-key #\t
                                            :modifiers core:+modifier-super+))
    (draw screen)
    (assert-eql 90f0 (fourth (bounds-of screen :fixed)))))

;;; --- a grid ---

(defun make-month ()
  (state-let ((uniform t))
    (lambda ()
      (column (:gap 8)
        (when (shortcut #\u :command) (setf uniform nil))
        (grid (:id :month :columns 3 :gap 4 :row-heights (if uniform :uniform :auto) :width 300)
          (dotimes (i 6)
            (panel (:id i :all 2)
              (column ()
                (label "x" :id :a)
                ;; The fifth cell, in the second row, is taller than the others.
                (when (= i 4)
                  (label "y" :id :b)
                  (label "z" :id :c))))))
        (grid (:id :shares :columns 2 :column-widths '(1 2) :gap 4 :width 304)
          (label "a" :id :one)
          (label "b" :id :two))))))

(define-test a-grid-lines-its-cells-up-in-columns-and-rows ()
  (let* ((screen (show 'make-month))
         (session (screen-session screen)))
    (flet ((cell (i) (m:find-node session :id i :within :month)))
      ;; Equal columns, each a third of what is left of 300 after the gaps.
      (dotimes (i 6)
        (assert-true (< (abs (- (third (s:node-bounds (cell i))) (/ 292 3))) 0.5) i))
      ;; Uniform rows: every cell is as tall as the tallest.
      (let ((tallest (fourth (s:node-bounds (cell 4)))))
        (dotimes (i 6)
          (assert-eql tallest (fourth (s:node-bounds (cell i))) i)))
      ;; Each cell says where it is.
      (assert-eq :grid (s:node-role (m:find-node session :id :month)))
      (assert-eql 1 (s:node-property (cell 4) :row))
      (assert-eql 1 (s:node-property (cell 4) :column))
      (assert-eql 0 (s:node-property (cell 2) :row))
      (assert-eql 2 (s:node-property (cell 2) :column))
      ;; Rows of their own heights, from the next frame: the first row is shorter than the second,
      ;; and the cells of a row are still one height.
      (core:handle-event (screen-app screen)
                         (core:make-key-event :timestamp 0 :kind :down :logical-key #\u
                                              :modifiers core:+modifier-super+))
      (draw screen)
      (assert-true (< (fourth (s:node-bounds (cell 0))) (fourth (s:node-bounds (cell 4)))))
      (assert-eql (fourth (s:node-bounds (cell 3))) (fourth (s:node-bounds (cell 4))))
      ;; Shares of the width: one column twice the other.
      (let ((one (third (s:node-bounds (m:find-node session :id :one))))
            (two (third (s:node-bounds (m:find-node session :id :two)))))
        (assert-true (< (abs (- two (* 2 one))) 0.5) (list one two))))))

;;; --- a container that is pressed ---

(defun make-days ()
  (state-let ((opened nil) (on nil) (inner 0))
    (lambda ()
      (column (:gap 8)
        (when (pressable (:id :day :all 6)
                (label "27" :id :number)
                (label "7 Nov" :id :date)
                (paragraph "Seder 30" :id :reading))
          (setf opened t))
        (label (if opened "Opened" "Closed") :id :opened)
        (when (pressable (:id :toggle :selected on :look :button :name "Candles" :all 6)
                (label "Show the times" :id :words)
                (when (button "Inner" :id :inner) (incf inner)))
          (setf on (not on)))
        (label (princ-to-string inner) :id :inner-count)))))

(defun glyph-colors-in (screen id)
  "The colours of the characters drawn inside the control ID."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-app screen) id)
    (let ((list (screen-list screen)))
      (remove-duplicates
       (loop :for index :below (core:display-list-count list)
             :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                        (<= left (core:display-list-x list index) (+ left width -1))
                        (<= top (core:display-list-y list index) (+ top height -1)))
               :collect (core:display-list-color list index))))))

(define-test a-press-on-what-is-in-a-pressable-presses-it ()
  (let ((screen (show 'make-days)))
    ;; Over the date, a label inside it, and not over the pressable's own padding.
    (click screen :date)
    (assert-equal "Opened" (text-in screen :opened))))

(define-test a-pressable-is-one-button-named-by-its-words ()
  (let* ((screen (show 'make-days))
         (session (screen-session screen))
         (day (m:find-node session :id :day))
         (toggle (m:find-node session :id :toggle)))
    (assert-eq :button (s:node-role day))
    (assert-equal "27, 7 Nov, Seder 30" (s:node-name day))
    ;; Its labels are its name, and not things of their own.
    (assert-eq nil (s:node-children day))
    ;; A toggle, named as it was told, holding the button inside it.
    (assert-eq :toggle-button (s:node-role toggle))
    (assert-equal "Candles" (s:node-name toggle))
    (assert-eq nil (s:node-property toggle :selected))
    (assert-equal '(:inner) (mapcar #'s:node-id (s:node-children toggle)))
    ;; Pressed on its words: at its middle is the button inside it, which takes that press.
    (click screen :words)
    (assert-true (s:node-property (m:find-node session :id :toggle) :selected))
    ;; On, it is drawn in the selected row's colour, though it looks like a button.
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :toggle)
      (assert-eql (theme:theme-row-selected (theme:find-theme :dark))
                  (color-at screen (+ x 2) (+ y 2))))))

(define-test a-button-inside-a-pressable-takes-its-own-press ()
  (let ((screen (show 'make-days)))
    (click screen :inner)
    (assert-equal "1" (text-in screen :inner-count))
    (assert-eq nil (s:node-property (node-of screen :toggle) :selected))))

(define-test the-keyboard-presses-a-pressable ()
  (let* ((screen (show 'make-days))
         (session (screen-session screen)))
    (m:press-key session :tab)
    (assert-true (s:node-property (node-of screen :day) :focused))
    (m:press-key session :space)
    (assert-equal "Opened" (text-in screen :opened))))

(define-test a-pressable-answers-the-pointer-and-looks-like-a-button-when-asked ()
  (let* ((screen (show 'make-days))
         (theme (theme:find-theme :dark)))
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :day)
      (assert-eql (theme:theme-panel theme) (color-at screen (+ x 2) (+ y 2)))
      (pointer screen :move (+ x 2) (+ y 2))
      (assert-false (eql (theme:theme-panel theme) (color-at screen (+ x 2) (+ y 2)))))
    ;; The look of a button: its colour, and the words in it in the button's text colour.
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :toggle)
      (assert-eql (theme:theme-button theme) (color-at screen (+ x 2) (+ y 2))))
    (assert-equal (list (theme:theme-button-text theme)) (glyph-colors-in screen :words))
    (assert-equal (list (theme:theme-text theme)) (glyph-colors-in screen :date))))

;;; --- colours that follow the theme ---

(defun make-tinted ()
  (lambda ()
    (column (:gap 8)
      (panel (:id :dark-tint :all 10 :color '(:accent :alpha 0.25))
        (label "a" :id :a))
      (with-theme (:light)
        (panel (:id :light-tint :all 10 :color '(:accent :alpha 0.25))
          (label "b" :id :b :color :dim-text)))
      (panel (:id :mixed :all 10 :color '(:mix :accent :background 0.5))
        (label "c" :id :c :color "#ff0000")))))

(define-test a-colour-named-by-the-theme-is-the-theme-it-is-drawn-in ()
  (let ((screen (show 'make-tinted))
        (dark (theme:find-theme :dark))
        (light (theme:find-theme :light)))
    (flet ((corner (id)
             (multiple-value-bind (x y) (core:lookup-target (screen-app screen) id)
               (color-at screen (+ x 3) (+ y 3)))))
      (assert-eql (dpb 64 (byte 8 0) (theme:theme-accent dark)) (corner :dark-tint))
      (assert-eql (dpb 64 (byte 8 0) (theme:theme-accent light)) (corner :light-tint))
      (assert-false (eql (corner :dark-tint) (corner :light-tint)))
      (assert-eql (theme:mix-colors (theme:theme-accent dark) (theme:theme-background dark) 0.5)
                  (corner :mixed)))
    (assert-equal (list (theme:theme-dim-text light)) (glyph-colors-in screen :b))
    (assert-equal (list (theme:parse-color "#ff0000")) (glyph-colors-in screen :c))))

;;; --- a menu longer than the window ---

(defparameter +trees+
  (loop :for i :below 60
        :collect (format nil "~a ~2,'0d" (aref #("Ash" "Birch" "Cedar" "Elm" "Fir" "Oak") (floor i 10)) i)))

(defun make-trees ()
  (state-let ((chosen 0) (few 0))
    (lambda ()
      (column (:gap 4)
        (multiple-value-bind (index changed) (dropdown +trees+ chosen :id :tree)
          (when changed (setf chosen index)))
        (label (nth chosen +trees+) :id :tree-chosen)
        (multiple-value-bind (index changed) (dropdown +trees+ few :id :few :max-rows 5)
          (when changed (setf few index)))))))

(defun menu-rows-of (session)
  (mapcar #'s:node-name (s:find-nodes (m:see session) :role :menu-item)))

(define-test a-long-menu-shows-what-fits-and-the-keys-and-wheel-move-it ()
  (let* ((screen (show 'make-trees))
         (session (screen-session screen)))
    (m:click session :id :tree)
    (let ((rows (menu-rows-of session))
          (menu (first (s:find-nodes (m:see session) :role :menu))))
      ;; Some of the sixty, the first among them, in a menu inside the window.
      (assert-true (< 3 (length rows) 60) (length rows))
      (assert-equal "Ash 00" (first rows))
      (assert-true (<= (+ (second (s:node-bounds menu)) (fourth (s:node-bounds menu))) 320)
                   (s:node-bounds menu))
      ;; The wheel moves the rows.
      (m:scroll session :role :menu :dy -5)
      (assert-false (equal "Ash 00" (first (menu-rows-of session)))))
    ;; Down thirty times: the highlighted row is in view, and Return chooses it.
    (dotimes (i 30) (m:press-key session :down))
    (assert-true (member "Elm 30" (menu-rows-of session) :test #'equal) (menu-rows-of session))
    (m:press-key session :return)
    (assert-equal "Elm 30" (text-in screen :tree-chosen))
    ;; Letters typed go to the first row that begins with them.
    (m:click session :id :tree)
    (m:press-key session #\o)
    (m:press-key session #\a)
    (m:press-key session :return)
    (assert-equal "Oak 50" (text-in screen :tree-chosen))))

(define-test a-menu-shows-no-more-rows-than-it-is-told-and-an-agent-reaches-any ()
  (let* ((screen (show 'make-trees))
         (session (screen-session screen)))
    (m:click session :id :few)
    (assert-eql 5 (length (menu-rows-of session)))
    (m:press-key session :escape)
    (m:set-value session "Fir 45" :id :tree)
    (assert-equal "Fir 45" (text-in screen :tree-chosen))
    (m:set-value session "Birch 12" :id :tree)
    (assert-equal "Birch 12" (text-in screen :tree-chosen))))

;;; --- a segmented choice ---

(defun make-views ()
  (state-let ((view 0))
    (lambda ()
      (column (:gap 8)
        (multiple-value-bind (index changed)
            (choice '("Readings" "Calendar") view :id :view :style :segmented)
          (when changed (setf view index)))
        (label (nth view '("Readings" "Calendar")) :id :view-shown)))))

(define-test a-segmented-choice-is-joined-segments-and-chooses-as-a-choice-does ()
  (let* ((screen (show 'make-views))
         (session (screen-session screen))
         (theme (theme:find-theme :dark))
         (node (node-of screen :view)))
    (assert-eq :radio-group (s:node-role node))
    (assert-eq :segmented (s:node-property node :style))
    ;; Joined: the second segment begins where the first ends.
    (destructuring-bind (first second) (s:node-property node :option-bounds)
      (assert-true (< (abs (- (second first) (second second))) 0.5))
      (assert-true (< (abs (- (+ (first first) (third first)) (first second))) 0.5) (list first second))
      ;; A press on the second chooses it, and it is filled, its words in the button's colour.
      (pointer screen :move (+ (first second) 10) (+ (second second) 10))
      (pointer screen :down (+ (first second) 10) (+ (second second) 10) :button 0 :buttons 1)
      (pointer screen :up (+ (first second) 10) (+ (second second) 10) :button 0)
      (assert-equal "Calendar" (text-in screen :view-shown))
      (assert-eql (theme:theme-row-selected theme)
                  (color-at screen (+ (first second) 10) (+ (second second) 10))))
    (m:set-value session "Readings" :id :view)
    (assert-equal "Readings" (text-in screen :view-shown))))

;;; --- a tab bar ---

(defun make-tabbed ()
  (state-let ((page 0))
    (lambda ()
      (column (:gap 8)
        (multiple-value-bind (index changed) (tabs '("Design" "Wire" "Theme") page :id :pages)
          (when changed (setf page index)))
        (label (nth page '("Designing" "Wiring" "Theming")) :id :page-shown)))))

(define-test a-tab-bar-chooses-the-page-shown-and-marks-the-tab-chosen ()
  (let* ((screen (show 'make-tabbed))
         (session (screen-session screen))
         (theme (theme:find-theme :dark))
         (node (node-of screen :pages)))
    (assert-eq :radio-group (s:node-role node))
    (assert-eq :tabs (s:node-property node :style))
    (destructuring-bind (design wire theme-tab) (s:node-property node :option-bounds)
      (declare (ignore theme-tab))
      (flet ((under (bounds)
               ;; Under the middle of a tab's words, where the chosen one is marked.
               (color-at screen (+ (first bounds) (/ (third bounds) 2))
                         (+ (second bounds) (fourth bounds) -2))))
        (assert-eql (theme:theme-row-selected theme) (under design))
        (assert-false (eql (theme:theme-row-selected theme) (under wire)))
        ;; A press on the second chooses it, and the mark goes with it.
        (let ((x (+ (first wire) 10)) (y (+ (second wire) 6)))
          (pointer screen :move x y)
          (pointer screen :down x y :button 0 :buttons 1)
          (pointer screen :up x y :button 0))
        (draw screen)
        (assert-equal "Wiring" (text-in screen :page-shown))
        (assert-eql (theme:theme-row-selected theme) (under wire))
        (assert-false (eql (theme:theme-row-selected theme) (under design)))))
    ;; Tab gives it the keyboard (a press does not, as on a Mac), and Right goes to the next tab.
    (key screen #\Tab)
    (key screen core:+key-right+)
    (assert-equal "Theming" (text-in screen :page-shown))
    (m:set-value session "Design" :id :pages)
    (assert-equal "Designing" (text-in screen :page-shown))))

;;; --- a tree view ---

(defparameter +garden+ '(("Fruit" ("Apple") ("Pear")) ("Veg" ("Leek")))
  "Items of a tree: each its name and its children.")

(defun garden-key (item)
  (intern (string-upcase (first item)) :keyword))

(defun make-garden ()
  (state-let ((picked nil))
    (lambda ()
      (column (:id :page :gap 8 :cross-align :stretch)
        (multiple-value-bind (key changed)
            (tree +garden+ :id :garden :children #'rest :label #'first :key #'garden-key
                                :selected picked)
          (when changed (setf picked key)))
        (label (format nil "~(~a~)" picked) :id :picked)))))

(define-test a-tree-selects-opens-and-closes-by-the-pointer-and-the-arrows ()
  (let ((screen (show 'make-garden)))
    (flet ((shown-p (id) (and (core:lookup-target (screen-app screen) id) t))
           ;; Where a row's words begin: the row is as wide as the tree.
           (left-of (id) (first (s:node-bounds (first (s:node-children (node-of screen id))))))
           (press-at (x y)
             (pointer screen :move x y)
             (pointer screen :down x y :button 0 :buttons 1)
             (pointer screen :up x y :button 0)
             (draw screen))
           (picked () (text-in screen :picked)))
      ;; Everything is open until it is closed, each level further in.
      (assert-true (every #'shown-p '(:fruit :apple :pear :veg :leek)))
      (assert-eql 16 (round (- (left-of :apple) (left-of :fruit))))
      (assert-eq :outline (s:node-role (node-of screen :garden)))
      (assert-eq :row (s:node-role (node-of screen :leek)))
      (assert-eql 1 (s:node-property (node-of screen :leek) :level))
      ;; A press on a row's words selects it.
      (destructuring-bind (x y w h) (bounds-of screen :apple)
        (press-at (+ x (/ w 2)) (+ y (/ h 2))))
      (assert-equal "apple" (picked))
      (assert-true (s:node-property (node-of screen :apple) :selected))
      ;; A press on the mark before an item opens or closes it, and selects it.
      (destructuring-bind (x y w h) (bounds-of screen :fruit)
        (declare (ignore w))
        (press-at (+ x 12) (+ y (/ h 2))))
      (assert-equal "fruit" (picked))
      (assert-false (shown-p :apple))
      (assert-false (s:node-property (node-of screen :fruit) :expanded))
      ;; The press gave the tree the keyboard: Down, then Right into what is open, Left out of it,
      ;; Left again closes it, and Right opens it.
      (key screen core:+key-down+)
      (assert-equal "veg" (picked))
      (key screen core:+key-right+)
      (assert-equal "leek" (picked))
      (key screen core:+key-left+)
      (assert-equal "veg" (picked))
      (key screen core:+key-left+)
      (assert-false (shown-p :leek))
      (key screen core:+key-right+)
      (assert-true (shown-p :leek))
      (assert-equal "veg" (picked)))))

;;; --- the weight of text ---

(defun make-weighted ()
  (state-let ((bold nil))
    (lambda ()
      (column (:gap 8)
        (when (shortcut #\b :command) (setf bold t))
        (label "Time" :id :time :weight :bold)
        (label "Time" :id :plain-time)
        (column (:id :narrow :max-width 90)
          ;; 22 characters: two lines of 11 at 8 points, three of 10 at 9 points in bold.
          (paragraph "Havdalah at nightfall." :id :note :weight (if bold :bold :regular)))))))

(define-test bold-text-is-drawn-in-the-bold-font-and-laid-out-again ()
  (let ((screen (show 'make-weighted)))
    ;; The fake context draws bold characters a point wider.
    (assert-eql 36f0 (third (bounds-of screen :time)))
    (assert-eql 32f0 (third (bounds-of screen :plain-time)))
    (let ((regular (fourth (bounds-of screen :note))))
      (core:handle-event (screen-app screen)
                         (core:make-key-event :timestamp 0 :kind :down :logical-key #\b
                                              :modifiers core:+modifier-super+))
      (draw screen)
      ;; Broken again in the bold font: a line more.
      (assert-eql (* 1.5 regular) (fourth (bounds-of screen :note))))))

;;; --- an edge round a panel ---

(defun make-outlined ()
  (state-let ((marked nil))
    (lambda ()
      (column (:gap 8)
        (when (shortcut #\m :command) (setf marked t))
        (panel (:id :today :all 6 :border-color (if marked :text :accent) :border-width 2)
          (label "Today" :id :today-label))))))

(defun strokes-at (screen x y)
  "The (width colour) of each stroke on SCREEN's display list that starts at X, Y."
  (let ((list (screen-list screen)))
    (loop :for index :below (core:display-list-count list)
          :for width = (core:stroke-width-of (core:display-list-kind list index))
          :when (and width (= x (core:display-list-x list index))
                     (= y (core:display-list-y list index)))
            :collect (list width (core:display-list-color list index)))))

(define-test a-panel-can-have-an-edge-of-its-own ()
  (let ((screen (show 'make-outlined))
        (theme (theme:find-theme :dark)))
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :today)
      (assert-equal (list (list 2f0 (theme:theme-accent theme))) (strokes-at screen x y))
      ;; Its colour changed, and nothing else: drawn again in the new one.
      (core:handle-event (screen-app screen)
                         (core:make-key-event :timestamp 0 :kind :down :logical-key #\m
                                              :modifiers core:+modifier-super+))
      (draw screen)
      (assert-equal (list (list 2f0 (theme:theme-text theme))) (strokes-at screen x y)))))

(defun make-days-in-a-scroll ()
  (lambda ()
    (padding (:all 20)
      (scroll (:id :days :height 100)
        (dotimes (i 5)
          (pressable (:id i :all 6)
            (label (format nil "Day ~d" i) :id :day)))))))

(define-test the-ring-of-a-pressable-with-the-keyboard-shows-past-the-edge-of-a-scroll ()
  (let* ((screen (show 'make-days-in-a-scroll))
         (session (screen-session screen))
         (ring (theme:theme-ring (theme:find-theme :dark))))
    (m:press-key session :tab)
    (assert-true (s:node-property (m:find-node session :id 0) :focused))
    ;; The first day is at the top of the scroll's window; its ring reaches above it.
    (multiple-value-bind (x y) (core:lookup-target (screen-app screen) :days)
      (assert-eql ring (color-at screen (+ x 20) (- y 1))))))

;;; --- a view inside a view ---

(define-view t-card
  (:row :card (:gap 4)
   (:label :who (:text "Card"))))

(define-view t-cards
  (:column :cards (:gap 4)
   (:view :first (:name t-card))
   (:view :second (:name t-card))
   (:view :loop (:name t-cards))
   (:view :missing (:name t-nowhere))))

(defun make-cards ()
  (lambda () (view t-cards)))

(define-test a-view-is-drawn-inside-another-and-one-inside-itself-says-so ()
  (let* ((screen (show 'make-cards))
         (session (screen-session screen)))
    ;; Twice, each with its own id outermost.
    (assert-equal "Card" (s:node-name (m:find-node session :id :who :within :first)))
    (assert-equal "Card" (s:node-name (m:find-node session :id :who :within :second)))
    ;; Inside itself, and not defined: said, in its place.
    (assert-true (search "inside itself" (text-in screen :loop)))
    (assert-true (search "No view is called" (text-in screen :missing)))))

;;; --- one thing at the top ---

(defun make-two-at-the-top ()
  (lambda ()
    (label "First" :id :first-words)
    (label "Second" :id :second-words)))

(define-test two-things-at-the-top-are-an-error-even-with-ids-of-their-own ()
  ;; The second took the first one's place, and the first was not seen, with nothing said.
  (let ((condition (handler-case (progn (show 'make-two-at-the-top) nil)
                     (duplicate-id (condition) condition))))
    (assert-true condition)
    (assert-true (search "Put them in a container" (princ-to-string condition)))))

;;; --- docked panes ---

(defun make-docked ()
  (state-let ((layout '(:split :horizontal 150 (:tabs :alpha :alpha :beta) (:tabs :gamma :gamma))))
    (lambda ()
      (column (:id :page :fill t :cross-align :stretch)
        (setf layout
              (dock (:id :panes :layout layout :flex 1)
                (:alpha "Alpha"
                 (tree +garden+ :id :garden :children #'rest :label #'first :key #'garden-key))
                (:beta "Beta" (label "In beta" :id :beta-words))
                (:gamma "Gamma" (label "In gamma" :id :gamma-words))))
        (label (format nil "~(~s~)" layout) :id :layout)))))

(define-test a-pane-dragged-by-its-tab-goes-in-a-group-or-splits-one-and-keeps-what-it-holds ()
  (let ((screen (show 'make-docked)))
    (labels ((shown-p (id) (and (core:lookup-target (screen-app screen) id) t))
             (middle (id)
               (destructuring-bind (x y w h) (bounds-of screen id)
                 (values (+ x (/ w 2)) (+ y (/ h 2)))))
             (press-at (x y)
               (pointer screen :move x y)
               (pointer screen :down x y :button 0 :buttons 1)
               (pointer screen :up x y :button 0)
               (draw screen))
             (drag (from x1 y1)
               (multiple-value-bind (x0 y0) (middle from)
                 (pointer screen :move x0 y0)
                 (pointer screen :down x0 y0 :button 0 :buttons 1)
                 (pointer screen :move (+ x0 10) (+ y0 10) :buttons 1)
                 (pointer screen :move x1 y1 :buttons 1)
                 (pointer screen :move x1 y1 :buttons 1)
                 (pointer screen :up x1 y1 :button 0)
                 (dotimes (i 3) (draw screen))))
             (layout () (s:node-name (node-of screen :layout))))
      ;; A tab shows its pane.
      (assert-true (shown-p :garden))
      (multiple-value-call #'press-at (middle :beta))
      (assert-true (shown-p :beta-words))
      (assert-false (shown-p :garden))
      (multiple-value-call #'press-at (middle :alpha))
      ;; Something closed in the tree, which must stay closed when its pane moves.
      (destructuring-bind (x y w h) (bounds-of screen :fruit)
        (declare (ignore w))
        (press-at (+ x 12) (+ y (/ h 2))))
      (assert-false (shown-p :apple))
      ;; Alpha let go of on Gamma's tabs: it is in Gamma's group, and shown.
      (multiple-value-bind (x y) (middle :gamma)
        (drag :alpha (+ x 30) y))
      (assert-true (search "(:tabs :alpha :gamma :alpha)" (layout)) (layout))
      (assert-true (shown-p :fruit))
      (assert-false (shown-p :apple))
      ;; Beta, alone in its group, let go of in the middle of the other: its group and the divider
      ;; go.
      (multiple-value-bind (x y) (middle :fruit)
        (drag :beta x (+ y 100)))
      (assert-equal "(:tabs :beta :gamma :alpha :beta)" (layout))
      ;; Gamma let go of near the bottom edge: the group is split, one over the other.
      (drag :gamma 180 300)
      (assert-equal "(:split :vertical 153.0 (:tabs :beta :alpha :beta) (:tabs :gamma :gamma))"
                    (layout)))))

;;; --- shortcuts in panes ---

(defun make-two-editors ()
  (state-let ((left 0) (right 0))
    (lambda ()
      (column (:id :page :fill t :cross-align :stretch)
        (dock (:id :editors :layout '(:split :horizontal 170 (:tabs :left :left) (:tabs :right :right))
               :flex 1)
          (:left "Left"
           (when (shortcut #\z :command) (incf left))
           (button "In the left" :id :left-button))
          (:right "Right"
           (when (shortcut #\z :command) (incf right))
           (text-field "" :id :right-field)))
        (label (format nil "~d ~d" left right) :id :undone)))))

(define-test a-shortcut-in-a-pane-is-heard-only-in-the-pane-last-pressed-in-or-with-the-keyboard ()
  (let ((screen (show 'make-two-editors)))
    (flet ((press-in (id)
             (destructuring-bind (x y w h) (bounds-of screen id)
               (let ((x (+ x (/ w 2))) (y (+ y (/ h 2))))
                 (pointer screen :move x y)
                 (pointer screen :down x y :button 0 :buttons 1)
                 (pointer screen :up x y :button 0)
                 (draw screen)))))
      ;; The first pane, until another is pressed in.
      (key screen #\z :command)
      (assert-equal "1 0" (text-in screen :undone))
      (press-in :right-field)
      (key screen #\z :command)
      (assert-equal "1 1" (text-in screen :undone))
      (press-in :left-button)
      (key screen #\z :command)
      (assert-equal "2 1" (text-in screen :undone))
      ;; The keyboard is where Tab takes it: past the field (which had it) round to the left
      ;; button, then on (by the divider, which is in neither pane) to the field on the right,
      ;; each pane with it.
      (key screen #\Tab)
      (key screen #\z :command)
      (assert-equal "3 1" (text-in screen :undone))
      (loop :repeat 6
            :until (s:node-property (node-of screen :right-field) :focused)
            :do (key screen #\Tab))
      (assert-true (s:node-property (node-of screen :right-field) :focused))
      (key screen #\z :command)
      (assert-equal "3 2" (text-in screen :undone)))))

;;; --- an app in a pane ---

(defvar *inner* nil "The app in the pane of the last frame of an app with one.")

(defun make-inner ()
  (state-let ((count 0) (words ""))
    (lambda ()
      (column (:id :inner-page :gap 8)
        (when (button "Add one" :id :plus) (incf count))
        (label (format nil "Count ~d" count) :id :count)
        (multiple-value-bind (text edited) (text-field words :id :words :width 120)
          (when edited (setf words text)))))))

(defun make-outer ()
  (state-let ((zoom 1))
    (lambda ()
      (column (:id :page :fill t :cross-align :stretch)
        (when (button "Zoom" :id :zoom) (setf zoom (if (= zoom 1) 2 1)))
        (let ((inner (app-pane 'make-inner :id :running :zoom zoom :flex 1)))
          (setf *inner* inner))))))

(define-test an-app-in-a-pane-works-through-the-pane-at-its-zoom ()
  (let ((screen (show 'make-outer)))
    (labels ((inner-bounds (id) (multiple-value-list (core:lookup-target *inner* id)))
             (inner-name (id)
               (s:node-name (first (s:find-nodes (core:accessibility-tree *inner*) :id id))))
             (inner-value (id)
               (s:node-value (first (s:find-nodes (core:accessibility-tree *inner*) :id id))))
             (pane () (bounds-of screen :running))
             (press-inner (id zoom)
               ;; The middle of the inner app's ID, through the pane at ZOOM.
               (destructuring-bind (px py pw ph) (pane)
                 (declare (ignore pw ph))
                 (destructuring-bind (x y w h) (inner-bounds id)
                   (let ((x (+ px (* zoom (+ x (/ w 2))))) (y (+ py (* zoom (+ y (/ h 2))))))
                     (pointer screen :move x y)
                     (pointer screen :down x y :button 0 :buttons 1)
                     (pointer screen :up x y :button 0)
                     (draw screen))))))
      ;; It is laid out in the pane, as wide as it is.
      (assert-true *inner*)
      (assert-equal "Count 0" (inner-name :count))
      ;; A press on its button, through the pane, is its own.
      (press-inner :plus 1)
      (assert-equal "Count 1" (inner-name :count))
      ;; The press gave the pane the keyboard; a press on its field gives the field it, and typing
      ;; goes there.
      (press-inner :words 1)
      (dolist (character (coerce "hi" 'list))
        (core:handle-event (screen-app screen)
                           (core:make-key-event :timestamp 0 :kind :down :logical-key character
                                                :modifiers 0))
        (draw screen))
      (assert-equal "hi" (inner-value :words))
      ;; Twice as large: the pane's app is laid out in half the room, drawn twice the size, and
      ;; pressed through the zoom.
      (click screen :zoom)
      (flet ((drawn-p (x y w h)
               ;; A rectangle of the window's list, nearly at X, Y, W by H.
               (let ((list (screen-list screen)))
                 (loop :for i :below (core:display-list-count list)
                       :thereis (and (< (abs (- (core:display-list-x list i) x)) 1)
                                     (< (abs (- (core:display-list-y list i) y)) 1)
                                     (< (abs (- (core:display-list-width list i) w)) 1)
                                     (< (abs (- (core:display-list-height list i) h)) 1))))))
        (destructuring-bind (px py pw ph) (pane)
          (declare (ignore pw ph))
          (destructuring-bind (x y w h) (inner-bounds :plus)
            (assert-true (drawn-p (+ px (* 2 x)) (+ py (* 2 y)) (* 2 w) (* 2 h))
                         (list px py x y w h)))))
      (press-inner :plus 2)
      (assert-equal "Count 2" (inner-name :count))
      ;; Its text is drawn with fonts twice the size, from where the zoom puts each pen: the fake
      ;; context's characters are 8 points at every size, so the second of the count's words is 8
      ;; points after the first at the zoom (with real fonts, twice as wide, 16).
      (let* ((list (screen-list screen))
             (top (second (pane)))
             ;; The C of the count, in the pane (the window's own words are above it), and the
             ;; glyph after it.
             (c (loop :for i :below (core:display-list-count list)
                      :when (and (= (core:display-list-kind list i) core:+command-glyph+)
                                 (>= (core:display-list-y list i) top)
                                 (= (round (core:display-list-parameter list i)) (char-code #\C)))
                        :return i)))
        (assert-true c)
        (assert-eql 8 (round (- (core:display-list-x list (1+ c)) (core:display-list-x list c))))))))

;;; --- the inspector beside an app in a pane ---

(defun make-inspected ()
  (lambda ()
    (row (:id :page :fill t :cross-align :stretch)
      (let ((inner (app-pane 'make-inner :id :running :width 150)))
        (setf *inner* inner)
        (inspector-pane inner :id :inspecting)))))

(define-test the-inspector-in-a-pane-picks-and-presses-in-the-app-in-a-pane ()
  (let ((screen (show 'make-inspected)))
    (flet ((inner-name (id)
             (s:node-name (first (s:find-nodes (core:accessibility-tree *inner*) :id id)))))
      (click screen :inspector-pick)
      ;; The press on the app's button, through its pane, picks it, and does not press it.
      (destructuring-bind (px py pw ph) (bounds-of screen :running)
        (declare (ignore pw ph))
        (multiple-value-bind (x y w h) (core:lookup-target *inner* :plus)
          (let ((x (+ px x (/ w 2))) (y (+ py y (/ h 2))))
            (pointer screen :move x y)
            (pointer screen :down x y :button 0 :buttons 1)
            (pointer screen :up x y :button 0)
            (dotimes (i 3) (draw screen)))))
      (assert-equal "Count 0" (inner-name :count))
      (assert-equal "Kind: button" (s:node-name (node-of screen (list :inspector-fact 0))))
      ;; Press, in the inspector: done to the app in the pane, at the next frame.
      (click screen :inspector-press)
      (dotimes (i 3) (draw screen))
      (assert-equal "Count 1" (inner-name :count)))))

;;; --- a tree in a view ---

(define-view t-plants
  (:column :plants-page (:gap 4)
   (:tree :plants (:items ("Fruit" "  Apple" "    Wild apple" "Vegetables")))))

(defun make-plants ()
  (state-let ((picked 0))
    (lambda ()
      (column (:id :page :gap 8)
        (view t-plants (tree :plants :value picked))
        (label (format nil "~d" picked) :id :picked)))))

(define-test a-tree-in-a-view-is-its-lines-each-indented-a-level-deeper-and-wired-by-value ()
  (let ((screen (show 'make-plants)))
    ;; Its rows are known by their lines' places.
    (assert-equal '(0 1 2 0)
                  (mapcar (lambda (row) (s:node-property (node-of screen row) :level)) '(0 1 2 3)))
    (assert-equal "Wild apple" (s:node-name (node-of screen 2)))
    (destructuring-bind (x y w h) (bounds-of screen 3)
      (pointer screen :move (+ x (/ w 2)) (+ y (/ h 2)))
      (pointer screen :down (+ x (/ w 2)) (+ y (/ h 2)) :button 0 :buttons 1)
      (pointer screen :up (+ x (/ w 2)) (+ y (/ h 2)) :button 0))
    (draw screen)
    (assert-equal "3" (text-in screen :picked))))

;;; --- dragging ---

(defun make-drag-and-drop ()
  (state-let ((items (list "a" "b" "c")) (clicks 0))
    (lambda ()
      (column (:id :page :gap 20)
        (when (drag-source (:id :new :payload "new" :label "New")
                (label "Drag me" :id :words))
          (incf clicks))
        (multiple-value-bind (payload at)
            (drop-target (:id :list :gap 4)
              (dolist (item items)
                (label item :id (intern (string-upcase item) :keyword))))
          (when payload
            (setf items (append (subseq items 0 at) (list payload) (nthcdr at items)))))
        (label (format nil "~{~a~^ ~} ~d" items clicks) :id :state)))))

(define-test what-is-dragged-is-dropped-where-it-is-let-go-and-a-press-in-place-is-a-click ()
  (let ((screen (show 'make-drag-and-drop)))
    (click screen :new)
    (assert-equal "a b c 1" (text-in screen :state))
    ;; Pressed on the source, moved and let go between a and b.
    (multiple-value-bind (sx sy sw sh) (core:lookup-target (screen-app screen) :new)
      (multiple-value-bind (ax ay aw ah) (core:lookup-target (screen-app screen) :a)
        (declare (ignore aw))
        (let* ((x0 (+ sx (/ sw 2))) (y0 (+ sy (/ sh 2)))
               (x1 (+ ax 4)) (y1 (+ ay ah 2)))
          (pointer screen :move x0 y0)
          (pointer screen :down x0 y0 :button 0 :buttons 1)
          (pointer screen :move (+ x0 10) (+ y0 10))
          (pointer screen :move x1 y1)
          (pointer screen :up x1 y1 :button 0))))
    (draw screen)
    (assert-equal "a new b c 1" (text-in screen :state))))

;;; --- two panes and a divider ---

(defun make-split ()
  (state-let ((presses 0))
    (lambda ()
      (column (:id :page :fill t :cross-align :stretch)
        (let ((size (split (:id :sides :size 120 :min 60 :flex 1)
                      (column (:id :left)
                        (label "Left" :id :left-words)
                        (row (:id :buttons :gap 8)
                          (button "One" :id :one)
                          (button "Two" :id :two)
                          (when (button "Three" :id :three)
                            (incf presses))))
                      (label "Right" :id :right-words))))
          (label (format nil "~d ~d" (round size) presses) :id :said))))))

(define-test a-divider-dragged-or-moved-by-the-keys-sizes-the-panes-and-a-pane-cuts-what-overflows ()
  (let ((screen (show 'make-split)))
    (flet ((left-of (id) (nth-value 0 (core:lookup-target (screen-app screen) id)))
           (width-of (id) (nth-value 2 (core:lookup-target (screen-app screen) id)))
           (drag-divider (to)
             (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :divider)
               (let ((x0 (+ x (/ w 2))) (y0 (+ y (/ h 2))))
                 (pointer screen :move x0 y0)
                 (pointer screen :down x0 y0 :button 0 :buttons 1)
                 (pointer screen :move to y0 :buttons 1)
                 (pointer screen :up to y0 :button 0)
                 (draw screen)))))
      (assert-equal "120 0" (text-in screen :said))
      (assert-equal 120 (round (width-of :first)))
      (assert-equal 126 (round (left-of :right-words)))
      ;; Dragged by its middle: the first pane is as wide as where the middle was let go, less half
      ;; the divider.
      (drag-divider 203)
      (assert-equal "200 0" (text-in screen :said))
      (assert-equal 200 (round (width-of :first)))
      (assert-equal 206 (round (left-of :right-words)))
      ;; It leaves the second pane its least, and the first its own.
      (drag-divider 500)
      (assert-equal "294 0" (text-in screen :said))
      (drag-divider 0)
      (assert-equal "60 0" (text-in screen :said))
      ;; The press gave it the keyboard: an arrow moves it.
      (key screen core:+key-right+)
      (assert-equal "76 0" (text-in screen :said))
      ;; What does not fit in the first pane is cut at its edge: not shown, so not a target, and a
      ;; press where it would be is not on it.
      (assert-false (core:lookup-target (screen-app screen) :three))
      (drag-divider 300)
      (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :three)
        (let ((x (+ x (/ w 2))) (y (+ y (/ h 2))))
          (flet ((press ()
                   (pointer screen :move x y)
                   (pointer screen :down x y :button 0 :buttons 1)
                   (pointer screen :up x y :button 0)
                   (draw screen)))
            (press)
            (assert-equal "294 1" (text-in screen :said))
            (drag-divider 0)
            (assert-true (> x 66))
            (press)
            (assert-equal "60 1" (text-in screen :said))))))))

;;; --- the app's own menus ---

(defun make-with-menus ()
  (state-let ((said "nothing"))
    (lambda ()
      (let ((file (app-menu "File" '(("New" :key #\n) ("Open…" :key #\o) :separator
                                     ("Save" :key #\s) ("Save As…" :key #\s :shift t))))
            (view (app-menu "View" '("Bigger" "Smaller"))))
        (when file (setf said (format nil "File ~a" file)))
        (when view (setf said (format nil "View ~a" view))))
      (label said :id :said))))

(define-test the-apps-own-menus-are-described-and-an-item-chosen-is-returned-by-its-menu ()
  (let ((screen (show 'make-with-menus)))
    ;; What the menu bar is given: each menu's title, then its items.
    (let ((menus (core:app-menus (screen-app screen))))
      (assert-equal '("File" "View") (list (aref menus 0) (aref menus 2)))
      (assert-equal "Save" (first (nth 3 (aref menus 1)))))
    (flet ((choose (menu item)
             (core:handle-event (screen-app screen)
                                (core:make-menu-event :timestamp 0 :menu menu :item item))
             (draw screen)))
      (choose "File" "Save As…")
      (assert-equal "File Save As…" (text-in screen :said))
      (choose "View" "Smaller")
      (assert-equal "View Smaller" (text-in screen :said))
      ;; A menu the app does not have is nothing to it.
      (choose "Help" "About")
      (assert-equal "View Smaller" (text-in screen :said)))))

;;; --- the tree while frames fail ---

(define-test while-frames-fail-the-tree-is-that-of-the-frame-shown ()
  ;; The machine interface and VoiceOver read the tree; while frames fail, the window shows the last
  ;; good frame, and the tree must be of it, so that what they press is where it is shown.
  (let ((core:*show-frame-errors* t)
        (*error-output* (make-broadcast-stream)))
    (let* ((screen (show 'make-breakable))
           (session (screen-session screen))
           (before (s:node-bounds (m:find-node session :id :mend))))
      (click screen :break)
      (assert-equal before (s:node-bounds (m:find-node session :id :mend)))
      ;; And a press on it through the machine interface mends the app.
      (m:click session :id :mend)
      (assert-false (search "failed" (all-text screen))))
    ;; Asked for first while frames fail, there is no good tree to give: the window says what failed.
    (let* ((screen (show 'make-breakable))
           (session (screen-session screen)))
      (click screen :break)
      (assert-true (search "The app failed in this frame"
                           (s:node-name (m:find-node session :role :static-text)))))))

;;; --- a design surface takes no input ---

(defvar *surface-log* '()
  "What the widgets inside MAKE-INERT-SURFACE's surface did, which should be nothing.")

(defun make-inert-surface ()
  (state-let ((outside ""))
    (lambda ()
      (column (:id :page :gap 8)
        (setf outside (text-field outside :id :outside))
        (design-surface (:id :canvas)
          (column (:id :design :gap 8)
            (when (button "Inner" :id :inner)
              (push :inner *surface-log*))
            ;; Each of these takes input whatever is under the pointer or has the keyboard.
            (when (shortcut #\k :command)
              (push :shortcut *surface-log*))
            (when (dialog (:id :dialog :open t :title "A dialog")
                    (when (button "In the dialog" :id :in-dialog)
                      (push :in-dialog *surface-log*)))
              (push :escape *surface-log*))
            (when (canvas 60 40 (lambda (list context w h) (declare (ignore list context w h)))
                          :id :area :focusable t :autofocus t)
              (push :canvas *surface-log*))))))))

(define-test nothing-in-a-design-surface-takes-input-not-even-what-floats-or-asks-for-the-keyboard ()
  (setf *surface-log* '())
  (let* ((screen (show 'make-inert-surface))
         (session (screen-session screen)))
    ;; The canvas that asks for the keyboard is not given it.
    (assert-false (s:node-property (m:find-node session :id :area) :focused))
    ;; A shortcut and the dialog's Escape: taken by nothing in it.
    (m:press-key session :k :modifiers '(:super))
    (m:press-key session :escape)
    ;; A press where the dialog's button is drawn: the dialog does not take the pointer.
    (click screen :in-dialog)
    (assert-equal '() *surface-log*)
    ;; And the dialog does not keep the keyboard: Tab goes to the field outside the surface.
    (m:press-key session :tab)
    (m:type-text session "q")
    (assert-equal "q" (s:node-value (m:find-node session :id :outside)))
    (assert-equal '() *surface-log*)))

;;; --- views: a design, and the code wired to it ---

(define-view t-login
  (:column :form (:gap 8)
   (:label :heading (:text "Sign in"))
   (:text-field :name nil)
   (:row :actions (:gap 8)
    (:button :go (:text "Go"))
    (:button :later (:text "Later")))
   (:column :recent nil)))

(defun make-wired-login ()
  (state-let ((name "") (presses 0))
    (lambda ()
      (column (:id :page :gap 8)
        (view t-login
          (label :heading :text (format nil "Hello ~a" name))
          (text-field :name :value name)
          (button :go :enabled (plusp (length name)) :on-press (incf presses))
          (column :recent :contents (label (format nil "~d" presses) :id :count)))
        (when (button "Clear" :id :clear)
          (setf name ""))))))

(define-test a-view-is-drawn-as-designed-and-does-what-the-code-wired-to-it ()
  (let* ((screen (show 'make-wired-login))
         (session (screen-session screen)))
    ;; An option worked out by the code, each frame.
    (assert-equal "Hello " (text-in screen :heading))
    ;; What is not wired is as the view has it.
    (assert-equal "Later" (text-in screen :later))
    (assert-true (s:node-property (m:find-node session :id :go) :disabled))
    ;; :value: typing sets the variable (the heading shows it), and the variable is what it shows.
    (m:type-text session "Ann" :id :name)
    (assert-equal "Hello Ann" (text-in screen :heading))
    (assert-false (s:node-property (m:find-node session :id :go) :disabled))
    (m:click session :id :clear)
    (assert-equal "" (s:node-value (m:find-node session :id :name)))
    ;; :on-press runs on a press, and :contents is drawn in its container.
    (m:type-text session "Bo" :id :name)
    (m:click session :id :go)
    (m:click session :id :go)
    (assert-equal "2" (text-in screen :count))
    (assert-true (m:find-node session :id :count :within :recent))))

(define-test a-view-defined-again-is-drawn-without-the-code-being-compiled-again ()
  (let ((before (almighty-bolt/ui/views:find-view 't-login))
        (screen (show 'make-wired-login)))
    (flet ((define (form)
             (handler-bind ((warning #'muffle-warning))
               (almighty-bolt/ui/views:set-view 't-login form))))
      (unwind-protect
           (progn
             (assert-equal '() (define (subst "Not now" "Later" before :test #'equal)))
             (draw screen)
             (assert-equal "Not now" (text-in screen :later))
             ;; A design without a widget the code wires: said when it is defined, and an error
             ;; when it is drawn (which a window shows over its last good frame).
             (let ((problems (define (list :column :form '(:gap 8)
                                           '(:label :heading ()) '(:text-field :name ())
                                           '(:column :recent ())))))
               (assert-true (some (lambda (p) (search ":GO is wired" p)) problems) problems))
             (assert-error 'error (draw screen)))
        (define before)))))

;; An app's own widget, in the catalogue as the library's are.
(define-widget-entry :t-stars (:title "Stars"
                               :options ((:count :number 3) (:of :number 5))
                               :value :count)
  (row (:id id :gap 4)
    (dotimes (i (round of))
      (when (button (if (< i count) "*" ".") :id i)
        (when set-value (funcall set-value (1+ i)))))))

(define-view t-settings
  (:column :settings (:gap 8)
   (:slider :volume (:value 50))
   (:checkbox :mute (:label "Mute"))
   (:list-box :device (:items ("Speakers" "Headphones" "HDMI")))
   (:t-stars :rating nil)
   (:label :summary (:text "-"))))

(defun make-wired-settings ()
  (state-let ((volume 20) (mute nil) (device 0) (rating 2))
    (lambda ()
      (view t-settings
        (slider :volume :value volume)
        (checkbox :mute :value mute)
        (list-box :device :value device)
        (t-stars :rating :value rating)
        (label :summary :text (format nil "~d ~a ~d ~d" (round volume) (if mute "muted" "on")
                                      device rating))))))

(define-test every-widget-that-holds-a-value-can-be-wired-to-a-place-and-so-can-your-own ()
  (let* ((screen (show 'make-wired-settings))
         (session (screen-session screen)))
    (assert-equal "20 on 0 2" (text-in screen :summary))
    (m:set-value session 80 :id :volume)
    (m:set-value session t :id :mute)
    (m:set-value session 2 :id :device)
    (m:click session :id 3 :within :rating)
    (assert-equal "80 muted 2 4" (text-in screen :summary))
    ;; The app's own widget is in the catalogue, where a designer lists it.
    (assert-true (find :t-stars (almighty-bolt/ui/catalogue:catalogue)
                       :key #'almighty-bolt/ui/catalogue:entry-kind))))

(define-view t-person
  (:row :person (:gap 4)
   (:label :who nil)
   (:button :remove (:text "Remove"))))

(defun make-people ()
  (state-let ((removed "nobody"))
    (lambda ()
      (column (:id :people :gap 4)
        (dolist (person '("Ann" "Bo"))
          (view (t-person :id (intern (string-upcase person) :keyword))
            (label :who :text person)
            (button :remove :on-press (setf removed person))))
        (label (format nil "Removed ~a" removed) :id :removed)))))

(define-test one-view-can-be-drawn-more-than-once-in-a-container-each-wired-to-its-own ()
  (let* ((screen (show 'make-people))
         (session (screen-session screen)))
    (assert-equal "Ann" (s:node-name (m:find-node session :id :who :within :ann)))
    (assert-equal "Bo" (s:node-name (m:find-node session :id :who :within :bo)))
    (m:click session :id :remove :within :bo)
    (assert-equal "Removed Bo" (text-in screen :removed))
    (m:click session :id :remove :within :ann)
    (assert-equal "Removed Ann" (text-in screen :removed))))

(defun compiling (form)
  "What is said when FORM is compiled, as a user's code is: the errors and warnings, as strings."
  (let ((said '()))
    ;; SBCL reports an error in a macro's expansion as a condition of its own, which is not an
    ;; ERROR, so every condition is heard but the compiler's notes on speed.
    (handler-bind ((condition (lambda (condition)
                                (unless (typep condition 'sb-ext:compiler-note)
                                  (push (princ-to-string condition) said)
                                  (when (typep condition 'warning)
                                    (muffle-warning condition))))))
      (compile nil form))
    (nreverse said)))

(define-test wiring-that-does-not-fit-is-an-error-when-it-is-compiled ()
  (loop :for (wiring why) :in '(((button :nope) ":NOPE is wired, and the view has no widget")
                                ((text-field :go) ":GO is wired as a text-field, and in the view it is a button")
                                ((button :go :colour 3) "a button takes")
                                ((label :heading :on-press (print 1)) "a label takes")
                                ((slab :go) "no widget of the kind slab"))
        :do (let ((said (compiling `(lambda () (view t-login ,wiring)))))
              (assert-true (some (lambda (s) (search why s :test #'char-equal)) said) why said)))
  ;; Wiring that fits says nothing.
  (assert-equal '() (compiling '(lambda () (view t-login (button :go :on-press (print 1)))))))

(define-test a-view-file-compiles-while-code-loaded-before-does-not-fit-it ()
  ;; A view saved in the designer, its file compiled again (by ASDF, say) while code that wired the
  ;; old design is loaded: a style warning, not a failure, which would stop the system loading.
  (let ((source (merge-pathnames (format nil "view-~d.lisp" (random 1000000000))
                                 (uiop:temporary-directory)))
        (fasl (merge-pathnames (format nil "view-~d.fasl" (random 1000000000))
                               (uiop:temporary-directory))))
    (unwind-protect
         ;; The code that wires it is held (in a special variable, which the collector sees), so
         ;; it can still run, and it does not fit.
         (let ((*held-wiring* (handler-bind ((warning #'muffle-warning))
                         (almighty-bolt/ui/views:set-view 't-recompiled
                                                          '(:column :c () (:button :gone ())))
                         (compile nil '(lambda () (view t-recompiled (button :gone :on-press (print 1))))))))
           (with-open-file (out source :direction :output)
             (write-string "(in-package #:almighty-bolt/t/app) (almighty-bolt:define-view t-recompiled (:column :c ()))" out))
           ;; Not muffled: a warning muffled is not counted by COMPILE-FILE.
           (multiple-value-bind (output warnings failure)
               (let ((*error-output* (make-broadcast-stream)))
                 (compile-file source :output-file fasl))
             (declare (ignore output))
             (assert-true warnings)
             (assert-false failure)))
      (remhash 't-recompiled almighty-bolt/ui/views::*views*)
      (dolist (file (list source fasl))
        (when (probe-file file) (delete-file file))))))

(defvar *held-wiring* nil
  "Code that wires a view, held while a test needs it to be: a special variable, which the garbage
collector sees, where a local one that is not used again may not be.")

(defun wiring-compiled-elsewhere (form)
  "FORM compiled on a thread of its own, and the function returned. Compiled on this thread, what the
compiler made can stay on its stack, where the garbage collector takes it to be held."
  (sb-thread:join-thread
   (sb-thread:make-thread (lambda ()
                            (handler-bind ((warning #'muffle-warning))
                              (compile nil form))))))

(define-test wiring-of-code-compiled-again-is-forgotten-and-of-code-still-held-is-not ()
  ;; Code that wired :gone, compiled again without it: once the old code is gone, a view without
  ;; :gone fits. While the old code is held (it could still run), it does not.
  (unwind-protect
       (flet ((define (form)
                (handler-bind ((warning #'muffle-warning))
                  (almighty-bolt/ui/views:set-view 't-again form))))
         (define '(:column :c () (:button :gone ()) (:button :kept ())))
         (wiring-compiled-elsewhere '(lambda () (view t-again (button :gone) (button :kept))))
         (let ((*held-wiring* (wiring-compiled-elsewhere '(lambda () (view t-again (button :kept))))))
           (assert-equal '() (define '(:column :c () (:button :kept ())))))
         ;; Code that fits when it is compiled, and is held while the view loses its widget.
         (define '(:column :c () (:button :kept ()) (:button :also ())))
         (let ((*held-wiring* (wiring-compiled-elsewhere '(lambda () (view t-again (button :also))))))
           (assert-true (some (lambda (p) (search ":ALSO is wired" p))
                              (define '(:column :c () (:button :kept ())))))))
    (remhash 't-again almighty-bolt/ui/views::*views*)))

(define-test wiring-to-a-view-not-yet-defined-is-a-style-warning ()
  (let ((said (compiling '(lambda () (view t-not-defined (button :go))))))
    (assert-true (some (lambda (s) (search "Load the file that defines it" s)) said) said)))

;;; --- a frame that fails ---

(defun make-breakable ()
  (state-let ((align :start))
    (lambda ()
      (column (:id :page :gap 8 :main-align align)
        (label "Still here" :id :still)
        ;; :stretch is a cross-align; as a main-align, layout fails in every frame.
        (when (button "Break" :id :break)
          (setf align :stretch))
        (when (button "Mend" :id :mend)
          (setf align :start))))))

(defun all-text (screen)
  "Every character drawn, in order."
  (let ((list (screen-list screen)))
    (coerce (loop :for index :below (core:display-list-count list)
                  :when (= (core:display-list-kind list index) core:+command-glyph+)
                    :collect (code-char (round (core:display-list-parameter list index))))
            'string)))

(define-test a-window-whose-frame-fails-shows-the-last-good-one-and-can-be-mended ()
  ;; As a window's host makes its frames: the app's state makes layout fail in every frame, which
  ;; showed the error again on each move of the pointer and nothing else. The last good frame is
  ;; shown, with the error along the bottom; it is said once; and what was there still works.
  (let ((core:*show-frame-errors* t)
        (said (make-string-output-stream)))
    (let ((screen (show 'make-breakable))
          (*error-output* said))
      (click screen :break)
      (let ((text (all-text screen)))
        (assert-true (search "Still here" text) text)
        (assert-true (search "Frame failed: " text) text)
        (assert-true (search "STRETCH" text) text))
      (draw screen)
      (draw screen)
      ;; A press taken while it fails builds again before laying out; that is not a frame mended.
      (click screen :break)
      (let ((log (get-output-stream-string said)))
        (assert-eql 1 (count #\Newline log) log)
        (assert-true (search "STRETCH" log) log))
      (click screen :mend)
      (assert-false (search "failed" (all-text screen)))
      (assert-equal "Still here" (text-in screen :still))))
  ;; In a test or at the REPL, the error is the caller's, as before.
  (let ((screen (show 'make-breakable)))
    (assert-error 'error (click screen :break))))

;;; --- the app's own window ---

(defvar *seen-window* :unset
  "What THIS-WINDOW was in the last frame of MAKE-SEES-ITS-WINDOW's app.")

(defun make-sees-its-window ()
  (lambda ()
    (setf *seen-window* (this-window))
    (label "Mine" :id :mine)))

(define-test an-app-names-its-own-window ()
  ;; The window a host draws in is the host, as OPEN-WINDOW returns it, so what the app sees in its
  ;; frame can be closed with CLOSE-WINDOW or put a window BESIDE it (the smoke check does both in
  ;; real windows). With no host, in a test's context, and outside a frame, there is none.
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let ((host (host:make-host (make-instance 'core:app-instance) (device:default-device)
                                  target:+pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
            (instance (core:with-kept-state ((core:make-kept-state))
                        (app:make-app-instance 'make-sees-its-window))))
        (setf *seen-window* :unset)
        (draw-on-host host instance)
        (assert-eq host *seen-window*)
        (let ((other (host:make-host (make-instance 'core:app-instance) (device:default-device)
                                     target:+pixel-format-bgra8-unorm+ :capacity 1024 :scale 2)))
          (draw-on-host other instance)
          (assert-eq other *seen-window*))
        (setf *seen-window* :unset)
        (show 'make-sees-its-window)
        (assert-eq nil *seen-window*)
        (assert-eq nil (this-window))))))

;;; --- the inspector ---
;;;
;;; The inspector is a window of its own. Here it is a second screen whose app looks at the first's
;;; app, as the window's does; a press in one is followed by a frame of the other, as a window
;;; that took input draws the others. What it asks to be done is noted, and done by the test with a
;;; session on the app's screen, as the window's thread does with a session on the app's window.

(defvar *asked* '() "What the inspector asked to be done and has not been, with its report function.")

(defstruct (inspecting (:constructor %make-inspecting (app panel))) app panel)

(defun inspect-app (app)
  "An inspector's screen, 700 points tall, looking at the screen APP."
  (let* ((state (core:make-kept-state))
         (instance (almighty-bolt/ui/immediate:make-immediate-app
                   (almighty-bolt/ui/inspector:make-inspector
                    (lambda () (screen-app app))
                    (lambda (action report)
                      (setf *asked* (append *asked* (list (cons action report))))))))
         (panel (%make-screen instance (fake:make-fake-context :width 360f0 :height 700f0)
                              (core:make-display-list :capacity 16384) state)))
    (draw panel)
    (draw app)
    (%make-inspecting app panel)))

(defun inspecting (constructor)
  "The app of CONSTRUCTOR, shown, and an inspector looking at it."
  (inspect-app (show constructor)))

(defun app-of (inspecting) (inspecting-app inspecting))
(defun panel-of (inspecting) (inspecting-panel inspecting))

(defun in-panel (inspecting function &rest arguments)
  "Do FUNCTION to the inspector's screen, and then draw the app's, as a window that took input draws
the others."
  (apply function (panel-of inspecting) arguments)
  (draw (app-of inspecting))
  (draw (panel-of inspecting)))

(defun in-app (inspecting function &rest arguments)
  (apply function (app-of inspecting) arguments)
  (draw (panel-of inspecting))
  (draw (app-of inspecting)))

(defun inspector-says (inspecting)
  (text-in (panel-of inspecting) :inspector))

(defun pick (inspecting id)
  "Pick the widget ID, and take the pointer from the app's window to the inspector's, as a hand does."
  (in-panel inspecting #'click :inspector-pick)
  (in-app inspecting #'click id)
  (in-app inspecting #'pointer :leave 0 0))

(defun do-asked (inspecting)
  "Do what the inspector asked for, and tell it what went wrong, as the window's thread does."
  (let ((asked *asked*))
    (setf *asked* '())
    (loop :for (action . report) :in asked
          :do (let ((message (m:perform-actions (screen-session (app-of inspecting)) (list action))))
                (when message (funcall report message))))
    (draw (app-of inspecting))
    (draw (panel-of inspecting))))

(defun draft-of (inspecting)
  (s:node-value (node-of (panel-of inspecting) :inspector-draft)))

(defun command-option-i (screen)
  (core:handle-event (screen-app screen)
                     (core:make-key-event :timestamp 0 :kind :down :logical-key #\i
                                          :modifiers (logior core:+modifier-super+
                                                             core:+modifier-alt+)))
  (draw screen))

(defvar *own-i* 0)

(defun make-own-i ()
  "An app whose own shortcut is Command-Option-I."
  (lambda ()
    (when (shortcut #\i :command :option) (incf *own-i*))
    (label "I" :id :i)))

(define-test command-option-i-opens-the-inspector-only-while-the-app-is-developed ()
  (let* ((opened '())
         (almighty-bolt/ui/inspection:*inspector-opener* (lambda (instance) (push instance opened)))
         (*own-i* 0))
    ;; An app that is delivered: the keys are its own. (OPEN-APP, run from a REPL before the tests,
    ;; leaves *DEVELOPMENT* true.)
    (let ((screen (let ((app:*development* nil)) (show 'make-own-i))))
      (command-option-i screen)
      (assert-eql 1 *own-i*)
      (assert-eq '() opened))
    ;; One that is developed: they open (or close) the inspector of its window, and the app does not
    ;; see them.
    (let* ((screen (let ((app:*development* t)) (show 'make-own-i))))
      (command-option-i screen)
      (assert-eql 1 *own-i*)
      (assert-equal (list (screen-app screen)) opened))))

(define-test the-inspector-picks-a-widget-instead-of-pressing-it-and-says-what-it-is ()
  (let ((*asked* '())
        (both (inspecting 'counter:make-counter)))
    (assert-true (search "Press Pick" (inspector-says both)))
    (in-panel both #'click :inspector-pick)
    (assert-true (search "Press a widget" (inspector-says both)))
    (in-app both #'click :count-button)
    ;; Picked, and not pressed.
    (assert-equal "0" (text-in (app-of both) :count))
    (let ((says (inspector-says both)))
      (dolist (line '("Kind: button" "Role: button" "Id: :count-button" "Name: Count"
                      "Theme: :dark" "Enabled"))
        (assert-true (search line says) line says)))
    (let ((app (app-of both)))
      (multiple-value-bind (x y width height) (core:lookup-target (screen-app app) :count-button)
        (assert-true (search (format nil "At: ~d, ~d" (round x) (round y)) (inspector-says both)))
        (assert-true (search (format nil "Size: ~d by ~d" (round width) (round height))
                             (inspector-says both)))
        ;; Outlined in the app's window, in the inspector's ring colour, just inside each edge.
        (let ((ring (theme-setting (find-theme :dark) :ring)))
          (assert-eql ring (color-at app (+ x (/ width 2)) (+ y 1)))
          (assert-eql ring (color-at app (+ x (/ width 2)) (+ y height -1)))
          (assert-eql ring (color-at app (+ x 1) (+ y (/ height 2))))
          (assert-eql ring (color-at app (+ x width -1) (+ y (/ height 2)))))))
    ;; An agent or a screen reader sees the app, and nothing of the outline.
    (let ((tree (core:accessibility-tree (screen-app (app-of both)))))
      (assert-true (s:find-nodes tree :id :count-button))
      (assert-false (s:find-nodes tree :id :inspector-picked)))
    ;; The pick is done: the next press is the app's.
    (in-app both #'click :count-button)
    (assert-equal "1" (text-in (app-of both) :count))))

(define-test a-pick-is-input-its-frame-took-so-the-inspector-is-drawn-after-it ()
  ;; A window draws the others after a frame that built again because it took input; the press
  ;; that picks is taken before the app's function, and must count as that too.
  (let* ((*asked* '())
         (both (inspecting 'counter:make-counter))
         (app (app-of both)))
    (in-panel both #'click :inspector-pick)
    (multiple-value-bind (x y w h) (core:lookup-target (screen-app app) :count-button)
      (pointer app :move (+ x (/ w 2)) (+ y (/ h 2)))
      (core:handle-event (screen-app app)
                         (core:make-pointer-event :timestamp 0 :kind :down :x (float (+ x (/ w 2)) 1d0)
                                                  :y (float (+ y (/ h 2)) 1d0) :button 0 :buttons 1
                                                  :click-count 1))
      (assert-true (> (core:settle-frame (screen-app app) (screen-context app) (screen-list app))
                      1)))
    (draw (panel-of both))
    (assert-true (search "Id: :count-button" (inspector-says both)))))

(defun make-two-labels ()
  "Two labels, which take no pointer: nothing is hot over them."
  (lambda ()
    (column (:gap 20)
      (label "Upper" :id :upper)
      (label "Lower" :id :lower))))

(define-test while-the-inspector-picks-every-move-of-the-pointer-asks-for-a-frame ()
  ;; A window draws when an event asks it to. A move from one label to another changes nothing the
  ;; app sees, and asks for no frame; while the inspector picks, the outline goes with the pointer,
  ;; and the move must ask for one, as must the pointer leaving for the inspector's window.
  (let* ((*asked* '())
         (both (inspecting 'make-two-labels))
         (app (app-of both))
         (instance (screen-app app)))
    (flet ((move-to (id)
             (multiple-value-bind (x y w h) (core:lookup-target instance id)
               (core:handle-event instance (core:make-pointer-event
                                           :timestamp 0 :kind :move :x (float (+ x (/ w 2)) 1d0)
                                           :y (float (+ y (/ h 2)) 1d0) :button nil :buttons 0
                                           :click-count 0))))
           (leave ()
             (core:handle-event instance (core:make-pointer-event :timestamp 0 :kind :leave :x 0d0
                                                                 :y 0d0 :button nil :buttons 0
                                                                 :click-count 0))))
      (move-to :upper)
      (draw app)
      (assert-false (move-to :lower))
      (draw app)
      (in-panel both #'click :inspector-pick)
      (move-to :upper)
      (draw app)
      (assert-true (move-to :lower))
      (draw app)
      (multiple-value-bind (x y) (core:lookup-target instance :lower)
        (assert-eql (theme-setting (find-theme :dark) :ring) (color-at app (+ x 1) (+ y 1))))
      (assert-true (leave))
      (draw app)
      ;; Gone with the pointer.
      (multiple-value-bind (x y) (core:lookup-target instance :lower)
        (assert-false (eql (theme-setting (find-theme :dark) :ring) (color-at app (+ x 1) (+ y 1))))))))

(define-test escape-stops-the-inspector-picking-in-either-window ()
  (let ((*asked* '())
        (both (inspecting 'counter:make-counter)))
    (in-panel both #'click :inspector-pick)
    (in-app both #'key #\Escape)
    (assert-true (search "Press Pick" (inspector-says both)))
    (in-app both #'click :count-button)
    (assert-equal "1" (text-in (app-of both) :count))
    (in-panel both #'click :inspector-pick)
    (in-panel both #'key #\Escape)
    (assert-true (search "Press Pick" (inspector-says both)))
    (in-app both #'click :count-button)
    (assert-equal "2" (text-in (app-of both) :count))))

(define-test the-inspector-picks-in-a-dialog ()
  (let ((*asked* '())
        (both (inspecting 'make-confirm)))
    (draw (app-of both))
    (pick both :delete)
    (assert-equal "none, 0 behind" (text-in (app-of both) :answer))
    (assert-true (search "Name: Delete" (inspector-says both)))))

(define-test what-the-inspector-picked-is-picked-still-when-the-app-is-made-again ()
  (let ((*asked* '())
        (both (inspecting 'counter:make-counter)))
    (pick both :count-button)
    (let* ((app (app-of both))
           (new (core:with-kept-state ((screen-state app))
                  (app:make-app-instance 'counter:make-counter))))
      (core:inherit-state new (screen-app app))
      (setf (screen-app app) new)
      (draw app)
      (draw (panel-of both))
      (assert-true (search "Id: :count-button" (inspector-says both)) (inspector-says both)))))

(defun make-twins ()
  "Two buttons the same size, one under the other."
  (lambda ()
    (column (:gap 20)
      (button "One" :id :one)
      (button "Two" :id :two))))

(define-test picked-again-the-outline-goes-to-the-new-one ()
  (let* ((*asked* '())
         (both (inspecting 'make-twins))
         (app (app-of both))
         (ring (theme-setting (find-theme :dark) :ring)))
    (in-panel both #'click :inspector-pick)
    ;; While it picks, the outline is on what the pointer is over, and goes with it.
    (flet ((over (id)
             (multiple-value-bind (x y w h) (core:lookup-target (screen-app app) id)
               (pointer app :move (+ x (/ w 2)) (+ y (/ h 2)))
               (values x y)))
           (outlined-p (x y) (eql ring (color-at app (+ x 1) (+ y 1)))))
      (multiple-value-bind (x y) (over :one)
        (assert-true (outlined-p x y)))
      (multiple-value-bind (x y) (over :two)
        (assert-true (outlined-p x y)))
      (multiple-value-bind (x y) (core:lookup-target (screen-app app) :one)
        (assert-false (outlined-p x y))))
    (in-app both #'click :one)
    (pick both :two)
    (assert-true (search "Id: :two" (inspector-says both)))
    (multiple-value-bind (x y) (core:lookup-target (screen-app app) :two)
      (assert-eql ring (color-at app (+ x 1) (+ y 1))))
    (multiple-value-bind (x y) (core:lookup-target (screen-app app) :one)
      (assert-false (eql ring (color-at app (+ x 1) (+ y 1)))))))

(defun make-heading-over-lines ()
  "A heading over a scroll container of many lines."
  (lambda ()
    (column (:gap 0)
      (label "Heading" :id :heading)
      (scroll (:id :lines :height 100)
        (column (:gap 0)
          (dotimes (i 40) (label (format nil "Line ~d" i) :id i)))))))

(define-test the-inspector-picks-what-shows-not-what-is-scrolled-out-of-sight-under-it ()
  (let* ((*asked* '())
         (both (inspecting 'make-heading-over-lines)))
    ;; Scrolled down, the first lines are above the container's window, where the heading is.
    (m:scroll (screen-session (app-of both)) :id :lines :dy -5)
    (assert-true (plusp (offset-of (app-of both) :lines)))
    (pick both :heading)
    (assert-true (search "Name: Heading" (inspector-says both)) (inspector-says both))))

(define-test the-inspector-presses-a-button-as-a-hand-does-after-the-frame ()
  (let ((*asked* '())
        (both (inspecting 'counter:make-counter)))
    (pick both :count-button)
    (in-panel both #'click :inspector-press)
    ;; Asked for, by its id, once (another frame does not ask again), and not done in the frame.
    (draw (panel-of both))
    (assert-equal '((:op :click :id :count-button)) (mapcar #'car *asked*))
    (assert-equal "0" (text-in (app-of both) :count))
    (do-asked both)
    (assert-equal "1" (text-in (app-of both) :count))))

(define-test the-inspector-types-what-it-is-given-into-a-field ()
  (let ((*asked* '())
        (both (inspecting 'todo:make-todo)))
    (pick both :draft)
    (in-panel both #'click :inspector-draft)
    (in-panel both #'type-text "Milk")
    ;; Typed into the inspector's field, not the app's.
    (assert-equal "" (s:node-value (node-of (app-of both) :draft)))
    (in-panel both #'click :inspector-set)
    (do-asked both)
    (assert-equal "Milk" (s:node-value (node-of (app-of both) :draft)))))

(define-test the-inspector-sets-a-slider-and-says-when-what-it-was-given-is-not-a-number ()
  (let ((*asked* '())
        (both (inspecting 'make-whole-slider)))
    (pick both :whole)
    ;; The field has the slider's value to start with.
    (assert-equal "5.0" (draft-of both))
    (in-panel both #'click :inspector-draft)
    (in-panel both #'key #\a :command)
    (in-panel both #'type-text "lots")
    (in-panel both #'click :inspector-set)
    (assert-true (search "\"lots\" is not a number." (inspector-says both)))
    (assert-eq '() *asked*)
    (in-panel both #'click :inspector-draft)
    (in-panel both #'key #\a :command)
    (in-panel both #'type-text "3")
    (in-panel both #'click :inspector-set)
    (do-asked both)
    (assert-equal "3" (text-in (app-of both) :whole-value))))

(define-test the-inspector-says-why-what-it-asked-for-could-not-be-done ()
  (let ((*asked* '())
        (both (inspecting 'make-locked)))
    (pick both :volume)
    (in-panel both #'click :inspector-draft)
    (in-panel both #'key #\a :command)
    (in-panel both #'type-text "50")
    (in-panel both #'click :inspector-set)
    (do-asked both)
    (assert-true (search "is disabled" (inspector-says both)) (inspector-says both))
    (assert-eql 10d0 (s:node-value (node-of (app-of both) :volume)))))

(define-test the-inspector-chooses-an-option-of-a-choice ()
  (let ((*asked* '())
        (both (inspecting 'make-settings)))
    (pick both :delivery)
    (in-panel both #'click '(:inspector-option 2))
    (do-asked both)
    (assert-equal "Collect" (s:node-value (node-of (app-of both) :delivery)))))

(define-test the-inspector-chooses-a-row-of-a-dropdown-by-its-words ()
  (let ((*asked* '())
        (both (inspecting 'make-picker)))
    (pick both :size)
    (in-panel both #'click '(:inspector-option 2))
    (assert-equal '((:op :set-value :value "Large" :id :size)) (mapcar #'car *asked*))
    (do-asked both)
    (assert-equal "Large" (s:node-value (node-of (app-of both) :size)))))

(defun make-inked ()
  "A button in the window's theme, one in the theme :test-ink, and one in a theme with no name."
  (lambda ()
    (column (:gap 10)
      (button "Plain" :id :plain-ink)
      (with-theme (:test-ink)
        (button "Inked" :id :inked))
      (with-theme ((make-theme :accent "#808020"))
        (button "Nameless" :id :nameless)))))

(defun type-into (inspecting id text)
  (in-panel inspecting #'click id)
  (in-panel inspecting #'key #\a :command)
  (in-panel inspecting #'type-text text))

(defun change-theme (inspecting setting value)
  (type-into inspecting :inspector-setting setting)
  (type-into inspecting :inspector-setting-value value)
  (in-panel inspecting #'click :inspector-change-theme))

(define-test the-inspector-changes-a-setting-of-the-theme-a-widget-is-drawn-in ()
  (define-theme :test-ink (:base :light) :accent "#20a090")
  (let* ((*asked* '())
         (both (inspecting 'make-inked))
         (plain (middle-color (show 'make-inked) :plain-ink)))
    (pick both :inked)
    (type-into both :inspector-setting "accent")
    (assert-true (search ":accent is #20a090 now." (inspector-says both)) (inspector-says both))
    (type-into both :inspector-setting-value "#c06030")
    (in-panel both #'click :inspector-change-theme)
    ;; The theme is defined again, and what is drawn in it is drawn so; the rest is not.
    (assert-eql (parse-color "#c06030") (middle-color (app-of both) :inked))
    (assert-eql plain (middle-color (app-of both) :plain-ink))
    ;; On its own base, with what it was given otherwise, written down as so.
    (let ((form (theme-form (find-theme :test-ink))))
      (assert-equal '(:base :light) (third form))
      (assert-equal "#c06030" (getf (cdddr form) :accent)))
    (assert-true (search "\"#c06030\"" (text-in (panel-of both) :inspector-theme-form)))))

(define-test the-inspector-says-what-is-wrong-with-a-change-to-a-theme ()
  (define-theme :test-ink (:base :light) :accent "#20a090")
  (let ((*asked* '())
        (both (inspecting 'make-inked)))
    (pick both :inked)
    (change-theme both "acent" "#c06030")
    (assert-true (search "no setting called \"acent\"" (inspector-says both)))
    (change-theme both "accent" "nope")
    (assert-true (search "is not a colour" (inspector-says both)))
    (change-theme both "row-height" "tall")
    (assert-true (search "takes a number" (inspector-says both)))
    (assert-eql (parse-color "#20a090") (theme-setting (find-theme :test-ink) :accent))))

(define-test a-theme-with-no-name-cannot-be-changed-in-the-inspector ()
  (define-theme :test-ink (:base :light) :accent "#20a090")
  (let ((*asked* '())
        (both (inspecting 'make-inked)))
    (pick both :nameless)
    (assert-true (search "a theme with no name" (inspector-says both)))
    (assert-false (core:lookup-target (screen-app (panel-of both)) :inspector-change-theme))))

(define-test the-inspector-is-drawn-in-the-theme-it-was-opened-in-when-that-theme-changes ()
  (let ((dark (find-theme :dark)))
    (unwind-protect
         (let ((*asked* '())
               (both (inspecting 'make-inked)))
           (pick both :plain-ink)
           (change-theme both "button" "#ff0000")
           ;; The app's button, in the window's theme, changes; the inspector's does not.
           (assert-eql (parse-color "#ff0000") (middle-color (app-of both) :plain-ink))
           (assert-eql (theme-setting dark :button) (middle-color (panel-of both) :inspector-pick)))
      (setf (gethash :dark theme::*themes*) dark))))
