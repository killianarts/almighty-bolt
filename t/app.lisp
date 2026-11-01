(defpackage #:almighty-bolt/t/app
  ;; This package uses the public one, whole: a symbol that the public package exports and that
  ;; Common Lisp or the test framework also has would be a conflict, and fail here.
  (:use #:cl #:lisp-unit2 #:almighty-bolt)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:m #:almighty-bolt/machine)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:counter #:almighty-bolt-examples/counter)
                    (#:todo #:almighty-bolt-examples/todo)
                    (#:gallery #:almighty-bolt-examples/gallery)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:window #:almighty-bolt/platform/macos/window)
                    (#:view #:almighty-bolt/platform/macos/view)
                    (#:a11y #:almighty-bolt/platform/macos/accessibility)))

(in-package #:almighty-bolt/t/app)

;;; A person's first hour with the library: the example apps, which use only the public package, are
;;; made as the host makes them, shown, clicked, typed into and made again. Events in, what is shown
;;; out (the text is read back from the display list, in which the fake context draws a character as
;;; a command whose parameter is its code).

(defstruct (screen (:constructor %make-screen (variant context list state)))
  variant context list state
  ;; What the host does after it has presented a frame (tell a screen reader), if anything.
  (after-draw nil))

(defun show (constructor)
  "Make the app of CONSTRUCTOR with a store for its state, as OPEN-APP does, and draw its first
frame."
  (let* ((state (core:make-kept-state))
         (variant (core:with-kept-state (state) (app:make-app-instance constructor)))
         (screen (%make-screen variant (fake:make-fake-context :width 360f0 :height 320f0)
                               (core:make-display-list :capacity 8192) state)))
    (draw screen)
    screen))

(defun draw (screen)
  (core:settle-frame (screen-variant screen) (screen-context screen) (screen-list screen))
  (when (screen-after-draw screen)
    (funcall (screen-after-draw screen))))

(defun pointer (screen kind x y &key button (buttons 0))
  (core:handle-event (screen-variant screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button button :buttons buttons
                                              :click-count (if (eq kind :move) 0 1)))
  (draw screen))

(defun click (screen id)
  (multiple-value-bind (x y width height) (core:lookup-target (screen-variant screen) id)
    (assert-true x id)
    (let ((px (+ x (/ width 2))) (py (+ y (/ height 2))))
      (pointer screen :move px py)
      (pointer screen :down px py :button 0 :buttons 1)
      (pointer screen :up px py :button 0))))

(defun type-text (screen string)
  (loop :for character :across string
        :do (core:handle-event (screen-variant screen)
                               (core:make-key-event :timestamp 0 :kind :down
                                                    :logical-key character :modifiers 0))
            (draw screen)))

(defun text-in (screen id)
  "The characters drawn inside the rectangle of the control ID."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-variant screen) id)
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
      (core:inherit-state new (screen-variant screen))
      (setf (screen-variant screen) new)
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
  (m:make-session :variant (lambda () (screen-variant screen))
                  :deliver (lambda (event)
                             (core:handle-event (screen-variant screen) event)
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
  (multiple-value-bind (left top width height) (core:lookup-target (screen-variant screen) :working)
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

;;; --- the gallery of every widget ---

(defparameter +every-widget+
  ;; What each widget and container that is not only layout is in the semantic tree. A widget added
  ;; to the library is added here and to the gallery (examples/gallery.lisp).
  '((label . :static-text) (paragraph . :paragraph) (button . :button) (text-field . :text-field)
    (text-area . :text-area) (slider . :slider) (choice . :radio-group) (checkbox . :checkbox)
    (list-box . :list) (progress . :progress-bar) (canvas . :canvas) (scrim . :overlay)
    (switch . :switch) (spinner . :busy-indicator)
    (scroll . :scroll-area) (lazy-list . :scroll-area)))

(defun roles-in (screen)
  (let ((roles '()))
    (s:walk-nodes (lambda (node) (pushnew (s:node-role node) roles))
                  (core:accessibility-tree (screen-variant screen)))
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
    (scroll-into-sight session screen :page :show-overlay)
    (m:click session :id :show-overlay)
    (let ((roles (roles-in screen)))
      (loop :for (widget . role) :in +every-widget+
            :do (assert-true (member role roles) widget role)))
    (assert-eql 10000 (s:node-property (node-of screen :tasks) :row-count))
    (assert-eq :scroll-area (s:node-role (node-of screen :inner)))
    (m:click session :id :close)
    (assert-false (member :overlay (roles-in screen)))))

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
         (boxes (s:find-nodes (core:accessibility-tree (screen-variant screen)) :role :checkbox)))
    (assert-equal '("Read the README" "Open a window from the REPL" "Change a label and compile it")
                  (mapcar #'s:node-name boxes))
    (assert-equal '(t nil nil) (mapcar #'s:node-value boxes))
    (click screen 2)
    (assert-equal '(t t nil)
                  (mapcar #'s:node-value
                          (s:find-nodes (core:accessibility-tree (screen-variant screen))
                                        :role :checkbox)))
    (let ((field (first (s:find-nodes (core:accessibility-tree (screen-variant screen))
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
  (first (s:find-nodes (core:accessibility-tree (screen-variant screen)) :id id)))

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
    (assert-eq nil (core:lookup-target (screen-variant screen) 5))
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
    ;; As little as shows all of it: its bottom, 168 points in, at the bottom of the window.
    (assert-eql 68f0 (offset-of screen :shelf))
    (assert-false (s:node-property (node-of screen 5) :hidden))
    ;; Back up to the first: its top at the top of the window.
    (dotimes (i 5) (m:press-key session :tab :modifiers '(:shift)))
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
    (multiple-value-bind (x y) (core:lookup-target (screen-variant screen) :notes)
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
                    :accessibility-tree (lambda () (core:accessibility-tree (screen-variant screen)))
                    :accessibility-press (lambda (x y)
                                           (dolist (spec '((:move nil 0 0) (:down 0 1 1) (:up 0 0 1)))
                                             (destructuring-bind (kind button buttons clicks) spec
                                               (core:handle-event
                                                (screen-variant screen)
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
                                           (core:accessibility-tree (screen-variant screen))
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
          (core:inherit-state new (screen-variant screen))
          (setf (screen-variant screen) new)
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
            (lambda () (core:accessibility-tree (screen-variant screen))))
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
