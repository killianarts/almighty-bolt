(defpackage #:almighty-bolt-studio/t/studio
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:studio #:almighty-bolt-studio)
                    (#:emacs #:almighty-bolt/emacs)
                    (#:views #:almighty-bolt/ui/views)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:host #:almighty-bolt/platform/macos/host)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:target #:almighty-bolt/render/metal/target)))

(in-package #:almighty-bolt-studio/t/studio)

;;; The studio worked as a person works it, through the machine interface, in a context with no
;;; window: both designers in one window, each kept while the other is shown.

(defstruct (screen (:constructor %make-screen (app context list kept)))
  app context list
  ;; What the studio's STATE-LETs keep, so that it can be made again as a reload makes it.
  kept)

(defun draw (screen)
  (core:settle-frame (screen-app screen) (screen-context screen) (screen-list screen)))

(defun open-studio ()
  "The studio and a machine session on it."
  (let* ((kept (core:make-kept-state))
         (instance (core:with-kept-state (kept)
                     (app:make-app-instance 'studio:make-studio)))
         (screen (%make-screen instance (fake:make-fake-context :width 1240f0 :height 3000f0)
                               (core:make-display-list :capacity 65536) kept)))
    (draw screen)
    (values screen
            (m:make-session :app (lambda () (screen-app screen))
                            :deliver (lambda (event)
                                       (core:handle-event (screen-app screen) event)
                                       (draw screen))
                            :settle (lambda () (dotimes (i 4) (draw screen)))
                            :on-main #'funcall))))

(defun shown-p (session id &optional within)
  "True if the screen has the thing ID (inside WITHIN): FIND-NODE says when it has not."
  (and (ignore-errors (if within
                          (m:find-node session :id id :within within)
                          (m:find-node session :id id)))
       t))

(defun menu-titles (screen)
  (let ((menus (core:app-menus (screen-app screen))))
    (loop :for i :from 0 :below (length menus) :by 2
          :collect (aref menus i))))

(defun color-at (screen x y)
  "The colour of the last rectangle on SCREEN's display list that covers the point X, Y and is drawn
inside every clip it is in."
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

(defun middle-color (screen session id within)
  "The colour drawn at the middle of the thing ID inside WITHIN."
  (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id id :within within))
    (color-at screen (+ x (/ w 2)) (+ y (/ h 2)))))

(define-test the-studio-is-in-its-own-theme-and-what-it-designs-in-the-windows ()
  (multiple-value-bind (screen session) (open-studio)
    (m:click session :id :add-button :within :left)
    (flet ((check (appearance background panel)
             (setf (core:context-appearance (screen-context screen)) appearance)
             (draw screen)
             ;; The title bar is white, or black in the dark; the designer's tool bar under it is
             ;; the studio's panel, a step off it.
             (assert-eql (bolt:parse-color background) (color-at screen 1 1) appearance)
             (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id :toolbar))
               (declare (ignore w h))
               (assert-eql (bolt:parse-color panel) (color-at screen (+ x 1) (+ y 1)) appearance))
             ;; The button on the canvas is as the app would draw it, in the window's theme, and
             ;; not in the studio's black or white.
             (assert-eql (bolt:theme-setting (bolt:find-theme appearance) :button)
                         (middle-color screen session :button-1 :canvas)
                         appearance)))
      (check :light "#ffffff" "#f2f2f2")
      (check :dark "#000000" "#1e1e1e"))))

(define-test the-app-in-the-app-pane-is-in-the-windows-theme-not-the-studios ()
  (multiple-value-bind (screen session) (open-studio)
    (m:click session :id :canvas-pane)
    (m:set-value session "almighty-bolt-studio/t/studio:make-tiny" :id :app-name)
    (dolist (appearance '(:light :dark))
      (setf (core:context-appearance (screen-context screen)) appearance)
      (dotimes (i 3) (draw screen))
      (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
        (multiple-value-bind (x y w h) (core:lookup-target studio:*running* :plus)
          (assert-eql (bolt:theme-setting (bolt:find-theme appearance) :button)
                      (color-at screen (+ px x (/ w 2)) (+ py y (/ h 2)))
                      appearance))))))

(define-test the-studios-theme-is-the-prototypes ()
  ;; What was imported from the prototype's design system: its greys, the line between parts, what
  ;; is well, its corners and its fonts, which are the system's own.
  (let ((light (bolt:find-theme :studio-light))
        (dark (bolt:find-theme :studio-dark)))
    (assert-eql (bolt:parse-color "#f2f2f2") (bolt:theme-setting light :panel))
    (assert-eql (bolt:parse-color "#1e1e1e") (bolt:theme-setting dark :panel))
    (assert-eql (bolt:parse-color "#000000") (bolt:theme-setting light :row-selected))
    (assert-eql 4f0 (float (bolt:theme-setting light :radius) 1f0))
    (assert-equal "System" (bolt:theme-setting light :font-name))
    (assert-equal "System Mono" (first (bolt:theme-setting dark :code-font))))
  ;; The colours of the chrome a theme has no setting for, light or dark as the window is.
  (flet ((tool (appearance name)
           (let ((app (app:make-app-instance
                       (lambda ()
                         (lambda ()
                           (bolt:label (almighty-bolt-studio/theme:tool-color name) :id :color)))))
                 (context (fake:make-fake-context :width 200f0 :height 100f0))
                 (said nil))
             (setf (core:context-appearance context) appearance)
             (core:settle-frame app context (core:make-display-list :capacity 256))
             (s:walk-nodes (lambda (node)
                             (when (eq (s:node-id node) :color)
                               (setf said (s:node-name node))))
                           (core:accessibility-tree app))
             said)))
    (assert-equal "#e4e4e4" (tool :light :line))
    (assert-equal "#2b2b2b" (tool :dark :line))
    (assert-equal "#3dd68c" (tool :light :ok))))

(define-test the-designers-parts-are-panes-each-kept-while-it-is-hidden ()
  (multiple-value-bind (screen session) (open-studio)
    ;; The GUI designer's parts, its toolbar and its File menu; the theme's settings behind a tab.
    (assert-true (shown-p session :canvas :canvas-part))
    (assert-true (shown-p session :wiring :options-part))
    (assert-false (shown-p session :theme-part))
    (assert-equal '("File") (menu-titles screen))
    (m:click session :id :add-button :within :left)
    (assert-true (shown-p session :button-1 :canvas))
    ;; The theme's settings in place of the options; the canvas is still there.
    (m:click session :id :theme-pane)
    (assert-true (shown-p session :settings :theme-part))
    (assert-false (shown-p session :options-part))
    (assert-true (shown-p session :button-1 :canvas))
    (m:set-value session "studio-test" :id :name :within :settings)
    (assert-true (search ":studio-test" (s:node-value (m:find-node session :id :source))))
    ;; The preview, behind the canvas's tab, is in the theme being made.
    (m:click session :id :preview-pane)
    (assert-true (shown-p session :preview-back :preview-part))
    ;; Back and forth: each is as it was left.
    (m:click session :id :options-pane)
    (m:click session :id :theme-pane)
    (assert-equal "studio-test" (s:node-value (m:find-node session :id :name :within :settings)))))

(defun press-keys (screen key &rest modifiers)
  "KEY pressed with MODIFIERS (:command, :shift), and the frames after it."
  (core:handle-event (screen-app screen)
                     (core:make-key-event :timestamp 0 :kind :down :logical-key key
                                          :modifiers (logior (if (member :command modifiers)
                                                                 core:+modifier-super+ 0)
                                                             (if (member :shift modifiers)
                                                                 core:+modifier-shift+ 0))))
  (dotimes (i 3) (draw screen)))

(defun pointer (screen kind x y &optional (modifiers 0))
  (core:handle-event (screen-app screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button 0
                                              :buttons (if (eq kind :up) 0 1)
                                              :click-count (if (eq kind :move) 0 1)
                                              :modifiers modifiers))
  (draw screen))

(define-test command-z-undoes-in-the-designer-last-used ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((source () (s:node-value (m:find-node session :id :source))))
      ;; A change in each; the theme's last, by a slider (a text field would undo its own typing).
      (m:click session :id :add-button :within :left)
      (m:click session :id :theme-pane)
      (m:set-value session 12 :id :radius :within :settings)
      (assert-true (search ":radius 12" (source)))
      (assert-true (shown-p session :button-1 :canvas))
      ;; Command-Z goes to the theme designer, used last: the view keeps its button.
      (press-keys screen #\z :command)
      (assert-false (search ":radius" (source)))
      (assert-true (shown-p session :button-1 :canvas))
      ;; Pressed in the GUI designer, Command-Z is its own.
      (m:set-value session 12 :id :radius :within :settings)
      (m:click session :id :canvas)
      (press-keys screen #\z :command)
      (assert-false (shown-p session :button-1 :canvas))
      (assert-true (search ":radius 12" (source))))))

;; An app for the studio to run.
(defun make-tiny ()
  (bolt:state-let ((count 0))
    (lambda ()
      (bolt:column (:id :tiny :gap 8)
        (when (bolt:button "Add one" :id :plus) (incf count))
        (bolt:label (format nil "Count ~d" count) :id :count)))))

(define-test the-app-runs-in-a-pane-and-the-inspector-beside-it-works-it ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((count-shown ()
             (s:node-name (first (s:find-nodes (core:accessibility-tree studio:*running*) :id :count))))
           (press-plus ()
             (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
               (multiple-value-bind (x y w h) (core:lookup-target studio:*running* :plus)
                 (let ((x (+ px x (/ w 2))) (y (+ py y (/ h 2))))
                   (pointer screen :move x y)
                   (pointer screen :down x y)
                   (pointer screen :up x y)
                   (dotimes (i 3) (draw screen)))))))
      ;; With no app, the canvas has the view drawn alone.
      (m:click session :id :canvas-pane)
      (assert-false studio:*running*)
      (assert-true (shown-p session :canvas))
      (m:set-value session "almighty-bolt-studio/t/studio:make-tiny" :id :app-name)
      (m:set-value session "Run" :id :canvas-mode)
      (assert-true studio:*running*)
      (press-plus)
      (assert-equal "Count 1" (count-shown))
      ;; The inspector, beside it, picks its button and presses it.
      (m:click session :id :inspect-pane)
      (m:click session :id :inspector-pick)
      (press-plus)
      (assert-equal "Count 1" (count-shown))
      (assert-equal "Kind: button" (s:node-name (m:find-node session :id (list :inspector-fact 0))))
      (m:click session :id :inspector-press)
      (dotimes (i 3) (draw screen))
      (assert-equal "Count 2" (count-shown)))))

;;; --- the running app edited in the studio ---

;; A view, and an app that draws it with its count wired in.
(bolt:define-view tiny-view
  (:column :tiny-page (:gap 8)
   (:button :plus (:text "Add one"))
   (:label :count (:text "Count"))))

(defparameter *tiny-view* (almighty-bolt/ui/views:find-view 'tiny-view)
  "The view as this file defines it, put back after a test edits it.")

(defun make-viewed ()
  (bolt:state-let ((count 0))
    (lambda ()
      (bolt:view tiny-view
        (button :plus :on-press (incf count))
        (label :count :text (format nil "Count ~d" count))))))

(define-test a-widget-picked-in-the-running-app-is-edited-in-its-view-and-the-app-changes ()
  (multiple-value-bind (screen session) (open-studio)
    (labels ((inner (id)
               (first (s:find-nodes (core:accessibility-tree studio:*running*) :id id)))
             (press-plus ()
               (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
                 (multiple-value-bind (x y w h) (core:lookup-target studio:*running* :plus)
                   (let ((x (+ px x (/ w 2))) (y (+ py y (/ h 2))))
                     (pointer screen :move x y)
                     (pointer screen :down x y)
                     (pointer screen :up x y)
                     (dotimes (i 3) (draw screen))))))
             (facts ()
               (loop :for i :from 0
                     :for node = (ignore-errors (m:find-node session :id (list :inspector-fact i)))
                     :while node :collect (s:node-name node))))
      (unwind-protect
           (progn
             (m:click session :id :canvas-pane)
             (m:set-value session "almighty-bolt-studio/t/studio:make-viewed" :id :app-name)
             (m:set-value session "Run" :id :canvas-mode)
             (press-plus)
             (assert-equal "Count 1" (s:node-name (inner :count)))
             ;; Picked in the running app: the inspector says which view drew it, and what of it the
             ;; code gives.
             (m:click session :id :inspect-pane)
             (m:click session :id :inspector-pick)
             (press-plus)
             (assert-true (member "View: almighty-bolt-studio/t/studio::tiny-view, its :plus"
                                  (facts) :test #'string=)
                          (facts))
             (assert-true (member "Given by the code: :on-press" (facts) :test #'string=) (facts))
             ;; Edit in the View: the canvas and the options, the view open in them, live, with
             ;; the button selected.
             (m:click session :id :inspector-edit-view)
             (dotimes (i 2) (draw screen))
             ;; On the canvas, the running app, where what is edited shows.
             (assert-true (shown-p session :running))
             (assert-equal "Button :plus"
                           (s:node-name (m:find-node session :id :selected-what)))
             ;; Its words changed there: the running app changes as they are typed, and keeps its
             ;; count.
             (m:set-value session "Add two" :id :option-text)
             (dotimes (i 2) (draw screen))
             (assert-equal "Add two" (s:node-name (inner :plus)))
             (assert-equal "Count 1" (s:node-name (inner :count)))
             ;; Undone in the designer: the running app is as it was.
             (press-keys screen #\z :command)
             (assert-equal "Add one" (s:node-name (inner :plus))))
        (almighty-bolt/ui/views:set-view 'tiny-view *tiny-view*)))))

;;; --- the bridge to Emacs, as the studio uses it ---

(defun emacs-there ()
  "Be Emacs, asking for requests: then it is there to be asked. What is queued is dropped."
  (loop :while (emacs:next-request 0)))

(defun emacs-gone ()
  (setf emacs::*last-asked* nil
        emacs::*requests* '()))

(defun requests ()
  "What the studio asked Emacs to do since this was last asked, oldest first."
  (loop :for request = (emacs:next-request 0) :while request :collect request))

(defun run-in-studio (session constructor &key (mode "Run"))
  "Run the app CONSTRUCTOR makes on the canvas, in MODE (\"Run\": the app has the presses)."
  (m:click session :id :canvas-pane)
  (m:set-value session (format nil "almighty-bolt-studio/t/studio:~(~a~)" constructor)
               :id :app-name)
  (m:set-value session mode :id :canvas-mode))

(defun press-in-running (screen id)
  "Press the widget ID of the app running on the canvas, where the canvas shows it."
  (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
    (multiple-value-bind (x y w h) (core:lookup-target studio:*running* id)
      (let ((x (+ px x (/ w 2))) (y (+ py y (/ h 2))))
        (pointer screen :move x y)
        (pointer screen :down x y)
        (pointer screen :up x y)
        (dotimes (i 3) (draw screen))))))

(defun in-running (screen id fraction &optional (across 1/2))
  "The point on the canvas FRACTION of the way down the widget ID of the app running there, and
ACROSS of the way along it."
  (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
    (multiple-value-bind (x y w h) (core:lookup-target studio:*running* id)
      (values (+ px x (* w across)) (+ py y (* h fraction))))))

;; A view with a row inside its column, and an app that draws it.
(bolt:define-view nested-view
  (:column :nested-page (:gap 8)
   (:row :bar (:gap 8)
    (:button :first (:text "First"))
    (:button :second (:text "Second")))
   (:label :under (:text "Under"))))

(defparameter *nested-view* (almighty-bolt/ui/views:find-view 'nested-view))

(defun make-nested ()
  (lambda ()
    (bolt:view nested-view)))

(defun drag-to (screen from-x from-y to-x to-y)
  "Press at FROM, move to TO in steps, and let go."
  (pointer screen :move from-x from-y)
  (pointer screen :down from-x from-y)
  (dotimes (i 4)
    (let ((f (/ (1+ i) 4)))
      (pointer screen :move (+ from-x (* f (- to-x from-x))) (+ from-y (* f (- to-y from-y))))))
  (pointer screen :up to-x to-y)
  (dotimes (i 3) (draw screen)))

(defun view-kinds (name)
  "The kinds and ids of the widgets in the outermost container of the view NAME, in order."
  (mapcar (lambda (form) (list (first form) (second form)))
          (cdddr (views:find-view name))))

(define-test what-is-let-go-of-on-the-running-app-goes-into-its-view-where-it-was ()
  (multiple-value-bind (screen session) (open-studio)
    (unwind-protect
         (progn
           (run-in-studio session 'make-viewed :mode "Design")
           (dotimes (i 3) (draw screen))
           ;; A label from the catalogue, let go of low on the button: after it, before the count;
           ;; the view, opened live, has it there, and so does the running app.
           (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id :add-label
                                                                             :within :left))
             (multiple-value-bind (to-x to-y) (in-running screen :plus 0.9)
               (drag-to screen (+ x (/ w 2)) (+ y (/ h 2)) to-x to-y)))
           (assert-equal '((:button :plus) (:label :label-1) (:label :count))
                         (view-kinds 'tiny-view))
           (assert-true (s:find-nodes (core:accessibility-tree studio:*running*) :id :label-1))
           ;; One of the view's own, dragged high on the button: before it.
           (multiple-value-bind (from-x from-y) (in-running screen :count 0.5)
             (multiple-value-bind (to-x to-y) (in-running screen :plus 0.1)
               (drag-to screen from-x from-y to-x to-y)))
           (assert-equal '((:label :count) (:button :plus) (:label :label-1))
                         (view-kinds 'tiny-view)))
      (views:set-view 'tiny-view *tiny-view*))))

(define-test what-is-let-go-of-goes-into-the-innermost-container-along-it ()
  (multiple-value-bind (screen session) (open-studio)
    (unwind-protect
         (progn
           (run-in-studio session 'make-nested :mode "Design")
           (dotimes (i 3) (draw screen))
           ;; On the right of the row's first button: into the row, between its buttons, not into
           ;; the column around it.
           (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id :add-label
                                                                             :within :left))
             (multiple-value-bind (to-x to-y) (in-running screen :first 1/2 9/10)
               (drag-to screen (+ x (/ w 2)) (+ y (/ h 2)) to-x to-y)))
           (assert-equal '((:row :bar) (:label :under)) (view-kinds 'nested-view))
           (assert-equal '((:button :first) (:label :label-1) (:button :second))
                         (mapcar (lambda (form) (list (first form) (second form)))
                                 (cdddr (fourth (views:find-view 'nested-view))))))
      (views:set-view 'nested-view *nested-view*))))

(define-test the-canvas-bar-tries-a-widget-shows-sample-data-and-picks-the-apps-theme ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((inner (id)
             (s:node-name (first (s:find-nodes (core:accessibility-tree studio:*running*) :id id))))
           (button-color ()
             (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
               (multiple-value-bind (x y w h) (core:lookup-target studio:*running* :plus)
                 (color-at screen (+ px x (/ w 2)) (+ py y (/ h 2)))))))
      (unwind-protect
           (progn
             (setf (core:context-appearance (screen-context screen)) :light)
             (run-in-studio session 'make-viewed :mode "Design")
             (dotimes (i 3) (draw screen))
             ;; Command-press, designing: the app has it, and nothing is picked.
             (multiple-value-bind (x y) (in-running screen :plus 1/2)
               (pointer screen :move x y)
               (pointer screen :down x y core:+modifier-super+)
               (pointer screen :up x y core:+modifier-super+)
               (dotimes (i 3) (draw screen)))
             (assert-equal "Count 1" (inner :count))
             (assert-false (shown-p session :selected-what))
             ;; A plain press picks again.
             (press-in-running screen :plus)
             (assert-equal "Count 1" (inner :count))
             (assert-equal "Button :plus" (s:node-name (m:find-node session :id :selected-what)))
             ;; The app drawn dark in a light window, and as the window again.
             (assert-eql (bolt:theme-setting (bolt:find-theme :light) :button) (button-color))
             (m:set-value session "Dark" :id :canvas-theme)
             (dotimes (i 3) (draw screen))
             (assert-eql (bolt:theme-setting (bolt:find-theme :dark) :button) (button-color))
             (m:set-value session "System" :id :canvas-theme)
             (dotimes (i 3) (draw screen))
             (assert-eql (bolt:theme-setting (bolt:find-theme :light) :button) (button-color))
             ;; Sample: the view drawn alone, the app not running there; Live, back as it was.
             (m:set-value session "Sample" :id :canvas-data)
             (assert-false studio:*running*)
             (assert-true (shown-p session :plus :canvas))
             (m:set-value session "Live" :id :canvas-data)
             (assert-equal "Count 1" (inner :count)))
        (views:set-view 'tiny-view *tiny-view*)))))

(define-test the-options-say-where-each-value-comes-from-the-container-and-the-wiring ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((said (id &optional within)
             (s:node-name (if within
                              (m:find-node session :id id :within within)
                              (m:find-node session :id id)))))
      (unwind-protect
           (progn
             (run-in-studio session 'make-viewed :mode "Design")
             (dotimes (i 3) (draw screen))
             ;; The count's words are the code's; its size the kind's default.
             (press-in-running screen :count)
             (assert-equal "wired" (said :option-text-source))
             (assert-equal "default" (said :option-size-source))
             ;; The button's words are the view's.
             (press-in-running screen :plus)
             (assert-equal "given" (said :option-text-source))
             (assert-equal "default" (said :option-enabled-source))
             ;; The column it is in, whose gap is changed from here: the view has it, live.
             (assert-equal "In COL :tiny-page · 2" (said :container-what))
             (assert-equal "8" (s:node-value (m:find-node session :id :container-gap)))
             (m:set-value session "20" :id :container-gap)
             (assert-eql 20 (getf (third (views:find-view 'tiny-view)) :gap))
             ;; Where the code that wires it is: this file, at its VIEW form.
             (let ((line (with-open-file (in (asdf:system-relative-pathname
                                              "almighty-bolt-studio" "t/studio.lisp"))
                           (loop :for text = (read-line in nil)
                                 :for number :from 1
                                 :while text
                                 :when (search "(bolt:view tiny-view" text)
                                   :return number))))
               (assert-equal (format nil "Wired in studio.lisp, line ~d." line)
                             (said :wiring-where))))
        (views:set-view 'tiny-view *tiny-view*)))))

(define-test the-frames-pane-charts-the-frames-of-the-app-on-the-canvas ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((frames-said ()
             (let ((words (s:node-name (m:find-node session :id :frames-summary))))
               (values (parse-integer words :junk-allowed t) words))))
      (m:click session :id :frames-pane)
      (assert-equal "No frames yet: run an app on the canvas." (nth-value 1 (frames-said)))
      (run-in-studio session 'make-tiny)
      (m:click session :id :frames-pane)
      (dotimes (i 3) (draw screen))
      (let ((before (frames-said)))
        (assert-true (plusp before))
        (assert-true (search " · last: build " (nth-value 1 (frames-said)))
                     (nth-value 1 (frames-said)))
        ;; Each frame timed: its build, layout and paint took some time.
        (assert-true (every (lambda (frame) (plusp (reduce #'+ frame)))
                            (bolt:app-pane-frames studio:*running*)))
        (assert-true (shown-p session :frames-chart))
        ;; A press makes the app draw again: another frame.
        (press-in-running screen :plus)
        (assert-true (> (frames-said) before))))))

(define-test on-the-canvas-a-press-picks-the-widget-of-its-view-and-in-run-is-the-apps ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((inner (id)
             (s:node-name (first (s:find-nodes (core:accessibility-tree studio:*running*) :id id)))))
      (unwind-protect
           (progn
             (run-in-studio session 'make-viewed :mode "Design")
             (dotimes (i 3) (draw screen))
             ;; Designing: the press picks the button, in its view, opened live; the app has none
             ;; of it.
             (press-in-running screen :plus)
             (assert-equal "Count 0" (inner :count))
             (assert-equal "Button :plus" (s:node-name (m:find-node session :id :selected-what)))
             ;; Marked where the app draws it: a bracket just outside its corner, and a tag that
             ;; says it and that the code wires it.
             (assert-equal "Button :plus · wired"
                           (s:node-name (m:find-node session :id :selection-tag-text)))
             (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
               (multiple-value-bind (x y) (core:lookup-target studio:*running* :plus)
                 (assert-eql (bolt:theme-setting (bolt:find-theme (core:context-appearance
                                                                   (screen-context screen)))
                                                 :ring)
                             (color-at screen (+ px x -1) (+ py y -1)))))
             ;; Another widget of the view, picked as the view is live.
             (press-in-running screen :count)
             (assert-equal "Label :count" (s:node-name (m:find-node session :id :selected-what)))
             ;; Run: the press is the app's, and the selection stays.
             (m:set-value session "Run" :id :canvas-mode)
             (press-in-running screen :plus)
             (assert-equal "Count 1" (inner :count))
             (assert-equal "Label :count" (s:node-name (m:find-node session :id :selected-what))))
        (almighty-bolt/ui/views:set-view 'tiny-view *tiny-view*)))))

(defun pick-in-running (screen session id)
  "Pick the widget ID of the running app with the inspector."
  (m:click session :id :inspect-pane)
  (m:click session :id :inspector-pick)
  (multiple-value-bind (px py) (core:lookup-target (screen-app screen) :running)
    (multiple-value-bind (x y w h) (core:lookup-target studio:*running* id)
      (let ((x (+ px x (/ w 2))) (y (+ py y (/ h 2))))
        (pointer screen :move x y)
        (pointer screen :down x y)
        (pointer screen :up x y)
        (dotimes (i 3) (draw screen))))))

(define-test edit-source-asks-emacs-for-the-view-s-clause-the-wiring-or-the-code ()
  (multiple-value-bind (screen session) (open-studio)
    (unwind-protect
         (progn
           (emacs-there)
           ;; A widget a view drew: its clause in the view's file, and where the code wires it.
           (run-in-studio session 'make-viewed)
           (pick-in-running screen session :plus)
           (m:click session :id :inspector-edit-source)
           (let ((request (first (requests))))
             (assert-eq :show-view (getf request :op))
             (assert-true (search "studio.lisp" (getf request :file)) (getf request :file))
             (assert-equal "tiny-view" (getf request :name))
             (assert-equal "plus" (getf request :widget)))
           (m:click session :id :inspector-edit-wiring)
           (let ((request (first (requests))))
             (assert-eq :show-wiring (getf request :op))
             (assert-true (search "studio.lisp" (getf request :file)) (getf request :file)))
           ;; A widget no view drew: the app's constructor, at its id.
           (run-in-studio session 'make-tiny)
           (pick-in-running screen session :plus)
           (m:click session :id :inspector-edit-source)
           (let ((request (first (requests))))
             (assert-eq :show-code (getf request :op))
             (assert-equal ":plus" (getf request :search))
             ;; The definition is the first form at the offset (SBCL's offset is where the form
             ;; before it ended).
             (assert-equal '(defun make-tiny)
                           (subseq (let ((*package* (find-package :almighty-bolt-studio/t/studio)))
                                     (read-from-string (uiop:read-file-string (getf request :file))
                                                       t nil :start (getf request :offset)))
                                   0 2)))
           ;; Without Emacs, the inspector says so.
           (emacs-gone)
           (m:click session :id :inspector-edit-source)
           (assert-true (search "not connected"
                                (s:node-name (m:find-node session :id :inspector-message)))))
      (emacs-gone))))

(define-test point-in-emacs-selects-the-widget-in-the-studio-and-a-view-typed-there-is-taken-up ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((inner (id)
             (first (s:find-nodes (core:accessibility-tree studio:*running*) :id id))))
      (unwind-protect
           (progn
             (run-in-studio session 'make-viewed)
             ;; Point on the button's clause, in Emacs: the view in the designer, the button
             ;; selected, the running app on the canvas.
             (emacs:point-in-view "tiny-view" "almighty-bolt-studio/t/studio" "plus")
             (dotimes (i 3) (draw screen))
             (assert-equal "Button :plus" (s:node-name (m:find-node session :id :selected-what)))
             (assert-true (shown-p session :running))
             ;; The view typed in Emacs: the running app and the designer both take it up.
             (emacs-there)
             (assert-equal ""
                           (emacs:view-typed "(bolt:define-view tiny-view
                                                (:column :tiny-page (:gap 8)
                                                 (:button :plus (:text \"Typed\"))
                                                 (:label :count (:text \"Count\"))))"
                                             "almighty-bolt-studio/t/studio"))
             (dotimes (i 3) (draw screen))
             (assert-equal "Typed" (s:node-name (inner :plus)))
             ;; The designer too: the button's words, in its options.
             (assert-equal "Typed" (s:node-value (m:find-node session :id :option-text)))
             ;; Not sent back to Emacs, which has it.
             (assert-false (requests))
             ;; Undone in the designer (its button: the keyboard is in the App field, whose
             ;; Command-Z undoes its own typing): back in the running app, and in Emacs's buffer.
             (m:click session :id :undo :within :toolbar)
             (assert-equal "Add one" (s:node-name (inner :plus)))
             (let ((request (first (requests))))
               (assert-eq :replace-view (getf request :op))
               (assert-true (search "\"Add one\"" (getf request :text)) (getf request :text))))
        (emacs-gone)
        (views:set-view 'tiny-view *tiny-view*)))))

;;; --- the canvas and the app at sizes of their own; the outline as the selection moves ---

(defun outline-places (screen)
  "Where the frame's outlines are: (x y width height) each."
  (let ((ui (almighty-bolt/ui/immediate:immediate-app-ui (screen-app screen))))
    (loop :for float :across (almighty-bolt/ui/ui:ui-floats ui)
          :when (eq (almighty-bolt/ui/box:box-kind float) :outline)
            :collect (mapcar #'round (list (almighty-bolt/ui/box:box-x float)
                                           (almighty-bolt/ui/box:box-y float)
                                           (almighty-bolt/ui/box:box-w float)
                                           (almighty-bolt/ui/box:box-h float))))))

(define-test the-outline-s-rows-select-in-the-canvas-whatever-is-on-the-right ()
  ;; With the theme's settings on the right, nothing there changes when a row is chosen: the
  ;; canvas's outline must still go round what was.
  (multiple-value-bind (screen session) (open-studio)
    (m:click session :id :theme-pane)
    (dolist (row (s:find-nodes (m:find-node session :id :outline-tree) :role :row))
      (let* ((name (s:node-name row))
             (id (intern (string-upcase (subseq name (1+ (position #\: name))
                                                (position #\Space name :start (position #\: name))))
                         :keyword)))
        (destructuring-bind (x y w h) (s:node-bounds row)
          (pointer screen :move (+ x (/ w 2)) (+ y (/ h 2)))
          (pointer screen :down (+ x (/ w 2)) (+ y (/ h 2)))
          (pointer screen :up (+ x (/ w 2)) (+ y (/ h 2)))
          (dotimes (i 3) (draw screen)))
        (assert-equal (list (mapcar #'round (s:node-bounds (m:find-node session :id id
                                                                                :within :canvas))))
                      (outline-places screen)
                      name)))))

(defun resize (screen width)
  (setf (core:context-width (screen-context screen)) (float width 1f0))
  (dotimes (i 2) (draw screen)))

(define-test the-canvas-and-the-app-are-laid-out-at-their-own-sizes-which-the-person-sets ()
  (multiple-value-bind (screen session) (open-studio)
    (labels ((field-width ()
               (third (s:node-bounds (m:find-node session :id :name :within :canvas))))
             (size-set (within)
               ;; The size the person set, which the stage is where there is room for it.
               (s:node-value (m:find-node session :id :stage-width :within within)))
             (app-width ()
               ;; The window the app in the pane is laid out in, in its own points.
               (core:context-width
                (almighty-bolt/ui/app-pane::app-pane-state-context
                 (gethash studio:*running* almighty-bolt/ui/app-pane::*pane-states*)))))
      ;; The view at the canvas's 480, however wide the window is: its field does not change. (At
      ;; 940, the canvas's pane is narrower than the stage, and the field still shows whole.)
      (let ((wide (field-width)))
        (resize screen 940)
        (assert-eql wide (field-width))
        (resize screen 1240)
        (assert-equal "480" (size-set :canvas-stage))
        ;; Its width typed, and taken with Return; and dragged by the grip, 50 points.
        (m:set-value session "520" :id :stage-width :within :canvas-stage)
        (m:press-key session :return)
        (assert-equal "520" (size-set :canvas-stage))
        (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id :stage-grip))
          (let ((x (+ x (/ w 2))) (y (+ y (/ h 2))))
            (pointer screen :move x y)
            (pointer screen :down x y)
            (pointer screen :move (+ x 50) y)
            (pointer screen :up (+ x 50) y)
            (dotimes (i 3) (draw screen))))
        (assert-equal "570" (size-set :canvas-stage))
        ;; The pane has room for less: the stage is cut off at its edge, not made smaller.
        (assert-true (< (third (s:node-bounds (m:find-node session :id :stage
                                                                   :within :canvas-stage)))
                        570))
        (assert-eql wide (field-width)))
      ;; The app on the canvas, at the size the view was given there, as its window's would be,
      ;; whatever the pane: and at 200%, the same size in its own points, shown twice as large.
      (run-in-studio session 'make-tiny)
      (assert-eql 570f0 (app-width))
      (m:set-value session "200%" :id :zoom)
      (dotimes (i 3) (draw screen))
      (assert-eql 570f0 (app-width))
      (resize screen 900)
      (assert-eql 570f0 (app-width))
      (assert-equal "570" (size-set :canvas-stage)))))

;;; --- the title bar and the modes ---

(defun words-of (session id)
  "What the row ID of the title bar says, its words joined by spaces."
  (format nil "~{~a~^ ~}" (mapcar #'s:node-name (s:node-children (m:find-node session :id id)))))

(defun lisp-help (session)
  "What the title bar's words about the Lisp say to a screen reader."
  (s:node-help (first (s:node-children (m:find-node session :id :lisp)))))

(defun to-mode (session title)
  (m:set-value session title :id :modes))

(define-test the-title-bar-says-the-studio-its-modes-where-the-person-is-and-the-image ()
  (multiple-value-bind (screen session) (open-studio)
    (assert-equal "ALMIGHTY-BOLT STUDIO" (s:node-name (m:find-node session :id :studio-name)))
    (assert-equal '("DESIGN" "WIRE" "DEBUG" "THEME" "AGENT")
                  (s:node-property (m:find-node session :id :modes) :options))
    (assert-eql 0 (s:node-property (m:find-node session :id :modes) :selected))
    ;; The view the designer starts with, in no file yet, and the Lisp the studio runs in.
    (assert-equal "VIEW ■ SIGN-IN ■ CL-USER ■ NOT SAVED" (words-of session :where))
    (assert-equal (format nil "~:@(~a ~a~)" (lisp-implementation-type)
                          (lisp-implementation-version))
                  (words-of session :lisp))
    (assert-equal "No view is live in this image." (lisp-help session))
    (flet ((dot () (destructuring-bind (x y w h) (s:node-bounds (m:find-node session :id :live-dot))
                     (color-at screen (+ x (/ w 2)) (+ y (/ h 2))))))
      ;; Grey while no view is live (the window is dark).
      (assert-eql (bolt:parse-color "#565656") (dot))
      (unwind-protect
           (progn
             ;; From Theme, point in Emacs on a view's widget: Design, with that view, live.
             (to-mode session "THEME")
             (assert-false (shown-p session :canvas-part))
             (emacs:point-in-view "tiny-view" "almighty-bolt-studio/t/studio" "plus")
             (dotimes (i 3) (draw screen))
             (assert-eql 0 (s:node-property (m:find-node session :id :modes) :selected))
             (assert-true (shown-p session :canvas-part))
             (assert-equal "Button :plus" (s:node-name (m:find-node session :id :selected-what)))
             (assert-equal "VIEW ■ TINY-VIEW ■ STUDIO ■ STUDIO.LISP"
                           (words-of session :where))
             (assert-equal "The view tiny-view is live in this image: an edit changes the running app as it is made."
                           (lisp-help session))
             (assert-eql (bolt:parse-color "#3dd68c") (dot))
             ;; The app run says whose it is.
             (run-in-studio session 'make-viewed)
             (assert-equal "VIEW ■ TINY-VIEW ■ STUDIO ■ STUDIO.LISP"
                           (words-of session :where)))
        (emacs-gone)
        (views:set-view 'tiny-view *tiny-view*)))))

(define-test the-designers-bar-is-in-design-alone-and-its-file-menu-in-every-mode ()
  (multiple-value-bind (screen session) (open-studio)
    ;; In Design, without a Save of its own: the title bar has it.
    (assert-true (shown-p session :toolbar))
    (assert-true (shown-p session :undo :toolbar))
    (assert-false (shown-p session :save :toolbar))
    (dolist (mode '("WIRE" "DEBUG" "THEME" "AGENT"))
      (to-mode session mode)
      (assert-false (shown-p session :toolbar) mode)
      (assert-equal '("File") (menu-titles screen) mode))))

(define-test each-mode-has-its-panes-and-keeps-how-they-are-arranged ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    ;; Design has every pane: the theme's settings behind the options' tab.
    (m:click session :id :theme-pane)
    (assert-true (shown-p session :theme-part))
    (to-mode session "WIRE")
    (assert-true (shown-p session :views-part))
    (assert-true (shown-p session :view-code-part))
    (assert-true (shown-p session :code-part))
    (assert-true (shown-p session :options-part))
    (assert-false (shown-p session :widgets-part))
    (to-mode session "DEBUG")
    (assert-true (shown-p session :canvas-part))
    (assert-true (shown-p session :inspecting))
    (assert-false (shown-p session :options-part))
    (to-mode session "THEME")
    (assert-true (shown-p session :preview-part))
    (assert-true (shown-p session :theme-part))
    (assert-false (shown-p session :options-part))
    (to-mode session "AGENT")
    (assert-true (shown-p session :agent-note))
    (assert-false (shown-p session :theme-part))
    ;; Back in Design, as it was left: the theme's settings in front, not the options.
    (to-mode session "DESIGN")
    (assert-true (shown-p session :theme-part))
    (assert-false (shown-p session :options-part))
    ;; And in Theme, its app brought to the front, which Design does not change.
    (to-mode session "THEME")
    (m:click session :id :canvas-pane)
    (to-mode session "DESIGN")
    (to-mode session "THEME")
    (assert-true (shown-p session :canvas-part))
    (assert-false (shown-p session :preview-part))))

(define-test save-in-the-title-bar-saves-the-view-from-any-mode ()
  (multiple-value-bind (screen session) (open-studio)
    (let* ((name (format nil "studio-title-save-~d" (get-universal-time)))
           (path (merge-pathnames (format nil "~a.lisp" name) (uiop:temporary-directory)))
           (asked '()))
      (unwind-protect
           (progn
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (declare (ignore types offered))
                     (push kind asked)
                     (namestring path)))
             (to-mode session "THEME")
             (m:click session :id :title-save)
             (assert-equal '(:save) asked)
             (assert-true (probe-file path))
             (assert-equal (format nil "VIEW ■ ~:@(~a~) ■ CL-USER ■ ~:@(~a~).LISP" name name)
                           (words-of session :where))
             ;; Saved again, to its own file, without asking; and by the File menu, in Theme too,
             ;; where the designer's bar is not.
             (m:click session :id :title-save)
             (assert-equal '(:save) asked)
             (delete-file path)
             (core:handle-event (screen-app screen)
                                (core:make-menu-event :timestamp 0 :menu "File" :item "Save…"))
             (dotimes (i 3) (draw screen))
             (assert-true (probe-file path)))
        (when (probe-file path) (delete-file path))))))

(define-test run-window-opens-the-app-being-run-in-a-window-of-its-own ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    (let* ((opened '())
           (studio:*open-window* (lambda (constructor &rest options)
                                   (push (cons constructor options) opened))))
      (assert-true (s:node-property (m:find-node session :id :run-window) :disabled))
      (run-in-studio session 'make-viewed)
      (assert-false (s:node-property (m:find-node session :id :run-window) :disabled))
      (m:click session :id :run-window)
      (assert-equal '((make-viewed :title "almighty-bolt-studio/t/studio:make-viewed"
                                   :width 480 :height 360))
                    opened))))

(define-test going-to-design-to-edit-keeps-how-the-person-arranged-it ()
  (multiple-value-bind (screen session) (open-studio)
    (unwind-protect
         (progn
           ;; The widgets moved in with the canvas, by the keyboard: they share its group.
           (m:click session :id :widgets-pane)
           (m:press-key session :right :modifiers '(:control :shift))
           (assert-true (shown-p session :widgets-part))
           (assert-false (shown-p session :canvas-part))
           (to-mode session "DEBUG")
           ;; Point in Emacs: the canvas in front of the widgets, which stay in its group.
           (emacs:point-in-view "tiny-view" "almighty-bolt-studio/t/studio" "plus")
           (dotimes (i 3) (draw screen))
           (assert-true (shown-p session :canvas-part))
           (assert-false (shown-p session :widgets-part)))
      (emacs-gone)
      (views:set-view 'tiny-view *tiny-view*))))

(defun real-layout (width height &optional (appearance :light))
  "The studio laid out, WIDTH by HEIGHT points, in the system's own fonts (a host's context, with no
window): its app, after a few frames."
  (let* ((host (host:make-host (make-instance 'core:app-instance) (device:default-device)
                               target:+pixel-format-bgra8-unorm+ :capacity 65536 :scale 2))
         (context (host:host-context host))
         (app (core:with-kept-state ((core:make-kept-state))
                (app:make-app-instance 'studio:make-studio))))
    (setf (core:context-width context) (float width 1f0)
          (core:context-height context) (float height 1f0)
          (core:context-scale context) 2f0
          (core:context-appearance context) appearance)
    (dotimes (i 3)
      (core:settle-frame app context (host:host-display-list host)))
    app))

(define-test the-title-bar-fits-the-window-the-studio-opens-in ()
  ;; In the system's fonts, as the window draws them: its last button inside the window's 1240
  ;; points, in every mode, with nothing laid over anything else.
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let ((app (real-layout 1240 820)))
        (multiple-value-bind (x y w) (core:lookup-target app :run-window)
          (declare (ignore y))
          (assert-true (<= (+ x w) 1240) (+ x w)))
        (let ((right 0))
          (dolist (id '(:brand :modes :where :image-state :title-save :run-window))
            (multiple-value-bind (x y w) (core:lookup-target app id)
              (declare (ignore y))
              (assert-true (<= right x) id right x)
              (setf right (+ x w)))))))))

;;; --- the rail, the sections, the bottom panel and the status bar ---

(defun name-of (session id &optional within)
  (s:node-name (if within
                   (m:find-node session :id id :within within)
                   (m:find-node session :id id))))

(define-test the-rail-says-the-mode-the-view-and-how-many-widgets-it-has ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    (assert-equal "DESIGN ■ VIEW SIGN-IN ■ 9 WIDGETS" (name-of session :rail-words))
    (m:click session :id :add-button :within :left)
    (to-mode session "THEME")
    (assert-equal "THEME ■ VIEW SIGN-IN ■ 10 WIDGETS" (name-of session :rail-words))))

(define-test the-designers-sections-have-the-studios-headers ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    ;; Title, kana and count; the heading keeps its id, which the outline is labelled by.
    (assert-equal "CATALOGUE" (name-of session :palette-heading))
    (assert-equal "カタログ" (name-of session :kana (list :palette-heading :header)))
    (assert-equal (princ-to-string (length (almighty-bolt/ui/catalogue:catalogue)))
                  (name-of session :count (list :palette-heading :header)))
    (assert-equal "9" (name-of session :count (list :outline-heading :header)))
    (assert-equal "OUTLINE" (name-of session :outline-tree))
    (assert-equal "配線" (name-of session :kana (list :wiring-heading :header)))
    ;; The catalogue's count is of what it shows, searched.
    (m:set-value session "field" :id :palette-search)
    (assert-equal "1" (name-of session :count (list :palette-heading :header)))))

(defun status-of (session)
  (words-of session :status-words))

(define-test the-status-bar-says-what-was-done-and-the-bottom-panel-the-problems ()
  (multiple-value-bind (screen session) (open-studio)
    (assert-equal "READY ■ NOT SAVED ■ 0 WIRES ■ 0 PROBLEMS ■ UNDO 0" (status-of session))
    (assert-equal "オールマイティーになれ" (name-of session :motto))
    (m:click session :id :add-button :within :left)
    (assert-true (search "■ UNDO 1" (status-of session)))
    (unwind-protect
         (progn
           ;; The view live; the code that draws it (MAKE-VIEWED, compiled) wires two of its
           ;; widgets.
           (emacs:point-in-view "tiny-view" "almighty-bolt-studio/t/studio" "plus")
           (dotimes (i 3) (draw screen))
           (let ((status (status-of session)))
             ;; What was last done, first: the view opened live.
             (assert-eql 0 (search "LIVE: TINY-VIEW CHANGES IN THE RUNNING APP" status) status)
             (assert-true (search "SAVED STUDIO.LISP ■ 2 WIRES ■ 0 PROBLEMS" status) status))
           (m:click session :id :problems-pane)
           (assert-true (shown-p session :no-problems))
           ;; The count's label taken out of the view: its wiring no longer fits.
           (m:click session :id :count :within :canvas)
           (m:click session :id :remove :within :toolbar)
           (dotimes (i 3) (draw screen))
           (let ((status (status-of session)))
             (assert-true (search "1 WIRE ■ 1 PROBLEM ■" status) status))
           (assert-true (search ":count" (name-of session (list :problem 0)) :test #'char-equal))
           (assert-equal "Problems · 1" (name-of session :problems-pane))
           ;; The bottom panel in every mode.
           (to-mode session "DEBUG")
           (assert-true (shown-p session (list :problem 0))))
      (emacs-gone)
      (views:set-view 'tiny-view *tiny-view*))))

;;; --- the REPL ---

(defun repl-says (session)
  "What the REPL's lines say, one string each."
  (mapcar #'s:node-name (s:node-children (m:find-node session :id :repl-lines))))

(defun enter (session text)
  (m:type-text session text :id :repl-input)
  (m:press-key session :return))

(define-test the-repl-evaluates-in-the-apps-package-and-shows-what-it-printed-and-returned ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    (assert-equal "cl-user>" (name-of session :repl-prompt))
    (enter session "(+ 1 2)")
    (assert-equal '("cl-user> (+ 1 2)" "3") (repl-says session))
    (assert-equal "" (s:node-value (m:find-node session :id :repl-input)))
    ;; In the app's package, once it is run.
    (run-in-studio session 'make-viewed)
    (assert-equal "studio>" (name-of session :repl-prompt))
    (enter session "(package-name *package*)")
    (assert-equal '("studio> (package-name *package*)" "\"ALMIGHTY-BOLT-STUDIO/T/STUDIO\"")
                  (last (repl-says session) 2))
    ;; What was printed, then each value; * is the last value.
    (enter session "(progn (princ \"hi\") (values 21 2))")
    (assert-equal (list "studio> (progn (princ \"hi\") (values 21 2))" "hi" (format nil "21~%2"))
                  (last (repl-says session) 3))
    (enter session "(* * 2)")
    (assert-equal "42" (car (last (repl-says session))))
    ;; An error, and a form not finished, are said, and the REPL goes on.
    (enter session "(error \"Boom ~d\" 3)")
    (assert-equal "Error: Boom 3" (car (last (repl-says session))))
    (enter session "(+ 1")
    (assert-eql 0 (search "Error: " (car (last (repl-says session)))))
    (enter session "(+ 2 2)")
    (assert-equal "4" (car (last (repl-says session))))
    ;; More than the panel shows: it keeps to the last.
    (dotimes (i 12) (enter session (format nil "(+ ~d 1)" i)))
    (let* ((node (m:find-node session :id :repl-scroll))
           (shown (fourth (s:node-bounds node))))
      (assert-true (> (s:node-property node :content-height) shown))
      (assert-eql (- (s:node-property node :content-height) shown)
                  (s:node-property node :offset)))))

(define-test up-and-down-in-the-repl-go-through-what-was-typed ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    (flet ((field () (s:node-value (m:find-node session :id :repl-input))))
      (enter session "(+ 1 1)")
      (enter session "(+ 2 2)")
      (m:type-text session "(list" :id :repl-input)
      (m:press-key session :up)
      (assert-equal "(+ 2 2)" (field))
      (m:press-key session :up)
      (assert-equal "(+ 1 1)" (field))
      (m:press-key session :up)
      (assert-equal "(+ 1 1)" (field))
      (m:press-key session :down)
      (assert-equal "(+ 2 2)" (field))
      (m:press-key session :down)
      (assert-equal "" (field))
      ;; One taken back and entered again is evaluated again.
      (m:press-key session :up)
      (m:press-key session :return)
      (assert-equal "4" (car (last (repl-says session)))))))

;;; --- Wire: the views, and the state of the app on the canvas ---

(defun make-studio-again (screen)
  "Make the studio again with what it kept, as the host does when code is compiled."
  (let ((new (core:with-kept-state ((screen-kept screen))
               (app:make-app-instance 'studio:make-studio))))
    (core:inherit-state new (screen-app screen))
    (setf (screen-app screen) new)
    (dotimes (i 3) (draw screen))))

(defun state-value (session name)
  "What Wire's State shows of the variable NAME of the app on the canvas."
  (s:node-value (m:find-node session :id :state-value :within (list :state-row name 1))))

(defun enter-state (session name text)
  "Type TEXT as the value of the variable NAME in Wire's State, and press Return."
  (m:set-value session (format nil "~a~%" text) :id :state-value :within (list :state-row name 1)))

(define-test wire-lists-the-views-with-their-wires-and-opens-the-one-pressed ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    (to-mode session "WIRE")
    ;; Every view the image has, each with how many of its widgets the code loaded wires: the
    ;; button and the label of MAKE-VIEWED's, none of the nested one's.
    (assert-equal "View tiny-view, 2 wires"
                  (s:node-name (m:find-node session :id (list :view-row 'tiny-view))))
    (assert-equal "View nested-view, 0 wires"
                  (s:node-name (m:find-node session :id (list :view-row 'nested-view))))
    (assert-false (s:node-property (m:find-node session :id (list :view-row 'tiny-view)) :selected))
    ;; Pressed, it is the view open in the designer, live, and chosen in the list.
    (m:click session :id (list :view-row 'tiny-view))
    (assert-true (search "TINY-VIEW" (words-of session :where)))
    (assert-true (s:node-property (m:find-node session :id (list :view-row 'tiny-view)) :selected))
    (assert-false (s:node-property (m:find-node session :id (list :view-row 'nested-view))
                                   :selected))))

(define-test wire-shows-the-state-of-the-app-live-and-a-value-entered-there-is-the-apps ()
  (multiple-value-bind (screen session) (open-studio)
    (flet ((count-shown ()
             (s:node-name (first (s:find-nodes (core:accessibility-tree studio:*running*)
                                               :id :count)))))
      (to-mode session "WIRE")
      (assert-true (search "No app is running"
                           (s:node-name (m:find-node session :id :state-note))))
      (run-in-studio session 'make-tiny)
      ;; Its one variable, live: pressed in the app, it shows the new count.
      (assert-equal "0" (state-value session 'count))
      (press-in-running screen :plus)
      (assert-equal "1" (state-value session 'count))
      ;; A value typed and entered is the app's, which shows it.
      (enter-state session 'count "41")
      (assert-equal "Count 41" (count-shown))
      (assert-equal "41" (state-value session 'count))
      ;; And the field follows the app again.
      (press-in-running screen :plus)
      (assert-equal "42" (state-value session 'count))
      ;; What does not read is said to be wrong, and the app keeps its value.
      (enter-state session 'count "(1")
      (assert-true (s:node-property (m:find-node session :id :state-value
                                                         :within (list :state-row 'count 1))
                                    :invalid))
      (assert-equal "Count 42" (count-shown))
      ;; Kept when the studio is made again, as when its code is compiled.
      (make-studio-again screen)
      (assert-equal "Count 42" (count-shown)))))

(defun define-pinnable (initial)
  "Define MAKE-PINNABLE again, as compiling an edit of it would, with INITIAL as the first value of
its two variables."
  (setf (fdefinition 'make-pinnable)
        (compile nil `(lambda ()
                        (bolt:state-let ((count ,initial) (other ,initial)
                                         (table (make-hash-table)))
                          (lambda ()
                            (bolt:label (format nil "~d ~d" count other) :id :count)))))))

(define-test a-pinned-variable-keeps-its-value-when-its-first-value-is-edited ()
  (define-pinnable 0)
  (multiple-value-bind (screen session) (open-studio)
    (flet ((shown ()
             (s:node-name (first (s:find-nodes (core:accessibility-tree studio:*running*)
                                               :id :count)))))
      (to-mode session "WIRE")
      (run-in-studio session 'make-pinnable)
      (enter-state session 'count "5")
      (enter-state session 'other "5")
      (assert-equal "5 5" (shown))
      ;; What does not print to be read back is shown, and not edited.
      (assert-false (eq :text-field
                        (s:node-role (m:find-node session :id :state-value
                                                          :within (list :state-row 'table 1)))))
      (assert-true (search "HASH-TABLE"
                           (s:node-name (m:find-node session :id :state-value
                                                             :within (list :state-row 'table 1)))))
      ;; COUNT pinned; the first values edited, and the app made again: COUNT keeps its value, and
      ;; OTHER starts again from its new one.
      (m:click session :id :state-pin :within (list :state-row 'count 1))
      (assert-true (s:node-property (m:find-node session :id :state-pin
                                                         :within (list :state-row 'count 1))
                                    :selected))
      (define-pinnable 10)
      (make-studio-again screen)
      (assert-equal "5 10" (shown))
      ;; Unpinned, it is kept while its first value is as it is now, and edited again, starts again.
      (m:click session :id :state-pin :within (list :state-row 'count 1))
      (make-studio-again screen)
      (assert-equal "5 10" (shown))
      (define-pinnable 20)
      (make-studio-again screen)
      (assert-equal "20 20" (shown)))))
;;; --- Wire: the view's file and the code's ---

(defun line-of (text start &key (from 0))
  "The number (from 1) of the first line of TEXT, from the line numbered FROM on, that begins, after
its spaces, with START."
  (loop :for line :in (uiop:split-string text :separator '(#\Newline))
        :for number :from 1
        :when (and (>= number from)
                   (eql 0 (search start (string-left-trim '(#\Space) line))))
          :return number))

(defun caret-to-line (session id line)
  "Put the caret of the code view ID on LINE, as a person does: press the view, then Up or Down."
  (m:click session :id id)
  ;; A long line goes on under no number, and Up and Down go a line as it shows.
  (flet ((now ()
           (let ((node (m:find-node session :id id)))
             (1+ (count #\Newline (s:node-value node) :end (s:node-property node :caret))))))
    (loop :repeat 200
          :until (= (now) line)
          :do (m:press-key session (if (> line (now)) :down :up)))))

(defun marks-of (session id)
  (s:node-property (m:find-node session :id id) :marks))

(define-test wire-shows-the-views-file-and-the-codes-in-step-with-the-selection ()
  (multiple-value-bind (screen session) (open-studio)
    (declare (ignore screen))
    (to-mode session "WIRE")
    (m:click session :id (list :view-row 'tiny-view))
    (run-in-studio session 'make-viewed)
    (m:click session :id :view-code-pane)
    (let* ((view-text (s:node-value (m:find-node session :id :view-code-text)))
           (code-text (s:node-value (m:find-node session :id :code-text)))
           (view-start (line-of code-text "(bolt:view tiny-view")))
      ;; The view as its file is written, and the code that wires it, this file, from where it is.
      (assert-true (search "(almighty-bolt:define-view tiny-view" view-text))
      (assert-equal "DEFINED IN THE IMAGE WRITTEN BY THE DESIGNER, READABLE BY YOU"
                    (format nil "~{~a~^ ~}"
                            (loop :for i :from 0 :below 2
                                  :collect (s:node-name (m:find-node session :id (list :words i)
                                                                             :within :view-code-foot)))))
      ;; The form of this file that wires it, numbered from its first line, which the header says.
      (assert-true (eql 0 (search "(defun make-viewed ()" code-text)))
      (assert-true (search code-text (uiop:read-file-string (views:wiring-file 'tiny-view))))
      (assert-equal (format nil "studio.lisp:~d"
                            (line-of (uiop:read-file-string (views:wiring-file 'tiny-view))
                                     "(defun make-viewed ()"))
                    (s:node-name (m:find-node session :id :count :within '(:code-heading :header))))
      (assert-true view-start)
      ;; The caret put on a widget's line in the view selects it, and its line is marked there and
      ;; in the code.
      (caret-to-line session :view-code-text (line-of view-text "(:button :plus"))
      (assert-equal "Button :plus" (s:node-name (m:find-node session :id :selected-what)))
      (assert-equal (list (list (line-of view-text "(:button :plus") ":plus, selected"))
                    (marks-of session :view-code-text))
      (assert-equal (list (list (line-of code-text "(button :plus" :from view-start)
                                ":plus, selected"))
                    (marks-of session :code-text))
      ;; And on a clause in the code, the same.
      (caret-to-line session :code-text (line-of code-text "(label :count" :from view-start))
      (assert-equal "Label :count" (s:node-name (m:find-node session :id :selected-what)))
      (assert-equal (list (list (line-of view-text "(:label :count") ":count, selected"))
                    (marks-of session :view-code-text)))))

;; A view that only the code in a file of the test's own wires.
(bolt:define-view file-view
  (:column :file-page (:gap 8)
   (:label :words (:text "Words"))))

(define-test wire-reads-the-codes-file-again-when-it-is-written ()
  (uiop:with-temporary-file (:pathname file :type "lisp" :keep nil)
    (flet ((write-code (lines-before words)
             (with-open-file (out file :direction :output :if-exists :supersede)
               (format out "(in-package #:almighty-bolt-studio/t/studio)~%")
               (dotimes (i lines-before) (terpri out))
               ;; Another view's code first, which is not the one to show.
               (format out "~%(defun make-other ()~%  (lambda () (bolt:view nested-view)))~%")
               (format out "~%(defun make-from-file ()~%  (lambda ()~%    (bolt:view file-view~%      ~
                            (label :words~%       :text ~s))))~%~%;; After it.~%(defvar *after* 1)~%"
                       words))))
      (write-code 0 "From a file")
      (load file)
      (multiple-value-bind (screen session) (open-studio)
        (declare (ignore screen))
        (to-mode session "WIRE")
        (m:click session :id (list :view-row 'file-view))
        (m:click session :id :view-code-pane)
        (flet ((code () (s:node-value (m:find-node session :id :code-text))))
          ;; The form that wires it, alone, from its first line.
          (assert-equal (format nil "(defun make-from-file ()~%  (lambda ()~%    (bolt:view file-view~%      ~
                                     (label :words~%       :text \"From a file\"))))~%")
                        (code))
          (assert-equal (format nil "~a:6" (file-namestring file))
                        (s:node-name (m:find-node session :id :count :within '(:code-heading :header))))
          (caret-to-line session :view-code-text
                         (line-of (s:node-value (m:find-node session :id :view-code-text))
                                  "(:label :words"))
          (assert-equal (list (list (line-of (code) "(label :words") ":words, selected"))
                        (marks-of session :code-text))
          ;; Written again (saved in Emacs, say), a second later: shown as it is now, the clause
          ;; marked where it is now.
          (sleep 1.1)
          (write-code 3 "Written again")
          (m:press-key session :escape)
          (assert-true (search "Written again" (code)))
          (assert-equal (format nil "~a:9" (file-namestring file))
                        (s:node-name (m:find-node session :id :count :within '(:code-heading :header))))
          (assert-equal (list (list (line-of (code) "(label :words") ":words, selected"))
                        (marks-of session :code-text)))))))
