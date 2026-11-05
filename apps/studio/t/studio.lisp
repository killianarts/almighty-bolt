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
                    (#:views #:almighty-bolt/ui/views)))

(in-package #:almighty-bolt-studio/t/studio)

;;; The studio worked as a person works it, through the machine interface, in a context with no
;;; window: both designers in one window, each kept while the other is shown.

(defstruct (screen (:constructor %make-screen (app context list)))
  app context list)

(defun draw (screen)
  (core:settle-frame (screen-app screen) (screen-context screen) (screen-list screen)))

(defun open-studio ()
  "The studio and a machine session on it."
  (let* ((instance (core:with-kept-state ((core:make-kept-state))
                    (app:make-app-instance 'studio:make-studio)))
         (screen (%make-screen instance (fake:make-fake-context :width 1240f0 :height 3000f0)
                               (core:make-display-list :capacity 65536))))
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
    (flet ((check (appearance panel)
             (setf (core:context-appearance (screen-context screen)) appearance)
             (draw screen)
             ;; The tool bar at the top is the studio's panel: a step off white, or off black in
             ;; the dark.
             (assert-eql (bolt:parse-color panel) (color-at screen 1 1) appearance)
             ;; The button on the canvas is as the app would draw it, in the window's theme, and
             ;; not in the studio's black or white.
             (assert-eql (bolt:theme-setting (bolt:find-theme appearance) :button)
                         (middle-color screen session :button-1 :canvas)
                         appearance)))
      (check :light "#f2f2f2")
      (check :dark "#1e1e1e"))))

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

(defun pointer (screen kind x y)
  (core:handle-event (screen-app screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button 0
                                              :buttons (if (eq kind :up) 0 1)
                                              :click-count (if (eq kind :move) 0 1)))
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
      (m:click session :id :run-pane)
      (assert-true (shown-p session :run-hint))
      (m:set-value session "almighty-bolt-studio/t/studio:make-tiny" :id :app-name)
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
             (m:click session :id :run-pane)
             (m:set-value session "almighty-bolt-studio/t/studio:make-viewed" :id :app-name)
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
             (assert-true (shown-p session :plus :canvas))
             ;; The running app beside it, where what is edited shows.
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

(defun run-in-studio (session constructor)
  (m:click session :id :run-pane)
  (m:set-value session (format nil "almighty-bolt-studio/t/studio:~(~a~)" constructor)
               :id :app-name))

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
             ;; selected, the running app beside it.
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
             (assert-equal "Typed" (s:node-name (m:find-node session :id :plus :within :canvas)))
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
             (id (intern (string-upcase (subseq name (1+ (position #\: name)))) :keyword)))
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
      ;; The view at the canvas's 480, however wide the window is: its field does not change.
      (let ((wide (field-width)))
        (resize screen 900)
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
      ;; The app at its own size, as its window's would be, whatever the pane: and at 200%, the
      ;; same size in its own points, shown twice as large.
      (run-in-studio session 'make-tiny)
      (assert-eql 480f0 (app-width))
      (m:set-value session "200%" :id :zoom)
      (dotimes (i 3) (draw screen))
      (assert-eql 480f0 (app-width))
      (resize screen 900)
      (assert-eql 480f0 (app-width))
      (assert-equal "480" (size-set :run-stage)))))
