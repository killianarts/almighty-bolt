(defpackage #:almighty-bolt-studio
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:gui-designer #:almighty-bolt-gui-designer)
                    (#:theme-designer #:almighty-bolt-theme-designer)
                    (#:emacs #:almighty-bolt/emacs)
                    (#:studio-theme #:almighty-bolt-studio/theme)
                    (#:title-bar #:almighty-bolt-studio/title-bar)
                    (#:repl #:almighty-bolt-studio/repl)
                    (#:wire #:almighty-bolt-studio/wire)
                    (#:chrome #:almighty-bolt-studio/chrome))
  (:export #:make-studio
           #:open-studio
           #:*running*
           #:*open-window*))

(in-package #:almighty-bolt-studio)

;;; The studio of the roadmap, begun: the parts of the GUI designer and of the theme designer in one
;;; window, as panes of a dock, which the person arranges. Each designer is made once, its parts
;;; sharing what it holds (the view, the theme being made, the undo of each), which is kept while a
;;; part is hidden, when a pane is moved, and when the code is compiled again. Each designer takes
;;; Command-Z, in each of its panes; the dock gives a shortcut to the pane last pressed in or that
;;; has the keyboard, so it undoes in the designer last used.
;;;
;;; With Emacs connected (emacs/almighty-bolt.el, over Sly), the studio and a buffer are kept in step
;;; through the image: Edit Source in the inspector takes Emacs to a picked widget's clause in its
;;; view, or to the code that drew it, and point on a clause in Emacs selects its widget here.

(defparameter +layout+
  '(:split :horizontal 300 (:tabs :widgets-pane :widgets-pane)
    (:split :horizontal 340 (:tabs :canvas-pane :canvas-pane :preview-pane)
     (:tabs :options-pane :options-pane :theme-pane :inspect-pane)
     :second))
  "How the panes are first arranged: the widgets on the left, the canvas (and the theme's preview)
in the middle, the selected widget's options (and the theme's settings) on the right, which keep
their widths when the window is made larger or smaller, the middle taking what changes; the
inspector of the app running behind the options' tab.")

(defparameter +zooms+ '(("50%" 1/2) ("100%" 1) ("200%" 2))
  "The zooms the app is shown at.")

(defvar *running* nil
  "The app running on the studio's canvas, as the last frame drew it, or NIL: for a tool that looks
at it, and a test.")

(defun named-constructor (name)
  "The function that NAME (\"package:function\", as typed) names, if it is one, or NIL."
  (let* ((colon (position #\: name))
         (package (and colon (find-package (string-upcase (subseq name 0 colon)))))
         (symbol (and package
                      (find-symbol (string-upcase (string-left-trim ":" (subseq name colon)))
                                   package))))
    (and symbol (fboundp symbol) symbol)))

(defparameter +mode-layouts+
  `(:design ,+layout+
    :wire (:split :horizontal 300 (:tabs :views-pane :views-pane)
           (:split :horizontal 340
            (:split :horizontal 420 (:tabs :view-code-pane :view-code-pane :canvas-pane)
             (:tabs :code-pane :code-pane))
            (:tabs :options-pane :options-pane) :second))
    :debug (:split :horizontal 340 (:tabs :canvas-pane :canvas-pane)
                   (:tabs :inspect-pane :inspect-pane) :second)
    :theme (:split :horizontal 340 (:tabs :preview-pane :preview-pane :canvas-pane)
                   (:tabs :theme-pane :theme-pane) :second)
    :agent (:tabs :agent-pane :agent-pane))
  "How each mode's panes are first arranged. Design has every pane; Wire has the views and the
state of the app on the canvas; the view's code (the canvas behind it) and the code that wires it;
and the options (with the wiring), until its own panes are built; Debug the canvas and the inspector
of the app on it, and Theme the theme's preview and settings and the canvas.")

(defvar *open-window* 'bolt:open-window
  "What Run Window calls, as OPEN-WINDOW is called: a test gives one that notes the call.")

(defun milliseconds (ns)
  (/ ns 1000000.0))

(defparameter +frame-budget-ns+ 16666667
  "A frame's time at 60 a second: the chart's height is this, or the longest frame if longer.")

(defun frames-chart (frames)
  "The times of FRAMES (APP-PANE-FRAMES's: each a build, a layout and a paint, in nanoseconds), each
a bar of three parts, the latest on the right, and the last in words."
  (if (null frames)
      (bolt:paragraph "No frames yet: run an app on the canvas." :id :frames-summary
                                                                 :color :dim-text)
      (let ((top (max +frame-budget-ns+ (reduce #'max frames :key (lambda (f) (reduce #'+ f))))))
        (bolt:canvas 360 64
                     (lambda (list context width height)
                       (declare (ignore context))
                       (let ((colors (list (bolt:theme-color :accent) (bolt:theme-color :ring)
                                           (bolt:theme-color :dim-text)))
                             (step (/ width 120)))
                         (loop :for frame :in frames
                               :for x :from (- width (* step (length frames))) :by step
                               :do (let ((y height))
                                     (loop :for ns :in frame
                                           :for color :in colors
                                           :do (let ((h (* height (/ ns top))))
                                                 (decf y h)
                                                 (bolt:push-rounded-rect list x y (max 1 (- step 1))
                                                                         h 0 color)))))))
                     :id :frames-chart)
        (destructuring-bind (build layout paint) (car (last frames))
          (bolt:label (format nil "~d frame~:p · last: build ~,2f ms, layout ~,2f ms, paint ~,2f ms"
                              (length frames) (milliseconds build) (milliseconds layout)
                              (milliseconds paint))
                      :id :frames-summary :font :code)))))

(defun make-studio ()
  (multiple-value-bind (toolbar widgets canvas options view-keys open-view about save select)
      (gui-designer:designer-parts)
    (multiple-value-bind (settings preview theme-keys) (theme-designer:designer-parts)
      (let ((repl (repl:make-repl)))
        (bolt:state-let ((mode :design)
                         ;; Each mode's arrangement of its panes, kept while another mode is shown.
                         (layouts (copy-list +mode-layouts+))
                         ;; The app to run on the canvas, by the name of the function that makes
                         ;; it, its zoom, and whether a press there picks a widget (:design) or is
                         ;; the app's (:run).
                         (app-name "")
                         (zoom 1)
                         (canvas-mode :design)
                         ;; Whether the app runs on the canvas (:live) or the view is drawn alone
                         ;; (:sample), and the appearance it is drawn in (NIL: the window's).
                         (data :live)
                         (look nil)
                         ;; True for the frame after Edit in the View: the canvas and the options
                         ;; are brought to the front.
                         (to-canvas nil)
                         ;; What is typed in Wire's State for each variable, until Return enters it.
                         (state-drafts (make-hash-table :test 'equal))
                         ;; What Wire's code panes keep: where each one's caret was, and the code's
                         ;; file as last read.
                         (code-memory (make-hash-table :test 'equal))
                         ;; The bottom panel: how tall it is, and how its panes are arranged.
                         (bottom-size 200)
                         (bottom-layout nil))
          (lambda ()
            ;; Point in Emacs on a view's widget: that view, live, with it selected.
            (multiple-value-bind (name id) (emacs:take-selection)
              (when name
                (funcall open-view name id)
                (setf to-canvas t)))
            (let ((constructor (named-constructor app-name))
                  (facts (funcall about))
                  ;; The designer's headings as the studio's sections.
                  (gui-designer:*heading* 'chrome:section-header))
              (labels ((widgets-pane ()
                         ;; The catalogue and the outline, which scroll when the pane is short.
                         (bolt:scroll (:id :widgets-scroll :flex 1)
                           (bolt:stack (:id :widgets-part) (funcall widgets)))
                         (funcall view-keys))
                       ;; The app being made, running on the canvas with its own data, or the view
                       ;; drawn alone (its sample data); in the theme the bar picks, or the window's,
                       ;; not the studio's.
                       (canvas-pane ()
                         (bolt:column (:id :canvas-column :flex 1 :fill t :cross-align :stretch)
                           (canvas-bar)
                           (bolt:with-theme ((or look (bolt:appearance)))
                             (bolt:stack (:id :canvas-part :flex 1)
                               (setf *running*
                                     (funcall canvas :app (and (eq data :live) constructor)
                                                     :zoom zoom :mode canvas-mode :appearance look
                                                     :title (if (and (eq data :live) constructor)
                                                                app-name
                                                                (getf facts :name)))))))
                         (funcall view-keys))
                       ;; Over the canvas: the app to run on it and whether it runs (Live) or the
                       ;; view is drawn alone (Sample); whether a press picks a widget (Design,
                       ;; where Command-press tries it) or is the app's (Run), the zoom, and the
                       ;; theme it is drawn in.
                       (canvas-bar ()
                         (flet ((segments (id labels values value)
                                  (multiple-value-bind (index changed)
                                      (bolt:choice labels (or (position value values) 0)
                                                   :id id :style :segmented)
                                    (if changed (nth index values) value))))
                           (bolt:panel (:id :canvas-bar :all 8 :radius 0)
                             (bolt:column (:id :canvas-bar-rows :gap 6 :cross-align :stretch)
                               (bolt:row (:id :canvas-bar-row :gap 8 :cross-align :center :fill t)
                                 (bolt:label "App" :id :run-label)
                                 ;; The app's name takes what room is left.
                                 (multiple-value-bind (text edited)
                                     (bolt:text-field app-name :id :app-name :flex 1 :width 120
                                                               :labelled-by :run-label
                                                               :invalid (and (plusp (length app-name))
                                                                             (null constructor)))
                                   (when edited (setf app-name text)))
                                 (setf data (segments :canvas-data '("Live" "Sample")
                                                      '(:live :sample) data)))
                               (bolt:row (:id :canvas-bar-row-2 :gap 8 :cross-align :center)
                                 (setf canvas-mode (segments :canvas-mode '("Design" "Run")
                                                             '(:design :run) canvas-mode))
                                 (setf zoom (segments :zoom (mapcar #'first +zooms+)
                                                      (mapcar #'second +zooms+) zoom))
                                 (setf look (segments :canvas-theme '("System" "Light" "Dark")
                                                      '(nil :light :dark) look)))))))
                       ;; Wire's left: the views, each with how many of its widgets are wired, to
                       ;; open one; and the state of the app on the canvas, live, to edit and pin.
                       (views-pane ()
                         (bolt:scroll (:id :views-scroll :flex 1)
                           (bolt:padding (:id :views-part :all 12)
                             (bolt:column (:id :views-column :gap 16 :fill t :cross-align :stretch)
                               (let ((name (wire:views-list (getf facts :views) (getf facts :live))))
                                 (when name
                                   (funcall open-view name)))
                               (wire:state-list *running* state-drafts)))))
                       ;; The view as its file is written, and the code that wires it from its
                       ;; file; a press on a widget's line selects it.
                       (view-code-pane ()
                         (bolt:padding (:id :view-code-part :all 12 :flex 1)
                           (let ((id (wire:view-code facts code-memory)))
                             (when id (funcall select id))))
                         (funcall view-keys))
                       (code-pane ()
                         (bolt:padding (:id :code-part :all 12 :flex 1)
                           (let ((id (wire:wiring-code facts code-memory)))
                             (when id (funcall select id))))
                         (funcall view-keys))
                       (options-pane ()
                         (bolt:stack (:id :options-part :flex 1) (funcall options))
                         (funcall view-keys))
                       (theme-pane ()
                         (bolt:stack (:id :theme-part :flex 1) (funcall settings))
                         (funcall theme-keys))
                       (preview-pane ()
                         (bolt:stack (:id :preview-part :flex 1) (funcall preview))
                         (funcall theme-keys))
                       ;; The inspector of the app running.
                       (inspect-pane ()
                         (bolt:inspector-pane *running* :id :inspecting
                                              ;; A widget a view drew, opened in the designer, live.
                                              :edit-view (lambda (name widget)
                                                           (funcall open-view name widget)
                                                           (setf to-canvas t))
                                              ;; In Emacs: the view's clause, the wiring, or the
                                              ;; app's code (its constructor) for a widget no view
                                              ;; drew.
                                              :edit-source
                                              (lambda (what facts)
                                                (nth-value
                                                 1 (ecase what
                                                     (:view (emacs:show-view (getf facts :view)
                                                                             (getf facts :view-widget)))
                                                     (:wiring (emacs:show-wiring
                                                               (getf facts :view)
                                                               (getf facts :view-widget)))
                                                     (:code (emacs:show-code
                                                             (named-constructor app-name)
                                                             (getf facts :id))))))))
                       (agent-pane ()
                         (bolt:padding (:id :agent-part :all 24)
                           (bolt:paragraph "The agent mode is not built yet: a change asked for in words, made through the machine interface, and proposed as a change to the view and the code."
                                           :id :agent-note :color :dim-text)))
                       ;; The mode's panes, in its own dock, whose arrangement is kept.
                       (mode-panes ()
                         (setf (getf layouts mode)
                               (ecase mode
                                 (:design
                                  (bolt:dock (:id :panes :layout (getf layouts :design) :flex 1 :min 220)
                                    (:widgets-pane "Widgets" (widgets-pane))
                                    (:canvas-pane "Canvas" (canvas-pane))
                                    (:options-pane "Options" (options-pane))
                                    (:theme-pane "Theme" (theme-pane))
                                    (:preview-pane "Preview" (preview-pane))
                                    (:inspect-pane "Inspector" (inspect-pane))))
                                 (:wire
                                  (bolt:dock (:id :wire-panes :layout (getf layouts :wire) :flex 1
                                              :min 220)
                                    (:views-pane "Views" (views-pane))
                                    (:view-code-pane "View" (view-code-pane))
                                    (:code-pane "Code" (code-pane))
                                    (:canvas-pane "Canvas" (canvas-pane))
                                    (:options-pane "Options" (options-pane))))
                                 (:debug
                                  (bolt:dock (:id :debug-panes :layout (getf layouts :debug) :flex 1
                                              :min 220)
                                    (:canvas-pane "Canvas" (canvas-pane))
                                    (:inspect-pane "Inspector" (inspect-pane))))
                                 (:theme
                                  (bolt:dock (:id :theme-panes :layout (getf layouts :theme) :flex 1
                                              :min 220)
                                    (:preview-pane "Preview" (preview-pane))
                                    (:theme-pane "Theme" (theme-pane))
                                    (:canvas-pane "Canvas" (canvas-pane))))
                                 (:agent
                                  (bolt:dock (:id :agent-panes :layout (getf layouts :agent) :flex 1)
                                    (:agent-pane "Agent" (agent-pane)))))))
                       ;; Under the panes: the REPL, the problems, the frames, the events and the
                       ;; agent, in a dock of their own, in every mode.
                       (bottom-panel (facts)
                         (let ((problems (getf facts :problems)))
                           (setf bottom-layout
                                 (bolt:dock (:id :bottom-panes :layout bottom-layout :flex 1)
                                   (:repl-pane "REPL" (repl-pane))
                                   (:problems-pane (format nil "Problems · ~d" (length problems))
                                                   (bolt:padding (:id :problems-part :all 12)
                                                     (bolt:column (:gap 8 :fill t :cross-align :stretch)
                                                       (chrome:section-header :problems-heading "Problems"
                                                                              (length problems))
                                                       (if problems
                                                           (loop :for problem :in problems
                                                                 :for i :from 0
                                                                 :do (bolt:paragraph problem :id (list :problem i)
                                                                                             :color :field-invalid))
                                                           (bolt:paragraph "None: the view and the code that wires it fit, as far as the code loaded says."
                                                                           :id :no-problems :color :dim-text)))))
                                   (:frames-pane "Frames"
                                                 (bolt:padding (:id :frames-part :all 12)
                                                   (bolt:column (:gap 8 :fill t :cross-align :stretch)
                                                     (chrome:section-header :frames-heading "Frames")
                                                     (frames-chart (bolt:app-pane-frames *running*)))))
                                   (:events-pane "Events"
                                                 (bolt:padding (:id :events-part :all 12)
                                                   (bolt:column (:gap 8 :fill t :cross-align :stretch)
                                                     (chrome:section-header :events-heading "Events")
                                                     (bolt:paragraph "The events are not recorded here yet: they come with the timeline, in Debug."
                                                                     :id :events-note :color :dim-text))))
                                   (:agent-log-pane "Agent"
                                                    (bolt:padding (:id :agent-log-part :all 12)
                                                      (bolt:column (:gap 8 :fill t :cross-align :stretch)
                                                        (chrome:section-header :agent-heading "Agent")
                                                        (bolt:paragraph "The agent is not built yet."
                                                                        :id :agent-log-note :color :dim-text))))))))
                       ;; In the package of the app being run, or else the view's.
                       (repl-pane ()
                         (bolt:panel (:id :repl-part :all 12 :radius 0 :color :background :flex 1)
                           (funcall repl (let ((colon (position #\: app-name)))
                                           (if (and colon constructor)
                                               (subseq app-name 0 colon)
                                               (getf facts :package))))))
                       ;; What the status bar says: what was last done, where the view is saved, how
                       ;; many of its widgets are wired, the problems, and the steps Undo can take.
                       (status-parts (facts)
                         (let ((status (getf facts :status))
                               (file (getf facts :file)))
                           (list (if (plusp (length status)) status "Ready")
                                 (if file (format nil "Saved ~a" (file-namestring file)) "Not saved")
                                 (format nil "~d wire~:p" (getf facts :wires))
                                 (format nil "~d problem~:p" (length (getf facts :problems)))
                                 (format nil "Undo ~d" (getf facts :undo))))))

                ;; The studio in its own theme, light or dark as the system is; what it designs, in
                ;; the window's own (see the canvas).
                (bolt:with-theme ((studio-theme:studio-theme))
                  (bolt:column (:id :studio :fill t :cross-align :stretch)
                    (multiple-value-bind (now save-pressed run-pressed)
                        (title-bar:title-bar mode facts app-name (and constructor t))
                      (setf mode now)
                      (when save-pressed
                        (funcall save))
                      (when (and run-pressed constructor)
                        (funcall *open-window* constructor :title app-name
                                                           :width (getf facts :width)
                                                           :height (getf facts :height))))
                    ;; The designer's bar in Design, without a Save (the title bar has it); in every
                    ;; mode, what else it does: it takes up a view typed in Emacs, has the File menu,
                    ;; and draws the dialogs Save asks with.
                    (funcall toolbar :shown (eq mode :design) :save-button nil)
                    ;; Each part in a container of its own, so that ids of the two designers do not
                    ;; meet; a stack, which gives what is in it the room it has (a column would give
                    ;; a child with no flex as much height as it likes, and a part's own flex would
                    ;; then have nothing to share). A designer's keys come after its part, so that a
                    ;; text field with the keyboard undoes its own typing first.
                    ;; The rail, the mode's panes over the bottom panel, and the status bar.
                    (bolt:row (:id :studio-body :flex 1 :fill t :cross-align :stretch)
                      (chrome:rail mode facts)
                      (bolt:panel (:id :rail-line :color (studio-theme:tool-color :line) :radius 0 :width 1))
                      (setf bottom-size
                            (bolt:split (:id :bottom-split :direction :vertical :size bottom-size
                                         :keep :second :min 120 :flex 1)
                                        (mode-panes)
                                        (bottom-panel facts))))
                    (bolt:panel (:id :status-line :color (studio-theme:tool-color :line) :radius 0 :height 1))
                    (chrome:status-bar (status-parts facts))
                    ;; The view to edit in front, in Design, on the canvas with the app it is in.
                    (when to-canvas
                      (setf to-canvas nil
                            mode :design
                            (getf layouts :design) (bolt:show-pane (bolt:show-pane (getf layouts :design)
                                                                                   :canvas-pane)
                                                                   :options-pane)))))))))))))

(defun open-studio ()
  "Open the studio in a window, from the REPL."
  (bolt:open-app 'make-studio :title "almighty-bolt studio" :width 1240 :height 820))
