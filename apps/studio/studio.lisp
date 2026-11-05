(defpackage #:almighty-bolt-studio
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:gui-designer #:almighty-bolt-gui-designer)
                    (#:theme-designer #:almighty-bolt-theme-designer)
                    (#:emacs #:almighty-bolt/emacs))
  (:import-from #:almighty-bolt-studio/theme
                #:studio-theme)
  (:export #:make-studio
           #:open-studio
           #:*running*))

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
    (:split :horizontal 340 (:tabs :canvas-pane :canvas-pane :preview-pane :run-pane)
     (:tabs :options-pane :options-pane :theme-pane :inspect-pane)
     :second))
  "How the panes are first arranged: the widgets on the left, the canvas (and the theme's preview)
in the middle, the selected widget's options (and the theme's settings) on the right, which keep
their widths when the window is made larger or smaller, the middle taking what changes; the app
running behind the canvas's tab, and the inspector of it behind the options'.")

(defparameter +zooms+ '(("50%" 1/2) ("100%" 1) ("200%" 2))
  "The zooms the app is shown at.")

(defvar *running* nil
  "The app running in the studio's App pane, as the last frame drew it, or NIL: for a tool that
looks at it, and a test.")

(defun named-constructor (name)
  "The function that NAME (\"package:function\", as typed) names, if it is one, or NIL."
  (let* ((colon (position #\: name))
         (package (and colon (find-package (string-upcase (subseq name 0 colon)))))
         (symbol (and package
                      (find-symbol (string-upcase (string-left-trim ":" (subseq name colon)))
                                   package))))
    (and symbol (fboundp symbol) symbol)))

(defun make-studio ()
  (multiple-value-bind (toolbar widgets canvas options view-keys open-view)
      (gui-designer:designer-parts)
    (multiple-value-bind (settings preview theme-keys) (theme-designer:designer-parts)
      (bolt:state-let ((layout +layout+)
                       ;; The app to run, by the name of the function that makes it, and its zoom.
                       (app-name "")
                       (zoom 1)
                       ;; The app's own size, as its window's would be, which the person drags or
                       ;; types: it is laid out at it whatever the pane is.
                       (app-w 480)
                       (app-h 360)
                       (app-drafts (make-hash-table))
                       ;; True for the frame after Edit in the View: the canvas and the options
                       ;; are brought to the front.
                       (to-canvas nil))
        (lambda ()
          ;; Point in Emacs on a view's widget: that view, live, with it selected.
          (multiple-value-bind (name id) (emacs:take-selection)
            (when name
              (funcall open-view name id)
              (setf to-canvas t)))
          ;; The studio in its own theme, light or dark as the system is; what it designs, in the
          ;; window's own (see the canvas).
          (bolt:with-theme ((studio-theme))
            (bolt:column (:id :studio :fill t :cross-align :stretch)
              (funcall toolbar)
              ;; Each part in a container of its own, so that ids of the two designers do not meet;
              ;; a stack, which gives what is in it the room it has (a column would give a child
              ;; with no flex as much height as it likes, and a part's own flex would then have
              ;; nothing to share). A designer's keys come after its part, so that a text field with
              ;; the keyboard undoes its own typing first.
              (setf layout
                    (bolt:dock (:id :panes :layout layout :flex 1 :min 220)
                      (:widgets-pane "Widgets"
                       (bolt:stack (:id :widgets-part :flex 1) (funcall widgets))
                       (funcall view-keys))
                      ;; The view as the app would draw it: in the window's theme, not the studio's.
                      (:canvas-pane "Canvas"
                       (bolt:with-theme ((bolt:appearance))
                         (bolt:stack (:id :canvas-part :flex 1) (funcall canvas)))
                       (funcall view-keys))
                      (:options-pane "Options"
                       (bolt:stack (:id :options-part :flex 1) (funcall options))
                       (funcall view-keys))
                      (:theme-pane "Theme"
                       (bolt:stack (:id :theme-part :flex 1) (funcall settings))
                       (funcall theme-keys))
                      (:preview-pane "Preview"
                       (bolt:stack (:id :preview-part :flex 1) (funcall preview))
                       (funcall theme-keys))
                      ;; The app being made, running, at a zoom.
                      (:run-pane "App"
                       (let ((constructor (named-constructor app-name)))
                         (bolt:column (:id :run-part :flex 1 :fill t :cross-align :stretch :gap 8)
                           (bolt:row (:id :run-bar :gap 8 :cross-align :center)
                             (bolt:label "App" :id :run-label)
                             (multiple-value-bind (text edited)
                                 (bolt:text-field app-name :id :app-name :width 280
                                                           :labelled-by :run-label
                                                           :invalid (and (plusp (length app-name))
                                                                         (null constructor)))
                               (when edited (setf app-name text)))
                             (multiple-value-bind (index changed)
                                 (bolt:choice (mapcar #'first +zooms+)
                                              (or (position zoom +zooms+ :key #'second) 1)
                                              :id :zoom :style :segmented)
                               (when changed (setf zoom (second (nth index +zooms+))))))
                           (if constructor
                               (multiple-value-bind (w h)
                                   (gui-designer:sized-stage
                                    :run-stage app-w app-h
                                    (lambda ()
                                      (setf *running* (bolt:app-pane constructor :id :running
                                                                                 :zoom zoom :flex 1)))
                                    app-drafts :scale zoom)
                                 (setf app-w w app-h h))
                               (progn
                                 (setf *running* nil)
                                 (bolt:paragraph "The name of the function that makes the app, as open-app takes it: my-app:make-app."
                                                 :id :run-hint :color :dim-text))))))
                      ;; The inspector of the app running.
                      (:inspect-pane "Inspector"
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
                                                           (getf facts :id))))))))))
              ;; The view to edit in front, and the app it is in beside it, where its changes show:
              ;; behind the canvas's tab, it would not be drawn.
              (when to-canvas
                (setf to-canvas nil
                      layout (bolt:show-pane (bolt:show-pane layout :canvas-pane) :options-pane))
                (when (member :run-pane (bolt:pane-group layout :canvas-pane))
                  (setf layout (bolt:place-pane layout :run-pane :canvas-pane :right 340)))))))))))

(defun open-studio ()
  "Open the studio in a window, from the REPL."
  (bolt:open-app 'make-studio :title "almighty-bolt studio" :width 1240 :height 820))
