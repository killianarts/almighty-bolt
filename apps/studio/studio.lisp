(defpackage #:almighty-bolt-studio
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:gui-designer #:almighty-bolt-gui-designer)
                    (#:theme-designer #:almighty-bolt-theme-designer))
  (:export #:make-studio
           #:open-studio))

(in-package #:almighty-bolt-studio)

;;; The studio of the roadmap, begun: the parts of the GUI designer and of the theme designer in one
;;; window, as panes of a dock, which the person arranges. Each designer is made once, its parts
;;; sharing what it holds (the view, the theme being made, the undo of each), which is kept while a
;;; part is hidden, when a pane is moved, and when the code is compiled again. Each designer takes
;;; Command-Z, in each of its panes; the dock gives a shortcut to the pane last pressed in or that
;;; has the keyboard, so it undoes in the designer last used.

(defparameter +layout+
  '(:split :horizontal 300 (:tabs :widgets-pane :widgets-pane)
    (:split :horizontal 620 (:tabs :canvas-pane :canvas-pane :preview-pane)
     (:tabs :options-pane :options-pane :theme-pane)))
  "How the panes are first arranged: the widgets on the left, the canvas (and the theme's preview)
in the middle, the selected widget's options (and the theme's settings) on the right.")

(defun make-studio ()
  (multiple-value-bind (toolbar widgets canvas options view-keys) (gui-designer:designer-parts)
    (multiple-value-bind (settings preview theme-keys) (theme-designer:designer-parts)
      (bolt:state-let ((layout +layout+))
        (lambda ()
          (bolt:column (:id :studio :fill t :cross-align :stretch)
            (funcall toolbar)
            ;; Each part in a container of its own, so that ids of the two designers do not meet;
            ;; a stack, which gives what is in it the room it has (a column would give a child with
            ;; no flex as much height as it likes, and a part's own flex would then have nothing
            ;; to share). A designer's keys come after its part, so that a text field with the
            ;; keyboard undoes its own typing first.
            (setf layout
                  (bolt:dock (:id :panes :layout layout :flex 1)
                    (:widgets-pane "Widgets"
                     (bolt:stack (:id :widgets-part :flex 1) (funcall widgets))
                     (funcall view-keys))
                    (:canvas-pane "Canvas"
                     (bolt:stack (:id :canvas-part :flex 1) (funcall canvas))
                     (funcall view-keys))
                    (:options-pane "Options"
                     (bolt:stack (:id :options-part :flex 1) (funcall options))
                     (funcall view-keys))
                    (:theme-pane "Theme"
                     (bolt:stack (:id :theme-part :flex 1) (funcall settings))
                     (funcall theme-keys))
                    (:preview-pane "Preview"
                     (bolt:stack (:id :preview-part :flex 1) (funcall preview))
                     (funcall theme-keys))))))))))

(defun open-studio ()
  "Open the studio in a window, from the REPL."
  (bolt:open-app 'make-studio :title "almighty-bolt studio" :width 1240 :height 820))
