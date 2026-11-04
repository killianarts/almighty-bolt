(defpackage #:almighty-bolt-studio
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:gui-designer #:almighty-bolt-gui-designer)
                    (#:theme-designer #:almighty-bolt-theme-designer))
  (:export #:make-studio
           #:open-studio))

(in-package #:almighty-bolt-studio)

;;; The studio of the roadmap, begun: the GUI designer and the theme designer in one window, as panes
;;; of a dock, tabs of one group to begin with, which the person can put side by side. Each is the
;;; app it is in a window of its own, made once and drawn in its pane; what each holds (the view,
;;; the theme being made, their undo) is kept while the other is shown, when a pane is moved, and
;;; when the code is compiled again. Each takes Command-Z, and the dock gives it to the pane last
;;; pressed in or that has the keyboard.

(defun make-studio ()
  (let ((views (gui-designer:make-designer))
        (theme (theme-designer:make-designer)))
    (bolt:state-let ((layout nil))
      (lambda ()
        (bolt:column (:id :studio :fill t :cross-align :stretch)
          ;; Each in a container of its own id, so that the GUI designer's :heading and the theme
          ;; designer's are two things; a stack, which gives what is in it the room it has (a
          ;; column would give a child with no flex as much height as it likes, and the
          ;; designer's own flex would then have nothing to share).
          (setf layout
                (bolt:dock (:id :panes :layout layout :flex 1)
                  (:views-pane "Views"
                   (bolt:stack (:id :views-page :flex 1)
                     (funcall views)))
                  (:theme-pane "Theme"
                   (bolt:stack (:id :theme-page :flex 1)
                     (funcall theme))))))))))

(defun open-studio ()
  "Open the studio in a window, from the REPL."
  (bolt:open-app 'make-studio :title "almighty-bolt studio" :width 1240 :height 820))
