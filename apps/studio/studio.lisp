(defpackage #:almighty-bolt-studio
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:gui-designer #:almighty-bolt-gui-designer)
                    (#:theme-designer #:almighty-bolt-theme-designer))
  (:export #:make-studio
           #:open-studio))

(in-package #:almighty-bolt-studio)

;;; The studio of the roadmap, begun: the GUI designer and the theme designer in one window, a tab
;;; for each. Each is the app it is in a window of its own, made once and drawn when its tab is
;;; chosen, in a container of its own, so that their ids do not meet; what each holds (the view, the
;;; theme being made, their undo) is kept while the other is shown, and when the code is compiled
;;; again.

(defparameter +pages+ '("Views" "Theme")
  "The tabs of the studio, in order.")

(defun make-studio ()
  (let ((views (gui-designer:make-designer))
        (theme (theme-designer:make-designer)))
    (bolt:state-let ((page 0))
      (lambda ()
        (bolt:column (:id :studio :fill t :cross-align :stretch)
          (bolt:panel (:id :studio-bar :radius 0 :left 12 :right 12 :top 6)
            (multiple-value-bind (index changed) (bolt:tabs +pages+ page :id :pages)
              (when changed (setf page index))))
          ;; Each in a container of its own id, so that the GUI designer's :heading and the theme
          ;; designer's are two things; a stack, which gives what is in it the room it has (a
          ;; column would give a child with no flex as much height as it likes, and the
          ;; designer's own flex would then have nothing to share).
          (if (zerop page)
              (bolt:stack (:id :views-page :flex 1)
                (funcall views))
              (bolt:stack (:id :theme-page :flex 1)
                (funcall theme))))))))

(defun open-studio ()
  "Open the studio in a window, from the REPL."
  (bolt:open-app 'make-studio :title "almighty-bolt studio" :width 1240 :height 820))
