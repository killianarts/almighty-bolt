(defpackage #:almighty-bolt-studio/t/studio
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:studio #:almighty-bolt-studio)))

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

(define-test both-designers-are-in-one-window-each-kept-while-the-other-is-shown ()
  (multiple-value-bind (screen session) (open-studio)
    ;; The GUI designer first, with its File menu.
    (assert-true (shown-p session :canvas :views-page))
    (assert-false (shown-p session :theme-page))
    (assert-equal '("File") (menu-titles screen))
    ;; A widget added to the view.
    (m:click session :id :add-button :within :left)
    (assert-true (shown-p session :button-1 :canvas))
    ;; The theme designer, in place of it; the menu is the GUI designer's, and goes with it.
    (m:click session :id :theme-pane)
    (assert-true (shown-p session :preview-back :theme-page))
    (assert-false (shown-p session :views-page))
    (assert-equal '() (menu-titles screen))
    (m:set-value session "studio-test" :id :name :within :theme-page)
    (assert-true (search ":studio-test" (s:node-value (m:find-node session :id :source))))
    ;; Back and forth: each is as it was left.
    (m:click session :id :views-pane)
    (assert-true (shown-p session :button-1 :canvas))
    (m:click session :id :theme-pane)
    (assert-equal "studio-test" (s:node-value (m:find-node session :id :name :within :theme-page)))))

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

(define-test side-by-side-command-z-undoes-in-the-designer-last-used ()
  (multiple-value-bind (screen session) (open-studio)
    ;; The theme's tab dragged to the right edge: the two designers side by side.
    (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :theme-pane)
      (let ((x0 (+ x (/ w 2))) (y0 (+ y (/ h 2))))
        (pointer screen :move x0 y0)
        (pointer screen :down x0 y0)
        (pointer screen :move (+ x0 10) (+ y0 10))
        (pointer screen :move 1200 600)
        (pointer screen :move 1200 600)
        (pointer screen :up 1200 600)
        (dotimes (i 3) (draw screen))))
    (assert-true (shown-p session :canvas :views-page))
    (assert-true (shown-p session :preview-back :theme-page))
    ;; A change in each; the theme's last.
    (m:click session :id :add-button :within :left)
    (m:set-value session "studio-test" :id :name :within :theme-page)
    (assert-true (shown-p session :button-1 :canvas))
    ;; Command-Z goes to the theme designer, pressed in last: the view keeps its button.
    (press-keys screen #\z :command)
    (assert-equal "my-theme" (s:node-value (m:find-node session :id :name :within :theme-page)))
    (assert-true (shown-p session :button-1 :canvas))
    ;; Pressed in the GUI designer, Command-Z is its own.
    (m:click session :id :canvas)
    (press-keys screen #\z :command)
    (assert-false (shown-p session :button-1 :canvas))))
