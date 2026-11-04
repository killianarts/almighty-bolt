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
