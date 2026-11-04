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
    (m:set-value session "Theme" :id :pages)
    (assert-true (shown-p session :preview-back :theme-page))
    (assert-false (shown-p session :views-page))
    (assert-equal '() (menu-titles screen))
    (m:set-value session "studio-test" :id :name :within :theme-page)
    (assert-true (search ":studio-test" (s:node-value (m:find-node session :id :source))))
    ;; Back and forth: each is as it was left.
    (m:set-value session "Views" :id :pages)
    (assert-true (shown-p session :button-1 :canvas))
    (m:set-value session "Theme" :id :pages)
    (assert-equal "studio-test" (s:node-value (m:find-node session :id :name :within :theme-page)))))
