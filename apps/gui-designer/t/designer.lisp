(defpackage #:almighty-bolt-gui-designer/t/designer
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:designer #:almighty-bolt-gui-designer)))

(in-package #:almighty-bolt-gui-designer/t/designer)

;;; The GUI designer worked as a person works it, through the machine interface and the pointer, in
;;; a context with no window. The view it designs is drawn with the real widgets, so the machine
;;; interface finds them (inside the canvas); a press or typing on them is the designer's, not
;;; theirs.

(defstruct (screen (:constructor %make-screen (app context list)))
  app context list)

(defun draw (screen)
  (core:settle-frame (screen-app screen) (screen-context screen) (screen-list screen)))

(defun open-designer ()
  "The designer and a machine session on it."
  (let* ((instance (core:with-kept-state ((core:make-kept-state))
                    (app:make-app-instance 'designer:make-designer)))
         (screen (%make-screen instance (fake:make-fake-context :width 1100f0 :height 640f0)
                               (core:make-display-list :capacity 65536))))
    (draw screen)
    (values screen
            (m:make-session :app (lambda () (screen-app screen))
                            :deliver (lambda (event)
                                       (core:handle-event (screen-app screen) event)
                                       (draw screen))
                            :settle (lambda () (dotimes (i 4) (draw screen)))
                            :on-main #'funcall))))

(defun pointer (screen kind x y)
  "The pointer moving to X, Y, or its primary button going down or up there, and one frame: what the
event changed shows in it."
  (core:handle-event (screen-app screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button 0
                                              :buttons (if (eq kind :down) 1 0)
                                              :click-count (if (eq kind :move) 0 1)))
  (draw screen))

(defun color-at (screen x y)
  "The colour of the last rectangle drawn over the point X, Y."
  (let ((list (screen-list screen)) (color nil))
    (dotimes (index (core:display-list-count list) color)
      (let ((kind (core:display-list-kind list index)))
        (when (and (or (= kind core:+command-rect+) (= kind core:+command-rounded-rect+))
                   (<= (core:display-list-x list index) x)
                   (< x (+ (core:display-list-x list index) (core:display-list-width list index)))
                   (<= (core:display-list-y list index) y)
                   (< y (+ (core:display-list-y list index) (core:display-list-height list index))))
          (setf color (core:display-list-color list index)))))))

(defun shown-name (session id)
  (s:node-name (m:find-node session :id id)))

(define-test the-view-is-drawn-with-the-real-widgets ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (let ((button (m:find-node session :id :sign-in :within :canvas))
          (field (m:find-node session :id :name :within :canvas)))
      (assert-eq :button (s:node-role button))
      (assert-equal "Sign in" (s:node-name button))
      (assert-eq :text-field (s:node-role field)))
    (assert-true (search "Nothing" (shown-name session :selected-nothing)))))

(define-test a-press-on-a-widget-of-the-view-selects-it ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :sign-in :within :canvas)
    (assert-equal "Button :sign-in" (shown-name session :selected-what))
    (assert-equal "text: \"Sign in\"" (shown-name session :selected-text))
    (m:click session :id :heading :within :canvas)
    (assert-equal "Label :heading" (shown-name session :selected-what))
    (assert-equal "weight: :bold" (shown-name session :selected-weight))))

(define-test typing-on-a-text-field-of-the-view-selects-it-and-types-nothing ()
  ;; The press that would give the field the keyboard is the designer's; with no keyboard, the field
  ;; takes none of the keys.
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:type-text session "abc" :id :name :within :canvas)
    (assert-equal "" (s:node-value (m:find-node session :id :name :within :canvas)))
    (assert-equal "Text field :name" (shown-name session :selected-what))))

(define-test a-press-between-widgets-selects-the-container-they-are-in ()
  (multiple-value-bind (screen session) (open-designer)
    ;; Right of the heading, which is narrower than the form: in the form, on nothing in it.
    (multiple-value-bind (fx fy fw) (core:lookup-target (screen-app screen) :sign-in-form)
      (declare (ignore fy))
      (multiple-value-bind (hx hy hw) (core:lookup-target (screen-app screen) :heading)
        (assert-true (< (+ hx hw) (+ fx fw -10)))
        (pointer screen :move (+ fx fw -4) (+ hy 4))
        (pointer screen :down (+ fx fw -4) (+ hy 4))
        (pointer screen :up (+ fx fw -4) (+ hy 4))))
    (assert-equal "Column :sign-in-form" (shown-name session :selected-what))))

(define-test the-pointer-and-the-selection-are-outlined ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((dark (bolt:find-theme :dark)))
      (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :forgot)
        (declare (ignore w))
        (flet ((edge () (color-at screen (+ x 1) (+ y (/ h 2)))))
          (assert-false (eql (bolt:theme-setting dark :dim-text) (edge)))
          (pointer screen :move (+ x 10) (+ y (/ h 2)))
          (assert-eql (bolt:theme-setting dark :dim-text) (edge))
          ;; Pressed, it is outlined as the selection in the frame of the press.
          (pointer screen :down (+ x 10) (+ y (/ h 2)))
          (assert-eql (bolt:theme-setting dark :ring) (edge))
          (pointer screen :up (+ x 10) (+ y (/ h 2)))
          (assert-equal "Button :forgot" (shown-name session :selected-what))
          (pointer screen :leave 0 0)
          (assert-eql (bolt:theme-setting dark :ring) (edge)))))))
