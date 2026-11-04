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

(defun shown-value (session id)
  (s:node-value (m:find-node session :id id)))

(defun in-view (session id)
  "The node of the widget ID of the view being designed, or NIL if the view has none."
  (handler-case (m:find-node session :id id :within :canvas)
    (error () nil)))

(defun top-of (session id)
  (second (s:node-bounds (m:find-node session :id id :within :canvas))))

(defun press-beside-heading (screen)
  "A press in the form, right of its heading: on the form and nothing in it."
  (multiple-value-bind (fx fy fw) (core:lookup-target (screen-app screen) :sign-in-form)
    (declare (ignore fy))
    (multiple-value-bind (hx hy hw) (core:lookup-target (screen-app screen) :heading)
      (assert-true (< (+ hx hw) (+ fx fw -10)))
      (pointer screen :move (+ fx fw -4) (+ hy 4))
      (pointer screen :down (+ fx fw -4) (+ hy 4))
      (pointer screen :up (+ fx fw -4) (+ hy 4)))))

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
    (assert-equal "Sign in" (shown-value session :option-text))
    (m:click session :id :heading :within :canvas)
    (assert-equal "Label :heading" (shown-name session :selected-what))
    (assert-equal ":bold" (shown-value session :option-weight))))

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
    (press-beside-heading screen)
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

;;; --- changing the view ---

(define-test a-widget-added-goes-into-the-selected-container-or-after-the-selected-widget ()
  (multiple-value-bind (screen session) (open-designer)
    ;; With the form selected, at its end, below the buttons.
    (press-beside-heading screen)
    (m:click session :id :add-button :within :left)
    (assert-equal "Button :button-1" (shown-name session :selected-what))
    (assert-equal "Button" (s:node-name (in-view session :button-1)))
    (assert-true (> (top-of session :button-1) (top-of session :sign-in)))
    ;; With a label selected, just after it: between the heading and the label under it.
    (m:click session :id :heading :within :canvas)
    (m:click session :id :add-label :within :left)
    (assert-equal "Label :label-1" (shown-name session :selected-what))
    (assert-true (< (top-of session :heading) (top-of session :label-1)
                    (top-of session :name-label)))))

(define-test an-option-changed-changes-the-widget-in-the-view ()
  (multiple-value-bind (screen session) (open-designer)
    (m:click session :id :sign-in :within :canvas)
    (m:set-value session "Log in" :id :option-text)
    (assert-equal "Log in" (s:node-name (in-view session :sign-in)))
    (m:set-value session nil :id :option-enabled)
    (assert-true (s:node-property (in-view session :sign-in) :disabled))
    ;; A number: what is not one is shown as wrong and changes nothing. The form's gap, seen in how
    ;; far apart its first two widgets are.
    (press-beside-heading screen)
    (flet ((apart () (- (top-of session :name-label) (top-of session :heading))))
      (let ((before (apart)))
        (m:set-value session "wide" :id :option-gap)
        (assert-true (s:node-property (m:find-node session :id :option-gap) :invalid))
        (assert-equal before (apart))
        (m:set-value session "40" :id :option-gap)
        (assert-false (s:node-property (m:find-node session :id :option-gap) :invalid))
        (assert-equal (+ before 28) (apart))))))

(define-test an-id-can-be-changed-to-one-no-other-widget-has ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (m:click session :id :sign-in :within :canvas)
    ;; Typed, it is not changed yet: Return changes it.
    (m:set-value session "submit" :id :option-id)
    (assert-equal "Button :sign-in" (shown-name session :selected-what))
    (m:press-key session :return)
    (assert-equal "Button :submit" (shown-name session :selected-what))
    (assert-true (in-view session :submit))
    (assert-false (in-view session :sign-in))
    ;; Another widget's id is shown as wrong, and Return leaves the widget its own.
    (m:set-value session "forgot" :id :option-id)
    (assert-true (s:node-property (m:find-node session :id :option-id) :invalid))
    (m:press-key session :return)
    (assert-equal "Button :submit" (shown-name session :selected-what))
    (assert-true (in-view session :forgot))))

(define-test remove-takes-the-selected-widget-out-of-the-view ()
  (multiple-value-bind (screen session) (open-designer)
    (m:click session :id :forgot :within :canvas)
    (m:click session :id :remove :within :toolbar)
    (assert-false (in-view session :forgot))
    (assert-true (in-view session :sign-in))
    (assert-true (search "Nothing" (shown-name session :selected-nothing)))
    ;; The view itself cannot be removed.
    (press-beside-heading screen)
    (assert-true (s:node-property (m:find-node session :id :remove :within :toolbar) :disabled))))

(define-test a-view-saved-opens-again-as-it-was ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((path (merge-pathnames (format nil "view-~d.view" (random 1000000000))
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (declare (ignore kind offered))
                     (assert-equal '("view") types)
                     (namestring path)))
             (m:click session :id :sign-in :within :canvas)
             (m:set-value session "Log in" :id :option-text)
             ;; The heading's weight back to the default: then the file does not say it.
             (m:click session :id :heading :within :canvas)
             (m:set-value session ":regular" :id :option-weight)
             (assert-equal ":regular" (shown-value session :option-weight))
             (m:click session :id :save :within :toolbar)
             (assert-true (search "Saved to" (shown-name session :status)))
             (let ((saved (uiop:read-file-string path)))
               (assert-true (search "(:button :sign-in (:text \"Log in\"))" saved) saved)
               (assert-true (search "(:label :heading (:text \"Sign in\" :size 22))" saved) saved))
             (m:click session :id :new :within :toolbar)
             (assert-false (in-view session :sign-in))
             (m:click session :id :open :within :toolbar)
             (assert-true (search "Opened" (shown-name session :status)))
             (assert-equal "Log in" (s:node-name (in-view session :sign-in)))
             (assert-true (in-view session :password)))
        (when (probe-file path) (delete-file path))))))

(define-test a-file-that-is-not-a-view-is-not-opened ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((path (merge-pathnames (format nil "not-~d.view" (random 1000000000))
                                 (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output)
               (write-string "(:slab :nope ())" out))
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (declare (ignore kind types offered))
                     (namestring path)))
             (m:click session :id :open :within :toolbar)
             (assert-true (search "Could not open" (shown-name session :status)))
             (assert-true (in-view session :sign-in)))
        (when (probe-file path) (delete-file path))))))

(define-test the-outline-is-where-the-selected-widget-is-in-the-frame-it-changes-size ()
  ;; One key and one frame: a window draws no more until something happens, so the outline must be
  ;; round the button as that frame lays it out, not as the frame before did.
  (multiple-value-bind (screen session) (open-designer)
    (let ((ring (bolt:theme-setting (bolt:find-theme :dark) :ring)))
      (m:click session :id :forgot :within :canvas)
      (m:click session :id :option-text :within :selection)
      (multiple-value-bind (x0 y0 w0) (core:lookup-target (screen-app screen) :forgot)
        (declare (ignore x0 y0))
        (dolist (character (coerce " now" 'list))
          (core:handle-event (screen-app screen)
                             (core:make-key-event :timestamp 0 :kind :down :logical-key character
                                                  :modifiers 0))
          (draw screen))
        (multiple-value-bind (x y w h) (core:lookup-target (screen-app screen) :forgot)
          (assert-true (> w w0))
          (assert-eql ring (color-at screen (+ x w -1) (+ y (/ h 2)))))))))

;;; --- the outline ---

(define-test the-outline-lists-the-widgets-in-order-indented-by-depth ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    (let ((items (s:node-property (m:find-node session :id :outline-list) :items)))
      (assert-equal "Column :sign-in-form" (first items))
      (assert-equal "   Label :heading" (second items))
      (assert-equal "      Button :forgot" (car (last items)))
      (assert-equal 9 (length items)))
    ;; What is added is in it, where it went.
    (m:click session :id :heading :within :canvas)
    (m:click session :id :add-text-field :within :left)
    (assert-equal "   Text field :text-field-1"
                  (third (s:node-property (m:find-node session :id :outline-list) :items)))))

(define-test a-row-of-the-outline-selects-its-widget-and-the-view-selects-its-row ()
  (multiple-value-bind (screen session) (open-designer)
    (declare (ignore screen))
    ;; The row of the row of buttons, which a press in the view could only find between them.
    (m:set-value session 6 :id :outline-list)
    (assert-equal "Row :actions" (shown-name session :selected-what))
    (m:click session :id :password :within :canvas)
    (assert-equal "   Text field :password" (shown-value session :outline-list))))
