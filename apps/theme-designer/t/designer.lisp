(defpackage #:almighty-bolt-theme-designer/t/designer
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:designer #:almighty-bolt-theme-designer)))

(in-package #:almighty-bolt-theme-designer/t/designer)

;;; The designer worked as a person works it, through the machine interface, in a context with no
;;; window that is tall enough for all of it to show. What it draws is read back from the display
;;; list; what it writes down is read and evaluated, and must be the theme it shows.

(defstruct (screen (:constructor %make-screen (app context list)))
  app context list)

(defun draw (screen)
  (core:settle-frame (screen-app screen) (screen-context screen) (screen-list screen)))

(defun open-designer ()
  "The designer and a machine session on it."
  (let* ((instance (core:with-kept-state ((core:make-kept-state))
                    (app:make-app-instance 'designer:make-designer)))
         (screen (%make-screen instance (fake:make-fake-context :width 1200f0 :height 3000f0)
                               (core:make-display-list :capacity 65536))))
    (draw screen)
    (values screen
            (m:make-session :app (lambda () (screen-app screen))
                            :deliver (lambda (event)
                                       (core:handle-event (screen-app screen) event)
                                       (draw screen))
                            :settle (lambda () (dotimes (i 4) (draw screen)))
                            :on-main #'funcall))))

(defun color-at (screen x y)
  "The colour of the last rectangle drawn over the point X, Y, inside every clip it is in."
  (let ((list (screen-list screen)) (clips '()) (color nil))
    (flet ((over (index)
             (let ((left (core:display-list-x list index)) (top (core:display-list-y list index)))
               (and (<= left x) (< x (+ left (core:display-list-width list index)))
                    (<= top y) (< y (+ top (core:display-list-height list index)))))))
      (dotimes (index (core:display-list-count list) color)
        (let ((kind (core:display-list-kind list index)))
          (cond ((= kind core:+command-clip+) (push index clips))
                ((= kind core:+command-unclip+) (pop clips))
                ((and (or (= kind core:+command-rect+) (= kind core:+command-rounded-rect+))
                      (over index) (every #'over clips))
                 (setf color (core:display-list-color list index)))))))))

(defun middle-color (screen id)
  (multiple-value-bind (x y width height) (core:lookup-target (screen-app screen) id)
    (color-at screen (+ x (/ width 2)) (+ y (/ height 2)))))

(defun corner-color (screen id)
  "The colour just inside the top left of the control ID (a panel's background)."
  (multiple-value-bind (x y) (core:lookup-target (screen-app screen) id)
    (color-at screen (+ x 2) (+ y 2))))

(defun shown-text (session id)
  (s:node-value (m:find-node session :id id)))

(defun shown-name (session id)
  (s:node-name (m:find-node session :id id)))

(defun defined (session)
  "The theme that the definition the designer shows defines, once evaluated."
  (eval (read-from-string (shown-text session :source))))

(define-test the-accents-hue-changes-the-preview-and-what-is-written-down ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((dark (bolt:find-theme :dark)))
      (assert-eql (bolt:theme-setting dark :button) (middle-color screen :sample-button))
      ;; A green accent: hue 140, as the dark accent's saturation and lightness are.
      (m:set-value session 140 :id :accent-hue)
      (let* ((name (defined session))
             (theme (bolt:find-theme name))
             (accent (bolt:theme-setting theme :accent)))
        (assert-eq :my-theme name)
        (assert-false (eql (bolt:theme-setting dark :accent) accent))
        ;; The preview is in the theme written down: its button, and the hover worked out from it.
        (assert-eql accent (middle-color screen :sample-button))
        (assert-false (eql (bolt:theme-setting dark :button-hover)
                           (bolt:theme-setting theme :button-hover)))
        ;; Green, more than anything else.
        (multiple-value-bind (r g b) (values (ldb (byte 8 24) accent) (ldb (byte 8 16) accent)
                                             (ldb (byte 8 8) accent))
          (assert-true (> g (max r b)) r g b))
        ;; The seed's field says its colour.
        (assert-equal (bolt:color-hex accent) (shown-text session :accent-hex))))))

(define-test a-background-typed-in-shows-behind-the-preview-and-its-text-is-said-to-be-hard-to-read ()
  (multiple-value-bind (screen session) (open-designer)
    (assert-true (search "easy to read" (shown-name session :contrast-0)))
    ;; A grey near the dark text's: the text on it is hard to read.
    (m:set-value session "#c8cad0" :id :background-hex)
    (assert-eql (bolt:parse-color "#c8cad0") (corner-color screen :preview-back))
    (assert-true (search "too low" (shown-name session :contrast-0)))
    (assert-eql (bolt:parse-color "#c8cad0")
                (bolt:theme-setting (bolt:find-theme (defined session)) :background))))

(define-test starting-from-light-is-the-light-theme-and-says-no-more ()
  (multiple-value-bind (screen session) (open-designer)
    (m:set-value session 1 :id :base)
    (let ((light (bolt:find-theme :light)))
      (assert-eql (bolt:theme-setting light :button) (middle-color screen :sample-button))
      (assert-eql (bolt:theme-setting light :background) (corner-color screen :preview-back))
      (assert-equal (bolt:color-hex (bolt:theme-setting light :accent))
                    (shown-text session :accent-hex))
      (assert-equal "(almighty-bolt:define-theme :my-theme (:base :light))"
                    (shown-text session :source)))))

(define-test a-worked-out-colour-can-be-given-and-taken-back ()
  (multiple-value-bind (screen session) (open-designer)
    (m:set-value session "button" :id :override-which)
    (assert-equal "button" (shown-text session :override-which))
    (m:set-value session "#d04080" :id :override-hex)
    (assert-eql (bolt:parse-color "#d04080") (middle-color screen :sample-button))
    (assert-true (search ":button \"#d04080\"" (shown-text session :source)))
    (m:click session :id :override-clear)
    (assert-eql (bolt:theme-setting (bolt:find-theme :dark) :button)
                (middle-color screen :sample-button))
    (assert-false (search "#d04080" (shown-text session :source)))))

(define-test save-writes-a-file-that-defines-the-theme ()
  (multiple-value-bind (screen session) (open-designer)
    ;; A name no theme has yet, so that only loading the file can define it.
    (let* ((name (format nil "sea-~d" (random 1000000000)))
           (keyword (intern (string-upcase name) :keyword))
           (path (merge-pathnames (format nil "~a.lisp" name) (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (setf (core:context-file-chooser (screen-context screen))
                   (lambda (kind types offered)
                     (assert-eq :save kind)
                     (assert-equal '("lisp") types)
                     (assert-equal (format nil "~a.lisp" name) offered)
                     (namestring path)))
             (m:set-value session name :id :name)
             (m:set-value session 170 :id :accent-hue)
             (m:click session :id :save)
             (assert-true (search "Saved to" (shown-name session :saved)))
             (assert-false (member keyword (bolt:theme-names)))
             (load path)
             (assert-true (member keyword (bolt:theme-names)))
             (assert-equal (shown-text session :accent-hex)
                           (bolt:color-hex (bolt:theme-setting (bolt:find-theme keyword) :accent))))
        (when (probe-file path) (delete-file path))))))

(define-test the-definition-can-be-selected-and-copied ()
  (multiple-value-bind (screen session) (open-designer)
    (m:set-value session 140 :id :accent-hue)
    (m:click session :id :source)
    (m:press-key session :a :modifiers '(:super))
    (m:press-key session :c :modifiers '(:super))
    (let ((copied (core:context-clipboard-text (screen-context screen))))
      (assert-equal (shown-text session :source) copied)
      (assert-true (search ":accent" copied)))))

(define-test more-spacing-makes-the-preview-roomier-and-is-written-down ()
  (multiple-value-bind (screen session) (open-designer)
    (flet ((width (id) (nth-value 2 (core:lookup-target (screen-app screen) id))))
      (let ((before (width :sample-button)))
        (m:set-value session 150 :id :spacing)
        (assert-true (search "Spacing 150%" (shown-name session :spacing-label)))
        (assert-eql 1.5 (getf (cdddr (read-from-string (shown-text session :source))) :spacing))
        (assert-eql 1.5f0 (bolt:theme-setting (bolt:find-theme (defined session)) :spacing))
        ;; The sample button's padding at the sides is 21 for 14.
        (assert-eql 14f0 (- (width :sample-button) before))))))

(defun press-keys (screen key &rest modifiers)
  "KEY pressed with MODIFIERS (:command, :shift), and the frames after it."
  (core:handle-event (screen-app screen)
                     (core:make-key-event :timestamp 0 :kind :down :logical-key key
                                          :modifiers (logior (if (member :command modifiers)
                                                                 core:+modifier-super+ 0)
                                                             (if (member :shift modifiers)
                                                                 core:+modifier-shift+ 0))))
  (dotimes (i 3) (draw screen)))

(define-test every-change-is-undone-and-redone-a-run-of-changes-to-one-control-as-one-step ()
  (multiple-value-bind (screen session) (open-designer)
    (let ((dark (bolt:theme-setting (bolt:find-theme :dark) :button))
          (light (bolt:theme-setting (bolt:find-theme :light) :button)))
      (assert-false (s:node-property (m:find-node session :id :undo) :enabled))
      ;; Two changes to the accent's hue, one after the other, are one step; the start is another.
      (m:set-value session 140 :id :accent-hue)
      (m:set-value session 200 :id :accent-hue)
      (let ((blue (middle-color screen :sample-button)))
        (m:set-value session 1 :id :base)
        (assert-eql light (middle-color screen :sample-button))
        (m:click session :id :undo)
        (assert-eql blue (middle-color screen :sample-button))
        (assert-true (search "#" (shown-text session :source)))
        (m:click session :id :undo)
        (assert-eql dark (middle-color screen :sample-button))
        (assert-equal "(almighty-bolt:define-theme :my-theme (:base :dark))"
                      (shown-text session :source))
        (assert-false (s:node-property (m:find-node session :id :undo) :enabled))
        ;; Redo, by its key, puts them back in order.
        (press-keys screen #\z :command :shift)
        (assert-eql blue (middle-color screen :sample-button))
        (press-keys screen #\z :command :shift)
        (assert-eql light (middle-color screen :sample-button))
        ;; And Undo by its key; a change after it leaves nothing to redo.
        (press-keys screen #\z :command)
        (assert-eql blue (middle-color screen :sample-button))
        (m:set-value session 12 :id :radius)
        (assert-false (s:node-property (m:find-node session :id :redo) :enabled))
        ;; The hue changed again after another step: undone, it is as it was then, and not as the
        ;; slider moved it since.
        (let ((then (shown-text session :accent-hex)))
          (m:set-value session 300 :id :accent-hue)
          (assert-false (equal then (shown-text session :accent-hex)))
          (m:click session :id :undo)
          (assert-equal then (shown-text session :accent-hex)))))))
