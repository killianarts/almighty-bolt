(defpackage #:almighty-bolt/t/app
  ;; This package uses the public one, whole: a symbol that the public package exports and that
  ;; Common Lisp or the test framework also has would be a conflict, and fail here.
  (:use #:cl #:lisp-unit2 #:almighty-bolt)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:counter #:almighty-bolt-examples/counter)
                    (#:todo #:almighty-bolt-examples/todo)))

(in-package #:almighty-bolt/t/app)

;;; A person's first hour with the library: the example apps, which use only the public package, are
;;; made as the host makes them, shown, clicked, typed into and made again. Events in, what is shown
;;; out (the text is read back from the display list, in which the fake context draws a character as
;;; a command whose parameter is its code).

(defstruct (screen (:constructor %make-screen (variant context list state)))
  variant context list state)

(defun show (constructor)
  "Make the app of CONSTRUCTOR with a store for its state, as OPEN-APP does, and draw its first
frame."
  (let* ((state (core:make-kept-state))
         (variant (core:with-kept-state (state) (app:make-app-instance constructor)))
         (screen (%make-screen variant (fake:make-fake-context :width 360f0 :height 320f0)
                               (core:make-display-list :capacity 8192) state)))
    (draw screen)
    screen))

(defun draw (screen)
  (core:settle-frame (screen-variant screen) (screen-context screen) (screen-list screen)))

(defun pointer (screen kind x y &key button (buttons 0))
  (core:handle-event (screen-variant screen)
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button button :buttons buttons
                                              :click-count (if (eq kind :move) 0 1)))
  (draw screen))

(defun click (screen id)
  (multiple-value-bind (x y width height) (core:lookup-target (screen-variant screen) id)
    (assert-true x id)
    (let ((px (+ x (/ width 2))) (py (+ y (/ height 2))))
      (pointer screen :move px py)
      (pointer screen :down px py :button 0 :buttons 1)
      (pointer screen :up px py :button 0))))

(defun type-text (screen string)
  (loop :for character :across string
        :do (core:handle-event (screen-variant screen)
                               (core:make-key-event :timestamp 0 :kind :down
                                                    :logical-key character :modifiers 0))
            (draw screen)))

(defun text-in (screen id)
  "The characters drawn inside the rectangle of the control ID."
  (multiple-value-bind (left top width height) (core:lookup-target (screen-variant screen) id)
    (let ((list (screen-list screen)))
      (coerce (loop :for index :below (core:display-list-count list)
                    :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                               (<= left (core:display-list-x list index) (+ left width -1))
                               (<= top (core:display-list-y list index) (+ top height -1)))
                      :collect (code-char (round (core:display-list-parameter list index))))
              'string))))

(define-test the-counter-shows-its-count-and-counts-clicks ()
  (let ((screen (show 'counter:make-counter)))
    (assert-equal "0" (text-in screen :count))
    (click screen :count-button)
    (click screen :count-button)
    (assert-equal "2" (text-in screen :count))))

(define-test the-counter-made-again-by-new-code-keeps-its-count ()
  (let ((screen (show 'counter:make-counter)))
    (click screen :count-button)
    (click screen :count-button)
    (click screen :count-button)
    (let ((new (core:with-kept-state ((screen-state screen))
                 (app:make-app-instance 'counter:make-counter))))
      (core:inherit-state new (screen-variant screen))
      (setf (screen-variant screen) new)
      (draw screen)
      (assert-equal "3" (text-in screen :count)))))

(define-test the-list-adds-a-thing-written-in-its-field-and-counts-what-is-left ()
  (let ((screen (show 'todo:make-todo)))
    (assert-equal "2 left" (text-in screen :left))
    (click screen :draft)
    (type-text screen "Water the plants")
    (click screen :add)
    (assert-equal "3 left" (text-in screen :left))
    (assert-equal "Water the plants" (text-in screen 4))))

(define-test return-in-the-field-adds-the-thing-too ()
  (let ((screen (show 'todo:make-todo)))
    (click screen :draft)
    (type-text screen "Feed the cat")
    (type-text screen (string #\Return))
    (assert-equal "3 left" (text-in screen :left))
    (assert-equal "Feed the cat" (text-in screen 4))
    (assert-equal "" (text-in screen :draft))))

(define-test checking-a-thing-off-leaves-one-less ()
  (let ((screen (show 'todo:make-todo)))
    (click screen 2)
    (assert-equal "1 left" (text-in screen :left))
    (click screen 1)
    (assert-equal "2 left" (text-in screen :left))))

(define-test the-list-is-a-tree-of-check-boxes-named-by-what-is-to-be-done ()
  (let* ((screen (show 'todo:make-todo))
         (boxes (s:find-nodes (core:accessibility-tree (screen-variant screen)) :role :checkbox)))
    (assert-equal '("Read the README" "Open a window from the REPL" "Change a label and compile it")
                  (mapcar #'s:node-name boxes))
    (assert-equal '(t nil nil) (mapcar #'s:node-value boxes))
    (click screen 2)
    (assert-equal '(t t nil)
                  (mapcar #'s:node-value
                          (s:find-nodes (core:accessibility-tree (screen-variant screen))
                                        :role :checkbox)))
    (let ((field (first (s:find-nodes (core:accessibility-tree (screen-variant screen))
                                      :role :text-field))))
      (assert-eq :draft (s:node-id field))
      (assert-equal "" (s:node-value field)))))

(define-test everything-the-public-package-exports-is-something ()
  (let ((nothing '()))
    (do-external-symbols (symbol :almighty-bolt)
      (unless (or (fboundp symbol) (boundp symbol) (find-class symbol nil))
        (push symbol nothing)))
    (assert-equal '() nothing)))
