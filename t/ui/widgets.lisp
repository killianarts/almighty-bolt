(defpackage #:almighty-bolt/t/ui/widgets
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:i #:almighty-bolt/ui/immediate)
                    (#:w #:almighty-bolt/ui/widgets)
                    (#:fake #:almighty-bolt/test-support/fake-context)))

(in-package #:almighty-bolt/t/ui/widgets)

;;; What a person writing an app meets first: a frame is built from the function that describes the
;;; screen, and a widget without an :id is an error that says which widget. Frames in, an error or a
;;; display list out; nothing of the ui's insides is called.

(defun frame-of (view)
  "Build the first frame of an app whose screen is described by VIEW, a function."
  (let ((variant (i:make-immediate-variant view))
        (context (fake:make-fake-context :width 300f0 :height 200f0))
        (list (core:make-display-list :capacity 4096)))
    (core:settle-frame variant context list)
    list))

(defun widget-missing-its-id (view)
  "The widget that the frame of VIEW complains has no :id, or NIL if it does not complain."
  (handler-case (progn (frame-of view) nil)
    (w:missing-id (condition) (w:missing-id-widget condition))))

(defparameter +widgets-without-ids+
  ;; Each is a function that describes a screen with one widget, called as it is without an id.
  (list (cons 'w:label (lambda () (w:label "x")))
        (cons 'w:paragraph (lambda () (w:paragraph "x")))
        (cons 'w:progress (lambda () (w:progress 0.5)))
        (cons 'w:button (lambda () (w:button "x")))
        (cons 'w:text-field (lambda () (w:text-field "x")))
        (cons 'w:slider (lambda () (w:slider 1 0 2)))
        (cons 'w:choice (lambda () (w:choice '("a" "b") 0)))
        (cons 'w:checkbox (lambda () (w:checkbox "x" nil)))
        (cons 'w:list-box (lambda () (w:list-box '("a" "b") 0)))
        (cons 'w:canvas (lambda () (w:canvas 20 20 (lambda (&rest arguments)
                                                     (declare (ignore arguments))))))
        (cons 'w:scrim (lambda () (w:scrim)))))

(define-test a-widget-without-an-id-is-an-error-that-says-which ()
  (loop :for (widget . view) :in +widgets-without-ids+
        :do (assert-eq widget (widget-missing-its-id view) widget)))

(define-test the-same-widgets-with-an-id-build-a-frame ()
  (flet ((with-ids ()
           (w:label "x" :id :a)
           (w:paragraph "x" :id :b)
           (w:progress 0.5 :id :c)
           (w:button "x" :id :d)
           (w:text-field "x" :id :e)
           (w:slider 1 0 2 :id :f)
           (w:choice '("a" "b") 0 :id :g)
           (w:checkbox "x" nil :id :h)
           (w:list-box '("a" "b") 0 :id :i)
           (w:canvas 20 20 (lambda (&rest arguments) (declare (ignore arguments))) :id :j)
           (w:scrim :id :k)))
    (let ((list (frame-of (lambda () (w:stack (:id :root) (with-ids))))))
      (assert-true (plusp (core:display-list-count list))))))

(define-test the-message-names-the-widget-and-shows-how ()
  (let ((message (handler-case (frame-of (lambda () (w:button "x")))
                   (w:missing-id (condition) (princ-to-string condition)))))
    (assert-true (search "button" message) message)
    (assert-true (search ":id" message) message)))
