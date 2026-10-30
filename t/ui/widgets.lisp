(defpackage #:almighty-bolt/t/ui/widgets
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:i #:almighty-bolt/ui/immediate)
                    (#:w #:almighty-bolt/ui/widgets)
                    (#:u #:almighty-bolt/ui/ui)
                    (#:fake #:almighty-bolt/test-support/fake-context)))

(in-package #:almighty-bolt/t/ui/widgets)

;;; What a person writing an app meets first: a frame is built from the function that describes the
;;; screen, a widget without an :id is an error that says which widget, and two things that the ui
;;; could not tell apart are an error that says which. Frames in, an error or a display list out;
;;; nothing of the ui's insides is called.

(defun frame-of (view &key (capacity 4096))
  "Build the first frame of an app whose screen is described by VIEW, a function."
  (let ((variant (i:make-immediate-variant view))
        (context (fake:make-fake-context :width 300f0 :height 200f0))
        (list (core:make-display-list :capacity capacity)))
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

;;; --- two things with one id ---

(defun duplicate-in (view &key (capacity 4096))
  "The DUPLICATE-ID that the first frame of VIEW signals, or NIL if it builds."
  (handler-case (progn (frame-of view :capacity capacity) nil)
    (u:duplicate-id (condition) condition)))

(define-test two-siblings-with-one-id-are-an-error-that-names-both ()
  (let ((condition (duplicate-in (lambda ()
                                   (w:column ()
                                     (w:button "Go" :id :go)
                                     (w:label "Gone" :id :go))))))
    (assert-true condition)
    (assert-eq :go (u:duplicate-id-id condition))
    (assert-equal '(:button :label) (u:duplicate-id-kinds condition))
    (let ((message (princ-to-string condition)))
      (dolist (word '(":GO" "button" "label" "column" "loop over data"))
        (assert-true (search word message) (list word message))))))

(define-test the-container-is-named-by-its-id-if-it-has-one ()
  (let ((condition (duplicate-in (lambda ()
                                   (w:row (:id :controls)
                                     (w:button "A" :id :same)
                                     (w:button "B" :id :same))))))
    (assert-true (search "row :CONTROLS" (princ-to-string condition)))))

(define-test the-same-id-in-different-containers-is-two-things ()
  (assert-false (duplicate-in (lambda ()
                                (w:column ()
                                  (w:row (:id :first) (w:button "A" :id :go))
                                  (w:row (:id :second) (w:button "B" :id :go))))))
  ;; Containers that have no id are told apart by their places.
  (assert-false (duplicate-in (lambda ()
                                (w:column ()
                                  (w:row () (w:button "A" :id :go))
                                  (w:row () (w:button "B" :id :go)))))))

(define-test a-loop-over-data-that-repeats-an-id-is-an-error ()
  (let ((condition (duplicate-in (lambda ()
                                   (w:column ()
                                     (dolist (task '(1 2 3 1))
                                       (w:checkbox "task" nil :id task)))))))
    (assert-true condition)
    (assert-eql 1 (u:duplicate-id-id condition))))

(define-test a-number-that-is-the-place-of-a-thing-without-an-id-is-an-error ()
  ;; A container with no id is known by its place, 0 for the first; an id of 0 would be the same.
  (let ((condition (duplicate-in (lambda ()
                                   (w:column ()
                                     (w:row ())
                                     (w:button "A" :id 0))))))
    (assert-true condition)
    (assert-true (search "same to the ui" (princ-to-string condition)))))

(define-test two-things-at-the-top-of-the-screen-are-an-error ()
  (let ((condition (duplicate-in (lambda ()
                                   (w:column ())
                                   (w:column ())))))
    (assert-true condition)
    (assert-true (search "one thing" (princ-to-string condition)))))

(define-test the-same-ids-in-the-next-frame-are-not-a-duplicate ()
  (let ((variant (i:make-immediate-variant
                  (lambda ()
                    (w:column ()
                      (w:button "A" :id :a)
                      (w:button "B" :id :b)))))
        (context (fake:make-fake-context :width 300f0 :height 200f0))
        (list (core:make-display-list :capacity 4096)))
    (dotimes (frame 5)
      (core:settle-frame variant context list))
    (assert-true (plusp (core:display-list-count list)))))

(define-test a-screen-with-thousands-of-things-is-checked-too ()
  (flet ((screen (ids)
           (lambda ()
             (w:column ()
               (dolist (id ids)
                 (w:button "x" :id id))))))
    (assert-false (duplicate-in (screen (loop :for id :below 3000 :collect id)) :capacity 100000))
    ;; The repeat is of one from before the table of what was seen had to grow.
    (let ((condition (duplicate-in (screen (append (loop :for id :below 3000 :collect id) '(7)))
                                   :capacity 100000)))
      (assert-true condition)
      (assert-eql 7 (u:duplicate-id-id condition)))))
