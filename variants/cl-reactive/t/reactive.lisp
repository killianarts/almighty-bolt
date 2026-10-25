(defpackage #:almighty-bolt-variant-reactive/t/reactive
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:v #:almighty-bolt-variant-reactive/views)
                    (#:r #:almighty-bolt-variant-reactive/reactive)
                    (#:s #:almighty-bolt-variant-reactive/signals)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-reactive/t/reactive)

;;; The reactive variant with synthetic events and a context whose text is 8 points a
;;; character: signals written by handlers, components described again, nodes kept.

(defvar *context* nil)

(defun show (thunk)
  (setf *context* (fake:make-fake-context))
  (let ((variant (r:make-reactive-variant thunk))
        (list (core:make-display-list :capacity 1024)))
    (core:frame variant *context* list)
    (values variant list)))

(defun build (variant)
  (let ((list (core:make-display-list :capacity 1024)))
    (core:frame variant *context* list)
    list))

(defun glyphs (list)
  (coerce (loop :for index :below (core:display-list-count list)
                :when (= (core:display-list-kind list index) core:+command-glyph+)
                  :collect (code-char (round (core:display-list-parameter list index))))
          'string))

(defun pointer (variant kind x y &key button (buttons 0))
  (core:handle-event variant (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                                      :y (float y 1d0) :button button
                                                      :buttons buttons)))

(defun click (variant id)
  (multiple-value-bind (x y width height) (core:lookup-target variant id)
    (let ((px (+ x (/ width 2))) (py (+ y (/ height 2))))
      (pointer variant :move px py)
      (pointer variant :down px py :button 0 :buttons 1)
      (pointer variant :up px py :button 0))))

(defun key (variant character)
  (core:handle-event variant (core:make-key-event :timestamp 0 :kind :down
                                                  :logical-key character)))

(defun counter ()
  "A counter app. Returns the thunk, the count signal and a function giving how many times
each of the two components was described."
  (let ((count (s:make-signal 0))
        (outer 0)
        (inner 0))
    (values (lambda ()
              (incf outer)
              (v:padding (v:row (v:component (lambda ()
                                               (incf inner)
                                               (v:label (princ-to-string (s:value count))
                                                        :id :count)))
                                (v:button "Count" :id :button
                                                  :on-click (lambda () (s:update count #'1+)))
                                :gap 10)
                         :all 10))
            count
            (lambda () (list outer inner)))))

(define-test the-app-is-described-once-and-drawn ()
  (multiple-value-bind (thunk count runs) (counter)
    (declare (ignore count))
    (multiple-value-bind (variant list) (show thunk)
      (declare (ignore variant))
      (assert-equal "0Count" (glyphs list))
      (assert-equal '(1 1) (funcall runs)))))

(define-test a-click-writes-the-signal-and-only-the-component-that-read-it-is-described-again ()
  (multiple-value-bind (thunk count runs) (counter)
    (multiple-value-bind (variant) (show thunk)
      (assert-true (click variant :button))
      (assert-eql 1 (s:peek count))
      (let ((list (build variant)))
        (assert-equal "1Count" (glyphs list))
        (assert-equal '(1 2) (funcall runs)))
      ;; Two more clicks before a frame are one description of the component, not two.
      (click variant :button)
      (click variant :button)
      (assert-equal "3Count" (glyphs (build variant)))
      (assert-equal '(1 3) (funcall runs)))))

(define-test the-widgets-are-kept-between-descriptions ()
  (multiple-value-bind (thunk) (counter)
    (multiple-value-bind (variant) (show thunk)
      (let ((before (core:hit-target variant 12f0 20f0)))
        (click variant :button)
        (build variant)
        (assert-eql before (core:hit-target variant 12f0 20f0))
        (assert-eql :count (core:hit-target variant 12f0 20f0))))))

(define-test an-event-that-changes-nothing-asks-for-nothing ()
  (multiple-value-bind (thunk) (counter)
    (multiple-value-bind (variant) (show thunk)
      (assert-false (pointer variant :move 300 250))
      (assert-false (pointer variant :down 300 250 :button 0 :buttons 1)))))

;;; --- a field that the state controls ---

(define-test a-field-shows-what-the-state-says-and-a-refused-change-is-undone ()
  (let* ((text (s:make-signal "12"))
         (thunk (lambda ()
                  (v:column (v:text-field :id :f :value (s:value text)
                                          :on-change (lambda (new)
                                                       ;; Only digits are accepted.
                                                       (when (every #'digit-char-p new)
                                                         (setf (s:value text) new)))))))
         (variant (show thunk)))
    (assert-eql :f (core:hit-target variant 10f0 10f0))
    (multiple-value-bind (x y) (core:lookup-target variant :f)
      (pointer variant :move (+ x 10) (+ y 10))
      (pointer variant :down (+ x 10) (+ y 10) :button 0 :buttons 1)
      (pointer variant :up (+ x 10) (+ y 10) :button 0))
    (key variant core:+key-end+)
    (key variant #\3)
    (assert-equal "123" (s:peek text))
    (assert-equal "123" (glyphs (build variant)))
    ;; A letter is refused by the handler, which writes nothing, and the field goes back.
    (key variant #\x)
    (assert-equal "123" (s:peek text))
    (assert-equal "123" (glyphs (build variant)))))

;;; --- time ---

(define-test a-ticker-writes-a-signal-and-keeps-frames-coming-while-it-runs ()
  (let* ((ticks (s:make-signal 0))
         (variant (show (lambda () (v:column (v:label (princ-to-string (s:value ticks)))))))
         (limit 3))
    (r:add-ticker variant (lambda (now)
                            (declare (ignore now))
                            (s:update ticks #'1+)
                            (< (s:peek ticks) limit)))
    (core:take-frame-request *context*)
    (assert-equal "1" (glyphs (build variant)))
    (assert-true (core:take-frame-request *context*))
    (assert-equal "2" (glyphs (build variant)))
    (assert-true (core:take-frame-request *context*))
    (assert-equal "3" (glyphs (build variant)))
    (assert-false (core:take-frame-request *context*))))

(define-test a-signal-written-from-outside-asks-for-a-frame ()
  (let* ((signal (s:make-signal "a"))
         (variant (show (lambda () (v:column (v:label (s:value signal)))))))
    (core:take-frame-request *context*)
    (setf (s:value signal) "b")
    (assert-true (core:take-frame-request *context*))
    (assert-equal "b" (glyphs (build variant)))))

(define-test components-that-keep-changing-what-they-read-are-an-error ()
  (let ((signal (s:make-signal 0)))
    (assert-error 'error
        (show (lambda ()
                (let ((now (s:value signal)))
                  (setf (s:value signal) (1+ now))
                  (v:column (v:label (princ-to-string now)))))))))
