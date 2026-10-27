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

;;; --- a canvas that asks for the keyboard ---

(defun keys-a-canvas-gets (&key autofocus (focusable t))
  "Open a window with a canvas, press q, and return what the canvas was given."
  (let* ((keys '())
         (variant (show (lambda ()
                          (v:canvas :id :board :width 100 :height 80 :focusable focusable
                                    :autofocus autofocus
                                    :on-key (lambda (context event)
                                              (declare (ignore context))
                                              (push (core:key-event-logical-key event) keys)
                                              t))))))
    (key variant #\q)
    (reverse keys)))

(define-test a-canvas-that-asks-for-the-keyboard-has-it-when-the-window-opens ()
  (assert-equal '(#\q) (keys-a-canvas-gets :autofocus t)))

(define-test a-canvas-that-does-not-ask-or-cannot-have-the-keyboard-is-not-given-it ()
  (assert-equal '() (keys-a-canvas-gets))
  (assert-equal '() (keys-a-canvas-gets :autofocus t :focusable nil)))

;;; --- the caret of a canvas that takes text ---

(define-test the-caret-a-canvas-said-follows-it-when-it-moves-and-is-not-painted-again ()
  (let* ((text (s:make-signal "ab"))
         (painted 0)
         (variant (show (lambda ()
                          (v:row (v:component (lambda () (v:label (s:value text) :id :l)))
                                 (v:canvas :id :c :width 100 :height 80
                                           :paint (lambda (data canvas context list x y w h)
                                                    (declare (ignore data canvas list x y w h))
                                                    (incf painted)
                                                    (core:set-text-input-caret
                                                     context 5f0 6f0 1.5f0 12f0))))))))
    (flet ((settle ()
             (core:settle-frame variant *context* (core:make-display-list :capacity 1024)))
           (caret ()
             (let ((rect (core:context-text-input-rect *context*)))
               (list (aref rect 0) (aref rect 1)))))
      (settle)
      (multiple-value-bind (x y) (core:lookup-target variant :c)
        (assert-equal (list (+ x 5f0) (+ y 6f0)) (caret)))
      (let ((before painted))
        (setf (s:value text) "abcdefgh")
        (settle)
        (assert-eql before painted)
        (multiple-value-bind (x y) (core:lookup-target variant :c)
          (assert-true (> x 60f0))
          (assert-equal (list (+ x 5f0) (+ y 6f0)) (caret)))))))

;;; --- a checkbox that the state controls ---

(define-test a-checkbox-shows-what-the-state-says-and-a-refused-change-is-undone ()
  (let* ((on (s:make-signal nil))
         (refuse nil)
         (asked '())
         (thunk (lambda ()
                  (v:column (v:checkbox "Filter" :id :c :checked (s:value on)
                                                 :on-change (lambda (checked)
                                                              (push checked asked)
                                                              (unless refuse
                                                                (setf (s:value on) checked)))))))
         (count (lambda (list) (core:display-list-count list))))
    (multiple-value-bind (variant unchecked) (show thunk)
      (click variant :c)
      (assert-equal '(t) asked)
      (assert-true (s:peek on))
      ;; The mark in the box is one more command than the empty box has.
      (assert-eql (1+ (funcall count unchecked)) (funcall count (build variant)))
      (click variant :c)
      (assert-false (s:peek on))
      (assert-eql (funcall count unchecked) (funcall count (build variant)))
      ;; A change the app refuses leaves the box as the state says.
      (setf refuse t)
      (click variant :c)
      (assert-equal '(t nil t) (reverse asked))
      (assert-false (s:peek on))
      (assert-eql (funcall count unchecked) (funcall count (build variant))))))

(define-test the-space-bar-asks-the-app-to-change-a-checkbox-that-has-the-keyboard ()
  (let* ((on (s:make-signal nil))
         (variant (show (lambda ()
                          (v:column (v:checkbox "Filter" :id :c :checked (s:value on)
                                                         :on-change (lambda (checked)
                                                                      (setf (s:value on) checked))))))))
    (key variant #\Space)
    (assert-false (s:peek on))                                  ; the keyboard is nowhere yet
    (key variant #\Tab)
    (key variant #\Space)
    (assert-true (s:peek on))
    (key variant #\Space)
    (assert-false (s:peek on))))

;;; --- every handler of a canvas is given the context ---

(define-test every-handler-of-a-canvas-is-given-the-context-first ()
  (let* ((seen '())
         (variant (show (lambda ()
                          (v:canvas :id :board :width 100 :height 80 :focusable t
                                    :on-press (lambda (context x y button click-count)
                                                (declare (ignore x y button click-count))
                                                (push (list :press context) seen))
                                    :on-move (lambda (context x y)
                                               (declare (ignore x y))
                                               (push (list :move context) seen))
                                    :on-exit (lambda (context)
                                               (push (list :exit context) seen))
                                    :on-drag (lambda (context x y)
                                               (declare (ignore x y))
                                               (push (list :drag context) seen))
                                    :on-release (lambda (context x y inside)
                                                  (declare (ignore x y inside))
                                                  (push (list :release context) seen))
                                    :on-key (lambda (context event)
                                              (declare (ignore event))
                                              (push (list :key context) seen)
                                              t)
                                    :on-text (lambda (context string)
                                               (declare (ignore string))
                                               (push (list :text context) seen)
                                               t)
                                    :on-composition (lambda (context kind string start length)
                                                      (declare (ignore kind string start length))
                                                      (push (list :composition context) seen)
                                                      t)
                                    :on-wheel (lambda (context dx dy)
                                                (declare (ignore dx dy))
                                                (push (list :wheel context) seen)
                                                t))))))
    (multiple-value-bind (x y) (core:lookup-target variant :board)
      (pointer variant :move (+ x 30) (+ y 40))
      (pointer variant :down (+ x 30) (+ y 40) :button 0 :buttons 1)
      (pointer variant :move (+ x 50) (+ y 50) :buttons 1)
      (pointer variant :up (+ x 50) (+ y 50) :button 0)
      (key variant #\q)
      (text variant "x")
      (composition variant :started "に" 1 0)
      (core:handle-event variant (core:make-wheel-event :timestamp 0 :x (float (+ x 40) 1d0)
                                                        :y (float (+ y 40) 1d0)
                                                        :dx 0d0 :dy -3d0))
      (pointer variant :move 390 290))
    (assert-equal '(:move :press :drag :release :key :text :composition :wheel :exit)
                  (remove-duplicates (mapcar #'first (reverse seen)) :from-end t))
    (assert-true (every (lambda (entry) (eq *context* (second entry))) seen))))

;;; --- text and composition for a canvas that has the keyboard ---

(defun text (variant string)
  (core:handle-event variant (core:make-text-input-event :timestamp 0 :text string)))

(defun composition (variant kind text &optional (start 0) (length 0))
  (core:handle-event variant (core:make-composition-event :timestamp 0 :kind kind :text text
                                                          :selection-start start
                                                          :selection-length length)))

(define-test a-canvas-with-the-keyboard-gets-keys-text-and-composition-in-order ()
  (let* ((events '())
         (variant (show (lambda ()
                          (v:canvas :id :board :width 100 :height 80 :focusable t
                                    :on-key (lambda (context event)
                                              (declare (ignore context))
                                              (push (list :key (core:key-event-logical-key event))
                                                    events)
                                              t)
                                    :on-text (lambda (context string)
                                               (declare (ignore context))
                                               (push (list :text string) events)
                                               t)
                                    :on-composition (lambda (context kind string start length)
                                                      (declare (ignore context))
                                                      (push (list :composition kind string start
                                                                  length)
                                                            events)
                                                      t))))))
    (click variant :board)
    (key variant #\Backspace)
    (text variant "x")
    (composition variant :started "に" 1 0)
    (composition variant :committed "日")
    (key variant #\Return)
    (assert-equal '((:key #\Backspace) (:text "x") (:composition :started "に" 1 0)
                    (:composition :committed "日" 0 0) (:key #\Return))
                  (reverse events))))

(define-test a-canvas-without-the-keyboard-gets-no-text ()
  (let* ((events '())
         (variant (show (lambda ()
                          (v:canvas :id :board :width 100 :height 80 :focusable t
                                    :on-text (lambda (string) (push string events) t))))))
    (text variant "x")
    (assert-equal '() events)))


;;; --- components that are told they changed, and described again ---

(define-test a-component-told-twice-before-a-frame-is-described-once ()
  (let* ((a (s:make-signal 1))
         (b (s:make-signal 1))
         (runs 0)
         (variant (show (lambda ()
                          (v:component (lambda ()
                                         (incf runs)
                                         (v:label (format nil "~a ~a" (s:value a) (s:value b)))))))))
    (assert-eql 1 runs)
    (setf (s:value a) 2
          (s:value b) 2)
    (build variant)
    (assert-eql 2 runs)
    (assert-equal "2 2" (glyphs (build variant)))
    ;; Nothing changed, so nothing is described.
    (assert-eql 2 runs)))

(define-test components-are-described-outermost-first-and-told-again-while-they-are ()
  ;; The outer component reads OUTER; the inner reads INNER. Both change before the frame: the
  ;; outer is described first, which makes a new inner (the old one is let go) that reads the
  ;; new value, and the let-go inner is not described.
  (let* ((outer (s:make-signal 1))
         (inner (s:make-signal 10))
         (order '())
         (variant (show (lambda ()
                          (v:padding
                           (v:component
                            (lambda ()
                              (push (list :outer (s:value outer)) order)
                              (v:column (v:component
                                         (lambda ()
                                           (push (list :inner (s:value inner)) order)
                                           (v:label (format nil "~a" (s:value inner)))))
                                        :key (s:value outer)))))))))
    (setf order '())
    (setf (s:value inner) 11
          (s:value outer) 2)
    (build variant)
    ;; The outer first. Its column has a new key, so the inner is a new one, made with 11, and
    ;; the old inner, which is no longer on the screen, is not described.
    (assert-equal '((:outer 2) (:inner 11)) (reverse order))
    (assert-equal "11" (glyphs (build variant)))))
