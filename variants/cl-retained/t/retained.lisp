(defpackage #:almighty-bolt-variant-retained/t/retained
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:counter #:almighty-bolt-variant-retained/t/counter)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-retained/t/retained)

;;; The retained variant's frames and events, with a context whose text is 8 points
;;; a character. They run on any platform. The Counter is laid out as in the widget
;;; tests: the button is 68 by 28 at 48, 24, so its middle is at 82, 38, and the
;;; number is at 24, 31.

(defvar *label-layouts* 0
  "How many times a label has been laid out, to see when a frame does not lay out.")

(defmethod n:layout-node :before ((label w:label) context min-w max-w min-h max-h)
  (declare (ignore context min-w max-w min-h max-h))
  (incf *label-layouts*))

(defvar *context* nil)

(defun make-test-counter ()
  "The Counter, with a fresh context, and the number label and button."
  (setf *context* (fake:make-fake-context))
  (counter:make-counter))

(defun build (variant &optional (context *context*))
  "Build a frame of VARIANT and return its display list."
  (let ((list (core:make-display-list :capacity 256)))
    (core:frame variant context list)
    list))

(defun pointer (variant kind x y &key (button nil) (buttons 0))
  "Offer VARIANT a pointer event and return what it answers: true if it wants a frame."
  (core:handle-event variant
                     (core:make-pointer-event :timestamp 0 :kind kind
                                              :x (float x 1d0) :y (float y 1d0)
                                              :button button :buttons buttons)))

(defun key (variant logical-key &key (kind :down) (modifiers 0))
  (core:handle-event variant (core:make-key-event :timestamp 0 :kind kind
                                                  :logical-key logical-key
                                                  :modifiers modifiers)))

(defun click (variant x y)
  (pointer variant :move x y)
  (pointer variant :down x y :button 0 :buttons 1)
  (pointer variant :up x y :button 0))

(defparameter +button-x+ 82)
(defparameter +button-y+ 38)

;;; --- frames ---

(define-test the-first-frame-lays-out-and-paints ()
  (multiple-value-bind (variant number button) (make-test-counter)
    (declare (ignore number))
    (let ((list (build variant)))
      ;; A background, the number's glyph, the button and its five glyphs.
      (assert-eql 8 (core:display-list-count list))
      (assert-eql core:+command-rect+ (core:display-list-kind list 0))
      (assert-equal '(0f0 0f0 400f0 300f0)
                    (list (core:display-list-x list 0) (core:display-list-y list 0)
                          (core:display-list-width list 0) (core:display-list-height list 0)))
      (assert-equal '(48f0 24f0) (multiple-value-list (n:node-origin button))))))

(define-test a-frame-changes-no-state-so-it-settles-in-one-build ()
  (multiple-value-bind (variant) (make-test-counter)
    (assert-false (core:frame variant *context* (core:make-display-list :capacity 256)))))

(define-test a-frame-with-nothing-changed-does-not-lay-out-again ()
  (multiple-value-bind (variant) (make-test-counter)
    (let ((before *label-layouts*))
      (build variant)
      (assert-eql (+ before 2) *label-layouts*)  ; the number and the button's text
      (build variant)
      (build variant)
      (assert-eql (+ before 2) *label-layouts*))))

(define-test changing-a-labels-text-makes-the-next-frame-lay-out ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (let ((before *label-layouts*))
      (setf (w:label-text number) "10")
      ;; Nothing is laid out in between: the sizes are still the old ones.
      (assert-eql 8f0 (n:node-w number))
      (assert-eql before *label-layouts*)
      (build variant)
      (assert-eql (+ before 2) *label-layouts*)
      (assert-eql 16f0 (n:node-w number)))))

(define-test a-resized-window-lays-out-again ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    (let ((before *label-layouts*))
      (setf (core:context-width *context*) 500f0)
      (build variant)
      (assert-eql (+ before 2) *label-layouts*))))

;;; --- asking where things are ---

(define-test hit-target-answers-with-the-id-of-the-widget-under-the-point ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    ;; On the text of the button, which has no id, the answer is the button's.
    (assert-eql :count-button (core:hit-target variant 70f0 40f0))
    (assert-eql :count-button (core:hit-target variant 50f0 26f0))
    (assert-eql :count (core:hit-target variant 26f0 33f0))
    (assert-false (core:hit-target variant 300f0 200f0))))

(define-test lookup-target-answers-with-the-rectangle-of-a-widget ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    (assert-equal '(48f0 24f0 68f0 28f0)
                  (multiple-value-list (core:lookup-target variant :count-button)))
    (assert-equal '(24f0 31f0 8f0 14f0)
                  (multiple-value-list (core:lookup-target variant :count)))
    (assert-false (core:lookup-target variant :missing))))

(define-test hit-testing-uses-the-last-layout-not-the-latest-change ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (setf (w:label-text number) "1000000")
    ;; The number is now 56 wide, but until a frame lays that out, the button is
    ;; where the last frame put it.
    (assert-eql :count-button (core:hit-target variant 82f0 38f0))
    (assert-eql :count (core:hit-target variant 30f0 33f0))
    (assert-eql :count-button (core:hit-target variant 82f0 38f0))))

;;; --- the pointer ---

(define-test moving-onto-a-button-highlights-it-and-moving-off-undoes-it ()
  (multiple-value-bind (variant number button) (make-test-counter)
    (declare (ignore number))
    (build variant)
    (assert-true (pointer variant :move +button-x+ +button-y+))
    (assert-eql :hover (w:button-state button))
    (assert-true (eq button (retained:retained-variant-hover variant)))
    (assert-true (pointer variant :move 5 5))
    (assert-eql :normal (w:button-state button))
    (assert-false (retained:retained-variant-hover variant))))

(define-test a-move-that-changes-nothing-asks-for-no-frame ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    (pointer variant :move +button-x+ +button-y+)
    ;; Inside the same button, and then, once it has been left, over empty space.
    (assert-false (pointer variant :move (+ +button-x+ 3) (+ +button-y+ 2)))
    (assert-true (pointer variant :move 5 5))
    (assert-false (pointer variant :move 6 6))
    (assert-false (pointer variant :move 7 7))
    (assert-false (pointer variant :leave 7 7))))

(define-test the-look-of-a-button-is-painted-from-its-state ()
  (multiple-value-bind (variant number button) (make-test-counter)
    (declare (ignore number))
    (flet ((button-color ()
             (core:display-list-color (build variant) 2)))
      (let ((normal (button-color)))
        (pointer variant :move +button-x+ +button-y+)
        (let ((hover (button-color)))
          (pointer variant :down +button-x+ +button-y+ :button 0 :buttons 1)
          (let ((pressed (button-color)))
            (assert-eql :pressed (w:button-state button))
            (assert-eql 3 (length (remove-duplicates (list normal hover pressed))))))))))

(define-test pressing-and-releasing-on-a-button-clicks-it-once ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (assert-true (pointer variant :down +button-x+ +button-y+ :button 0 :buttons 1))
    (assert-equal "0" (w:label-text number))
    (assert-true (pointer variant :up +button-x+ +button-y+ :button 0))
    (assert-equal "1" (w:label-text number))
    (click variant +button-x+ +button-y+)
    (assert-equal "2" (w:label-text number))))

(define-test releasing-away-from-the-button-does-not-click ()
  (multiple-value-bind (variant number button) (make-test-counter)
    (build variant)
    (pointer variant :move +button-x+ +button-y+)
    (pointer variant :down +button-x+ +button-y+ :button 0 :buttons 1)
    ;; Dragged off, the button is no longer pressed to look at.
    (pointer variant :move 5 5 :buttons 1)
    (assert-eql :normal (w:button-state button))
    ;; Nothing visible changes when it goes up, so no frame is wanted.
    (assert-false (pointer variant :up 5 5 :button 0))
    (assert-equal "0" (w:label-text number))
    (assert-false (retained:retained-variant-pressed variant))))

(define-test dragging-off-and-back-on-still-clicks ()
  (multiple-value-bind (variant number button) (make-test-counter)
    (build variant)
    (pointer variant :down +button-x+ +button-y+ :button 0 :buttons 1)
    (pointer variant :move 5 5 :buttons 1)
    (pointer variant :move +button-x+ +button-y+ :buttons 1)
    (assert-eql :pressed (w:button-state button))
    (pointer variant :up +button-x+ +button-y+ :button 0)
    (assert-equal "1" (w:label-text number))))

(define-test pressing-on-nothing-does-nothing ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (assert-false (pointer variant :down 300 200 :button 0 :buttons 1))
    (assert-false (pointer variant :up 300 200 :button 0))
    ;; Nor does pressing on the number.
    (assert-false (pointer variant :down 26 33 :button 0 :buttons 1))
    (assert-false (pointer variant :up 26 33 :button 0))
    (assert-equal "0" (w:label-text number))))

(define-test only-the-left-button-clicks ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (pointer variant :move +button-x+ +button-y+)
    (assert-false (pointer variant :down +button-x+ +button-y+ :button 1 :buttons 2))
    (assert-false (pointer variant :up +button-x+ +button-y+ :button 1))
    (assert-equal "0" (w:label-text number))))

(define-test a-click-that-widens-the-number-moves-the-button-in-the-next-frame ()
  (multiple-value-bind (variant number button) (make-test-counter)
    (build variant)
    (dotimes (i 10)
      (click variant +button-x+ +button-y+))
    (assert-equal "10" (w:label-text number))
    ;; Not yet: nothing laid it out.
    (assert-eql 24f0 (n:node-x button))
    (build variant)
    (assert-eql 32f0 (n:node-x button))))

(define-test events-that-are-not-pointer-or-key-events-ask-for-nothing ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    (assert-false (core:handle-event variant (core:make-focus-event :timestamp 0 :focused-p t)))
    (assert-false (core:handle-event variant (core:make-resize-event :timestamp 0 :width 1d0
                                                                      :height 1d0)))))

;;; --- the keyboard focus ---

(defun two-buttons ()
  "Two buttons side by side. Returns the variant, the buttons and a list that
collects the names of the buttons clicked."
  (setf *context* (fake:make-fake-context))
  (let* ((clicked '())
         (first (w:button "One" :id :one :on-click (lambda () (push :one clicked))))
         (second (w:button "Two" :id :two :on-click (lambda () (push :two clicked))))
         (variant (retained:make-retained-variant (w:row first second :gap 8))))
    (build variant)
    (values variant first second (lambda () (reverse clicked)))))

(define-test tab-moves-the-focus-along-the-buttons-and-round ()
  (multiple-value-bind (variant first second) (two-buttons)
    (assert-true (key variant #\Tab))
    (assert-true (eq first (retained:retained-variant-focus variant)))
    (assert-true (w:button-focused first))
    (key variant #\Tab)
    (assert-true (eq second (retained:retained-variant-focus variant)))
    (assert-false (w:button-focused first))
    (key variant #\Tab)
    (assert-true (eq first (retained:retained-variant-focus variant)))))

(define-test shift-tab-moves-the-focus-the-other-way ()
  (multiple-value-bind (variant first second) (two-buttons)
    (key variant #\Tab :modifiers core:+modifier-shift+)
    (assert-true (eq second (retained:retained-variant-focus variant)))
    (key variant #\Tab :modifiers core:+modifier-shift+)
    (assert-true (eq first (retained:retained-variant-focus variant)))))

(define-test the-space-bar-and-return-click-the-focused-button ()
  (multiple-value-bind (variant first second clicked) (two-buttons)
    (declare (ignore first second))
    (key variant #\Space)                       ; nothing is focused yet
    (key variant #\Tab)
    (key variant #\Space)
    (key variant #\Tab)
    (key variant #\Return)
    (key variant #\Space :kind :up)             ; only a key going down counts
    (assert-equal '(:one :two) (funcall clicked))))

(define-test a-focused-button-is-painted-with-a-ring ()
  (multiple-value-bind (variant) (two-buttons)
    (let ((without (core:display-list-count (build variant))))
      (key variant #\Tab)
      (let ((list (build variant)))
        (assert-eql (1+ without) (core:display-list-count list))
        ;; The ring is drawn first and is larger than the button.
        (assert-eql core:+command-rounded-rect+ (core:display-list-kind list 1))
        (assert-eql (+ 4f0 (core:display-list-width list 2)) (core:display-list-width list 1))))))
