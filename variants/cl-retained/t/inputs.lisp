(defpackage #:almighty-bolt-variant-retained/t/inputs
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:i #:almighty-bolt-variant-retained/inputs)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-retained/t/inputs)

;;; The input widgets, driven by synthetic events through the variant, with a context
;;; whose text is 8 points a character, 10 above the baseline and 4 below.

(defvar *context* nil)

(defun show (root &key (width 400f0) (height 300f0))
  "A variant showing ROOT, with one frame built. Returns the variant and the list."
  (setf *context* (fake:make-fake-context :width width :height height))
  (let* ((variant (retained:make-retained-variant root))
         (list (core:make-display-list :capacity 2048)))
    (core:frame variant *context* list)
    (values variant list)))

(defun build (variant)
  (let ((list (core:make-display-list :capacity 2048)))
    (core:frame variant *context* list)
    list))

(defun pointer (variant kind x y &key (button nil) (buttons 0))
  (core:handle-event variant
                     (core:make-pointer-event :timestamp 0 :kind kind
                                              :x (float x 1d0) :y (float y 1d0)
                                              :button button :buttons buttons)))

(defun press (variant x y)
  (pointer variant :down x y :button 0 :buttons 1))

(defun release (variant x y)
  (pointer variant :up x y :button 0))

(defun key (variant character &key (modifiers 0))
  (core:handle-event variant (core:make-key-event :timestamp 0 :kind :down
                                                  :logical-key character :modifiers modifiers)))

(defun type-text (variant string)
  (loop :for character :across string :do (key variant character)))

(defun glyphs (list)
  "The characters that LIST draws, in order."
  (coerce (loop :for index :below (core:display-list-count list)
                :when (= (core:display-list-kind list index) core:+command-glyph+)
                  :collect (code-char (round (core:display-list-parameter list index))))
          'string))

;;; --- a text field ---

(defun two-fields ()
  (let* ((changes '())
         (a (i:text-field :id :a :width 120 :on-change (lambda (text) (push (list :a text) changes))))
         (b (i:text-field :id :b :width 120))
         (root (w:column a b :gap 10)))
    (multiple-value-bind (variant list) (show root)
      (values variant a b (lambda () (reverse changes)) list))))

(define-test a-text-field-is-as-wide-as-asked-and-a-line-and-a-bit-high ()
  (multiple-value-bind (variant a b) (two-fields)
    (declare (ignore variant))
    (assert-equal '(0f0 0f0 120f0 26f0) (list (n:node-x a) (n:node-y a) (n:node-w a) (n:node-h a)))
    (assert-equal '(0f0 36f0 120f0 26f0) (list (n:node-x b) (n:node-y b) (n:node-w b) (n:node-h b)))))

(define-test typing-goes-to-the-focused-field-and-reports-each-change ()
  (multiple-value-bind (variant a b changes) (two-fields)
    (key variant #\Tab)
    (assert-true (eq a (retained:retained-variant-focus variant)))
    (type-text variant "abc")
    (assert-equal "abc" (i:text-field-text a))
    (assert-equal "" (i:text-field-text b))
    (assert-equal '((:a "a") (:a "ab") (:a "abc")) (funcall changes))))

(define-test the-keys-edit-the-text-and-move-the-caret ()
  (multiple-value-bind (variant a) (two-fields)
    (key variant #\Tab)
    (type-text variant "abcd")
    (key variant #\Backspace)
    (assert-equal "abc" (i:text-field-text a))
    (key variant core:+key-left+)
    (key variant core:+key-left+)
    (key variant #\X)
    (assert-equal "aXbc" (i:text-field-text a))
    (key variant core:+key-delete-forward+)
    (assert-equal "aXc" (i:text-field-text a))
    (key variant core:+key-left+ :modifiers core:+modifier-shift+)
    (assert-equal 1 (core:text-state-selection-start (i:text-field-state a)))
    (assert-equal 2 (core:text-state-selection-end (i:text-field-state a)))))

(define-test a-key-that-edits-asks-for-a-frame-and-one-that-does-not-edit-does-not ()
  (multiple-value-bind (variant) (two-fields)
    (key variant #\Tab)
    (assert-true (key variant #\a))
    (assert-false (key variant core:+key-up+))
    ;; Moving the caret that is already at the end asks for nothing.
    (assert-false (key variant core:+key-right+))))

(define-test pressing-on-a-field-focuses-it-and-puts-the-caret-where-it-was-pressed ()
  (multiple-value-bind (variant a b) (two-fields)
    (setf (i:text-field-text b) "hello")
    (build variant)
    ;; x of 8 is the start of the text and each letter is 8 wide: 8 + 3 * 8 + 2 is in "l".
    (press variant 34 49)
    (assert-true (eq b (retained:retained-variant-focus variant)))
    (assert-eql 3 (core:text-state-caret (i:text-field-state b)))
    (release variant 34 49)
    (key variant #\!)
    (assert-equal "hel!lo" (i:text-field-text b))
    (assert-false (n:node-focused a))))

(define-test dragging-over-the-text-selects-it ()
  (multiple-value-bind (variant a) (two-fields)
    (setf (i:text-field-text a) "abcdef")
    (build variant)
    (press variant 8 10)
    (pointer variant :move 8 10 :buttons 1)
    (pointer variant :move 33 10 :buttons 1)
    (release variant 33 10)
    (assert-equal "abc" (core:text-state-selected-string (i:text-field-state a)))))

(define-test pressing-elsewhere-takes-the-keyboard-from-a-field ()
  (multiple-value-bind (variant a) (two-fields)
    (press variant 20 10)
    (release variant 20 10)
    (assert-true (eq a (retained:retained-variant-focus variant)))
    (press variant 300 250)
    (assert-false (retained:retained-variant-focus variant))
    (assert-false (n:node-focused a))
    ;; Typing now goes nowhere.
    (assert-false (key variant #\z))
    (assert-equal "" (i:text-field-text a))))

(define-test the-program-can-set-the-text-without-the-handler-being-called ()
  (multiple-value-bind (variant a b changes) (two-fields)
    (declare (ignore b))
    (setf (i:text-field-text a) "set")
    (build variant)
    (assert-equal "set" (i:text-field-text a))
    (assert-equal '() (funcall changes))
    ;; The caret is at the end, so typing adds to it.
    (press variant 20 10)
    (key variant core:+key-end+)
    (key variant #\!)
    (assert-equal "set!" (i:text-field-text a))))

(define-test return-submits-the-text-and-tab-moves-on ()
  (let* ((submitted '())
         (a (i:text-field :id :a :on-submit (lambda (text) (push text submitted))))
         (b (i:text-field :id :b)))
    (let ((variant (show (w:column a b))))
      (key variant #\Tab)
      (type-text variant "hi")
      (assert-false (key variant #\Return))     ; nothing to see changed
      (assert-equal '("hi") submitted)
      (key variant #\Tab)
      (assert-true (eq b (retained:retained-variant-focus variant))))))

(define-test an-invalid-field-is-painted-red-and-a-disabled-one-is-dimmed ()
  (multiple-value-bind (variant a) (two-fields)
    (flet ((fill-color ()
             ;; The ring, if there is one, comes first; then the border and the fill.
             (core:display-list-color (build variant) 2)))
      (let ((normal (fill-color)))
        (setf (i:text-field-invalid a) t)
        (let ((invalid (fill-color)))
          (assert-true (/= normal invalid))
          (setf (i:text-field-invalid a) nil)
          (setf (n:node-enabled a) nil)
          (let ((disabled (fill-color)))
            (assert-true (/= normal disabled))
            (assert-true (/= invalid disabled))))))))

(define-test a-disabled-field-takes-no-pointer-or-keys ()
  (multiple-value-bind (variant a) (two-fields)
    (setf (n:node-enabled a) nil)
    (build variant)
    (assert-false (press variant 20 10))
    (assert-false (retained:retained-variant-focus variant))
    (key variant #\Tab)
    ;; Tab skips it, to the other one.
    (assert-false (eq a (retained:retained-variant-focus variant)))))

(define-test the-field-draws-its-text-and-a-caret-only-when-it-has-the-keyboard ()
  (multiple-value-bind (variant a) (two-fields)
    (setf (i:text-field-text a) "abc")
    (let ((without (build variant)))
      (assert-equal "abc" (glyphs without))
      (key variant #\Tab)
      (let ((with (build variant)))
        ;; A ring, and the caret, which are two commands more.
        (assert-eql (+ 2 (core:display-list-count without)) (core:display-list-count with))))))

(define-test text-longer-than-the-field-scrolls-to-keep-the-caret-in-view ()
  (multiple-value-bind (variant a) (two-fields)
    (key variant #\Tab)
    (type-text variant "abcdefghijklmnopqrst")
    (let ((list (build variant)))
      ;; 104 points of room is 13 characters, and the end is what is shown.
      (assert-eql 13 (length (glyphs list)))
      (assert-equal "hijklmnopqrst" (subseq (glyphs list) 0 13) )
      (assert-equal 20 (length (i:text-field-text a))))))

;;; --- a slider ---

(defun a-slider (&key (value 0) on-change)
  (let ((slider (i:slider :id :s :minimum 0 :maximum 100 :value value :width 216
                          :on-change on-change)))
    (multiple-value-bind (variant) (show (w:column slider))
      (values variant slider))))

(define-test pressing-a-slider-moves-it-there-and-dragging-follows-the-pointer ()
  (let ((seen '()))
    (multiple-value-bind (variant slider) (a-slider :on-change (lambda (v) (push v seen)))
      ;; The thumb is 16 wide, so its middle moves between 8 and 208: 200 points.
      (press variant 108 12)
      (assert-eql 50d0 (i:slider-value slider))
      (pointer variant :move 158 12 :buttons 1)
      (assert-eql 75d0 (i:slider-value slider))
      ;; Dragged past an end, it stops at the end, even outside the slider.
      (pointer variant :move 900 300 :buttons 1)
      (assert-eql 100d0 (i:slider-value slider))
      (release variant 900 300)
      (assert-equal '(100d0 75d0 50d0) seen))))

(define-test the-arrow-keys-and-home-and-end-move-a-focused-slider ()
  (multiple-value-bind (variant slider) (a-slider :value 50)
    (key variant #\Tab)
    (key variant core:+key-right+)
    (assert-eql 55d0 (i:slider-value slider))
    (key variant core:+key-left+)
    (key variant core:+key-left+)
    (assert-eql 45d0 (i:slider-value slider))
    (key variant core:+key-end+)
    (assert-eql 100d0 (i:slider-value slider))
    (assert-false (key variant core:+key-right+))     ; already at the end: no change
    (key variant core:+key-home+)
    (assert-eql 0d0 (i:slider-value slider))))

(define-test the-program-moves-a-slider-without-the-handler-and-within-its-ends ()
  (let ((seen '()))
    (multiple-value-bind (variant slider) (a-slider :on-change (lambda (v) (push v seen)))
      (declare (ignore variant))
      (setf (i:slider-value slider) 250)
      (assert-eql 100d0 (i:slider-value slider))
      (setf (i:slider-value slider) -3)
      (assert-eql 0d0 (i:slider-value slider))
      (assert-equal '() seen))))

(define-test a-slider-draws-its-thumb-where-the-value-is ()
  (multiple-value-bind (variant) (a-slider :value 50)
    (let* ((list (build variant))
           (count (core:display-list-count list))
           (thumb (1- count)))
      ;; The thumb's left edge: 8 + 100 - 8.
      (assert-eql 100f0 (core:display-list-x list thumb))
      (assert-eql 16f0 (core:display-list-width list thumb)))))

;;; --- a choice ---

(defun a-choice (&key on-change)
  (let ((choice (i:choice '("one-way" "return") :id :c :on-change on-change)))
    (multiple-value-bind (variant) (show (w:column choice))
      (values variant choice))))

(define-test options-sit-side-by-side-each-with-a-dot-and-a-label ()
  (multiple-value-bind (variant choice) (a-choice)
    (declare (ignore variant))
    ;; "one-way" is 56 wide and "return" 48: 16 + 6 + 56 = 78, a gap of 20, and 16 + 6 + 48.
    (assert-equal '(0f0 98f0) (coerce (i:choice-lefts choice) 'list))
    (assert-eql 168f0 (n:node-w choice))))

(defun choice-lefts-of (choice)
  (coerce (i:choice-lefts choice) 'list))

(define-test clicking-an-option-chooses-it-once ()
  (let ((chosen '()))
    (multiple-value-bind (variant choice) (a-choice :on-change (lambda (i) (push i chosen)))
      (assert-eql 0 (i:choice-selected choice))
      (press variant 120 10)
      (release variant 120 10)
      (assert-eql 1 (i:choice-selected choice))
      (press variant 120 10)
      (release variant 120 10)
      (press variant 40 10)
      (release variant 40 10)
      (assert-eql 0 (i:choice-selected choice))
      (assert-equal '(0 1) chosen))))

(define-test the-arrows-change-the-choice-when-it-has-the-keyboard ()
  (multiple-value-bind (variant choice) (a-choice)
    (key variant #\Tab)
    (key variant core:+key-right+)
    (assert-eql 1 (i:choice-selected choice))
    (assert-false (key variant core:+key-right+))      ; no option after the last
    (key variant core:+key-left+)
    (assert-eql 0 (i:choice-selected choice))))

;;; --- a list box ---

(defun a-list (count &key on-select (rows 4))
  (let ((list-box (i:list-box :id :l :rows rows :width 150
                              :items (loop :for k :below count :collect (format nil "item ~d" k))
                              :on-select on-select)))
    (multiple-value-bind (variant list) (show (w:column list-box))
      (values variant list-box list))))

(define-test a-list-shows-its-rows-and-no-more ()
  (multiple-value-bind (variant list-box list) (a-list 10)
    (declare (ignore variant))
    (assert-eql 98f0 (n:node-h list-box))               ; 4 rows of 24, and the border
    (assert-eql 4 (count #\m (glyphs list)))            ; "item" has one m, four rows
    (assert-true (search "item 0" (glyphs list)))
    (assert-false (search "item 4" (glyphs list)))))

(define-test pressing-a-row-picks-it ()
  (let ((picked '()))
    (multiple-value-bind (variant list-box) (a-list 10 :on-select (lambda (i) (push i picked)))
      (press variant 40 1)
      (release variant 40 1)
      (assert-eql 0 (i:list-box-selected list-box))
      (press variant 40 60)
      (assert-eql 2 (i:list-box-selected list-box))
      ;; Pressing the picked row again picks nothing new.
      (press variant 40 60)
      (assert-equal '(2 0) picked))))

(define-test the-wheel-scrolls-a-row-at-a-time-and-stops-at-the-ends ()
  (multiple-value-bind (variant list-box) (a-list 10)
    (flet ((wheel (dy)
             (core:handle-event variant (core:make-wheel-event :timestamp 0 :x 40d0 :y 40d0
                                                               :dx 0d0 :dy (float dy 1d0)))))
      (wheel -2)
      (assert-eql 2 (i:list-box-top list-box))
      (wheel -100)
      (assert-eql 6 (i:list-box-top list-box))           ; 10 items, 4 rows: top at most 6
      (wheel 3)
      (assert-eql 3 (i:list-box-top list-box))
      (wheel 100)
      (assert-eql 0 (i:list-box-top list-box)))))

(define-test a-trackpads-points-scroll-a-row-for-each-24-of-them ()
  (multiple-value-bind (variant list-box) (a-list 10)
    (flet ((wheel (dy)
             (core:handle-event variant (core:make-wheel-event :timestamp 0 :x 40d0 :y 40d0
                                                               :dx 0d0 :dy (float dy 1d0)
                                                               :precise-p t))))
      (wheel -48)
      (assert-eql 2 (i:list-box-top list-box))
      ;; Small steps add up and are not lost: three of 10 points are a row and 6 points over.
      (dotimes (k 3) (wheel -10))
      (assert-eql 3 (i:list-box-top list-box))
      ;; Going back, the 6 points over count: 6 - 24 is not yet a row, 6 - 48 is one.
      (wheel 24)
      (assert-eql 3 (i:list-box-top list-box))
      (wheel 24)
      (assert-eql 2 (i:list-box-top list-box)))))

(define-test the-arrow-keys-move-the-pick-and-scroll-to-keep-it-in-view ()
  (multiple-value-bind (variant list-box) (a-list 10)
    (press variant 40 1)
    (release variant 40 1)
    (dotimes (k 5)
      (key variant core:+key-down+))
    (assert-eql 5 (i:list-box-selected list-box))
    (assert-eql 2 (i:list-box-top list-box))
    (key variant core:+key-up+)
    (assert-eql 4 (i:list-box-selected list-box))
    (assert-eql 2 (i:list-box-top list-box))))

(define-test the-program-can-replace-the-items-and-the-pick ()
  (multiple-value-bind (variant list-box) (a-list 10)
    (declare (ignore variant))
    (setf (i:list-box-selected list-box) 9)
    (assert-eql 6 (i:list-box-top list-box))
    (setf (i:list-box-items list-box) '("a" "b"))
    (assert-false (i:list-box-selected list-box))
    (assert-eql 0 (i:list-box-top list-box))
    (assert-equal '("a" "b") (coerce (i:list-box-items list-box) 'list))))

;;; --- a canvas ---

(define-test a-canvas-is-painted-by-the-app-and-gets-the-events-in-its-own-coordinates ()
  (let* ((events '())
         (painted 0)
         (canvas (i:canvas :id :c :width 100 :height 80 :focusable t
                           :paint (lambda (canvas context list x y w h)
                                    (declare (ignore canvas context))
                                    (incf painted)
                                    (core:push-rect list x y w h #xFF0000FF))
                           :on-press (lambda (canvas x y button click-count)
                                       (declare (ignore canvas))
                                       (push (list :press x y button click-count) events))
                           :on-drag (lambda (canvas x y)
                                      (declare (ignore canvas))
                                      (push (list :drag x y) events))
                           :on-release (lambda (canvas x y inside)
                                         (declare (ignore canvas))
                                         (push (list :release x y inside) events))
                           :on-key (lambda (canvas event)
                                     (declare (ignore canvas))
                                     (push (list :key (core:key-event-logical-key event)) events)
                                     t))))
    (let ((variant (show (w:padding canvas :all 10))))
      (assert-eql 1 painted)
      (build variant)
      (assert-eql 1 painted)                           ; kept until it is invalidated
      (press variant 30 40)
      (pointer variant :move 50 50 :buttons 1)
      (release variant 500 500)
      (key variant #\q)
      (assert-equal '((:press 20f0 30f0 0 1) (:drag 40f0 40f0) (:release 490f0 490f0 nil) (:key #\q))
                    (reverse events))
      (i:canvas-invalidate canvas)
      (build variant)
      (assert-eql 2 painted))))

;;; --- the caret of a canvas that takes text ---

(defun settle (variant)
  (core:settle-frame variant *context* (core:make-display-list :capacity 2048)))

(defun caret-in-the-window ()
  (let ((rect (core:context-text-input-rect *context*)))
    (list (aref rect 0) (aref rect 1))))

(define-test the-caret-a-canvas-said-follows-it-when-it-moves-and-is-not-painted-again ()
  (let* ((painted 0)
         (label (w:label "ab" :id :l))
         (canvas (i:canvas :id :c :width 100 :height 80
                           :paint (lambda (canvas context list x y w h)
                                    (declare (ignore canvas list x y w h))
                                    (incf painted)
                                    (core:set-text-input-caret context 5f0 6f0 1.5f0 12f0))))
         (variant (show (w:row label canvas :gap 10))))
    (settle variant)
    (multiple-value-bind (x y) (core:lookup-target variant :c)
      (assert-equal (list (+ x 5f0) (+ y 6f0)) (caret-in-the-window)))
    (assert-eql 1 painted)
    ;; A longer label moves the canvas to the right. The canvas is kept, and drawn there.
    (setf (w:label-text label) "abcdefgh")
    (settle variant)
    (assert-eql 1 painted)
    (multiple-value-bind (x y) (core:lookup-target variant :c)
      (assert-true (> x 60f0))
      (assert-equal (list (+ x 5f0) (+ y 6f0)) (caret-in-the-window)))))

;;; --- text and composition for a canvas that has the keyboard ---

(defun text (variant string)
  (core:handle-event variant (core:make-text-input-event :timestamp 0 :text string)))

(defun composition (variant kind text &optional (start 0) (length 0))
  (core:handle-event variant (core:make-composition-event :timestamp 0 :kind kind :text text
                                                          :selection-start start
                                                          :selection-length length)))

(define-test a-canvas-with-the-keyboard-gets-keys-text-and-composition-in-order ()
  (let* ((events '())
         (canvas (i:canvas :id :c :width 100 :height 80 :focusable t
                           :on-key (lambda (canvas event)
                                     (declare (ignore canvas))
                                     (push (list :key (core:key-event-logical-key event)) events)
                                     t)
                           :on-text (lambda (canvas string)
                                      (declare (ignore canvas))
                                      (push (list :text string) events)
                                      t)
                           :on-composition (lambda (canvas kind string start length)
                                             (declare (ignore canvas))
                                             (push (list :composition kind string start length)
                                                   events)
                                             t)))
         (variant (show (w:padding canvas :all 10))))
    (press variant 30 40)
    (release variant 30 40)
    (key variant #\Backspace)
    (text variant "x")
    (composition variant :started "に" 1 0)
    (composition variant :changed "にほ" 2 0)
    (composition variant :committed "日")
    (key variant #\Return)
    (assert-equal '((:key #\Backspace) (:text "x") (:composition :started "に" 1 0)
                    (:composition :changed "にほ" 2 0) (:composition :committed "日" 0 0)
                    (:key #\Return))
                  (reverse events))))

(define-test a-canvas-without-the-keyboard-gets-no-text ()
  (let* ((events '())
         (canvas (i:canvas :id :c :width 100 :height 80 :focusable t
                           :on-text (lambda (canvas string)
                                      (declare (ignore canvas))
                                      (push string events)
                                      t)))
         (variant (show (w:padding canvas :all 10))))
    (text variant "x")
    (composition variant :started "に")
    (assert-equal '() events)))

(define-test a-canvas-that-takes-no-text-does-not-take-it ()
  (let* ((canvas (i:canvas :id :c :width 100 :height 80 :focusable t))
         (variant (show (w:padding canvas :all 10))))
    (press variant 30 40)
    (assert-false (text variant "x"))))

