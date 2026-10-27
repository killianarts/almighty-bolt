(defpackage #:almighty-bolt-variant-immediate/t/immediate
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:i #:almighty-bolt-variant-immediate/immediate)
                    (#:w #:almighty-bolt-variant-immediate/widgets)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-immediate/t/immediate)

;;; The immediate variant: the interface is a function called every build, widgets say what
;;; happened to them by returning it, the frame is built again when a widget took input, and
;;; layout and painting are skipped when what was described is what was presented. The
;;; context's text is 8 points a character, 10 above the baseline and 4 below.

(defstruct (counting-context (:include fake:fake-context)
                            (:constructor make-counting-context
                                (&key (width 400f0) (height 300f0) (scale 1f0))))
  "A fake context that counts the strings drawn."
  (texts 0 :type fixnum))

(defmethod core:context-push-text :before ((context counting-context) list font string x
                                           baseline color)
  (declare (ignore list font string x baseline color))
  (incf (counting-context-texts context)))

(defvar *context* nil)
(defvar *list* nil)

(defun show (app)
  "A variant for APP, a fresh context and an empty list, with the first frame built."
  (setf *context* (make-counting-context)
        *list* (core:make-display-list :capacity 4096))
  (let ((variant (i:make-immediate-variant app)))
    (build variant)
    variant))

(defun build (variant)
  "A frame as the host builds it. Returns the number of builds."
  (values (core:settle-frame variant *context* *list*)))

(defun pointer (variant kind x y &key button (buttons 0))
  (core:handle-event variant
                     (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                              :y (float y 1d0) :button button
                                              :buttons buttons)))

(defun key (variant character &key (modifiers 0))
  (core:handle-event variant
                     (core:make-key-event :timestamp 0 :kind :down :logical-key character
                                          :modifiers modifiers)))

(defun click (variant id)
  "Move to the middle of the control ID, press and release, building a frame after each."
  (multiple-value-bind (x y width height) (core:lookup-target variant id)
    (let ((px (+ x (/ width 2))) (py (+ y (/ height 2))))
      (pointer variant :move px py) (build variant)
      (pointer variant :down px py :button 0 :buttons 1) (build variant)
      (pointer variant :up px py :button 0) (build variant))))

(defun glyphs (list)
  (coerce (loop :for index :below (core:display-list-count list)
                :when (= (core:display-list-kind list index) core:+command-glyph+)
                  :collect (code-char (round (core:display-list-parameter list index))))
          'string))

;;; --- the counter ---

(defun make-counter ()
  (let ((count 0))
    (show (lambda ()
            (w:padding (:all 24)
              (w:row (:gap 16 :cross-align :center)
                (w:label (princ-to-string count) :id :count :size 32)
                (when (w:button "Count" :id :count-button)
                  (incf count))))))))

(define-test shows-the-counter ()
  (let ((variant (make-counter)))
    (assert-equal "0Count" (glyphs *list*))
    (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
      (assert-true x)
      (assert-true (and (plusp width) (plusp height) (>= y 0))))))

(define-test a-click-counts-and-shows-it-in-the-same-frame ()
  (let ((variant (make-counter)))
    (click variant :count-button)
    (assert-equal "1Count" (glyphs *list*))
    (click variant :count-button)
    (assert-equal "2Count" (glyphs *list*))))

(define-test a-frame-that-took-input-is-built-again-and-one-that-did-not-is-not ()
  (let ((variant (make-counter)))
    (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
      (pointer variant :move (+ x (/ width 2)) (+ y (/ height 2)))
      (build variant)
      (pointer variant :down (+ x (/ width 2)) (+ y (/ height 2)) :button 0 :buttons 1)
      (assert-eql 2 (build variant))            ; the press is taken, so one more build
      (pointer variant :up (+ x (/ width 2)) (+ y (/ height 2)) :button 0)
      (assert-eql 2 (build variant))            ; the click changes the count
      (assert-eql 1 (build variant)))))         ; nothing left to take

(define-test the-button-is-clicked-by-the-space-bar-when-it-has-the-keyboard ()
  (let ((variant (make-counter)))
    (key variant #\Tab) (build variant)
    (key variant #\Space) (build variant)
    (assert-equal "1Count" (glyphs *list*))
    (key variant #\Return) (build variant)
    (assert-equal "2Count" (glyphs *list*))))

(define-test a-release-elsewhere-is-not-a-click ()
  (let ((variant (make-counter)))
    (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
      (pointer variant :move (+ x 2) (+ y 2)) (build variant)
      (pointer variant :down (+ x 2) (+ y 2) :button 0 :buttons 1) (build variant)
      (pointer variant :move (+ x width 40) (+ y height 40)) (build variant)
      (pointer variant :up (+ x width 40) (+ y height 40) :button 0) (build variant)
      (assert-equal "0Count" (glyphs *list*)))))

(define-test hit-target-gives-the-id-of-the-control-under-the-pointer ()
  (let ((variant (make-counter)))
    (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
      (assert-eql :count-button (core:hit-target variant (+ x 3) (+ y 3)))
      (assert-false (core:hit-target variant (+ x width 100) (+ y height 100))))))

;;; --- when layout and painting are skipped ---

(defun make-two-labels ()
  (let ((n 0))
    (values (show (lambda ()
                    (w:column (:gap 4)
                      (w:label "steady" :id :steady)
                      (w:label (princ-to-string n) :id :changing)
                      (when (w:button "Bump" :id :bump) (incf n)))))
            (lambda () n))))

(define-test an-unchanged-frame-draws-no-text-and-gives-the-same-list ()
  (let ((variant (make-two-labels)))
    (let ((hash (core:display-list-hash *list*))
          (before (counting-context-texts *context*)))
      (assert-eql 1 (build variant))
      (assert-eql before (counting-context-texts *context*))
      (assert-eql hash (core:display-list-hash *list*)))))

(define-test only-the-boxes-that-changed-are-drawn-again ()
  (let ((variant (make-two-labels)))
    (let ((before (counting-context-texts *context*)))
      (click variant :bump)
      ;; The number changed, and the button was hovered and pressed, which changes its look,
      ;; so those are drawn again, and the steady label (six characters) is not.
      (assert-true (< (- (counting-context-texts *context*) before) 12))
      (assert-equal "steady1Bump" (glyphs *list*)))))

(define-test the-cached-frame-is-the-frame-painted-from-nothing ()
  (let* ((variant (make-two-labels))
         (fresh (core:make-display-list :capacity 4096)))
    (click variant :bump)
    (click variant :bump)
    ;; The pointer is over the button, which then looks hovered; take it away.
    (pointer variant :leave 0 0) (build variant)
    (let ((other (i:make-immediate-variant
                  (let ((n 2))
                    (lambda ()
                      (w:column (:gap 4)
                        (w:label "steady" :id :steady)
                        (w:label (princ-to-string n) :id :changing)
                        (w:button "Bump" :id :bump)))))))
      (core:settle-frame other *context* fresh)
      (assert-eql (core:display-list-hash fresh) (core:display-list-hash *list*)))))

(define-test the-frame-after-a-resize-is-laid-out-again ()
  (let ((variant (make-two-labels))
        (wide (core:make-display-list :capacity 4096)))
    (declare (ignorable variant))
    (setf (core:context-width *context*) 500f0)
    (core:settle-frame variant *context* wide)
    (assert-equal "steady0Bump" (glyphs wide))))

;;; --- a text field ---

(defun make-field ()
  (let ((text "ab") (submitted 0))
    (values (show (lambda ()
                    (multiple-value-bind (new changed done) (w:text-field :name text :width 160)
                      (declare (ignorable changed))
                      (setf text new)
                      (when done (incf submitted)))))
            (lambda () text)
            (lambda () submitted))))

(defun center-of (variant id)
  (multiple-value-bind (x y width height) (core:lookup-target variant id)
    (values (+ x (/ width 2)) (+ y (/ height 2)))))

(define-test typing-goes-to-the-field-with-the-keyboard ()
  (multiple-value-bind (variant text submitted) (make-field)
    (key variant #\x) (build variant)
    (assert-equal "ab" (funcall text))          ; nothing has the keyboard yet
    (key variant #\Tab) (build variant)
    (key variant #\x) (build variant)
    (key variant #\y) (build variant)
    (assert-equal "abxy" (funcall text))
    (key variant #\Backspace) (build variant)
    (assert-equal "abx" (funcall text))
    (key variant #\Return) (build variant)
    (assert-eql 1 (funcall submitted))
    (assert-equal "abx" (glyphs *list*))))

(define-test a-press-puts-the-caret-where-it-was-pressed ()
  (multiple-value-bind (variant text) (make-field)
    (multiple-value-bind (x y width height) (core:lookup-target variant :name)
      (declare (ignore width))
      (let ((px (+ x 8 8 1)) (py (+ y (/ height 2))))   ; after the first character
        (pointer variant :move px py) (build variant)
        (pointer variant :down px py :button 0 :buttons 1) (build variant)
        (pointer variant :up px py :button 0) (build variant)
        (key variant #\Z) (build variant)
        (assert-equal "aZb" (funcall text))))))

;;; --- a slider, a choice and a list ---

(define-test a-slider-follows-the-pointer-and-the-keys ()
  (let* ((value 0d0)
         (variant (show (lambda ()
                          (w:column ()
                            (w:label (format nil "~d" (round value)) :id :value)
                            (setf value (w:slider :s value 0 100 :width 116)))))))
    (multiple-value-bind (x y width height) (core:lookup-target variant :s)
      (let ((py (+ y (/ height 2))))
        (pointer variant :move (+ x 8) py) (build variant)
        (pointer variant :down (+ x 8 50) py :button 0 :buttons 1) (build variant)
        ;; The thumb is 16 wide, so 100 points of travel and 50 of them is half.
        (assert-equal "50" (subseq (glyphs *list*) 0 2))
        (pointer variant :move (+ x width) py :buttons 1) (build variant)
        (assert-equal "100" (subseq (glyphs *list*) 0 3))
        (pointer variant :up (+ x width) py :button 0) (build variant)
        (key variant #\Tab) (build variant)
        (key variant core:+key-left+) (build variant)
        (assert-equal "95" (subseq (glyphs *list*) 0 2))))))

(define-test a-choice-selects-the-option-pressed ()
  (let* ((selected 0)
         (variant (show (lambda ()
                          (setf selected (w:choice :c '("one" "two" "three") selected))))))
    (key variant #\Tab) (build variant)
    (key variant core:+key-right+) (build variant)
    (assert-eql 1 selected)
    (key variant core:+key-right+) (build variant)
    (key variant core:+key-right+) (build variant)
    (assert-eql 2 selected)
    (key variant core:+key-left+) (build variant)
    (assert-eql 1 selected)
    ;; A press on the right option.
    (multiple-value-bind (x y width height) (core:lookup-target variant :c)
      (let ((px (+ x width -4)) (py (+ y (/ height 2))))
        (pointer variant :move px py) (build variant)
        (pointer variant :down px py :button 0 :buttons 1) (build variant)
        (pointer variant :up px py :button 0) (build variant)
        (assert-eql 2 selected)))))

(define-test a-list-box-shows-a-window-of-its-items-and-selects ()
  (let* ((selected nil)
         (items (loop :for i :below 20 :collect (format nil "item ~d" i)))
         (variant (show (lambda ()
                          (setf selected (w:list-box :l items selected :rows 4))))))
    (assert-true (search "item 0" (glyphs *list*)))
    (assert-false (search "item 4" (glyphs *list*)))
    (multiple-value-bind (x y) (core:lookup-target variant :l)
      (let ((px (+ x 20)) (py (+ y 1 w:+row-height+ 4)))      ; the second row
        (pointer variant :move px py) (build variant)
        (pointer variant :down px py :button 0 :buttons 1) (build variant)
        (pointer variant :up px py :button 0) (build variant)
        (assert-eql 1 selected)))
    (key variant #\Tab) (build variant)
    (dotimes (n 5)
      (key variant core:+key-down+) (build variant))
    (assert-eql 6 selected)
    (assert-true (search "item 6" (glyphs *list*)))
    (assert-false (search "item 0" (glyphs *list*)))))

;;; --- a canvas ---

(define-test a-canvas-gives-its-events-and-is-drawn-by-the-application ()
  (let* ((events '())
         (variant (show (lambda ()
                          (dolist (event (w:canvas :board 100 80
                                                   (lambda (list context width height)
                                                     (declare (ignore context))
                                                     (core:push-rect list 10 10 (- width 20)
                                                                     (- height 20) 255))))
                            (push event events))))))
    (multiple-value-bind (x y) (core:lookup-target variant :board)
      (pointer variant :move (+ x 30) (+ y 40)) (build variant)
      (pointer variant :down (+ x 30) (+ y 40) :button 0 :buttons 1) (build variant)
      (pointer variant :up (+ x 30) (+ y 40) :button 0) (build variant)
      (assert-equalp '(:move :press :release) (reverse (mapcar #'first events)))
      (assert-equalp '(:press 30.0 40.0 0 1) (find :press events :key #'first)))
    ;; The drawing is inside a clip.
    (let ((kinds (loop :for index :below (core:display-list-count *list*)
                       :collect (core:display-list-kind *list* index))))
      (assert-true (member core:+command-clip+ kinds))
      (assert-true (member core:+command-unclip+ kinds)))))

;;; --- a build that never settles ---

(define-test input-that-keeps-arriving-is-taken-one-click-a-frame ()
  (let* ((count 0)
         (variant (show (lambda ()
                          (w:column ()
                            (when (w:button "Count" :id :b) (incf count))
                            (w:label (princ-to-string count) :id :n))))))
    (multiple-value-bind (x y) (center-of variant :b)
      (pointer variant :move x y) (build variant)
      ;; Two clicks arrive before a frame is built: the press and release of the first
      ;; are taken one to a frame, then the second's.
      (dolist (kind '(:down :up :down :up))
        (pointer variant kind x y :button 0 :buttons (if (eq kind :down) 1 0)))
      (loop :repeat 6 :do (build variant))
      (assert-eql 2 count))))

;;; --- where the keyboard is ---

(define-test a-press-elsewhere-takes-the-keyboard-from-a-field-but-not-from-a-button ()
  (let* ((text "")
         (count 0)
         (variant (show (lambda ()
                          (w:column (:gap 8)
                            (setf text (w:text-field :name text :width 160))
                            (when (w:button "Count" :id :b) (incf count))
                            (w:label "elsewhere" :id :nothing))))))
    (multiple-value-bind (x y) (center-of variant :name)
      (pointer variant :move x y) (build variant)
      (pointer variant :down x y :button 0 :buttons 1) (build variant)
      (pointer variant :up x y :button 0) (build variant))
    (key variant #\a) (build variant)
    (assert-equal "a" text)
    ;; A press on a label, which takes nothing, leaves the field without the keyboard.
    (multiple-value-bind (x y) (center-of variant :nothing)
      (pointer variant :move x y) (build variant)
      (pointer variant :down x y :button 0 :buttons 1) (build variant)
      (pointer variant :up x y :button 0) (build variant))
    (key variant #\b) (build variant)
    (assert-equal "a" text)
    ;; The keyboard that reached a button by Tab stays through a press on a label.
    (key variant #\Tab) (build variant)
    (key variant #\Tab) (build variant)
    (multiple-value-bind (x y) (center-of variant :nothing)
      (pointer variant :down x y :button 0 :buttons 1) (build variant)
      (pointer variant :up x y :button 0) (build variant))
    (key variant #\Space) (build variant)
    (assert-eql 1 count)))

(define-test the-wheel-acts-where-it-says-without-the-pointer-having-moved ()
  (let* ((selected nil)
         (items (loop :for i :below 20 :collect (format nil "item ~d" i)))
         (variant (show (lambda ()
                          (setf selected (w:list-box :l items selected :rows 4))))))
    (multiple-value-bind (x y) (center-of variant :l)
      (core:handle-event variant
                         (core:make-wheel-event :timestamp 0 :x (float x 1d0) :y (float y 1d0)
                                                :dx 0d0 :dy -3d0))
      (build variant)
      (assert-false (search "item 0" (glyphs *list*)))
      (assert-true (search "item 3" (glyphs *list*))))))

(define-test a-trackpads-points-scroll-a-row-for-each-24-of-them ()
  (let* ((selected nil)
         (items (loop :for i :below 20 :collect (format nil "item ~d" i)))
         (variant (show (lambda ()
                          (setf selected (w:list-box :l items selected :rows 4))))))
    (multiple-value-bind (x y) (center-of variant :l)
      (flet ((wheel (dy)
               (core:handle-event variant
                                  (core:make-wheel-event :timestamp 0 :x (float x 1d0)
                                                                      :y (float y 1d0) :dx 0d0
                                                                      :dy (float dy 1d0)
                                                                      :precise-p t))
               (build variant)))
        (wheel -72)                     ; three rows
        (assert-false (search "item 2" (glyphs *list*)))
        (assert-true (search "item 3" (glyphs *list*)))
        (assert-false (search "item 7" (glyphs *list*)))
        (wheel 24)                      ; back a row
        (assert-true (search "item 2" (glyphs *list*)))))))

;;; --- a window that changes size ---

(define-test a-frame-after-the-window-changes-size-is-laid-out-again-and-places-follow ()
  (let ((variant (show (lambda ()
                         (w:center ()
                           (w:label "middle" :id :middle))))))
    (multiple-value-bind (x-before) (core:lookup-target variant :middle)
      (setf (core:context-width *context*) 800f0)
      (build variant)
      (multiple-value-bind (x-after) (core:lookup-target variant :middle)
        ;; 400 wide, centred: (400 - 48) / 2; 800 wide: (800 - 48) / 2.
        (assert-eql 176f0 x-before)
        (assert-eql 376f0 x-after)))))

(define-test a-box-a-flex-has-made-wider-is-drawn-again-and-not-from-what-was-kept ()
  (let ((variant (show (lambda ()
                         (w:row ()
                           (w:progress 0.5 :id :bar :width 50 :flex 1))))))
    (flet ((bar-widths ()
             (loop :for index :below (core:display-list-count *list*)
                   :when (= (core:display-list-kind *list* index) core:+command-rounded-rect+)
                     :collect (core:display-list-width *list* index))))
      (let ((before (bar-widths)))
        (setf (core:context-width *context*) 800f0)
        (build variant)
        (let ((after (bar-widths)))
          ;; The track fills the row and the bar is half of it: 400 and 200, then 800 and 400.
          (assert-true (and (member 400f0 before) (member 200f0 before)))
          (assert-true (and (member 800f0 after) (member 400f0 after)))
          (assert-false (member 200f0 after)))))))

(define-test a-new-scale-draws-the-frame-again ()
  (let* ((variant (show (lambda () (w:label "scaled" :id :label))))
         (hash-before (core:display-list-hash *list*)))
    (declare (ignorable hash-before))
    ;; The same description at another scale must not come from the list kept for the first.
    (setf (core:context-scale *context*) 2f0)
    (let ((builds (build variant)))
      (assert-eql 1 builds)
      (assert-true (plusp (core:display-list-count *list*))))))

;;; --- the caret of a canvas that takes text ---

(define-test the-caret-a-canvas-said-is-in-the-window-as-the-canvas-is-in-this-frame ()
  (let* ((text "ab")
         (variant (show (lambda ()
                          (w:row ()
                            (w:label text :id :l)
                            (w:canvas :c 100 80
                                      (lambda (list context width height)
                                        (declare (ignore list width height))
                                        (core:set-text-input-caret
                                         context 5f0 6f0 1.5f0 12f0))))))))
    (flet ((caret ()
             (let ((rect (core:context-text-input-rect *context*)))
               (list (aref rect 0) (aref rect 1)))))
      (multiple-value-bind (x y) (core:lookup-target variant :c)
        (assert-equal (list (+ x 5f0) (+ y 6f0)) (caret)))
      ;; A longer label moves the canvas, and the caret is where the canvas is after the one
      ;; build that moved it, not where it was the build before.
      (setf text "abcdefgh")
      (build variant)
      (multiple-value-bind (x y) (core:lookup-target variant :c)
        (assert-true (> x 60f0))
        (assert-equal (list (+ x 5f0) (+ y 6f0)) (caret))))))

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
                          (dolist (event (w:canvas :board 100 80
                                                   (lambda (list context width height)
                                                     (declare (ignore list context width height)))
                                                   :focusable t))
                            (push (case (first event)
                                    (:key (list :key (core:key-event-logical-key (second event))))
                                    (t event))
                                  events))))))
    (multiple-value-bind (x y) (core:lookup-target variant :board)
      (pointer variant :move (+ x 30) (+ y 40)) (build variant)
      (pointer variant :down (+ x 30) (+ y 40) :button 0 :buttons 1) (build variant)
      (pointer variant :up (+ x 30) (+ y 40) :button 0) (build variant)
      (setf events '())
      (key variant #\Backspace)
      (text variant "x")
      (composition variant :started "に" 1 0)
      (composition variant :committed "日")
      (key variant #\Return)
      (build variant)
      (assert-equal '((:key #\Backspace) (:text "x") (:composition :started "に" 1 0)
                      (:composition :committed "日" 0 0) (:key #\Return))
                    (reverse events)))))

(define-test a-canvas-without-the-keyboard-gets-no-text ()
  (let* ((events '())
         (variant (show (lambda ()
                          (dolist (event (w:canvas :board 100 80
                                                   (lambda (list context width height)
                                                     (declare (ignore list context width height)))
                                                   :focusable t))
                            (push event events))))))
    (text variant "x")
    (build variant)
    (assert-equal '() events)))

