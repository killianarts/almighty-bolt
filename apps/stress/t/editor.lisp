(defpackage #:almighty-bolt-stress/t/editor
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-stress/editor/model)
                    (#:p #:almighty-bolt-stress/editor/paint)
                    (#:c #:almighty-bolt-stress/editor/control)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-stress/t/editor)

;;; The text editor's document (a gap buffer with an index of lines), what the events do to it, and
;;; the painter (with the context whose text is 8 points a character).

(defun doc (string &key (caret 0) (anchor caret))
  (let ((document (m:make-document string)))
    (m:move-caret document caret)
    (setf (m:document-anchor document) anchor)
    document))

(defun key (character &key (modifiers 0))
  (core:make-key-event :timestamp 0 :kind :down :logical-key character :modifiers modifiers))

;;; --- the text made to edit ---

(define-test the-text-to-edit-is-the-same-every-time-and-fits ()
  (let* ((text (m:generate-text 300))
         (lines (loop :with start = 0
                      :for end = (position #\Newline text :start start)
                      :collect (subseq text start end)
                      :while end
                      :do (setf start (1+ end)))))
    (assert-equal text (m:generate-text 300))
    (assert-eql 300 (length lines))
    (assert-true (every (lambda (line) (<= (length line) 80)) lines))
    (assert-equal "" (first lines))
    (assert-equal "" (nth 13 lines))
    (assert-true (plusp (length (nth 1 lines))))
    (assert-true (> (length (nth 29 lines)) 40))))

;;; --- lines ---

(define-test lines-begin-after-each-newline-and-the-last-has-no-newline-after-it ()
  (let ((document (doc (format nil "ab~%~%cde"))))
    (assert-eql 3 (m:document-line-count document))
    (assert-equal '(0 3 4) (list (m:line-start document 0) (m:line-start document 1)
                                 (m:line-start document 2)))
    (assert-equal '(2 3 7) (list (m:line-end document 0) (m:line-end document 1)
                                 (m:line-end document 2)))
    (assert-equal "" (m:line-string document 1))
    (assert-equal "cde" (m:line-string document 2))
    (assert-equal '(0 0 0 1 2 2 2 2) (loop :for position :to 7 :collect (m:position-line document position)))
    (assert-eql 2 (m:position-column document 6))))

(define-test an-empty-document-has-one-line ()
  (let ((document (doc "")))
    (assert-eql 1 (m:document-line-count document))
    (assert-eql 0 (m:document-length document))
    (assert-equal "" (m:line-string document 0))))

;;; --- editing ---

(define-test the-document-is-what-a-string-with-the-same-edits-would-be ()
  ;; A fixed sequence of inserts and deletes at all sorts of places, checked after each against
  ;; plain strings and against an index of lines made again from the string.
  (let ((document (doc (m:generate-text 40)))
        (model (m:generate-text 40))
        (state 7))
    (flet ((next (limit)
             (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
             (mod (ash state -8) limit)))
      (dotimes (step 600)
        (let* ((length (length model))
               (start (next (1+ length)))
               (end (min length (+ start (if (zerop (next 3)) (next 12) 0))))
               (string (case (next 4)
                         (0 "") (1 (string #\Newline)) (2 "ab") (t (format nil "x~%yz~%")))))
          (setf (m:document-anchor document) start
                (m:document-caret document) end)
          (m:insert-text document string)
          (setf model (concatenate 'string (subseq model 0 start) string (subseq model end)))
          (unless (string= model (m:document-string document))
            (error "Step ~d: ~s is not ~s." step (m:document-string document) model))
          (let ((fresh (m:make-document model)))
            (assert-eql (m:document-line-count fresh) (m:document-line-count document))
            (dotimes (line (m:document-line-count fresh))
              (unless (= (m:line-start fresh line) (m:line-start document line))
                (error "Step ~d: line ~d begins at ~d, not ~d." step line
                       (m:line-start document line) (m:line-start fresh line))))))))))

(define-test typing-replaces-the-selection-and-leaves-the-caret-after-it ()
  (let ((document (doc "hello world" :caret 5 :anchor 0)))
    (m:insert-text document "howdy")
    (assert-equal "howdy world" (m:document-string document))
    (assert-eql 5 (m:document-caret document))
    (assert-false (m:selection-p document))))

(define-test backspace-and-forward-delete-take-the-selection-or-a-character ()
  (let ((document (doc "abcdef" :caret 3)))
    (m:delete-backward document)
    (assert-equal "abdef" (m:document-string document))
    (assert-eql 2 (m:document-caret document))
    (m:delete-forward document)
    (assert-equal "abef" (m:document-string document))
    (setf (m:document-anchor document) 0)
    (m:delete-backward document)
    (assert-equal "ef" (m:document-string document))
    (assert-false (m:delete-backward document))
    (setf (m:document-caret document) 2 (m:document-anchor document) 2)
    (assert-false (m:delete-forward document))
    (assert-equal "ef" (m:document-string document))))

(define-test the-caret-stays-in-its-column-through-short-lines ()
  (let ((document (doc (format nil "abcdef~%ab~%abcdef") :caret 5)))
    (m:move-vertically document 1)
    (assert-eql 9 (m:document-caret document))          ; the end of "ab"
    (m:move-vertically document 1)
    (assert-eql 15 (m:document-caret document))         ; column 5 again
    (m:move-vertically document 1)
    (assert-eql 15 (m:document-caret document))         ; nothing below
    (m:move-vertically document -2)
    (assert-eql 5 (m:document-caret document))))

(define-test an-extending-move-leaves-the-anchor ()
  (let ((document (doc "abcdef" :caret 2)))
    (m:move-caret document 4 :extend t)
    (assert-eql 2 (m:document-anchor document))
    (assert-equal "cd" (m:selected-string document))
    (m:move-caret document 5)
    (assert-false (m:selection-p document))
    (m:select-all document)
    (assert-equal "abcdef" (m:selected-string document))))

;;; --- what shows ---

(define-test scrolling-keeps-the-part-of-a-line-and-stays-inside-the-text ()
  (let ((document (doc (m:generate-text 100))))
    (assert-true (m:scroll-lines document 2.5f0))
    (assert-eql 2 (m:document-top-line document))
    (assert-true (m:scroll-lines document 0.5f0))
    (assert-eql 3 (m:document-top-line document))
    (assert-false (m:scroll-lines document 0.25f0))
    (m:scroll-lines document -50f0)
    (assert-eql 0 (m:document-top-line document))
    (assert-false (m:scroll-lines document -1f0))
    (m:scroll-lines document 500f0)
    (assert-eql 99 (m:document-top-line document))))

(define-test scrolling-to-the-caret-goes-just-far-enough ()
  (let ((document (doc (m:generate-text 100))))
    (m:move-caret document (m:line-start document 30))
    (m:scroll-to-caret document)
    (assert-eql (- 30 (1- m:+visible-lines+)) (m:document-top-line document))
    (m:scroll-to-caret document)
    (assert-eql (- 30 (1- m:+visible-lines+)) (m:document-top-line document))
    (m:move-caret document (m:line-start document 2))
    (m:scroll-to-caret document)
    (assert-eql 2 (m:document-top-line document))))

(define-test the-strings-of-the-lines-that-show-are-kept-until-the-text-changes ()
  (let ((document (doc (format nil "one~%two~%three"))))
    (assert-equal "two" (m:cached-line-string document 1))
    (assert-eq (m:cached-line-string document 1) (m:cached-line-string document 1))
    (m:move-caret document 2)                            ; not a change of the text
    (assert-eq (m:cached-line-string document 1) (m:cached-line-string document 1))
    (m:insert-text document "X")
    (assert-equal "two" (m:cached-line-string document 1))
    (assert-equal "onXe" (m:cached-line-string document 0))
    (m:move-caret document 6)
    (m:insert-text document "Y")
    (assert-equal "tYwo" (m:cached-line-string document 1))
    (assert-equal "onXe" (m:cached-line-string document 0))))

(define-test the-status-says-where-the-caret-is-and-is-made-again-only-when-something-changed ()
  (let ((document (doc (format nil "ab~%cd") :caret 4)))
    (assert-equal "Ln 2, Col 2    2 lines" (m:status-text document))
    (assert-eq (m:status-text document) (m:status-text document))
    (m:move-caret document 1 :extend t)
    (assert-equal "Ln 1, Col 2    2 lines    3 selected" (m:status-text document))))

(define-test the-line-number-text-is-kept ()
  (assert-equal "1234" (m:line-number-text 1234))
  (assert-eq (m:line-number-text 1234) (m:line-number-text 1234))
  (assert-equal "1170" (m:line-number-text 1170)))

;;; --- composition ---

(define-test a-composition-is-held-apart-until-it-is-committed ()
  (let ((document (doc "ab" :caret 1)))
    (m:set-composition document "ni" 1 1)
    (assert-equal "ab" (m:document-string document))
    (assert-equal "ni" (m:document-marked-text document))
    (m:commit-composition document "你")
    (assert-equal "a你b" (m:document-string document))
    (assert-equal "" (m:document-marked-text document))
    (assert-eql 2 (m:document-caret document))))

;;; --- the events ---

(define-test the-arrows-move-the-caret-and-shift-extends-the-selection ()
  (let ((document (doc "abcdef" :caret 3)))
    (assert-true (c:editor-key document (key core:+key-left+)))
    (assert-eql 2 (m:document-caret document))
    (c:editor-key document (key core:+key-right+ :modifiers core:+modifier-shift+))
    (c:editor-key document (key core:+key-right+ :modifiers core:+modifier-shift+))
    (assert-equal "cd" (m:selected-string document))
    ;; Without Shift a move collapses a selection to its own side.
    (c:editor-key document (key core:+key-left+))
    (assert-eql 2 (m:document-caret document))
    (assert-false (m:selection-p document))))

(define-test home-and-end-go-to-the-ends-of-the-line ()
  (let ((document (doc (format nil "abc~%defg") :caret 6)))
    (c:editor-key document (key core:+key-home+))
    (assert-eql 4 (m:document-caret document))
    (c:editor-key document (key core:+key-end+))
    (assert-eql 8 (m:document-caret document))))

(define-test characters-return-tab-and-backspace-edit ()
  (let ((document (doc "")))
    (c:editor-key document (key #\a))
    (c:editor-key document (key #\Return))
    (c:editor-key document (key #\b))
    (c:editor-key document (key #\Tab))
    (assert-equal (format nil "a~%b    ") (m:document-string document))
    (c:editor-key document (key #\Backspace))
    (assert-equal (format nil "a~%b   ") (m:document-string document))))

(define-test command-a-selects-all-and-other-commands-and-function-keys-are-not-the-editors ()
  (let ((document (doc "abc" :caret 1)))
    (assert-true (c:editor-key document (key #\a :modifiers core:+modifier-super+)))
    (assert-equal "abc" (m:selected-string document))
    (assert-false (c:editor-key document (key #\x :modifiers core:+modifier-super+)))
    (assert-false (c:editor-key document (key (code-char #xF704))))
    (assert-false (c:editor-key document (core:make-key-event :timestamp 0 :kind :up
                                                              :logical-key #\a)))
    (assert-equal "abc" (m:document-string document))))

(define-test text-from-the-input-method-is-inserted-and-empty-text-is-nothing ()
  (let ((document (doc "")))
    (assert-true (c:editor-text document "héllo"))
    (assert-false (c:editor-text document ""))
    (assert-equal "héllo" (m:document-string document))))

(define-test a-composition-selection-is-replaced-when-it-starts-and-shown-while-it-goes-on ()
  (let ((document (doc "abcd" :caret 3 :anchor 1)))
    (c:editor-composition document :started "k" 1 0)
    (assert-equal "ad" (m:document-string document))
    (c:editor-composition document :changed "ka" 2 0)
    (assert-equal "ka" (m:document-marked-text document))
    (assert-equal "ad" (m:document-string document))
    (c:editor-composition document :committed "か" 0 0)
    (assert-equal "aかd" (m:document-string document))
    (assert-equal "" (m:document-marked-text document))))

(define-test a-press-puts-the-caret-by-the-line-and-the-nearest-column ()
  (let ((document (doc (format nil "abcdef~%ghijkl~%mn"))))
    (setf (m:document-char-width document) 8f0)
    (let ((x (+ m:+gutter-width+ p:+text-inset+)))
      (assert-true (c:editor-press document (+ x 20) (+ m:+line-height+ 3)))
      (assert-eql (+ 7 2) (m:document-caret document))   ; 20/8 = 2.5 rounds to 2
      (c:editor-press document (+ x 400) (* 2 m:+line-height+))
      (assert-eql 16 (m:document-caret document))        ; past the end of "mn"
      (c:editor-press document -5 9999)
      (assert-eql 14 (m:document-caret document))        ; below the text: the last line, column 0
      (c:editor-press document 0 0)
      (assert-eql 0 (m:document-caret document)))))

(define-test a-drag-selects-from-the-press-and-above-or-below-scrolls ()
  (let ((document (doc (m:generate-text 100))))
    (setf (m:document-char-width document) 8f0)
    (c:editor-press document (+ m:+gutter-width+ p:+text-inset+) 0)
    (c:editor-drag document (+ m:+gutter-width+ p:+text-inset+) (* 3 m:+line-height+))
    (assert-true (m:selection-p document))
    (assert-eql (m:line-start document 3) (m:document-caret document))
    (c:editor-drag document 100 (+ (* m:+visible-lines+ m:+line-height+) 5))
    (assert-eql 1 (m:document-top-line document))))

(define-test the-wheel-scrolls-by-lines-of-the-text ()
  (let ((document (doc (m:generate-text 100))))
    (assert-true (c:editor-wheel document -5))           ; 5 wheel lines are 6 text lines
    (assert-eql 6 (m:document-top-line document))
    (assert-false (c:editor-wheel document 0))
    (c:editor-wheel document 100)
    (assert-eql 0 (m:document-top-line document))))

;;; --- painting ---

(defun paint (document &key (x 0f0) (y 0f0) (origin-x x) (origin-y y))
  (let ((context (fake:make-fake-context :width 740f0 :height 560f0))
        (list (core:make-display-list :capacity 8192)))
    (p:paint-editor context list x y 700f0 480f0 document origin-x origin-y)
    (values list context)))

(defun glyphs (list)
  (with-output-to-string (stream)
    (dotimes (i (core:display-list-count list))
      (when (= (core:display-list-kind list i) core:+command-glyph+)
        (write-char (code-char (round (core:display-list-parameter list i))) stream)))))

(defun rects (list &optional color)
  (loop :for i :below (core:display-list-count list)
        :when (and (= (core:display-list-kind list i) core:+command-rect+)
                   (or (null color) (= color (core:display-list-color list i))))
          :collect i))

(define-test the-lines-that-show-are-drawn-with-their-numbers-and-no-others ()
  (let* ((document (doc (m:generate-text 100)))
         (text (glyphs (paint document))))
    (assert-true (search (m:line-string document 1) text))
    (assert-true (search (m:line-string document 23) text))
    (assert-false (search (m:line-string document 24) text))
    (assert-true (search "24" text))))

(define-test the-top-line-is-the-first-that-is-drawn ()
  (let ((document (doc (m:generate-text 100))))
    (m:scroll-lines document 40f0)
    (let ((text (glyphs (paint document))))
      (assert-true (search (m:line-string document 40) text))
      (assert-false (search (m:line-string document 1) text))
      (assert-true (search "41" text)))))

(define-test the-painter-tells-the-context-where-the-caret-is-in-the-window ()
  (let ((document (doc (format nil "abc~%defgh") :caret 6)))
    (multiple-value-bind (list context) (paint document :x 0f0 :y 0f0 :origin-x 20f0 :origin-y 52f0)
      (declare (ignore list))
      (let ((rect (core:context-text-input-rect context))
            (text-x (+ m:+gutter-width+ p:+text-inset+)))
        (assert-true (core:context-text-input-p context))
        (assert-eql (+ 20f0 text-x (* 2 8)) (aref rect 0))
        (assert-eql (+ 52f0 m:+line-height+ 2) (aref rect 1))))
    (assert-eql 8f0 (m:document-char-width document))))

(define-test the-selection-and-the-caret-are-drawn-and-composed-text-is-underlined ()
  (let* ((plain (doc "abc def" :caret 0))
         (selected (doc "abc def" :caret 3 :anchor 0))
         (composing (doc "abc def" :caret 3)))
    (m:set-composition composing "か" 1 0)
    (let ((a (length (rects (paint plain))))
          (b (length (rects (paint selected))))
          (c (length (rects (paint composing)))))
      (assert-eql (1+ a) b)                              ; the selection
      (assert-eql (1+ a) c)                              ; the underline
      (assert-true (search "abcか def" (glyphs (paint composing)))))))

(define-test painting-the-same-text-again-allocates-little ()
  (let ((document (doc (m:generate-text 100) :caret 50))
        (context (fake:make-fake-context :width 740f0 :height 560f0))
        (list (core:make-display-list :capacity 8192)))
    (p:paint-editor context list 0f0 0f0 700f0 480f0 document)
    (let ((before (sb-ext:get-bytes-consed)))
      (dotimes (i 1000)
        (core:clear-display-list list)
        (p:paint-editor context list 0f0 0f0 700f0 480f0 document))
      ;; The fake context's own text lists are what is left.
      (assert-true (< (- (sb-ext:get-bytes-consed) before) 30000000)))))
