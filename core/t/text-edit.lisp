(defpackage #:almighty-bolt-core/t/text-edit
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:te #:almighty-bolt-core/text-edit)
                    (#:ev #:almighty-bolt-core/event)))

(in-package #:almighty-bolt-core/t/text-edit)

(defun state (string caret &optional (anchor caret))
  (te:make-text-state string :caret caret :anchor anchor))

(defun summary (state)
  "The text, the caret and the anchor, for comparing."
  (list (te:text-state-string state) (te:text-state-caret state) (te:text-state-anchor state)))

(defun key (character &key (modifiers 0) (kind :down))
  (ev:make-key-event :timestamp 0 :kind kind :logical-key character :modifiers modifiers))

(defun typed (state character &key (modifiers 0) (kind :down))
  "The state after the key, and whether it edited."
  (te:edit-text state (key character :modifiers modifiers :kind kind)))

;;; --- states ---

(define-test a-new-state-has-its-caret-at-the-end-and-no-selection ()
  (let ((state (te:make-text-state "abc")))
    (assert-equal '("abc" 3 3) (summary state))
    (assert-false (te:text-state-selection-p state))))

(define-test the-caret-and-anchor-are-kept-inside-the-text ()
  (assert-equal '("abc" 3 0) (summary (te:make-text-state "abc" :caret 99 :anchor -4))))

(define-test the-selection-is-between-the-anchor-and-the-caret-either-way-round ()
  (dolist (state (list (state "abcdef" 4 1) (state "abcdef" 1 4)))
    (assert-eql 1 (te:text-state-selection-start state))
    (assert-eql 4 (te:text-state-selection-end state))
    (assert-equal "bcd" (te:text-state-selected-string state))))

;;; --- editing ---

(define-test inserting-puts-text-at-the-caret ()
  (assert-equal '("abXc" 3 3) (summary (te:text-insert (state "abc" 2) "X"))))

(define-test inserting-replaces-the-selection ()
  (assert-equal '("aXef" 2 2) (summary (te:text-insert (state "abcdef" 4 1) "X"))))

(define-test inserting-nothing-changes-nothing ()
  (let ((state (state "abc" 1)))
    (assert-true (eq state (te:text-insert state "")))))

(define-test backspace-deletes-before-the-caret-or-the-selection ()
  (assert-equal '("ac" 1 1) (summary (te:text-delete-backward (state "abc" 2))))
  (assert-equal '("af" 1 1) (summary (te:text-delete-backward (state "abcdef" 5 1))))
  (let ((start (state "abc" 0)))
    (assert-true (eq start (te:text-delete-backward start)))))

(define-test forward-delete-deletes-after-the-caret-or-the-selection ()
  (assert-equal '("ac" 1 1) (summary (te:text-delete-forward (state "abc" 1))))
  (assert-equal '("af" 1 1) (summary (te:text-delete-forward (state "abcdef" 5 1))))
  (let ((end (state "abc" 3)))
    (assert-true (eq end (te:text-delete-forward end)))))

;;; --- moving ---

(define-test moving-by-a-character ()
  (assert-equal '("abc" 1 1) (summary (te:text-move (state "abc" 2) :left)))
  (assert-equal '("abc" 3 3) (summary (te:text-move (state "abc" 2) :right)))
  (assert-equal '("abc" 0 0) (summary (te:text-move (state "abc" 0) :left)))
  (assert-equal '("abc" 0 0) (summary (te:text-move (state "abc" 2) :start)))
  (assert-equal '("abc" 3 3) (summary (te:text-move (state "abc" 1) :end))))

(define-test moving-with-extend-grows-the-selection-from-the-anchor ()
  (assert-equal '("abc" 1 2) (summary (te:text-move (state "abc" 2) :left :extend t)))
  (assert-equal '("abc" 0 2) (summary (te:text-move (state "abc" 1 2) :left :extend t)))
  (assert-equal '("abc" 3 2) (summary (te:text-move (state "abc" 2) :end :extend t))))

(define-test moving-without-extend-puts-the-caret-at-the-edge-of-a-selection ()
  (assert-equal '("abcdef" 1 1) (summary (te:text-move (state "abcdef" 4 1) :left)))
  (assert-equal '("abcdef" 4 4) (summary (te:text-move (state "abcdef" 1 4) :right))))

(define-test moving-by-a-word ()
  (assert-equal '("one two" 4 4) (summary (te:text-move (state "one two" 7) :left :word t)))
  (assert-equal '("one two" 3 3) (summary (te:text-move (state "one two" 0) :right :word t)))
  (assert-equal '("one two" 7 7) (summary (te:text-move (state "one two" 3) :right :word t))))

(define-test selecting-all ()
  (assert-equal '("abc" 3 0) (summary (te:text-select-all (state "abc" 1))))
  (let ((all (te:text-select-all (state "abc" 1))))
    (assert-true (eq all (te:text-select-all all)))))

(define-test placing-the-caret-by-index ()
  (assert-equal '("abc" 1 1) (summary (te:text-place-caret (state "abc" 3) 1)))
  (assert-equal '("abc" 1 3) (summary (te:text-place-caret (state "abc" 3) 1 :extend t)))
  (assert-equal '("abc" 3 3) (summary (te:text-place-caret (state "abc" 0) 99))))

;;; --- keys ---

(define-test typing-a-character-inserts-it ()
  (multiple-value-bind (state edited) (typed (state "ac" 1) #\b)
    (assert-equal '("abc" 2 2) (summary state))
    (assert-true edited)))

(define-test typing-with-shift-gives-the-character-the-event-names ()
  (assert-equal '("A" 1 1)
                (summary (typed (te:make-text-state "") #\A :modifiers ev:+modifier-shift+))))

(define-test the-named-keys-edit-and-move ()
  (assert-equal '("ab" 2 2) (summary (typed (state "abc" 3) #\Backspace)))
  (assert-equal '("bc" 0 0) (summary (typed (state "abc" 0) ev:+key-delete-forward+)))
  (assert-equal '("abc" 1 1) (summary (typed (state "abc" 2) ev:+key-left+)))
  (assert-equal '("abc" 3 3) (summary (typed (state "abc" 2) ev:+key-right+)))
  (assert-equal '("abc" 0 0) (summary (typed (state "abc" 2) ev:+key-home+)))
  (assert-equal '("abc" 3 3) (summary (typed (state "abc" 0) ev:+key-end+)))
  (assert-equal '("abc" 1 2)
                (summary (typed (state "abc" 2) ev:+key-left+ :modifiers ev:+modifier-shift+))))

(define-test command-a-selects-all-and-the-arrows-with-command-go-to-the-ends ()
  (assert-equal '("abc" 3 0) (summary (typed (state "abc" 1) #\a :modifiers ev:+modifier-super+)))
  (assert-equal '("abc" 0 0) (summary (typed (state "abc" 2) ev:+key-left+
                                             :modifiers ev:+modifier-super+)))
  (assert-equal '("abc" 3 3) (summary (typed (state "abc" 1) ev:+key-right+
                                             :modifiers ev:+modifier-super+))))

(define-test keys-that-are-not-for-editing-are-left-alone ()
  (dolist (character (list #\Tab #\Return #\Escape ev:+key-up+ ev:+key-down+ ev:+key-page-up+))
    (multiple-value-bind (state edited) (typed (state "abc" 1) character)
      (assert-equal '("abc" 1 1) (summary state) character)
      (assert-false edited character)))
  ;; A shortcut with command or control is not typing.
  (assert-false (nth-value 1 (typed (state "abc" 1) #\x :modifiers ev:+modifier-super+)))
  (assert-false (nth-value 1 (typed (state "abc" 1) #\x :modifiers ev:+modifier-control+))))

(define-test only-a-key-going-down-edits ()
  (multiple-value-bind (state edited) (typed (state "abc" 1) #\x :kind :up)
    (assert-equal '("abc" 1 1) (summary state))
    (assert-false edited)))

(define-test a-key-with-no-character-does-nothing ()
  (multiple-value-bind (state edited)
      (te:edit-text (state "abc" 1) (key nil))
    (assert-equal '("abc" 1 1) (summary state))
    (assert-false edited)))
