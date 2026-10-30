(defpackage #:almighty-bolt/text-edit
  (:use #:cl)
  (:import-from #:almighty-bolt/event
                #:key-event
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-shift+
                #:+modifier-control+
                #:+modifier-alt+
                #:+modifier-super+
                #:+key-left+
                #:+key-right+
                #:+key-home+
                #:+key-end+
                #:+key-delete-forward+)
  (:export #:text-state
           #:make-text-state
           #:text-state-string
           #:text-state-caret
           #:text-state-anchor
           #:text-state-selection-start
           #:text-state-selection-end
           #:text-state-selection-p
           #:text-state-selected-string
           #:text-state=
           #:text-insert
           #:text-delete-backward
           #:text-delete-forward
           #:text-move
           #:text-select-all
           #:text-place-caret
           #:text-replace
           #:edit-text))

(in-package #:almighty-bolt/text-edit)

;;; The editing of one line of text, as values, shared by every variant (decision record
;;; 0012). A TEXT-STATE is a string, a caret and an anchor: the text between them is the
;;; selection, and when they are the same there is none. It is never changed; each
;;; operation returns a new one (or the same one, if nothing happened), so that a
;;; variant can keep it in a node, in a signal or in a table, and compare it with EQ to
;;; see whether an edit did anything. Indexes are in characters. How wide anything is
;;; comes from the text engine, which a state knows nothing about.

(defstruct (text-state (:constructor %make-text-state (string caret anchor))
                       (:copier nil))
  (string "" :type simple-string :read-only t)
  (caret 0 :type fixnum :read-only t)
  (anchor 0 :type fixnum :read-only t))

(defun make-text-state (string &key (caret (length string)) (anchor caret))
  "A state holding STRING, with the caret at CARET (by default the end) and no selection
unless ANCHOR is given."
  (let ((string (coerce string 'simple-string))
        (end (length string)))
    (%make-text-state string (max 0 (min caret end)) (max 0 (min anchor end)))))

(defun text-state-selection-start (state)
  (min (text-state-caret state) (text-state-anchor state)))

(defun text-state-selection-end (state)
  (max (text-state-caret state) (text-state-anchor state)))

(defun text-state-selection-p (state)
  "True if some text is selected."
  (/= (text-state-caret state) (text-state-anchor state)))

(defun text-state-selected-string (state)
  (subseq (text-state-string state)
          (text-state-selection-start state) (text-state-selection-end state)))

(defun text-state= (a b)
  "True if A and B hold the same text, caret and anchor."
  (and (or (eq a b)
           (string= (text-state-string a) (text-state-string b)))
       (= (text-state-caret a) (text-state-caret b))
       (= (text-state-anchor a) (text-state-anchor b))))

(defun text-replace (state start end replacement)
  "STATE with the characters from START to END replaced by REPLACEMENT, and the caret
after what was put in."
  (let* ((string (text-state-string state))
         (new (concatenate 'simple-string (subseq string 0 start) replacement (subseq string end)))
         (caret (+ start (length replacement))))
    (%make-text-state new caret caret)))

(defun text-insert (state text)
  "STATE with TEXT in place of the selection, or at the caret if there is none."
  (if (and (zerop (length text)) (not (text-state-selection-p state)))
      state
      (text-replace state (text-state-selection-start state) (text-state-selection-end state)
                    text)))

(defun text-delete-backward (state)
  "STATE without the selection, or without the character before the caret."
  (cond ((text-state-selection-p state) (text-insert state ""))
        ((zerop (text-state-caret state)) state)
        (t (text-replace state (1- (text-state-caret state)) (text-state-caret state) ""))))

(defun text-delete-forward (state)
  "STATE without the selection, or without the character after the caret."
  (cond ((text-state-selection-p state) (text-insert state ""))
        ((= (text-state-caret state) (length (text-state-string state))) state)
        (t (text-replace state (text-state-caret state) (1+ (text-state-caret state)) ""))))

(defun word-character-p (character)
  (or (alphanumericp character) (char= character #\_)))

(defun word-start (string index)
  "The index of the start of the word before INDEX."
  (let ((i index))
    (loop :while (and (plusp i) (not (word-character-p (char string (1- i))))) :do (decf i))
    (loop :while (and (plusp i) (word-character-p (char string (1- i)))) :do (decf i))
    i))

(defun word-end (string index)
  "The index of the end of the word after INDEX."
  (let ((i index) (end (length string)))
    (loop :while (and (< i end) (not (word-character-p (char string i)))) :do (incf i))
    (loop :while (and (< i end) (word-character-p (char string i))) :do (incf i))
    i))

(defun text-place-caret (state index &key extend)
  "STATE with the caret at INDEX, and with the anchor moved there too unless EXTEND,
which makes a selection from the old anchor."
  (let* ((index (max 0 (min index (length (text-state-string state)))))
         (anchor (if extend (text-state-anchor state) index)))
    (if (and (= index (text-state-caret state)) (= anchor (text-state-anchor state)))
        state
        (%make-text-state (text-state-string state) index anchor))))

(defun text-move (state direction &key extend word)
  "STATE with the caret moved: DIRECTION is :left, :right, :start or :end. With EXTEND
the selection grows or shrinks; without, a selection is dropped, and :left or :right
put the caret at its edge. WORD moves by a word."
  (let* ((string (text-state-string state))
         (caret (text-state-caret state)))
    (if (and (not extend) (text-state-selection-p state) (member direction '(:left :right)))
        (let ((edge (if (eq direction :left)
                        (text-state-selection-start state)
                        (text-state-selection-end state))))
          (text-place-caret state edge))
        (text-place-caret
         state
         (ecase direction
           (:left (if word (word-start string caret) (max 0 (1- caret))))
           (:right (if word (word-end string caret) (min (length string) (1+ caret))))
           (:start 0)
           (:end (length string)))
         :extend extend))))

(defun text-select-all (state)
  "STATE with all of its text selected."
  (let ((end (length (text-state-string state))))
    (if (and (zerop (text-state-anchor state)) (= (text-state-caret state) end))
        state
        (%make-text-state (text-state-string state) end 0))))

(defun printable-p (character)
  "True for a character that a key puts into text: not a control character, and not one
of the private use characters that name keys with no character."
  (and (graphic-char-p character)
       (not (<= #xE000 (char-code character) #xF8FF))))

(defun edit-text (state event)
  "What the key event EVENT does to the text STATE, as two values: the new state, and
true if the key was one that edits (so that nothing else should act on it). Typing a
character, Backspace, forward delete, the arrows (with Shift to select and Option to
move by a word), Home, End, and Command-A are handled; Tab, Return and Escape are not,
since what they mean is up to the field. Only keys going down count."
  (check-type event key-event)
  (if (not (eq (key-event-kind event) :down))
      (values state nil)
      (let* ((key (key-event-logical-key event))
             (modifiers (key-event-modifiers event))
             (shift (modifier-down-p modifiers +modifier-shift+))
             (word (modifier-down-p modifiers +modifier-alt+))
             (command (modifier-down-p modifiers +modifier-super+))
             (control (modifier-down-p modifiers +modifier-control+)))
        (cond
          ((null key) (values state nil))
          ((eql key #\Backspace) (values (text-delete-backward state) t))
          ((eql key +key-delete-forward+) (values (text-delete-forward state) t))
          ((eql key +key-left+)
           (values (text-move state (if command :start :left) :extend shift :word word) t))
          ((eql key +key-right+)
           (values (text-move state (if command :end :right) :extend shift :word word) t))
          ((eql key +key-home+) (values (text-move state :start :extend shift) t))
          ((eql key +key-end+) (values (text-move state :end :extend shift) t))
          ((and command (char-equal key #\a)) (values (text-select-all state) t))
          ((or command control) (values state nil))
          ((printable-p key) (values (text-insert state (string key)) t))
          (t (values state nil))))))
