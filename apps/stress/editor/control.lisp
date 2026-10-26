(defpackage #:almighty-bolt-stress/editor/control
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:modifier-down-p
                #:+modifier-shift+
                #:+modifier-control+
                #:+modifier-super+
                #:+key-up+
                #:+key-down+
                #:+key-left+
                #:+key-right+
                #:+key-delete-forward+
                #:+key-home+
                #:+key-end+
                #:+key-page-up+
                #:+key-page-down+)
  (:import-from #:almighty-bolt-stress/editor/model
                #:+line-height+
                #:+visible-lines+
                #:+gutter-width+
                #:+lines-per-wheel-line+
                #:document-caret
                #:document-top-line
                #:document-line-count
                #:document-char-width
                #:document-marked-text
                #:selection-p
                #:selection-start
                #:selection-end
                #:line-start
                #:line-end
                #:position-line
                #:move-caret
                #:select-all
                #:insert-text
                #:delete-backward
                #:delete-forward
                #:move-vertically
                #:scroll-lines
                #:scroll-to-caret
                #:set-composition
                #:commit-composition)
  (:import-from #:almighty-bolt-stress/editor/paint
                #:+text-inset+)
  (:export #:editor-key
           #:editor-text
           #:editor-composition
           #:editor-wheel
           #:editor-press
           #:editor-drag))

(in-package #:almighty-bolt-stress/editor/control)

;;; What the events do to the document, the same in every variant. Each function returns true if the
;;; event was one that the editor handles (so that the variant redraws, and says the key is used).

(defun editor-key (document event)
  "A key: the arrows (with Shift to extend the selection), Home, End, the pages, Backspace and
forward delete, Return, Tab, Command-A, and a character that stands for itself."
  (when (eq (key-event-kind event) :down)
    (let* ((key (key-event-logical-key event))
           (modifiers (key-event-modifiers event))
           (extend (modifier-down-p modifiers +modifier-shift+))
           (command (or (modifier-down-p modifiers +modifier-super+)
                        (modifier-down-p modifiers +modifier-control+)))
           (handled t))
      (cond ((and command (eql key #\a)) (select-all document))
            (command (setf handled nil))
            ((eql key +key-left+)
             (if (and (selection-p document) (not extend))
                 (move-caret document (selection-start document))
                 (move-caret document (1- (document-caret document)) :extend extend)))
            ((eql key +key-right+)
             (if (and (selection-p document) (not extend))
                 (move-caret document (selection-end document))
                 (move-caret document (1+ (document-caret document)) :extend extend)))
            ((eql key +key-up+) (move-vertically document -1 :extend extend))
            ((eql key +key-down+) (move-vertically document 1 :extend extend))
            ((eql key +key-page-up+) (move-vertically document (- +visible-lines+) :extend extend))
            ((eql key +key-page-down+) (move-vertically document +visible-lines+ :extend extend))
            ((eql key +key-home+)
             (move-caret document (line-start document (position-line document (document-caret document)))
                         :extend extend))
            ((eql key +key-end+)
             (move-caret document (line-end document (position-line document (document-caret document)))
                         :extend extend))
            ((eql key #\Backspace) (delete-backward document))
            ((eql key +key-delete-forward+) (delete-forward document))
            ((or (eql key #\Return) (eql key #\Newline)) (insert-text document (string #\Newline)))
            ((eql key #\Tab) (insert-text document "    "))
            ((and (characterp key) (graphic-char-p key) (not (<= #xF700 (char-code key) #xF8FF)))
             (insert-text document (string key)))
            (t (setf handled nil)))
      (when handled
        (scroll-to-caret document))
      handled)))

(defun editor-text (document text)
  "Text from the input method (or from a key that stands for itself, where the person's keyboard
makes it one)."
  (when (plusp (length text))
    (insert-text document text)
    (scroll-to-caret document)
    t))

(defun editor-composition (document kind text start length)
  "An input method composing: the text is shown where it will go and is put in the document when
it is committed."
  (ecase kind
    ((:started :changed)
     (when (and (selection-p document) (zerop (length (document-marked-text document))))
       (insert-text document ""))
     (set-composition document text start length))
    (:committed
     (commit-composition document text)
     (scroll-to-caret document)))
  t)

(defun editor-wheel (document dy)
  "The wheel, by DY lines (positive up, as the wheel reports it)."
  (scroll-lines document (- (* dy +lines-per-wheel-line+))))

(defun point-position (document x y)
  "The place in the text at the point X, Y of the editor: the line there, and the column nearest
by the width of a character."
  (let* ((line (max 0 (min (1- (document-line-count document))
                           (+ (document-top-line document) (floor y +line-height+)))))
         (column (round (- x +gutter-width+ +text-inset+) (document-char-width document))))
    (min (+ (line-start document line) (max 0 column)) (line-end document line))))

(defun editor-press (document x y)
  "A press puts the caret where it is."
  (move-caret document (point-position document x y)))

(defun editor-drag (document x y)
  "A drag extends the selection to where it is, and scrolls if it is above or below."
  (cond ((< y 0) (scroll-lines document -1f0))
        ((> y (* +visible-lines+ +line-height+)) (scroll-lines document 1f0)))
  (move-caret document (point-position document x y) :extend t))
