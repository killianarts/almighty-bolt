(defpackage #:almighty-bolt-stress/editor/control
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:model #:almighty-bolt-stress/editor/model)
                    (#:paint #:almighty-bolt-stress/editor/paint))
  (:export #:editor-key
           #:editor-text
           #:editor-composition
           #:editor-wheel
           #:editor-press
           #:editor-drag))

(in-package #:almighty-bolt-stress/editor/control)

;;; What the events do to the document. Each function returns true if the event was one that the
;;; editor handles (so that the app redraws, and says the key is used).

(defun editor-key (document event)
  "A key: the arrows (with Shift to extend the selection), Home, End, the pages, Backspace and
forward delete, Return, Tab, Command-A, and a character that stands for itself."
  (when (eq (bolt:key-event-kind event) :down)
    (let* ((key (bolt:key-event-logical-key event))
           (modifiers (bolt:key-event-modifiers event))
           (extend (bolt:modifier-down-p modifiers bolt:+modifier-shift+))
           (command (or (bolt:modifier-down-p modifiers bolt:+modifier-super+)
                        (bolt:modifier-down-p modifiers bolt:+modifier-control+)))
           (handled t))
      (cond ((and command (eql key #\a)) (model:select-all document))
            (command (setf handled nil))
            ((eql key bolt:+key-left+)
             (if (and (model:selection-p document) (not extend))
                 (model:move-caret document (model:selection-start document))
                 (model:move-caret document (1- (model:document-caret document)) :extend extend)))
            ((eql key bolt:+key-right+)
             (if (and (model:selection-p document) (not extend))
                 (model:move-caret document (model:selection-end document))
                 (model:move-caret document (1+ (model:document-caret document)) :extend extend)))
            ((eql key bolt:+key-up+) (model:move-vertically document -1 :extend extend))
            ((eql key bolt:+key-down+) (model:move-vertically document 1 :extend extend))
            ((eql key bolt:+key-page-up+) (model:move-vertically document (- model:+visible-lines+) :extend extend))
            ((eql key bolt:+key-page-down+) (model:move-vertically document model:+visible-lines+ :extend extend))
            ((eql key bolt:+key-home+)
             (model:move-caret document (model:line-start document (model:position-line document (model:document-caret document)))
                               :extend extend))
            ((eql key bolt:+key-end+)
             (model:move-caret document (model:line-end document (model:position-line document (model:document-caret document)))
                               :extend extend))
            ((eql key #\Backspace) (model:delete-backward document))
            ((eql key bolt:+key-delete-forward+) (model:delete-forward document))
            ((or (eql key #\Return) (eql key #\Newline)) (model:insert-text document (string #\Newline)))
            ((eql key #\Tab) (model:insert-text document "    "))
            ((and (characterp key) (graphic-char-p key) (not (<= #xF700 (char-code key) #xF8FF)))
             (model:insert-text document (string key)))
            (t (setf handled nil)))
      (when handled
        (model:scroll-to-caret document))
      handled)))

(defun editor-text (document text)
  "Text from the input method (or from a key that stands for itself, where the person's keyboard
makes it one)."
  (when (plusp (length text))
    (model:insert-text document text)
    (model:scroll-to-caret document)
    t))

(defun editor-composition (document kind text start length)
  "An input method composing: the text is shown where it will go and is put in the document when
it is committed."
  (ecase kind
    ((:started :changed)
     (when (and (model:selection-p document) (zerop (length (model:document-marked-text document))))
       (model:insert-text document ""))
     (model:set-composition document text start length))
    (:committed
     (model:commit-composition document text)
     (model:scroll-to-caret document)))
  t)

(defun editor-wheel (document dy)
  "The wheel, by DY lines (positive up, as the wheel reports it)."
  (model:scroll-lines document (- (* dy model:+lines-per-wheel-line+))))

(defun point-position (context document x y)
  "The place in the text at the point X, Y of the editor: the line there, and the character
nearest in the line, as the context measures it."
  (let* ((line (max 0 (min (1- (model:document-line-count document))
                           (+ (model:document-top-line document) (floor y model:+line-height+)))))
         (column (bolt:context-index-at-x context (bolt:context-font context model:+font-name+ model:+font-size+)
                                          (model:line-string document line)
                                          (- x model:+gutter-width+ paint:+text-inset+))))
    (min (+ (model:line-start document line) (max 0 column)) (model:line-end document line))))

(defun editor-press (context document x y)
  "A press puts the caret where it is."
  (model:move-caret document (point-position context document x y)))

(defun editor-drag (context document x y)
  "A drag extends the selection to where it is, and scrolls if it is above or below."
  (cond ((< y 0) (model:scroll-lines document -1f0))
        ((> y (* model:+visible-lines+ model:+line-height+)) (model:scroll-lines document 1f0)))
  (model:move-caret document (point-position context document x y) :extend t))
