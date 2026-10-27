(defpackage #:almighty-bolt-stress/editor/model
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:+points-per-line+)
  (:export #:+line-height+
           #:+visible-lines+
           #:+gutter-width+
           #:+editor-width+
           #:+editor-height+
           #:document
           #:make-document
           #:generate-text
           #:default-line-count
           #:make-sample-document
           #:document-length
           #:document-line-count
           #:document-caret
           #:document-anchor
           #:document-top-line
           #:document-marked-text
           #:document-marked-start
           #:document-marked-length
           #:document-version
           #:+lines-per-wheel-line+
           #:+font-name+
           #:+font-size+
           #:+number-font-size+
           #:line-number-text
           #:cached-line-string
           #:document-char
           #:line-start
           #:line-end
           #:line-string
           #:position-line
           #:position-column
           #:selection-p
           #:selection-start
           #:selection-end
           #:selected-string
           #:document-string
           #:move-caret
           #:select-all
           #:insert-text
           #:delete-backward
           #:delete-forward
           #:move-vertically
           #:scroll-lines
           #:scroll-to-caret
           #:clamp-top-line
           #:set-composition
           #:commit-composition
           #:status-text))

(in-package #:almighty-bolt-stress/editor/model)

;;; The text editor (the fourth stress app): a document held as a gap buffer, so that an edit at
;;; the caret moves no text but what lies in the gap's way, with an index of where each line begins,
;;; a caret, an anchor (the other end of a selection), and the line that shows at the top. The
;;; editing is here, on positions and strings, so that each variant has only to give it the events
;;; and to draw it again; the text is drawn by paint.lisp. A document of a few megabytes is held as
;;; one array of characters (four bytes each), which is what is being stressed: the line cache of the
;;; text engine and the drawing of a screenful of a large text.

(defparameter +font-name+ "Menlo" "The font of the text and of the line numbers: one of equal widths.")
(defconstant +font-size+ 12)
(defconstant +number-font-size+ 11)
(defconstant +line-height+ 20)
(defconstant +visible-lines+ 24)
(defconstant +gutter-width+ 56)
(defconstant +editor-width+ 700)
(defconstant +editor-height+ (* +visible-lines+ +line-height+))
(defconstant +lines-per-wheel-line+ (float (/ +points-per-line+ +line-height+) 1f0)
  "How far a line of the wheel scrolls the text, in lines: a line is 24 points and a text line 20.")

(defstruct (document (:constructor %make-document))
  (data (make-array 0 :element-type 'character) :type (simple-array character (*)))
  (gap-start 0 :type fixnum)
  (gap-end 0 :type fixnum)
  ;; The position at which each line begins (the first is 0), in the characters of the text without
  ;; its gap; LINE-COUNT of them are in use.
  (starts (make-array 0 :element-type 'fixnum) :type (simple-array fixnum (*)))
  (line-count 1 :type fixnum)
  (caret 0 :type fixnum)
  (anchor 0 :type fixnum)
  ;; The column that a move up or down tries to stay in.
  (desired-column 0 :type fixnum)
  (top-line 0 :type fixnum)
  (remainder 0f0 :type single-float)
  ;; What an input method is composing: the text, and the part of it that is selected.
  (marked-text "" :type string)
  (marked-start 0 :type fixnum)
  (marked-length 0 :type fixnum)
  ;; Counts the changes of what the document shows, so that a view can tell.
  (version 0 :type fixnum)
  ;; The strings of the lines that show, by the line's number mod the number of lines that show, and
  ;; which line each is, so that drawing the same text again makes no strings and an edit makes one.
  (cache-lines (make-array +visible-lines+ :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (*)))
  (cache-strings (make-array +visible-lines+ :initial-element "") :type simple-vector)
  ;; The status line, and the version it was made for.
  (status-version -1 :type fixnum)
  (status-string "" :type string))

(defun document-length (document)
  (- (length (document-data document))
     (- (document-gap-end document) (document-gap-start document))))

(declaim (inline document-char))
(defun document-char (document position)
  "The character at POSITION, counting the text and not the gap."
  (declare (type document document) (type fixnum position))
  (aref (document-data document)
        (if (< position (document-gap-start document))
            position
            (+ position (- (document-gap-end document) (document-gap-start document))))))

(defun document-substring (document start end)
  (let ((result (make-string (- end start))))
    (dotimes (i (- end start) result)
      (setf (char result i) (document-char document (+ start i))))))

(defun document-string (document)
  (document-substring document 0 (document-length document)))

;;; --- the line index ---

(defun index-lines (document)
  "Make the index of where lines begin from the text."
  (let* ((length (document-length document))
         (count 1))
    (dotimes (i length)
      (when (char= (document-char document i) #\Newline) (incf count)))
    (let ((starts (make-array (max 16 (* 2 count)) :element-type 'fixnum :initial-element 0))
          (line 1))
      (dotimes (i length)
        (when (char= (document-char document i) #\Newline)
          (setf (aref starts line) (1+ i))
          (incf line)))
      (setf (document-starts document) starts
            (document-line-count document) count))))

(defun make-document (string)
  "A document of STRING, with the caret at the start."
  ;; A gap that a person's typing will not use up: the array is not made again in the middle of it.
  (let* ((gap (max 1024 (floor (length string) 16)))
         (data (make-array (+ (length string) gap) :element-type 'character :initial-element #\Space)))
    (replace data string :start1 gap)
    (let ((document (%make-document :data data :gap-start 0 :gap-end gap)))
      (index-lines document)
      document)))

(defun line-start (document line)
  (aref (document-starts document) line))

(defun line-end (document line)
  "The position of the end of LINE, before its newline."
  (if (< (1+ line) (document-line-count document))
      (1- (line-start document (1+ line)))
      (document-length document)))

(defun line-string (document line)
  (document-substring document (line-start document line) (line-end document line)))

(defun position-line (document position)
  "The line that POSITION is on: the last line that begins at or before it."
  (let ((low 0) (high (1- (document-line-count document))))
    (loop :while (< low high)
          :do (let ((middle (ceiling (+ low high) 2)))
                (if (<= (line-start document middle) position)
                    (setf low middle)
                    (setf high (1- middle)))))
    low))

(defun position-column (document position)
  (- position (line-start document (position-line document position))))

;;; --- the gap ---

(defun move-gap (document position)
  "Put the gap at POSITION."
  (let* ((data (document-data document))
         (start (document-gap-start document))
         (end (document-gap-end document))
         (size (- end start)))
    (cond ((< position start)
           (replace data data :start1 (+ position size) :start2 position :end2 start)
           (setf (document-gap-start document) position
                 (document-gap-end document) (+ position size)))
          ((> position start)
           (replace data data :start1 start :start2 end :end2 (+ position size))
           (setf (document-gap-start document) position
                 (document-gap-end document) (+ position size))))))

(defun make-room (document count)
  "Make the gap at least COUNT characters, at the position it is."
  (let* ((size (- (document-gap-end document) (document-gap-start document))))
    (when (< size count)
      (let* ((old (document-data document))
             (extra (max count (floor (length old) 2) 64))
             (new (make-array (+ (length old) extra) :element-type 'character :initial-element #\Space))
             (tail (- (length old) (document-gap-end document))))
        (replace new old :end2 (document-gap-start document))
        (replace new old :start1 (- (length new) tail) :start2 (document-gap-end document))
        (setf (document-data document) new
              (document-gap-end document) (- (length new) tail))))))

(defun adjust-lines (document position removed inserted-string)
  "Fix the line index for REMOVED characters taken out at POSITION and INSERTED-STRING put there."
  (let* ((starts (document-starts document))
         (count (document-line-count document))
         (line (position-line document position))
         ;; The lines that begin inside what is removed go.
         (last (let ((l line))
                 (loop :while (and (< (1+ l) count)
                                   (<= (aref starts (1+ l)) (+ position removed)))
                       :do (incf l))
                 l))
         (new-starts (loop :for i :from 0 :below (length inserted-string)
                           :when (char= (char inserted-string i) #\Newline)
                             :collect (+ position i 1)))
         (delta (- (length inserted-string) removed))
         (kept (- count (1+ last)))
         (new-count (+ line 1 (length new-starts) kept)))
    (when (> new-count (length starts))
      (let ((bigger (make-array (* 2 new-count) :element-type 'fixnum :initial-element 0)))
        (replace bigger starts :end2 count)
        (setf starts bigger
              (document-starts document) bigger)))
    ;; The lines after the last that was touched move to make room, and by what changed.
    (replace starts starts :start1 (+ line 1 (length new-starts)) :start2 (1+ last) :end2 count)
    (loop :for i :from 0 :below kept
          :do (incf (aref starts (+ line 1 (length new-starts) i)) delta))
    (loop :for start :in new-starts
          :for i :from (1+ line)
          :do (setf (aref starts i) start))
    (setf (document-line-count document) new-count)))

(defun replace-range (document start end string)
  "Replace the text from START to END with STRING; the caret and the anchor are the caller's."
  (move-gap document start)
  (let ((removed (- end start)))
    (incf (document-gap-end document) removed)
    (make-room document (length string))
    (replace (document-data document) string :start1 (document-gap-start document))
    (incf (document-gap-start document) (length string))
    (let ((lines (document-line-count document)))
      (adjust-lines document start removed string)
      ;; An edit inside a line changes that line's string and no other's; one that adds or takes away
      ;; a line break moves the lines after it.
      (if (and (= lines (document-line-count document)) (not (find #\Newline string)))
          (forget-line document (position-line document start))
          (fill (document-cache-lines document) -1)))
    (incf (document-version document))))

(defun forget-line (document line)
  "Drop the kept string of LINE, if it has one."
  (let ((slot (mod line +visible-lines+)))
    (when (= (aref (document-cache-lines document) slot) line)
      (setf (aref (document-cache-lines document) slot) -1))))

;;; --- selection and the caret ---

(defun selection-p (document)
  (/= (document-caret document) (document-anchor document)))

(defun selection-start (document)
  (min (document-caret document) (document-anchor document)))

(defun selection-end (document)
  (max (document-caret document) (document-anchor document)))

(defun selected-string (document)
  (document-substring document (selection-start document) (selection-end document)))

(defun move-caret (document position &key extend keep-column)
  "Put the caret at POSITION (kept inside the text), leaving the anchor behind if EXTEND."
  (let ((position (max 0 (min position (document-length document)))))
    (setf (document-caret document) position)
    (unless extend
      (setf (document-anchor document) position))
    (unless keep-column
      (setf (document-desired-column document) (position-column document position)))
    (incf (document-version document))
    position))

(defun select-all (document)
  (setf (document-anchor document) 0)
  (move-caret document (document-length document) :extend t))

(defun move-vertically (document lines &key extend)
  "Move the caret LINES down (up if negative), to the column that it tries to stay in."
  (let* ((line (position-line document (document-caret document)))
         (target (max 0 (min (1- (document-line-count document)) (+ line lines)))))
    (move-caret document
                (min (+ (line-start document target) (document-desired-column document))
                     (line-end document target))
                :extend extend :keep-column t)))

;;; --- editing ---

(defun insert-text (document string)
  "Put STRING at the caret, in place of the selection if there is one."
  (let ((start (selection-start document))
        (end (selection-end document)))
    (replace-range document start end string)
    (let ((position (+ start (length string))))
      (setf (document-caret document) position
            (document-anchor document) position
            (document-desired-column document) (position-column document position)))
    string))

(defun delete-backward (document)
  "Delete the selection, or the character before the caret."
  (cond ((selection-p document) (insert-text document ""))
        ((plusp (document-caret document))
         (let ((position (1- (document-caret document))))
           (replace-range document position (document-caret document) "")
           (setf (document-caret document) position
                 (document-anchor document) position
                 (document-desired-column document) (position-column document position))
           t))))

(defun delete-forward (document)
  (cond ((selection-p document) (insert-text document ""))
        ((< (document-caret document) (document-length document))
         (replace-range document (document-caret document) (1+ (document-caret document)) "")
         t)))

;;; --- what shows ---

(defun clamp-top-line (document line)
  (max 0 (min line (1- (document-line-count document)))))

(defun scroll-lines (document lines)
  "Scroll by LINES (a fraction is kept for the next call). Returns true if the top line changed."
  (incf (document-remainder document) lines)
  (let ((whole (truncate (document-remainder document))))
    (decf (document-remainder document) whole)
    (let ((new (clamp-top-line document (+ (document-top-line document) whole))))
      (when (/= new (document-top-line document))
        (setf (document-top-line document) new)
        (incf (document-version document))
        t))))

(defun scroll-to-caret (document &optional (visible +visible-lines+))
  "Scroll just far enough that the caret's line shows."
  (let ((line (position-line document (document-caret document)))
        (top (document-top-line document)))
    (cond ((< line top) (setf (document-top-line document) line))
          ((>= line (+ top visible)) (setf (document-top-line document) (1+ (- line visible)))))))

;;; --- composition ---

(defun set-composition (document text selection-start selection-length)
  "What an input method is composing now (TEXT, with SELECTION-LENGTH characters selected from
SELECTION-START in it); it is shown at the caret and is not in the text yet."
  (setf (document-marked-text document) text
        (document-marked-start document) selection-start
        (document-marked-length document) selection-length)
  (incf (document-version document)))

(defun commit-composition (document text)
  "The composition is over, and TEXT is what it became."
  (set-composition document "" 0 0)
  (when (plusp (length text))
    (insert-text document text)))

(defun digit-count (number)
  (let ((count 1))
    (loop :while (>= number 10)
          :do (setf number (floor number 10))
              (incf count))
    count))

(defun grouped-length (number)
  "How many characters NUMBER takes with a comma between each three digits, as ~:d writes it."
  (let ((digits (digit-count number)))
    (+ digits (floor (1- digits) 3))))

(defun put-grouped (string at number)
  "Write NUMBER into STRING from AT, with commas; returns where it ends."
  (let ((end (+ at (grouped-length number))))
    (loop :for position :from (1- end) :downto at
          :for place :from 0
          :do (cond ((= (mod place 4) 3) (setf (char string position) #\,))
                    (t (setf (char string position) (code-char (+ 48 (mod number 10)))
                             number (floor number 10)))))
    end))

(defun status-text (document)
  "The status line, which is made again only when the document has changed. It is written into one
string, and not with FORMAT, which makes streams and pieces to make the one."
  (unless (= (document-status-version document) (document-version document))
    (let* ((caret (document-caret document))
           (line (1+ (position-line document caret)))
           (column (1+ (position-column document caret)))
           (lines (document-line-count document))
           (selected (and (selection-p document)
                          (- (selection-end document) (selection-start document))))
           (string (make-string (+ 3 (grouped-length line) 6 (grouped-length column)
                                   4 (grouped-length lines) 6
                                   (if selected (+ 4 (grouped-length selected) 9) 0))))
           (at 0))
      (flet ((text (text)
               (replace string text :start1 at)
               (incf at (length text)))
             (number (number)
               (setf at (put-grouped string at number))))
        (text "Ln ") (number line) (text ", Col ") (number column)
        (text "    ") (number lines) (text " lines")
        (when selected
          (text "    ") (number selected) (text " selected")))
      (setf (document-status-string document) string
            (document-status-version document) (document-version document))))
  (document-status-string document))

(defun cached-line-string (document line)
  "The string of LINE. The strings of the lines that show are kept until their text changes."
  (let ((slot (mod line +visible-lines+)))
    (unless (= (aref (document-cache-lines document) slot) line)
      (setf (svref (document-cache-strings document) slot) (line-string document line)
            (aref (document-cache-lines document) slot) line))
    (svref (document-cache-strings document) slot)))

(defparameter *numbers* (make-array 64 :initial-element nil)
  "The text of the last 64 line numbers, each as (number . text), by number mod 64.")

(defun line-number-text (number)
  (let* ((slot (mod number 64))
         (entry (svref *numbers* slot)))
    (if (and entry (eql (car entry) number))
        (cdr entry)
        (cdr (setf (svref *numbers* slot) (cons number (format nil "~d" number)))))))

;;; --- a document to edit ---

(defparameter +words+
  #("the" "of" "and" "text" "line" "caret" "frame" "layout" "retained" "reactive" "immediate"
    "widget" "signal" "cache" "glyph" "atlas" "shape" "paint" "event" "scroll" "buffer" "gap"
    "index" "cursor" "window" "display" "list" "node" "tree" "state" "build" "settle" "update"
    "input" "method" "compose" "commit" "select" "move" "delete" "insert" "measure" "break"
    "width" "height" "point" "pixel" "scale" "font" "size" "color" "clip" "rect" "round"
    "queue" "ring" "clock" "time" "replay" "script" "record" "result"))

(defun generate-text (lines)
  "A text of LINES lines made of words from a table by a fixed recipe: the same every time. Every
13th line is empty and every 29th is long; none is longer than 80 characters, so that it fits."
  (let ((state 12345))
    (flet ((next (limit)
             (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
             (mod (ash state -8) limit)))
      (with-output-to-string (stream)
        (dotimes (line lines)
          (unless (zerop (mod line 13))
            (let ((budget (if (zerop (mod line 29)) (+ 60 (next 20)) (+ 12 (next 40))))
                  (used 0))
              (loop
                (let* ((word (svref +words+ (next (length +words+))))
                       (need (+ (length word) (if (plusp used) 1 0))))
                  (when (and (plusp used) (> (+ used need) budget)) (return))
                  (when (plusp used) (write-char #\Space stream))
                  (write-string word stream)
                  (incf used need)
                  (when (>= used budget) (return))))))
          (when (< line (1- lines)) (write-char #\Newline stream)))))))

(defun default-line-count ()
  "The number of lines the editors open with: 20,000, or what the environment variable
EDITOR_LINES says."
  (or (let ((text (uiop:getenv "EDITOR_LINES")))
        (and text (plusp (length text)) (parse-integer text :junk-allowed t)))
      20000))

(defun make-sample-document (&optional (lines (default-line-count)))
  (make-document (generate-text lines)))
