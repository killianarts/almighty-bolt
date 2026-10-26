(defpackage #:almighty-bolt-core/text/line
  (:use #:cl)
  (:import-from #:almighty-bolt-core/text/core-foundation
                #:ccall
                #:cf-string
                #:cf-release
                #:cf-constant
                #:cf-array-count
                #:cf-array-ref
                #:font-attributes)
  (:import-from #:almighty-bolt-core/text/font
                #:font
                #:font-ct-font
                #:font-lines
                #:font-older-lines)
  (:export #:text-line
           #:shape-line
           #:get-line
           #:*lines-per-generation*
           #:clear-line-cache
           #:text-line-string
           #:text-line-font
           #:text-line-width
           #:text-line-ascent
           #:text-line-descent
           #:text-line-glyph-count
           #:text-line-glyphs
           #:text-line-glyph-fonts
           #:text-line-xs
           #:text-line-ys
           #:measure-text
           #:caret-x
           #:index-at-x
           #:caret-rect
           #:break-lines))

(in-package #:almighty-bolt-core/text/line)

;;; The measurement API that layout calls, over Core Text:
;;;
;;;   measure a run                 MEASURE-TEXT, or the width of a line
;;;   break a paragraph to a width  BREAK-LINES
;;;   map a point to a caret        INDEX-AT-X
;;;   map a caret to a rectangle    CARET-X and CARET-RECT
;;;
;;; A string is shaped once into a TEXT-LINE, which keeps its glyphs, their fonts
;;; and positions (what drawing needs) and the Core Text line (what the caret
;;; queries need). Lines are cached per font by string, and a lookup that finds
;;; its line allocates nothing. The cache is bounded (decision record 0025): it is of two
;;; generations, the lines used lately and the lines used before, and when the first holds
;;; *LINES-PER-GENERATION* the second is let go, with its Core Text lines, and the first
;;; becomes the second. A line that is found in the second is moved to the first. A
;;; program that draws the same screen again and again keeps its lines, and one that goes
;;; through a million different strings (the rows of a long list, the lines of a long
;;; document) keeps the last few thousand. A TEXT-LINE is for use at once: it is not kept
;;; past the next lookup of another string.
;;;
;;; Positions and metrics are single floats, in points. Indices are Lisp string
;;; indices: Core Text counts UTF-16 units, and the two differ after a character
;;; outside the Basic Multilingual Plane (an emoji), so the indices are converted.

(defstruct (text-line (:constructor %make-text-line))
  (string "" :type simple-string :read-only t)
  (font nil :type (or null font) :read-only t)
  (ct-line 0 :type (unsigned-byte 64) :read-only t)
  (width 0f0 :type single-float :read-only t)
  (ascent 0f0 :type single-float :read-only t)
  (descent 0f0 :type single-float :read-only t)
  (astral-p nil :read-only t)
  (glyph-count 0 :type fixnum :read-only t)
  (glyphs (make-array 0 :element-type '(unsigned-byte 16))
   :type (simple-array (unsigned-byte 16) (*)) :read-only t)
  (glyph-fonts (make-array 0 :element-type '(unsigned-byte 64))
   :type (simple-array (unsigned-byte 64) (*)) :read-only t)
  (xs (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)) :read-only t)
  (ys (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*)) :read-only t))

;;; UTF-16 and Lisp indices

(defun astral-string-p (string)
  "True if STRING has a character that UTF-16 writes as two units."
  (some (lambda (character) (> (char-code character) #xFFFF)) string))

(defun utf16-index (string index)
  "The UTF-16 offset of the character at Lisp INDEX."
  (let ((offset index))
    (dotimes (i index offset)
      (when (> (char-code (schar string i)) #xFFFF)
        (incf offset)))))

(defun lisp-index (string utf16-offset)
  "The Lisp index of the character at UTF-16 offset UTF16-OFFSET."
  (let ((offset 0)
        (i 0)
        (length (length string)))
    (loop :while (and (< i length) (< offset utf16-offset))
          :do (incf offset (if (> (char-code (schar string i)) #xFFFF) 2 1))
              (incf i))
    i))

(declaim (inline line-utf16 line-lisp))
(defun line-utf16 (line index)
  (if (text-line-astral-p line)
      (utf16-index (text-line-string line) index)
      index))

(defun line-lisp (line utf16-offset)
  (if (text-line-astral-p line)
      (lisp-index (text-line-string line) utf16-offset)
      utf16-offset))

;;; Shaping

(defun attributed-string (font string)
  "A new attributed string of STRING in FONT. The caller owns it."
  (let* ((cf (cf-string string))
         (attributes (font-attributes (font-ct-font font)))
         (attributed (ccall "CFAttributedStringCreate" :ulong
                            (:ulong 0) (:ulong cf) (:ulong attributes))))
    (cf-release cf)
    (cf-release attributes)
    attributed))

(defun line-metrics (ct-line)
  "The width, ascent and descent of CT-LINE as three double floats."
  (sb-alien:with-alien ((ascent sb-alien:double)
                        (descent sb-alien:double)
                        (leading sb-alien:double))
    (let ((width (ccall "CTLineGetTypographicBounds" :double
                        (:ulong ct-line)
                        (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr ascent))))
                        (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr descent))))
                        (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr leading)))))))
      (values width ascent descent))))

(defun fill-glyphs (ct-line glyphs glyph-fonts xs ys)
  "Copy the glyphs of CT-LINE, each run's font and the position of each glyph into
the arrays, which must be long enough."
  (let ((runs (ccall "CTLineGetGlyphRuns" :ulong (:ulong ct-line)))
        (font-key (cf-constant "kCTFontAttributeName"))
        (position 0))
    (dotimes (r (cf-array-count runs))
      (let* ((run (cf-array-ref runs r))
             (count (ccall "CTRunGetGlyphCount" :long (:ulong run)))
             (attributes (ccall "CTRunGetAttributes" :ulong (:ulong run)))
             (run-font (ccall "CFDictionaryGetValue" :ulong
                              (:ulong attributes) (:ulong font-key)))
             (points (make-array (* 2 count) :element-type 'double-float)))
        ;; A CFRange of zero length means the whole run. The glyphs are written
        ;; straight into our array, at this run's place in it.
        (sb-sys:with-pinned-objects (glyphs points)
          (ccall "CTRunGetGlyphs" :void
                 (:ulong run) (:long 0) (:long 0)
                 (:pointer (+ (sb-sys:sap-int (sb-sys:vector-sap glyphs)) (* 2 position))))
          (ccall "CTRunGetPositions" :void
                 (:ulong run) (:long 0) (:long 0)
                 (:pointer (sb-sys:sap-int (sb-sys:vector-sap points)))))
        (dotimes (i count)
          (setf (aref glyph-fonts (+ position i)) run-font
                (aref xs (+ position i)) (float (aref points (* 2 i)) 1f0)
                (aref ys (+ position i)) (float (aref points (1+ (* 2 i))) 1f0)))
        (incf position count)))))

(defun shape-line (font string)
  "Shape STRING in FONT into a new TEXT-LINE, asking Core Text. Use GET-LINE to
shape a string once."
  (let* ((string (copy-seq string))
         (attributed (attributed-string font string))
         (ct-line (ccall "CTLineCreateWithAttributedString" :ulong (:ulong attributed))))
    (cf-release attributed)
    (multiple-value-bind (width ascent descent) (line-metrics ct-line)
      (let* ((count (ccall "CTLineGetGlyphCount" :long (:ulong ct-line)))
             (glyphs (make-array count :element-type '(unsigned-byte 16)))
             (glyph-fonts (make-array count :element-type '(unsigned-byte 64)))
             (xs (make-array count :element-type 'single-float))
             (ys (make-array count :element-type 'single-float)))
        (fill-glyphs ct-line glyphs glyph-fonts xs ys)
        (%make-text-line :string string
                         :font font
                         :ct-line ct-line
                         :width (float width 1f0)
                         :ascent (float ascent 1f0)
                         :descent (float descent 1f0)
                         :astral-p (astral-string-p string)
                         :glyph-count count
                         :glyphs glyphs
                         :glyph-fonts glyph-fonts
                         :xs xs
                         :ys ys)))))

(defvar *lines-per-generation* 4096
  "How many lines a font keeps of those it used lately before the older ones are let go.")

(defun release-lines (table)
  "Release the Core Text lines of TABLE and empty it."
  (maphash (lambda (string line)
             (declare (ignore string))
             (cf-release (text-line-ct-line line)))
           table)
  (clrhash table))

(defun get-line (font string)
  "The TEXT-LINE for STRING in FONT, shaped the first time and kept for a while (see the note
above on generations). Allocates nothing when the line is already in the first generation."
  (let ((lines (font-lines font)))
    (or (gethash string lines)
        (let* ((older (font-older-lines font))
               (line (gethash string older)))
          (cond (line
                 ;; Used again: it moves to the generation of the lines used lately.
                 (remhash (text-line-string line) older)
                 (setf (gethash (text-line-string line) lines) line))
                (t
                 (when (>= (hash-table-count lines) *lines-per-generation*)
                   ;; The generation before is let go, and this one becomes it.
                   (release-lines older)
                   (rotatef (font-lines font) (font-older-lines font))
                   (setf lines (font-lines font)))
                 (let ((new (shape-line font string)))
                   (setf (gethash (text-line-string new) lines) new))))))))

(defun clear-line-cache (font)
  "Forget every line shaped in FONT and release its Core Text lines."
  (release-lines (font-lines font))
  (release-lines (font-older-lines font)))

(defun measure-text (font string)
  "The width, ascent and descent of STRING in FONT, in points, as three values."
  (let ((line (get-line font string)))
    (values (text-line-width line) (text-line-ascent line) (text-line-descent line))))

;;; Carets

(defun caret-x (line index)
  "How far from the start of LINE the caret sits before the character at INDEX
(INDEX may be the length of the string, for the caret after the last one)."
  (unless (<= 0 index (length (text-line-string line)))
    (error "Index ~d is outside a string of ~d characters."
           index (length (text-line-string line))))
  (sb-alien:with-alien ((secondary sb-alien:double))
    (float (ccall "CTLineGetOffsetForStringIndex" :double
                  (:ulong (text-line-ct-line line))
                  (:long (line-utf16 line index))
                  (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr secondary)))))
           1f0)))

(defun index-at-x (line x)
  "The index of the character that the point X, measured from the start of
LINE, is closest to: where a click there would put the caret."
  (let ((offset (ccall "CTLineGetStringIndexForPosition" :long
                       (:ulong (text-line-ct-line line))
                       (:double (float x 1d0)) (:double 0d0))))
    (min (length (text-line-string line))
         (max 0 (line-lisp line offset)))))

(defun caret-rect (line index)
  "The rectangle of the caret at INDEX, relative to the top left of LINE, as
four values: x, y, width and height. It is one point wide and as tall as the line."
  (values (caret-x line index) 0f0 1f0 (+ (text-line-ascent line) (text-line-descent line))))

;;; Breaking a paragraph

(defun break-lines (font string width)
  "Break STRING into lines no wider than WIDTH points where it can (a single
word wider than WIDTH gets a line of its own). Returns a list of (START END)
index pairs, one for each line, covering the string without gaps. A newline
ends a line."
  (let* ((string (copy-seq string))
         (astral (astral-string-p string))
         (attributed (attributed-string font string))
         (typesetter (ccall "CTTypesetterCreateWithAttributedString" :ulong (:ulong attributed)))
         (length (ccall "CFAttributedStringGetLength" :long (:ulong attributed)))
         (start 0)
         (lines '()))
    (flet ((to-lisp (offset) (if astral (lisp-index string offset) offset)))
      (loop :while (< start length)
            :do (let* ((count (max 1 (ccall "CTTypesetterSuggestLineBreak" :long
                                            (:ulong typesetter) (:long start)
                                            (:double (float width 1d0)))))
                       (end (min length (+ start count))))
                  (push (list (to-lisp start) (to-lisp end)) lines)
                  (setf start end))))
    (cf-release typesetter)
    (cf-release attributed)
    (nreverse lines)))
