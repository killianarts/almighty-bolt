(defpackage #:almighty-bolt/t/text/line
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:font #:almighty-bolt/text/font)
                    (#:line #:almighty-bolt/text/line)))

(in-package #:almighty-bolt/t/text/line)

;;; Core Text through the measurement API. These use fonts that ship with macOS,
;;; and compare widths with each other and with bounds rather than with exact
;;; numbers, which change a little between operating system versions.

(defmacro with-fonts ((helvetica menlo) &body body)
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let ((,helvetica (font:make-font "Helvetica" 16))
             (,menlo (font:make-font "Menlo" 12)))
         (declare (ignorable ,helvetica ,menlo))
         ,@body))))

(defun close-to (expected actual &optional (tolerance 0.01))
  (<= (abs (- expected actual)) tolerance))

;;; --- measuring ---

(define-test a-font-knows-its-metrics ()
  (with-fonts (helvetica menlo)
    (assert-eql 16f0 (font:font-size helvetica))
    (assert-true (< 10 (font:font-ascent helvetica) 20))
    (assert-true (< 1 (font:font-descent helvetica) 8))
    (assert-true (< (* 0.9 (font:font-size helvetica))
                    (font:font-line-height helvetica)
                    (* 1.5 (font:font-size helvetica))))
    (assert-true (typep (font:font-ascent helvetica) 'single-float))))

(define-test the-width-of-a-string-is-reasonable ()
  (with-fonts (helvetica menlo)
    (assert-eql 0f0 (line:measure-text helvetica ""))
    (let ((width (line:measure-text helvetica "Hello")))
      (assert-true (typep width 'single-float))
      (assert-true (< 25 width 60) width))
    (assert-true (> (line:measure-text helvetica "Hello, world")
                    (line:measure-text helvetica "Hello")))))

(define-test measuring-returns-width-ascent-and-descent ()
  (with-fonts (helvetica menlo)
    (multiple-value-bind (width ascent descent) (line:measure-text helvetica "Hello")
      (assert-true (plusp width))
      (assert-true (close-to (font:font-ascent helvetica) ascent 0.5))
      (assert-true (close-to (font:font-descent helvetica) descent 0.5)))))

(define-test a-monospaced-font-gives-every-character-the-same-width ()
  (with-fonts (helvetica menlo)
    (let ((one (line:measure-text menlo "i")))
      (assert-true (close-to one (line:measure-text menlo "W")))
      (assert-true (close-to (* 4 one) (line:measure-text menlo "iiii")))
      (assert-true (close-to (line:measure-text menlo "iiii") (line:measure-text menlo "WWWW"))))))

(define-test the-width-of-two-letters-is-about-the-sum-of-their-widths ()
  ;; Kerning moves them a little, so this is not exact.
  (with-fonts (helvetica menlo)
    (assert-true (close-to (+ (line:measure-text helvetica "a") (line:measure-text helvetica "b"))
                           (line:measure-text helvetica "ab") 1.0))))

(define-test text-in-other-scripts-gets-fonts-that-have-the-glyphs ()
  ;; Helvetica has no Japanese: Core Text substitutes a font for those glyphs, and
  ;; the line records which font each glyph came from.
  (with-fonts (helvetica menlo)
    (let ((line (line:get-line helvetica "Hello こんにちは")))
      (assert-true (> (length (remove-duplicates (line:text-line-glyph-fonts line))) 1))
      (assert-true (> (line:text-line-width line) (line:measure-text helvetica "Hello"))))))

(define-test a-line-holds-a-glyph-and-position-for-each-character ()
  (with-fonts (helvetica menlo)
    (let ((line (line:get-line helvetica "abc")))
      (assert-eql 3 (line:text-line-glyph-count line))
      (assert-eql 3 (length (line:text-line-glyphs line)))
      (assert-eql 0f0 (aref (line:text-line-xs line) 0))
      (assert-true (< (aref (line:text-line-xs line) 0)
                      (aref (line:text-line-xs line) 1)
                      (aref (line:text-line-xs line) 2))))))

;;; --- carets ---

(define-test the-caret-moves-from-the-start-to-the-end-of-the-line ()
  (with-fonts (helvetica menlo)
    (let* ((text "Hello, world")
           (line (line:get-line helvetica text)))
      (assert-true (close-to 0f0 (line:caret-x line 0)))
      (assert-true (close-to (line:text-line-width line) (line:caret-x line (length text)) 0.1))
      (loop :for i :from 1 :to (length text)
            :do (assert-true (> (line:caret-x line i) (line:caret-x line (1- i)))
                             i)))))

(define-test a-click-at-a-caret-finds-that-index ()
  (with-fonts (helvetica menlo)
    (let* ((text "Hello, world")
           (line (line:get-line helvetica text)))
      (dotimes (i (length text))
        (assert-eql i (line:index-at-x line (+ (line:caret-x line i) 0.5f0)) i)))))

(define-test a-click-outside-the-line-finds-the-ends ()
  (with-fonts (helvetica menlo)
    (let ((line (line:get-line helvetica "Hello")))
      (assert-eql 0 (line:index-at-x line -50f0))
      (assert-eql 5 (line:index-at-x line 500f0)))))

(define-test the-caret-rectangle-is-a-point-wide-and-a-line-tall ()
  (with-fonts (helvetica menlo)
    (let ((line (line:get-line helvetica "Hello")))
      (multiple-value-bind (x y width height) (line:caret-rect line 2)
        (assert-true (close-to (line:caret-x line 2) x))
        (assert-eql 0f0 y)
        (assert-eql 1f0 width)
        (assert-true (close-to (+ (line:text-line-ascent line) (line:text-line-descent line))
                               height))))))

(define-test an-index-outside-the-string-is-an-error ()
  (with-fonts (helvetica menlo)
    (let ((line (line:get-line helvetica "Hello")))
      (assert-error 'error (line:caret-x line 6))
      (assert-error 'error (line:caret-x line -1)))))

(define-test indices-count-characters-not-utf-16-units ()
  ;; The emoji is two UTF-16 units but one character.
  (with-fonts (helvetica menlo)
    (let* ((text (format nil "a~cb" (code-char #x1F600)))
           (line (line:get-line helvetica text)))
      (assert-eql 3 (length text))
      (assert-true (< (line:caret-x line 0) (line:caret-x line 1)
                      (line:caret-x line 2) (line:caret-x line 3)))
      (assert-eql 2 (line:index-at-x line (+ (line:caret-x line 2) 0.5f0)))
      (assert-eql 3 (line:index-at-x line 1000f0)))))

;;; --- breaking a paragraph ---

(define-test a-paragraph-breaks-into-lines-that-fit ()
  (with-fonts (helvetica menlo)
    (let* ((text "the quick brown fox jumps over the lazy dog")
           (lines (line:break-lines helvetica text 100)))
      (assert-true (>= (length lines) 3) lines)
      ;; They cover the string without gaps or overlap.
      (assert-eql 0 (first (first lines)))
      (assert-eql (length text) (second (car (last lines))))
      (loop :for (previous next) :on lines
            :while next
            :do (assert-eql (second previous) (first next)))
      ;; And each fits, trailing space aside.
      (dolist (range lines)
        (let ((piece (string-right-trim " " (subseq text (first range) (second range)))))
          (assert-true (<= (line:measure-text helvetica piece) 100.5) piece))))))

(define-test a-wide-enough-width-gives-one-line ()
  (with-fonts (helvetica menlo)
    (assert-equal '((0 11)) (line:break-lines helvetica "hello world" 1000))
    (assert-equal '() (line:break-lines helvetica "" 100))))

(define-test a-newline-ends-a-line ()
  (with-fonts (helvetica menlo)
    (let ((lines (line:break-lines helvetica (format nil "ab~%cd") 1000)))
      (assert-eql 2 (length lines))
      (assert-equal '(0 3) (first lines)))))

(define-test a-word-wider-than-the-width-gets-a-line-of-its-own ()
  (with-fonts (helvetica menlo)
    (let ((lines (line:break-lines helvetica "unbelievably long" 10)))
      (assert-true (>= (length lines) 2))
      (assert-eql 0 (first (first lines)))
      (assert-eql 17 (second (car (last lines)))))))

(define-test breaking-counts-characters-after-an-emoji ()
  (with-fonts (helvetica menlo)
    (let* ((text (format nil "~c ~c ~c" (code-char #x1F600) (code-char #x1F601) (code-char #x1F602)))
           (lines (line:break-lines helvetica text 1000)))
      (assert-equal (list (list 0 (length text))) lines))))

;;; --- the cache ---

(define-test the-same-string-in-the-same-font-is-one-line ()
  (with-fonts (helvetica menlo)
    (assert-true (eq (line:get-line helvetica "Hello") (line:get-line helvetica "Hello")))
    (assert-false (eq (line:get-line helvetica "Hello") (line:get-line menlo "Hello")))
    (assert-false (eq (line:get-line helvetica "Hello") (line:get-line helvetica "Hello!")))))

(define-test the-cache-is-not-fooled-by-a-string-that-changes ()
  (with-fonts (helvetica menlo)
    (let* ((text (make-array 3 :element-type 'character :initial-contents "abc" :adjustable t))
           (line (line:get-line helvetica text)))
      (setf (char text 0) #\z)
      ;; The cache kept its own copy, so the old key still finds the old line.
      (assert-true (eq line (line:get-line helvetica "abc")))
      (assert-false (eq line (line:get-line helvetica text))))))

(define-test clearing-the-cache-forgets-lines ()
  (with-fonts (helvetica menlo)
    (let ((line (line:get-line helvetica "Hello")))
      (line:clear-line-cache helvetica)
      (assert-false (eq line (line:get-line helvetica "Hello"))))))

;;; The cache is of two generations: what is used lately and what was used
;;; before; a font holds at most twice *LINES-PER-GENERATION* lines whatever it is asked to shape.

(defun cached (font)
  (+ (hash-table-count (font:font-lines font)) (hash-table-count (font:font-older-lines font))))

(define-test the-cache-holds-at-most-two-generations-of-lines ()
  (with-fonts (helvetica menlo)
    (let ((line:*lines-per-generation* 10))
      (dotimes (i 500)
        (line:get-line helvetica (format nil "row ~d" i))
        (assert-true (<= (cached helvetica) 20) i))
      (assert-true (>= (cached helvetica) 10)))))

(define-test a-line-that-is-used-again-is-kept-and-one-that-is-not-is-let-go ()
  (with-fonts (helvetica menlo)
    (let ((line:*lines-per-generation* 10))
      (let ((keep (line:get-line helvetica "keep"))
            (lose (line:get-line helvetica "lose")))
        ;; Eight more fill the first generation; the next turns it over.
        (dotimes (i 8) (line:get-line helvetica (format nil "a ~d" i)))
        (dotimes (i 3) (line:get-line helvetica (format nil "b ~d" i)))
        ;; "keep" is used now, and comes back to the first generation as it was.
        (assert-true (eq keep (line:get-line helvetica "keep")))
        ;; Two more turnovers: "lose" was in the one that went first.
        (dotimes (i 25) (line:get-line helvetica (format nil "c ~d" i)))
        (assert-false (eq lose (line:get-line helvetica "lose")))))))

(define-test a-line-shaped-again-after-it-was-let-go-is-the-same-line-to-the-eye ()
  (with-fonts (helvetica menlo)
    (let* ((line:*lines-per-generation* 4)
           (width (line:measure-text helvetica "The same string")))
      (dotimes (i 100) (line:get-line helvetica (format nil "other ~d" i)))
      (assert-true (close-to width (line:measure-text helvetica "The same string"))))))

(define-test the-cache-of-another-font-is-not-touched-by-the-turnover-of-this-one ()
  (with-fonts (helvetica menlo)
    (let ((line:*lines-per-generation* 5))
      (let ((mine (line:get-line menlo "mine")))
        (dotimes (i 200) (line:get-line helvetica (format nil "row ~d" i)))
        (assert-true (eq mine (line:get-line menlo "mine")))))))

;;; --- allocation ---
;;;
;;; The loops are functions compiled with this file: lisp-unit2 evaluates test
;;; bodies under a policy that allocates where compiled code does not.

(defun measure-repeatedly (font count)
  (let ((total 0f0))
    (declare (single-float total))
    (dotimes (i count)
      (incf total (line:measure-text font "Hello, world"))
      (incf total (line:text-line-width (line:get-line font "The quick brown fox"))))
    total))

(define-test shaping-a-new-line-allocates-little-more-than-the-line ()
  ;; A line of 37 characters is the arrays of its glyphs (18 bytes a glyph), its string and its
  ;; structure: about 950 bytes. Looking up the names of the system's constants made 340 bytes each,
  ;; a lookup of them five times a line, and the positions of the glyphs of a run were made in a
  ;; new array: together 1,400 bytes more.
  (with-fonts (helvetica menlo)
    (let ((strings (coerce (loop :for i :below 3000
                                 :collect (format nil "~d the quick brown fox jumps over" (+ 100000 i)))
                           'vector)))
      (assert-eql 37 (length (aref strings 0)))
      (dotimes (i 100) (line:get-line menlo (aref strings i)))
      (sb-ext:gc :full t)
      (let ((before (sb-ext:get-bytes-consed)))
        (loop :for i :from 100 :below 3000
              :do (line:get-line menlo (aref strings i)))
        (let ((each (/ (- (sb-ext:get-bytes-consed) before) 2900.0)))
          (assert-true (< each 1500) each))))))

(define-test the-glyphs-of-a-line-are-the-same-whether-or-not-the-buffer-for-their-positions-was-big-enough ()
  ;; The buffer for the positions of a run grows when a run is longer than it; the line that makes
  ;; it grow and the ones after it must still be right.
  (with-fonts (helvetica menlo)
    (let* ((short (line:shape-line menlo "abc"))
           (long (line:shape-line menlo (make-string 600 :initial-element #\m)))
           (again (line:shape-line menlo "abc")))
      (assert-eql 600 (line:text-line-glyph-count long))
      (assert-true (close-to (* 599 (aref (line:text-line-xs long) 1))
                             (aref (line:text-line-xs long) 599) 0.5))
      (assert-equalp (line:text-line-xs short) (line:text-line-xs again))
      (assert-equalp (line:text-line-glyphs short) (line:text-line-glyphs again)))))

(define-test measuring-a-string-already-seen-allocates-nothing ()
  (with-fonts (helvetica menlo)
    (measure-repeatedly helvetica 10)
    (let ((before (sb-ext:get-bytes-consed)))
      (measure-repeatedly helvetica 200000)
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        ;; 400,000 lookups. A cons or a boxed float each would be megabytes.
        (assert-true (< delta 300000) delta)))))
