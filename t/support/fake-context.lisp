(defpackage #:almighty-bolt-variant-support/fake-context
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context
                #:context-font
                #:context-measure
                #:context-break-lines
                #:context-caret-x
                #:context-index-at-x
                #:context-push-text
                #:push-glyph)
  (:export #:fake-context
           #:make-fake-context
           #:+char-width+
           #:+ascent+
           #:+descent+))

(in-package #:almighty-bolt-variant-support/fake-context)

;;; A context with text whose numbers are easy to work with, so that layout and
;;; painting can be checked exactly and without a Mac: every character is 8 points
;;; wide whatever the font, with an ascent of 10 and a descent of 4, and drawing a
;;; string appends one glyph command for each character, whose atlas index is the
;;; character's code. A paragraph breaks only at the width, between characters.

(defconstant +char-width+ 8f0)
(defconstant +ascent+ 10f0)
(defconstant +descent+ 4f0)

(defstruct (fake-context (:include context)
                         (:constructor make-fake-context
                             (&key (width 400f0) (height 300f0) (scale 1f0)))))

(defmethod context-font ((context fake-context) name size)
  (list name size))

(defmethod context-measure ((context fake-context) font string)
  (declare (ignore font))
  (values (* +char-width+ (length string)) +ascent+ +descent+))

(defmethod context-caret-x ((context fake-context) font string index)
  (declare (ignore font string))
  (* +char-width+ index))

(defmethod context-index-at-x ((context fake-context) font string x)
  (declare (ignore font))
  (max 0 (min (length string) (round x +char-width+))))

(defmethod context-break-lines ((context fake-context) font string width)
  (declare (ignore font))
  (let ((per-line (max 1 (floor width +char-width+))))
    (loop :for start :from 0 :below (max 1 (length string)) :by per-line
          :collect (list start (min (length string) (+ start per-line))))))

(defmethod context-push-text ((context fake-context) list font string x baseline color)
  (declare (ignore font))
  (loop :for i :from 0 :below (length string)
        :do (push-glyph list (+ x (* i +char-width+)) (- baseline +ascent+)
                        +char-width+ (+ +ascent+ +descent+) (char-code (char string i)) color))
  (length string))
