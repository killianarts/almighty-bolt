(defpackage #:almighty-bolt-variant-retained/t/fake-context
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context
                #:context-font
                #:context-measure
                #:context-push-text
                #:push-glyph)
  (:export #:fake-context
           #:make-fake-context
           #:fake-context-requests
           #:+char-width+
           #:+ascent+
           #:+descent+))

(in-package #:almighty-bolt-variant-retained/t/fake-context)

;;; A context with text whose numbers are easy to work with, so that layout and
;;; painting can be checked exactly and without a Mac: every character is 8 points
;;; wide whatever the font, with an ascent of 10 and a descent of 4, and drawing a
;;; string appends one glyph command for each character, whose atlas index is the
;;; character's code.

(defconstant +char-width+ 8f0)
(defconstant +ascent+ 10f0)
(defconstant +descent+ 4f0)

(defstruct (fake-context (:include context)
                         (:constructor make-fake-context
                             (&key (width 400f0) (height 300f0) (scale 1f0))))
  "Counts the frames that were requested through it, in REQUESTS."
  (requests 0 :type fixnum))

(defmethod context-font ((context fake-context) name size)
  (list name size))

(defmethod context-measure ((context fake-context) font string)
  (declare (ignore font))
  (values (* +char-width+ (length string)) +ascent+ +descent+))

(defmethod context-push-text ((context fake-context) list font string x baseline color)
  (declare (ignore font))
  (loop :for i :from 0 :below (length string)
        :do (push-glyph list (+ x (* i +char-width+)) (- baseline +ascent+)
                        +char-width+ (+ +ascent+ +descent+) (char-code (char string i)) color))
  (length string))
