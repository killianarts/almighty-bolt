(defpackage #:almighty-bolt/test-support/fake-context
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:context
                #:context-font
                #:context-measure
                #:context-break-lines
                #:context-caret-x
                #:context-index-at-x
                #:context-push-text
                #:context-push-image
                #:image-id
                #:pack-color
                #:push-image
                #:push-glyph)
  (:export #:fake-context
           #:make-fake-context
           #:+char-width+
           #:+ascent+
           #:+descent+))

(in-package #:almighty-bolt/test-support/fake-context)

;;; A context with text whose numbers are easy to work with, so that layout and
;;; painting can be checked exactly and without a Mac: every character is 8 points
;;; wide (9 in a bold font, so that the weight shows in a layout), with an ascent of 10 and a descent of 4, and drawing a
;;; string appends one glyph command for each character, whose atlas index is the
;;; character's code. A paragraph breaks at a line feed and at the width, between characters.

(defconstant +char-width+ 8f0)
(defconstant +ascent+ 10f0)
(defconstant +descent+ 4f0)

(defstruct (fake-context (:include context)
                         (:constructor make-fake-context
                             (&key (width 400f0) (height 300f0) (scale 1f0)))))

(defmethod context-font ((context fake-context) name size &optional weight tracking)
  (list name size (or weight :regular) (or tracking 0)))

(defun heavy-p (weight)
  "True for a weight of 600 or more, given as a number or a name."
  (if (numberp weight)
      (>= weight 600)
      (member weight '(:semibold :bold :extra-bold :black))))

(defun width-of (font)
  "How wide a character of FONT is: 9 points in a font of weight 600 or more, and 8 in any other,
and the font's tracking after each."
  (if (consp font)
      (+ (if (heavy-p (third font)) 9f0 +char-width+) (float (or (fourth font) 0) 1f0))
      +char-width+))

(defmethod context-measure ((context fake-context) font string)
  (values (* (width-of font) (length string)) +ascent+ +descent+))

(defmethod context-caret-x ((context fake-context) font string index)
  (declare (ignore string))
  (* (width-of font) index))

(defmethod context-index-at-x ((context fake-context) font string x)
  (max 0 (min (length string) (round x (width-of font)))))

(defmethod context-break-lines ((context fake-context) font string width)
  ;; As Core Text's does, a line feed ends a line, and is the last character of it.
  (let ((per-line (max 1 (floor width (width-of font))))
        (length (length string))
        (lines '()))
    (if (zerop length)
        (list (list 0 0))
        (let ((start 0))
          (loop :while (< start length)
                :do (let* ((feed (position #\Linefeed string :start start))
                           (stop (if feed (1+ feed) length)))
                      (loop :for from :from start :below stop :by per-line
                            :do (push (list from (min stop (+ from per-line))) lines))
                      (setf start stop)))
          (nreverse lines)))))

(defmethod context-push-text ((context fake-context) list font string x baseline color
                              &optional turn)
  ;; Turned, each character's cell is turned about X, BASELINE: up, the line goes up from there with
  ;; the ascent to the left; down, it goes down with the ascent to the right.
  (loop :with width = (width-of font)
        :with tall = (+ +ascent+ +descent+)
        :for i :from 0 :below (length string)
        :for code = (char-code (char string i))
        :do (ecase turn
              ((nil) (push-glyph list (+ x (* i width)) (- baseline +ascent+) width tall code color))
              (:up (push-glyph list (- x +ascent+) (- baseline (* (1+ i) width)) tall width code
                               color :up))
              (:down (push-glyph list (- x +descent+) (+ baseline (* i width)) tall width code
                                 color :down))))
  (length string))

(defmethod context-push-image ((context fake-context) list image x y width height &key tint (opacity 1))
  ;; There is no atlas: the command's parameter is the image's id, so that a test can see which
  ;; image was drawn.
  (push-image list x y width height (image-id image)
              (or tint (pack-color 255 255 255 (round (* 255 (max 0 (min 1 opacity))))))
              (and tint t)))
