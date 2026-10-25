(defpackage #:almighty-bolt-variant-null/null
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:handle-event
                #:request-frame
                #:context-width
                #:context-height
                #:context-font
                #:context-break-lines
                #:context-push-text
                #:make-display-list
                #:replace-display-list
                #:push-rounded-rect
                #:pack-color)
  (:export #:null-variant
           #:make-null-variant
           #:null-variant-scene
           #:null-variant-events-seen
           #:null-variant-animate-frames))

(in-package #:almighty-bolt-variant-null/null)

;;; The baseline variant of the charter: it replays a fixed, precomputed display
;;; list and does nothing else, so that what a frame costs with it is what the core
;;; costs. Every other variant's results are reported relative to it.
;;;
;;; The list is built once, from the first frame's size, through the context's text
;;; operations (so this file needs only the umbrella package). After that a frame
;;; copies the list into the frame's list and settles at once. Events are counted
;;; and ignored, so the host's event path can be seen working without costing a
;;; frame.

(defparameter +paragraph+
  "Text is shaped by Core Text, drawn by Core Graphics into a texture atlas, and put on screen by the Metal renderer. A paragraph breaks into lines that fit its width, and every line is shaped once and kept.")

(defclass null-variant (variant)
  ((scene :initform nil :accessor null-variant-scene)
   (events-seen :initform 0 :type fixnum :accessor null-variant-events-seen)
   ;; How many more frames to ask for, one after each frame, to test that a request
   ;; made while a frame is being drawn is honoured.
   (animate-frames :initarg :animate-frames :initform 0 :type fixnum
                   :accessor null-variant-animate-frames))
  (:documentation "Replays a fixed display list."))

(defun make-null-variant (&key (animate-frames 0))
  (make-instance 'null-variant :animate-frames animate-frames))

(defun build-scene (context)
  "The fixed list: a panel, a heading, a row of rounded rectangles, a paragraph
broken to the width of the drawing area, and a line of code."
  (let* ((width (context-width context))
         (height (context-height context))
         (list (make-display-list :capacity 4096))
         (heading-font (context-font context "Helvetica" 28))
         (body-font (context-font context "Helvetica" 15))
         (code-font (context-font context "Menlo" 13))
         (white (pack-color 255 255 255))
         (grey (pack-color 190 195 210))
         (green (pack-color 120 220 160)))
    (push-rounded-rect list 8 8 (- width 16) (- height 16) 10 (pack-color 36 40 56))
    (context-push-text context list heading-font "Baseline" 24 46 white)
    (dotimes (i 8)
      (push-rounded-rect list (+ 24 (* i 60)) 60 48 24 6 (pack-color (* i 30) 120 200)))
    (let ((y 112))
      (dolist (range (context-break-lines context body-font +paragraph+ (- width 48)))
        (destructuring-bind (start end) range
          (context-push-text context list body-font
                             (string-right-trim " " (subseq +paragraph+ start end))
                             24 y grey)
          (incf y 20)))
      (context-push-text context list code-font
                         "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))"
                         24 (+ y 12) green))
    list))

(defmethod frame ((variant null-variant) context list)
  (unless (null-variant-scene variant)
    (setf (null-variant-scene variant) (build-scene context)))
  (replace-display-list list (null-variant-scene variant))
  (when (plusp (null-variant-animate-frames variant))
    (decf (null-variant-animate-frames variant))
    (request-frame context))
  nil)

(defmethod handle-event ((variant null-variant) event)
  (declare (ignore event))
  (incf (null-variant-events-seen variant))
  nil)
