(defpackage #:almighty-bolt-bench-null/null
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals))
  (:export #:null-app
           #:make-null-app
           #:null-app-scene
           #:null-app-events-seen
           #:null-app-animate-frames))

(in-package #:almighty-bolt-bench-null/null)

;;; The baseline app: it replays a fixed, precomputed display
;;; list and does nothing else, so that what a frame costs with it is what the host and
;;; the renderer cost. What the ui costs can be read against it.
;;;
;;; The list is built once, from the first frame's size, through the context's text
;;; operations (so this file needs only the umbrella package). After that a frame
;;; copies the list into the frame's list and settles at once. Events are counted
;;; and ignored, so the host's event path can be seen working without costing a
;;; frame.

(defparameter +paragraph+
  "Text is shaped by Core Text, drawn by Core Graphics into a texture atlas, and put on screen by the Metal renderer. A paragraph breaks into lines that fit its width, and every line is shaped once and kept.")

(defclass null-app (core:app-instance)
  ((scene :initform nil :accessor null-app-scene)
   (events-seen :initform 0 :type fixnum :accessor null-app-events-seen)
   ;; How many more frames to ask for, one after each frame, to test that a request
   ;; made while a frame is being drawn is honoured.
   (animate-frames :initarg :animate-frames :initform 0 :type fixnum
                   :accessor null-app-animate-frames))
  (:documentation "Replays a fixed display list."))

(defun make-null-app (&key (animate-frames 0))
  (make-instance 'null-app :animate-frames animate-frames))

(defun build-scene (context)
  "The fixed list: a panel, a heading, a row of rounded rectangles, a paragraph
broken to the width of the drawing area, and a line of code."
  (let* ((width (core:context-width context))
         (height (core:context-height context))
         (list (core:make-display-list :capacity 4096))
         (heading-font (core:context-font context "Helvetica" 28))
         (body-font (core:context-font context "Helvetica" 15))
         (code-font (core:context-font context "Menlo" 13))
         (white (core:pack-color 255 255 255))
         (grey (core:pack-color 190 195 210))
         (green (core:pack-color 120 220 160)))
    (core:push-rounded-rect list 8 8 (- width 16) (- height 16) 10 (core:pack-color 36 40 56))
    (core:context-push-text context list heading-font "Baseline" 24 46 white)
    (dotimes (i 8)
      (core:push-rounded-rect list (+ 24 (* i 60)) 60 48 24 6 (core:pack-color (* i 30) 120 200)))
    (let ((y 112))
      (dolist (range (core:context-break-lines context body-font +paragraph+ (- width 48)))
        (destructuring-bind (start end) range
          (core:context-push-text context list body-font
                                  (string-right-trim " " (subseq +paragraph+ start end))
                                  24 y grey)
          (incf y 20)))
      (core:context-push-text context list code-font
                              "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))"
                              24 (+ y 12) green))
    list))

(defmethod core:frame ((instance null-app) context list)
  (unless (null-app-scene instance)
    (setf (null-app-scene instance) (build-scene context)))
  (core:replace-display-list list (null-app-scene instance))
  (when (plusp (null-app-animate-frames instance))
    (decf (null-app-animate-frames instance))
    (core:request-frame context))
  nil)

(defmethod core:handle-event ((instance null-app) event)
  (declare (ignore event))
  (incf (null-app-events-seen instance))
  nil)
