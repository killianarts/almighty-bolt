(defpackage #:almighty-bolt-core/display-list
  (:use #:cl)
  (:export #:display-list
           #:make-display-list
           #:display-list-capacity
           #:display-list-count
           #:display-list-full
           #:display-list-full-capacity
           #:clear-display-list
           #:+command-rect+
           #:+command-rounded-rect+
           #:+command-glyph+
           #:pack-color
           #:push-rect
           #:push-rounded-rect
           #:push-glyph
           #:display-list-kind
           #:display-list-x
           #:display-list-y
           #:display-list-width
           #:display-list-height
           #:display-list-parameter
           #:display-list-radius
           #:display-list-color
           #:display-list-hash))

(in-package #:almighty-bolt-core/display-list)

;;; The boundary between a variant and the renderer: a flat list of drawing
;;; commands that the variant appends to and the renderer reads. It is
;;; preallocated and reused every frame, so building one allocates nothing.
;;; The commands are stored as parallel arrays (one per field): an array of one
;;; numeric type is a block of raw numbers, so filling it allocates nothing.
;;; The GPU wants one command's fields together, so the renderer interleaves
;;; the arrays as it copies them. Commands are added as the renderer learns to
;;; draw them; borders, shadows, clips and transforms come later.
;;;
;;; Every command has a rectangle (x, y, width, height, in points from the top
;;; left), a colour, and one kind-specific number, the parameter: the corner
;;; radius of a rounded rectangle, or the index of a glyph in the glyph atlas.

(defconstant +command-rect+ 0 "A filled rectangle.")
(defconstant +command-rounded-rect+ 1 "A filled rectangle with rounded corners.")
(defconstant +command-glyph+ 2
  "One glyph: the rectangle is where its bitmap goes, the parameter is its index in
the glyph atlas, and the colour is the colour of the text.")

(deftype color () '(unsigned-byte 32))

(defun pack-color (red green blue &optional (alpha 255))
  "Pack four 0-255 channels into one RGBA word, red in the high byte."
  (declare (type (unsigned-byte 8) red green blue alpha))
  (logior (ash red 24) (ash green 16) (ash blue 8) alpha))

(define-condition display-list-full (error)
  ((capacity :initarg :capacity :reader display-list-full-capacity))
  (:report (lambda (condition stream)
             (format stream "The display list is full at ~d commands; a frame ~
                             must not emit more than the list was made for."
                     (display-list-full-capacity condition)))))

(defstruct (display-list (:constructor %make-display-list
                             (capacity kinds xs ys widths heights parameters colors)))
  (capacity 1 :type (integer 1 #.(1- array-dimension-limit)) :read-only t)
  (kinds nil :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (xs nil :type (simple-array single-float (*)) :read-only t)
  (ys nil :type (simple-array single-float (*)) :read-only t)
  (widths nil :type (simple-array single-float (*)) :read-only t)
  (heights nil :type (simple-array single-float (*)) :read-only t)
  (parameters nil :type (simple-array single-float (*)) :read-only t)
  (colors nil :type (simple-array color (*)) :read-only t)
  (count 0 :type fixnum))

(defun make-display-list (&key (capacity 65536))
  "An empty display list that holds up to CAPACITY commands."
  (check-type capacity (integer 1 #.(1- array-dimension-limit)))
  (flet ((floats () (make-array capacity :element-type 'single-float
                                         :initial-element 0f0)))
    (%make-display-list capacity
                        (make-array capacity :element-type '(unsigned-byte 8)
                                             :initial-element 0)
                        (floats) (floats) (floats) (floats) (floats)
                        (make-array capacity :element-type 'color
                                             :initial-element 0))))

(declaim (inline clear-display-list))
(defun clear-display-list (display-list)
  "Empty DISPLAY-LIST for the next frame. The storage is kept."
  (setf (display-list-count display-list) 0)
  display-list)

(declaim (inline push-command))
(defun push-command (display-list kind x y width height parameter color)
  (declare (type display-list display-list)
           (type (unsigned-byte 8) kind)
           (type real x y width height parameter)
           (type color color))
  (let ((i (display-list-count display-list)))
    (when (>= i (display-list-capacity display-list))
      (error 'display-list-full :capacity (display-list-capacity display-list)))
    (setf (aref (display-list-kinds display-list) i) kind
          (aref (display-list-xs display-list) i) (float x 1f0)
          (aref (display-list-ys display-list) i) (float y 1f0)
          (aref (display-list-widths display-list) i) (float width 1f0)
          (aref (display-list-heights display-list) i) (float height 1f0)
          (aref (display-list-parameters display-list) i) (float parameter 1f0)
          (aref (display-list-colors display-list) i) color
          (display-list-count display-list) (1+ i))
    i))

(defun push-rect (display-list x y width height color)
  "Append a filled rectangle. Returns its index."
  (push-command display-list +command-rect+ x y width height 0 color))

(defun push-rounded-rect (display-list x y width height radius color)
  "Append a filled rectangle with corners of RADIUS. Returns its index."
  (push-command display-list +command-rounded-rect+ x y width height radius color))

(defun push-glyph (display-list x y width height atlas-index color)
  "Append the glyph at ATLAS-INDEX in the glyph atlas, with its bitmap's top left
at X, Y and its size WIDTH by HEIGHT (all in points). Returns its index."
  (push-command display-list +command-glyph+ x y width height atlas-index color))

(defmacro define-command-accessor (name array-accessor)
  `(progn
     (declaim (inline ,name))
     (defun ,name (display-list index)
       ,(format nil "The ~(~a~) of command INDEX." (subseq (symbol-name name) 13))
       (unless (< -1 index (display-list-count display-list))
         (error "No command ~d in a display list holding ~d."
                index (display-list-count display-list)))
       (aref (,array-accessor display-list) index))))

(define-command-accessor display-list-kind display-list-kinds)
(define-command-accessor display-list-x display-list-xs)
(define-command-accessor display-list-y display-list-ys)
(define-command-accessor display-list-width display-list-widths)
(define-command-accessor display-list-height display-list-heights)
(define-command-accessor display-list-parameter display-list-parameters)
(define-command-accessor display-list-color display-list-colors)

(declaim (inline display-list-radius))
(defun display-list-radius (display-list index)
  "The corner radius of command INDEX (its parameter, for a rounded rectangle)."
  (display-list-parameter display-list index))

(defun display-list-hash (display-list)
  "A 62-bit FNV-1a style hash of the commands in DISPLAY-LIST, over the exact
bits of every field. Two frames that draw the same things hash alike, so replay
can check determinism without a GPU. The result is a fixnum, so hashing a frame
does not allocate (a full 64-bit result would be boxed as a bignum)."
  (declare (type display-list display-list)
           (optimize (speed 3) (safety 0)))
  (let ((hash #xCBF29CE484222325)
        (kinds (display-list-kinds display-list))
        (xs (display-list-xs display-list))
        (ys (display-list-ys display-list))
        (widths (display-list-widths display-list))
        (heights (display-list-heights display-list))
        (parameters (display-list-parameters display-list))
        (colors (display-list-colors display-list)))
    (declare (type (unsigned-byte 64) hash))
    (flet ((mix (word)
             (declare (type (unsigned-byte 32) word))
             (setf hash (ldb (byte 64 0)
                             (* (logxor hash word) #x100000001B3))))
           (bits (float)
             (declare (type single-float float))
             (ldb (byte 32 0) (sb-kernel:single-float-bits float))))
      (declare (inline mix bits))
      (mix (ldb (byte 32 0) (display-list-count display-list)))
      (dotimes (i (display-list-count display-list))
        (mix (aref kinds i))
        (mix (bits (aref xs i)))
        (mix (bits (aref ys i)))
        (mix (bits (aref widths i)))
        (mix (bits (aref heights i)))
        (mix (bits (aref parameters i)))
        (mix (aref colors i))))
    ;; Fold the top bits in, then keep 62 so the result is an immediate fixnum.
    (ldb (byte 62 0) (logxor hash (ash hash -62)))))
