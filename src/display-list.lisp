(defpackage #:almighty-bolt/display-list
  (:use #:cl)
  (:export #:display-list
           #:make-display-list
           #:display-list-capacity
           #:display-list-count
           #:display-list-full
           #:display-list-full-capacity
           #:clear-display-list
           #:replace-display-list
           #:append-display-list
           #:zoom-display-list
           #:unzoom-glyphs
           #:+command-rect+
           #:+command-rounded-rect+
           #:+command-glyph+
           #:+command-glyph-up+
           #:+command-glyph-down+
           #:glyph-kind-p
           #:+command-clip+
           #:+command-unclip+
           #:+command-rounded-stroke+
           #:+command-shadow+
           #:push-rounded-stroke
           #:push-shadow
           #:stroke-width-of
           #:push-clip
           #:push-unclip
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
           #:display-list-extra
           #:display-list-hash))

(in-package #:almighty-bolt/display-list)

;;; The boundary between an app and the renderer: a flat list of drawing
;;; commands that the app appends to and the renderer reads. It is
;;; preallocated and reused every frame, so building one allocates nothing.
;;; The commands are stored as parallel arrays (one per field): an array of one
;;; numeric type is a block of raw numbers, so filling it allocates nothing.
;;; The GPU wants one command's fields together, so the renderer interleaves
;;; the arrays as it copies them. Commands are added as the renderer learns to
;;; draw them; borders, shadows, clips and transforms come later.
;;;
;;; Every command has a rectangle (x, y, width, height, in points from the top
;;; left), a colour, and one kind-specific number, the parameter: the corner
;;; radius of a rounded rectangle, or the index of a glyph in the glyph atlas. A
;;; kind that needs a second number has the extra (a shadow's blur); for the others
;;; it is 0.

(defconstant +command-rect+ 0 "A filled rectangle.")
(defconstant +command-rounded-rect+ 1 "A filled rectangle with rounded corners.")
(defconstant +command-glyph+ 2
  "One glyph: the rectangle is where its bitmap goes, the parameter is its index in
the glyph atlas, and the colour is the colour of the text.")

(defconstant +command-glyph-up+ 5
  "A glyph turned a quarter to the left, as text that reads upwards has it: the rectangle is where
its bitmap goes once turned (its width is the bitmap's height), and the rest is as for a glyph.")
(defconstant +command-glyph-down+ 6
  "A glyph turned a quarter to the right, as text that reads downwards has it.")

(declaim (inline glyph-kind-p))
(defun glyph-kind-p (kind)
  "True if a command of KIND is a glyph, upright or turned: its parameter is an index in the atlas."
  (or (= kind +command-glyph+) (= kind +command-glyph-up+) (= kind +command-glyph-down+)))

(defconstant +command-shadow+ 7
  "A soft shadow: the rounded rectangle, its corner radius the parameter, blurred by the extra (in
points, as a design tool's blur is: the Gaussian's standard deviation is half of it), so that it
reaches that far and a half past the rectangle.")

(defconstant +command-clip+ 3
  "Start clipping what follows to the rectangle, inside the clip that is already on.
Nothing is drawn by it; it counts as a command and takes a place in the list.")
(defconstant +command-unclip+ 4
  "End the innermost clip that was started. Its rectangle is not used.")

(defconstant +command-rounded-stroke+ 16
  "The edge of a rounded rectangle, drawn inside it: the kind is this and the stroke's width in
quarter points added (17 to 255, a quarter of a point to 59.75 points), so that a stroke needs no
field of its own; the parameter is the corner radius, as for a filled one.")

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
                             (capacity kinds xs ys widths heights parameters colors extras)))
  (capacity 1 :type (integer 1 #.(1- array-dimension-limit)) :read-only t)
  (kinds nil :type (simple-array (unsigned-byte 8) (*)) :read-only t)
  (xs nil :type (simple-array single-float (*)) :read-only t)
  (ys nil :type (simple-array single-float (*)) :read-only t)
  (widths nil :type (simple-array single-float (*)) :read-only t)
  (heights nil :type (simple-array single-float (*)) :read-only t)
  (parameters nil :type (simple-array single-float (*)) :read-only t)
  (colors nil :type (simple-array color (*)) :read-only t)
  (extras nil :type (simple-array single-float (*)) :read-only t)
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
                                             :initial-element 0)
                        (floats))))

(declaim (inline clear-display-list))
(defun clear-display-list (display-list)
  "Empty DISPLAY-LIST for the next frame. The storage is kept."
  (setf (display-list-count display-list) 0)
  display-list)

(defun replace-display-list (target source)
  "Make TARGET hold the same commands as SOURCE and return TARGET. SOURCE is not
changed. Copies the arrays and allocates nothing. Signals DISPLAY-LIST-FULL, before
changing TARGET, if SOURCE holds more commands than TARGET has room for."
  (declare (type display-list target source))
  (let ((count (display-list-count source)))
    (when (> count (display-list-capacity target))
      (error 'display-list-full :capacity (display-list-capacity target)))
    (unless (eq target source)
      (replace (display-list-kinds target) (display-list-kinds source) :end2 count)
      (replace (display-list-xs target) (display-list-xs source) :end2 count)
      (replace (display-list-ys target) (display-list-ys source) :end2 count)
      (replace (display-list-widths target) (display-list-widths source) :end2 count)
      (replace (display-list-heights target) (display-list-heights source) :end2 count)
      (replace (display-list-parameters target) (display-list-parameters source)
               :end2 count)
      (replace (display-list-colors target) (display-list-colors source) :end2 count)
      (replace (display-list-extras target) (display-list-extras source) :end2 count)
      (setf (display-list-count target) count))
    target))

(declaim (inline push-command))
(defun push-command (display-list kind x y width height parameter color &optional (extra 0))
  (declare (type display-list display-list)
           (type (unsigned-byte 8) kind)
           (type real x y width height parameter extra)
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
          (aref (display-list-extras display-list) i) (float extra 1f0)
          (display-list-count display-list) (1+ i))
    i))

(defun push-rect (display-list x y width height color)
  "Append a filled rectangle. Returns its index."
  (push-command display-list +command-rect+ x y width height 0 color))

(defun push-rounded-rect (display-list x y width height radius color)
  "Append a filled rectangle with corners of RADIUS. Returns its index."
  (push-command display-list +command-rounded-rect+ x y width height radius color))

(defun push-rounded-stroke (display-list x y width height radius stroke-width color)
  "Append the edge of a rectangle with corners of RADIUS, STROKE-WIDTH points wide and inside it.
Returns its index."
  (push-command display-list
                (+ +command-rounded-stroke+ (max 1 (min 239 (round (* 4 stroke-width)))))
                x y width height radius color))

(defun push-shadow (display-list x y width height radius blur color)
  "Append a soft shadow of the rounded rectangle X, Y, WIDTH by HEIGHT with corner RADIUS, blurred by
BLUR points (as a design tool's blur: it reaches about BLUR and a half past the rectangle, and 0 is
the rectangle's sharp edge), in COLOR. Draw it before what casts it. Returns its index."
  (push-command display-list +command-shadow+ x y width height (max 0 radius) color (max 0 blur)))

(defun stroke-width-of (kind)
  "The width in points of the stroke that a command of KIND is, or NIL if it is not one."
  (and (> kind +command-rounded-stroke+) (/ (- kind +command-rounded-stroke+) 4f0)))

(defun push-glyph (display-list x y width height atlas-index color &optional turn)
  "Append the glyph at ATLAS-INDEX in the glyph atlas, with its bitmap's top left
at X, Y and its size WIDTH by HEIGHT (all in points). TURN :UP draws the bitmap turned a quarter
to the left and :DOWN a quarter to the right, and X, Y, WIDTH and HEIGHT are then the rectangle it
fills turned (WIDTH is the bitmap's height). Returns its index."
  (push-command display-list (ecase turn
                               ((nil) +command-glyph+)
                               (:up +command-glyph-up+)
                               (:down +command-glyph-down+))
                x y width height atlas-index color))

(defun append-display-list (target source &optional (dx 0f0) (dy 0f0))
  "Append the commands of SOURCE to TARGET, each moved DX across and DY down, and return
TARGET. SOURCE is not changed. Copies the arrays and allocates nothing. Signals
DISPLAY-LIST-FULL, before changing TARGET, if TARGET has not room for all of them.
SOURCE may be TARGET, which doubles it."
  (declare (type display-list target source)
           (type single-float dx dy)
           (optimize speed))
  (let ((count (display-list-count source))
        (start (display-list-count target)))
    (declare (type fixnum count start))
    (when (> (+ start count) (display-list-capacity target))
      (error 'display-list-full :capacity (display-list-capacity target)))
    (replace (display-list-kinds target) (display-list-kinds source)
             :start1 start :end2 count)
    (replace (display-list-widths target) (display-list-widths source)
             :start1 start :end2 count)
    (replace (display-list-heights target) (display-list-heights source)
             :start1 start :end2 count)
    (replace (display-list-parameters target) (display-list-parameters source)
             :start1 start :end2 count)
    (replace (display-list-colors target) (display-list-colors source)
             :start1 start :end2 count)
    (replace (display-list-extras target) (display-list-extras source)
             :start1 start :end2 count)
    ;; Only the positions change on the way.
    (let ((target-xs (display-list-xs target))
          (source-xs (display-list-xs source))
          (target-ys (display-list-ys target))
          (source-ys (display-list-ys source)))
      (dotimes (i count)
        (setf (aref target-xs (+ start i)) (+ (aref source-xs i) dx)
              (aref target-ys (+ start i)) (+ (aref source-ys i) dy))))
    (setf (display-list-count target) (+ start count))
    target))

(defun push-clip (display-list x y width height)
  "Append a command that clips everything after it, up to the matching PUSH-UNCLIP, to the
rectangle X, Y, WIDTH by HEIGHT (and to every clip it is inside). Returns its index."
  (push-command display-list +command-clip+ x y width height 0 0))

(defun push-unclip (display-list)
  "Append a command that ends the innermost clip. Returns its index."
  (push-command display-list +command-unclip+ 0 0 0 0 0 0))

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
(define-command-accessor display-list-extra display-list-extras)

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
        (colors (display-list-colors display-list))
        (extras (display-list-extras display-list)))
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
        (mix (aref colors i))
        ;; Only a kind that has an extra mixes it, so that a frame of the others hashes as it did.
        (when (= (aref kinds i) +command-shadow+)
          (mix (bits (aref extras i))))))
    ;; Fold the top bits in, then keep 62 so the result is an immediate fixnum.
    (ldb (byte 62 0) (logxor hash (ash hash -62)))))

;;; --- zoom ---
;;;
;;; An app drawn at a zoom (an app in a pane of a designer, larger or smaller than it is) is laid out
;;; and drawn in its own points, and the list is then made ZOOM times larger. Its text is drawn with
;;; fonts ZOOM times the size, so that it is as sharp as text of that size, and UNZOOM-GLYPHS puts
;;; those glyphs back in the app's points about where they were drawn from; the zoom then makes them
;;; what they were drawn as.

(defun zoom-display-list (target source zoom)
  "Make TARGET hold SOURCE's commands ZOOM times larger, about the top left, and return TARGET: each
rectangle and corner radius, and the width of each stroke (to the nearest quarter point, at least
one), and a shadow's blur. A glyph keeps its atlas index. Signals DISPLAY-LIST-FULL, before changing TARGET, if TARGET
has not room for them."
  (declare (type display-list target source) (type single-float zoom))
  (replace-display-list target source)
  (let ((kinds (display-list-kinds target)) (xs (display-list-xs target))
        (ys (display-list-ys target)) (ws (display-list-widths target))
        (hs (display-list-heights target)) (ps (display-list-parameters target))
        (es (display-list-extras target)))
    (dotimes (i (display-list-count target) target)
      (let ((kind (aref kinds i)))
        (setf (aref xs i) (* (aref xs i) zoom)
              (aref ys i) (* (aref ys i) zoom)
              (aref ws i) (* (aref ws i) zoom)
              (aref hs i) (* (aref hs i) zoom)
              (aref es i) (* (aref es i) zoom))
        (cond ((glyph-kind-p kind))
              ((> kind +command-rounded-stroke+)
               (setf (aref ps i) (* (aref ps i) zoom)
                     (aref kinds i) (+ +command-rounded-stroke+
                                       (max 1 (min 239 (round (* (- kind +command-rounded-stroke+)
                                                                 zoom)))))))
              (t (setf (aref ps i) (* (aref ps i) zoom))))))))

(defun unzoom-glyphs (list start x baseline zoom)
  "Put the glyphs of LIST from the command START on, drawn ZOOM times their size from the pen at X
and the BASELINE, back in unzoomed points about that place."
  (declare (type display-list list) (type single-float x baseline zoom))
  (let ((kinds (display-list-kinds list)) (xs (display-list-xs list))
        (ys (display-list-ys list)) (ws (display-list-widths list))
        (hs (display-list-heights list)))
    (loop :for i :from start :below (display-list-count list)
          :when (glyph-kind-p (aref kinds i))
            :do (setf (aref xs i) (+ x (/ (- (aref xs i) x) zoom))
                      (aref ys i) (+ baseline (/ (- (aref ys i) baseline) zoom))
                      (aref ws i) (/ (aref ws i) zoom)
                      (aref hs i) (/ (aref hs i) zoom)))
    list))
