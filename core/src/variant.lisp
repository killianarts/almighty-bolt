(defpackage #:almighty-bolt-core/variant
  (:use #:cl)
  (:import-from #:almighty-bolt-core/clock
                #:now-ns
                #:real-time-source)
  (:export #:variant
           #:handle-event
           #:frame
           #:hit-target
           #:lookup-target
           #:accessibility-tree
           #:context
           #:make-context
           #:context-width
           #:context-height
           #:context-scale
           #:context-frame-number
           #:context-build-number
           #:context-frame-requested
           #:context-requester
           #:context-text-input-p
           #:context-text-input-rect
           #:set-text-input-caret
           #:place-text-input-caret
           #:with-painting
           #:context-painting
           #:context-events-ns
           #:context-events
           #:take-events
           #:context-phases-p
           #:context-phase-ns
           #:+phase-update+
           #:+phase-layout+
           #:+phase-paint+
           #:+phase-first-build+
           #:+phase-all-builds+
           #:+phase-slots+
           #:reset-phases
           #:with-phase
           #:context-now-ns
           #:request-frame
           #:take-frame-request
           #:deliver-event
           #:no-text-support
           #:no-text-support-operation
           #:context-font
           #:context-measure
           #:context-break-lines
           #:context-caret-x
           #:context-index-at-x
           #:context-caret-rect
           #:context-push-text))

(in-package #:almighty-bolt-core/variant)

;;; The variant contract of decision record 0009: what the core asks of a variant,
;;; and what it gives a variant to work with. Nothing here touches a window, the GPU
;;; or the operating system's text engine; the host supplies those behind the
;;; context.

;;; --- what a variant implements ---

(defclass variant () ()
  (:documentation "The base class of variants. A variant is a subclass that
implements FRAME, and whichever of the other operations it needs."))

(defgeneric handle-event (variant event)
  (:documentation "Offer EVENT to VARIANT. Called on the main thread, once for each
event, in the order the platform delivered them. Returns true if a frame is
wanted. A variant that acts on events does so here; one that rebuilds its widgets
every frame keeps the event in a list of its own for FRAME to read.")
  (:method ((variant variant) event)
    (declare (ignore variant event))
    nil))

(defgeneric frame (variant context display-list)
  (:documentation "Build a frame: lay out and append drawing commands to
DISPLAY-LIST, which is empty. Returns true if app state changed during the build,
in which case the core builds again (see SETTLE-FRAME). A variant must provide it.")
  (:method ((variant variant) context display-list)
    (declare (ignore context display-list))
    (error "~s does not implement FRAME." variant)))

(defgeneric hit-target (variant x y)
  (:documentation "The stable id (anything comparable with EQL) of the target under
the point X, Y (single floats, in points from the top left), or NIL. Answers for the
last frame that was presented.")
  (:method ((variant variant) x y)
    (declare (ignore variant x y))
    nil))

(defgeneric lookup-target (variant test-id)
  (:documentation "The rectangle of the target with id TEST-ID, as four values (x, y,
width, height), or NIL if there is none. Replay uses it to find where to click.")
  (:method ((variant variant) test-id)
    (declare (ignore variant test-id))
    nil))

(defgeneric accessibility-tree (variant)
  (:documentation "The accessibility tree of the last settled frame. A stub that
returns NIL until the machine interface (H3) needs it.")
  (:method ((variant variant))
    (declare (ignore variant))
    nil))

;;; --- what the core gives a variant ---

(defconstant +phase-update+ 0
  "The phase of a build that brings the variant's structure up to date with the application:
what a retained variant's tickers do, what a reactive one's signals and reconciler do, and
calling the interface function of an immediate one.")
(defconstant +phase-layout+ 1 "The phase in which sizes and places are worked out.")
(defconstant +phase-paint+ 2 "The phase in which the display list is made.")
(defconstant +phase-first-build+ 3
  "Not a phase of a variant: the time of the first build of a frame, set by SETTLE-FRAME.")
(defconstant +phase-all-builds+ 4
  "Not a phase of a variant: the time of all the builds of a frame, set by SETTLE-FRAME.")
(defconstant +phase-slots+ 5)

(defstruct (context (:constructor make-context (&key width height scale requester)))
  "What a variant can see of the host, made once and updated in place by the host.
WIDTH and HEIGHT are the size of the drawing area in points and SCALE the pixels
per point. FRAME-NUMBER counts frames and BUILD-NUMBER (from 1) counts the builds
within the frame. REQUESTER is what REQUEST-FRAME calls; the host replaces it."
  (width 0f0 :type single-float)
  (height 0f0 :type single-float)
  (scale 1f0 :type single-float)
  (frame-number 0 :type fixnum)
  (build-number 0 :type fixnum)
  (frame-requested nil :type boolean)
  (requester #'flag-frame-request :type function)
  ;; Instrumentation (the charter's phases): when PHASES-P is true, WITH-PHASE adds the real
  ;; time of what it runs to a slot of PHASE-NS, and SETTLE-FRAME times its builds. The host
  ;; clears the slots at the start of a frame and reads them at the end.
  ;; Text input (the input method of the operating system): a canvas that has something the
  ;; person is typing into says so, and where the caret is, in points from the top left of the
  ;; canvas, as x, y, width and height (TEXT-INPUT-LOCAL), by painting with SET-TEXT-INPUT-CARET.
  ;; After each build the core adds the canvas's place (PLACE-TEXT-INPUT-CARET), and the sum, in
  ;; the window, is TEXT-INPUT-RECT, which the host gives the input method to put its candidates
  ;; by. The host sends text and composition events only while TEXT-INPUT-P. PAINTING is the id
  ;; of the canvas whose painter is running (see WITH-PAINTING) and TEXT-INPUT-CANVAS the id of
  ;; the one that said where the caret is; with no id the rectangle is taken to be in the window.
  (text-input-p nil :type boolean)
  (text-input-rect (make-array 4 :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (4)) :read-only t)
  (text-input-local (make-array 4 :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (4)) :read-only t)
  (text-input-canvas nil)
  (painting nil)
  ;; The cost of events, when PHASES-P: the time DELIVER-EVENT spent in the variant's HANDLE-EVENT
  ;; and the number of events since TAKE-EVENTS (the host takes them with each frame, so that the
  ;; frame and the events that came before it are one cost, whichever variant does the work in
  ;; HANDLE-EVENT and whichever in FRAME).
  (events-ns 0 :type (unsigned-byte 62))
  (events 0 :type (unsigned-byte 62))
  (phases-p nil :type boolean)
  (phase-ns (make-array +phase-slots+ :element-type '(unsigned-byte 64) :initial-element 0)
   :type (simple-array (unsigned-byte 64) (*)) :read-only t))

(defun set-text-input-caret (context x y width height)
  "Say that something takes text input, with the caret at X, Y and WIDTH by HEIGHT in points from
the top left of the canvas that is being painted. After each build SETTLE-FRAME puts the canvas
where it now is, so the input method is given a place in the window even when the canvas has
moved and was not painted again. Called outside a canvas's painter, the place is taken to be in
the window. Does not allocate."
  (declare (type single-float x y width height))
  (let ((local (context-text-input-local context))
        (rect (context-text-input-rect context)))
    (setf (aref local 0) x (aref local 1) y (aref local 2) width (aref local 3) height
          (aref rect 0) x (aref rect 1) y (aref rect 2) width (aref rect 3) height
          (context-text-input-canvas context) (context-painting context)
          (context-text-input-p context) t))
  nil)

(defmacro with-painting ((context id) &body body)
  "Run BODY, the painter of the canvas with the id ID, so that SET-TEXT-INPUT-CARET knows which
canvas its caret is in. A variant wraps each call of an app's canvas painter in it."
  (let ((c (gensym "CONTEXT"))
        (before (gensym "BEFORE")))
    `(let* ((,c ,context)
            (,before (context-painting ,c)))
       (setf (context-painting ,c) ,id)
       (unwind-protect (progn ,@body)
         (setf (context-painting ,c) ,before)))))

(defun place-text-input-caret (variant context)
  "Put the caret that SET-TEXT-INPUT-CARET was given in the window: add the place that VARIANT
now has for the canvas that said it. Called by SETTLE-FRAME after the last build of a frame; it
does nothing unless a canvas with an id takes text input."
  (let ((canvas (context-text-input-canvas context)))
    (when (and canvas (context-text-input-p context))
      (multiple-value-bind (x y) (lookup-target variant canvas)
        (when x
          (let ((local (context-text-input-local context))
                (rect (context-text-input-rect context)))
            (setf (aref rect 0) (+ (aref local 0) (float x 1f0))
                  (aref rect 1) (+ (aref local 1) (float y 1f0))))))))
  nil)

(defun reset-phases (context)
  "Clear the times that WITH-PHASE and SETTLE-FRAME have added up."
  (fill (context-phase-ns context) 0)
  nil)

(defmacro with-phase ((context phase) &body body)
  "Run BODY and, if CONTEXT's instrumentation is on, add the real time it took (not the
core clock, which replay makes virtual) to PHASE (+PHASE-UPDATE+, +PHASE-LAYOUT+ or
+PHASE-PAINT+). Off, it costs one test of a flag. Returns the values of BODY."
  (let ((c (gensym "CONTEXT"))
        (start (gensym "START"))
        (f (gensym "BODY")))
    `(let ((,c ,context))
       (flet ((,f () ,@body))
         (declare (dynamic-extent #',f))
         (if (context-phases-p ,c)
             (let ((,start (real-time-source)))
               (multiple-value-prog1 (,f)
                 (incf (aref (context-phase-ns ,c) ,phase)
                       (the (unsigned-byte 62) (- (real-time-source) ,start)))))
             (,f))))))

(defun flag-frame-request (context)
  "The default requester: note that a frame is wanted."
  (setf (context-frame-requested context) t))

(defun request-frame (context)
  "Ask for a frame. Many requests before a frame make one frame. A variant never
runs its own loop; this is how it asks for the next frame of an animation."
  (funcall (context-requester context) context)
  nil)

(defun take-frame-request (context)
  "True if a frame has been requested since the last call, and forget the request."
  (prog1 (context-frame-requested context)
    (setf (context-frame-requested context) nil)))

(declaim (inline context-now-ns))
(defun context-now-ns (context)
  "The time in nanoseconds, from the core clock (which replay can replace)."
  (declare (ignore context))
  (now-ns))

(defun deliver-event (variant context event)
  "Offer EVENT to VARIANT and, if it wants a frame, request one. Returns true if a
frame was requested. With the context's instrumentation on, the real time that
HANDLE-EVENT took is added to the context's events (see TAKE-EVENTS)."
  (when (if (context-phases-p context)
            (let ((start (real-time-source)))
              (multiple-value-prog1 (handle-event variant event)
                (incf (context-events-ns context) (- (real-time-source) start))
                (incf (context-events context))))
            (handle-event variant event))
    (request-frame context)
    t))

(defun take-events (context)
  "The real time in nanoseconds that DELIVER-EVENT has spent in HANDLE-EVENT, and the number of
events it handled, since the last call, as two values; and forget them. They are counted only
while the context's instrumentation is on. Does not allocate."
  (let ((ns (context-events-ns context))
        (events (context-events context)))
    (setf (context-events-ns context) 0
          (context-events context) 0)
    (values ns events)))

;;; --- text, which the host supplies ---
;;;
;;; The text engine and the glyph atlas belong to the host's platform and renderer,
;;; so a variant reaches them through the context. A font is an opaque value that
;;; only CONTEXT-FONT makes. These are the measurement API of decision record 0003
;;; and one operation to draw a line of text. A context made without a host has none
;;; of them.

(define-condition no-text-support (error)
  ((operation :initarg :operation :reader no-text-support-operation))
  (:report (lambda (condition stream)
             (format stream "This context has no text support, which ~s needs."
                     (no-text-support-operation condition)))))

(defun no-text (operation)
  (error 'no-text-support :operation operation))

(defgeneric context-font (context name size)
  (:documentation "The font called NAME at SIZE points, the same object for the same
arguments. If the system has no such font, a system font.")
  (:method ((context context) name size)
    (declare (ignore name size))
    (no-text 'context-font)))

(defgeneric context-measure (context font string)
  (:documentation "The width, ascent and descent of STRING in FONT, in points, as
three values.")
  (:method ((context context) font string)
    (declare (ignore font string))
    (no-text 'context-measure)))

(defgeneric context-break-lines (context font string width)
  (:documentation "Break STRING into lines no wider than WIDTH points where it can.
Returns a list of (START END) index pairs that cover the string.")
  (:method ((context context) font string width)
    (declare (ignore font string width))
    (no-text 'context-break-lines)))

(defgeneric context-caret-x (context font string index)
  (:documentation "How far from the start of STRING the caret sits before the
character at INDEX (which may be the length of the string).")
  (:method ((context context) font string index)
    (declare (ignore font string index))
    (no-text 'context-caret-x)))

(defgeneric context-index-at-x (context font string x)
  (:documentation "The index of the character that the point X, measured from the
start of STRING, is closest to: where a click there would put the caret.")
  (:method ((context context) font string x)
    (declare (ignore font string x))
    (no-text 'context-index-at-x)))

(defgeneric context-caret-rect (context font string index)
  (:documentation "The rectangle of the caret at INDEX, relative to the top left of
the line, as four values: x, y, width and height.")
  (:method ((context context) font string index)
    (declare (ignore font string index))
    (no-text 'context-caret-rect)))

(defgeneric context-push-text (context display-list font string x baseline color)
  (:documentation "Append a glyph command to DISPLAY-LIST for each glyph of STRING in
FONT, starting at X with its baseline at BASELINE (points, y down), in COLOR.
Returns the number of commands added.")
  (:method ((context context) display-list font string x baseline color)
    (declare (ignore display-list font string x baseline color))
    (no-text 'context-push-text)))
