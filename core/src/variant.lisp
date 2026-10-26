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
  ;; Text input (the input method of the operating system): a variant that has something the
  ;; person is typing into says so, and where the caret is, in points from the top left of the
  ;; window, as x, y, width and height. The host sends text and composition events, and gives
  ;; the input method the caret's place to put its candidates by, only while TEXT-INPUT-P.
  (text-input-p nil :type boolean)
  (text-input-rect (make-array 4 :element-type 'single-float :initial-element 0f0)
   :type (simple-array single-float (4)) :read-only t)
  (phases-p nil :type boolean)
  (phase-ns (make-array +phase-slots+ :element-type '(unsigned-byte 64) :initial-element 0)
   :type (simple-array (unsigned-byte 64) (*)) :read-only t))

(defun set-text-input-caret (context x y width height)
  "Say that something takes text input, with the caret at X, Y and WIDTH by HEIGHT in the window.
Does not allocate."
  (declare (type single-float x y width height))
  (let ((rect (context-text-input-rect context)))
    (setf (aref rect 0) x (aref rect 1) y (aref rect 2) width (aref rect 3) height
          (context-text-input-p context) t))
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
frame was requested."
  (when (handle-event variant event)
    (request-frame context)
    t))

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
