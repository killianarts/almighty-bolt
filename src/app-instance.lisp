(defpackage #:almighty-bolt/app-instance
  (:use #:cl)
  (:import-from #:almighty-bolt/clock
                #:now-ns
                #:real-time-source)
  (:export #:app-instance
           #:handle-event
           #:*show-frame-errors*
           #:frame
           #:hit-target
           #:lookup-target
           #:accessibility-tree
           #:app-menus
           #:inherit-state
           #:retire-app
           #:forget-painting
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
           #:glyphs-full
           #:context-forget-glyphs
           #:no-text-support-operation
           #:context-font
           #:context-measure
           #:context-break-lines
           #:context-caret-x
           #:context-index-at-x
           #:context-caret-rect
           #:context-push-text
           #:context-clipboard-text
           #:context-set-clipboard-text
           #:context-choose-file
           #:context-file-chooser
           #:context-appearance
           #:context-cursor
           #:context-window
           #:+cursors+))

(in-package #:almighty-bolt/app-instance)

;;; The contract between a window's host and the app it runs: what the host asks of an app
;;; instance, and what it gives one to work with. Nothing here touches a window, the GPU or the
;;; operating system's text engine; the host supplies those behind the context.
;;;
;;; An app instance is what a window runs. The library's is IMMEDIATE-APP, which calls an app's
;;; function every frame and keeps its ui (src/ui/immediate.lisp); MAKE-APP-INSTANCE makes one
;;; from an app's constructor, and compiling code makes another, which takes the old one's ui. The
;;; benchmarks have NULL-APP, which replays a fixed display list. (Until 0.3 this was the "variant"
;;; contract, from when the project compared three kinds of ui.)

;;; --- what an app instance implements ---

(defclass app-instance () ()
  (:documentation "The base class of what a window runs. A subclass implements FRAME, and whichever
of the other operations it needs."))

(defgeneric handle-event (instance event)
  (:documentation "Offer EVENT to INSTANCE. Called on the main thread, once for each
event, in the order the platform delivered them. Returns true if a frame is
wanted. An app that acts on events does so here; one that rebuilds its widgets
every frame keeps the event in a list of its own for FRAME to read.")
  (:method ((instance app-instance) event)
    (declare (ignore instance event))
    nil))

(defvar *show-frame-errors* nil
  "True while a frame is made for a window that should go on showing when the frame fails: an app
whose FRAME signals an error then shows its last frame with the error over it, says what the error
was, and the error goes no further. The host binds it round a window's frames; a test and the REPL
leave it false and see the error.")

(defgeneric frame (instance context display-list)
  (:documentation "Build a frame: lay out and append drawing commands to
DISPLAY-LIST, which is empty. Returns true if app state changed during the build,
in which case the host builds again (see SETTLE-FRAME). An app must provide it.")
  (:method ((instance app-instance) context display-list)
    (declare (ignore context display-list))
    (error "~s does not implement FRAME." instance)))

(defgeneric hit-target (instance x y)
  (:documentation "The stable id (anything comparable with EQL) of the target under
the point X, Y (single floats, in points from the top left), or NIL. Answers for the
last frame that was presented.")
  (:method ((instance app-instance) x y)
    (declare (ignore instance x y))
    nil))

(defgeneric lookup-target (instance test-id)
  (:documentation "The rectangle of the target with id TEST-ID, as four values (x, y,
width, height), or NIL if there is none. Replay uses it to find where to click.")
  (:method ((instance app-instance) test-id)
    (declare (ignore instance test-id))
    nil))

(defgeneric app-menus (instance)
  (:documentation "The app's own menus as its last frame described them, for the menu bar while its
window has the keyboard: a vector of each menu's title then its items (see APP-MENU), or NIL.")
  (:method ((instance app-instance))
    nil))

(defgeneric accessibility-tree (instance)
  (:documentation "The accessibility tree of the last settled frame, or NIL if there is none. The
immediate app's is its semantic tree (see the ui's semantics); the default is NIL.")
  (:method ((instance app-instance))
    (declare (ignore instance))
    nil))

(defgeneric inherit-state (new old)
  (:documentation "NEW is about to take the place of OLD in a window, because the app was made
again after its code changed. Carry over what OLD kept for itself that is not the app's own
state: what the person did to the widgets (the focus, a caret, a scroll). Called on the main
thread before NEW draws. If it signals an error, OLD stays and NEW is retired.")
  (:method (new old)
    (declare (ignore new old))
    nil))

(defgeneric forget-painting (instance)
  (:documentation "Forget every drawing command INSTANCE kept from frames before, so that the next
frame paints everything afresh. Called when the glyphs those commands name have been forgotten
(CONTEXT-FORGET-GLYPHS).")
  (:method (instance)
    (declare (ignore instance))
    nil))

(defgeneric retire-app (instance)
  (:documentation "INSTANCE is not used any more: it was replaced by one that has drawn, or it was
the replacement and its first frame failed. Let go of what it holds outside itself. Called on the
main thread.")
  (:method (instance)
    (declare (ignore instance))
    nil))

;;; --- what the host gives an app ---

(defconstant +phase-update+ 0
  "The phase of a build that brings the app up to date with the application: for the
immediate ui, calling the interface function of the app.")
(defconstant +phase-layout+ 1 "The phase in which sizes and places are worked out.")
(defconstant +phase-paint+ 2 "The phase in which the display list is made.")
(defconstant +phase-first-build+ 3
  "Not a phase of an app: the time of the first build of a frame, set by SETTLE-FRAME.")
(defconstant +phase-all-builds+ 4
  "Not a phase of an app: the time of all the builds of a frame, set by SETTLE-FRAME.")
(defconstant +phase-slots+ 5)

(defparameter +cursors+
  '(:arrow :text :pointing-hand :crosshair :open-hand :closed-hand :not-allowed
    :resize-left-right :resize-up-down)
  "The shapes the pointer can have over a window: the arrow, the text cursor (an I-beam), a pointing
hand, a crosshair, an open and a closed hand, a circle with a line through it, and the arrows that
resize sideways and up and down.")

(defstruct (context (:constructor make-context (&key width height scale requester)))
  "What an app can see of the host, made once and updated in place by the host.
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
  ;; Instrumentation: when PHASES-P is true, WITH-PHASE adds the real
  ;; time of what it runs to a slot of PHASE-NS, and SETTLE-FRAME times its builds. The host
  ;; clears the slots at the start of a frame and reads them at the end.
  ;; Text input (the input method of the operating system): a canvas that has something the
  ;; person is typing into says so, and where the caret is, in points from the top left of the
  ;; canvas, as x, y, width and height (TEXT-INPUT-LOCAL), by painting with SET-TEXT-INPUT-CARET.
  ;; After each build the host adds the canvas's place (PLACE-TEXT-INPUT-CARET), and the sum, in
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
  ;; The cost of events, when PHASES-P: the time DELIVER-EVENT spent in the app's HANDLE-EVENT
  ;; and the number of events since TAKE-EVENTS (the host takes them with each frame, so that the
  ;; frame and the events that came before it are one cost, whichever app does the work in
  ;; HANDLE-EVENT and whichever in FRAME).
  (events-ns 0 :type (unsigned-byte 62))
  (events 0 :type (unsigned-byte 62))
  (phases-p nil :type boolean)
  ;; What was copied, for a context whose host has no clipboard of the system's (a test, a replay):
  ;; see CONTEXT-CLIPBOARD-TEXT.
  (clipboard nil :type (or null string))
  ;; For a context with no window (a test): a function of the kind (:open or :save), the types and
  ;; the name, that answers as a person would, with a path or NIL. See CONTEXT-CHOOSE-FILE.
  (file-chooser nil :type (or null function))
  ;; Whether the system shows windows dark or light, :dark or :light: a window follows it unless
  ;; the app draws in a theme of its own. The host of a window keeps it as the system's; a context
  ;; with no window is :dark unless a test sets it.
  (appearance :dark :type (member :dark :light))
  ;; The shape the pointer has over the window, one of +CURSORS+, as the last frame said; the host of
  ;; a window shows it.
  (cursor :arrow :type keyword)
  ;; The window the context draws in, as OPEN-WINDOW returns it (the host sets it to itself), so
  ;; that an app can name its own window (THIS-WINDOW); NIL for a context that no host made.
  (window nil)
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
canvas its caret is in. The ui wraps each call of an app's canvas painter in it."
  (let ((c (gensym "CONTEXT"))
        (before (gensym "BEFORE")))
    `(let* ((,c ,context)
            (,before (context-painting ,c)))
       (setf (context-painting ,c) ,id)
       (unwind-protect (progn ,@body)
         (setf (context-painting ,c) ,before)))))

(defun place-text-input-caret (instance context)
  "Put the caret that SET-TEXT-INPUT-CARET was given in the window: add the place that INSTANCE
now has for the canvas that said it. Called by SETTLE-FRAME after the last build of a frame; it
does nothing unless a canvas with an id takes text input."
  (let ((canvas (context-text-input-canvas context)))
    (when (and canvas (context-text-input-p context))
      (multiple-value-bind (x y) (lookup-target instance canvas)
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
library's clock, which replay makes virtual) to PHASE (+PHASE-UPDATE+, +PHASE-LAYOUT+ or
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
  "Ask for a frame. Many requests before a frame make one frame. An app never
runs its own loop; this is how it asks for the next frame of an animation."
  (funcall (context-requester context) context)
  nil)

(defun take-frame-request (context)
  "True if a frame has been requested since the last call, and forget the request."
  (prog1 (context-frame-requested context)
    (setf (context-frame-requested context) nil)))

(declaim (inline context-now-ns))
(defun context-now-ns (context)
  "The time in nanoseconds, from the library's clock (which replay can replace)."
  (declare (ignore context))
  (now-ns))

(defun deliver-event (instance context event)
  "Offer EVENT to INSTANCE and, if it wants a frame, request one. Returns true if a
frame was requested. With the context's instrumentation on, the real time that
HANDLE-EVENT took is added to the context's events (see TAKE-EVENTS)."
  (when (if (context-phases-p context)
            (let ((start (real-time-source)))
              (multiple-value-prog1 (handle-event instance event)
                (incf (context-events-ns context) (- (real-time-source) start))
                (incf (context-events context))))
            (handle-event instance event))
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
;;; so an app reaches them through the context. A font is an opaque value that
;;; only CONTEXT-FONT makes. These are the measurement operations
;;; and one operation to draw a line of text. A context made without a host has none
;;; of them.

(define-condition no-text-support (error)
  ((operation :initarg :operation :reader no-text-support-operation))
  (:report (lambda (condition stream)
             (format stream "This context has no text support, which ~s needs."
                     (no-text-support-operation condition)))))

(define-condition glyphs-full (error)
  ()
  (:documentation "The context has no room for another glyph. SETTLE-FRAME answers it by
forgetting the glyphs (CONTEXT-FORGET-GLYPHS) and the painting that names them, and building the
frame again."))

(defgeneric context-forget-glyphs (context)
  (:documentation "Forget every glyph the context has made room for, so that there is room again.
Drawing commands made before name glyphs that are gone, and must not be drawn.")
  (:method ((context context))
    nil))

(defun no-text (operation)
  (error 'no-text-support :operation operation))

(defgeneric context-font (context name size &optional weight tracking)
  (:documentation "The font called NAME at SIZE points, of WEIGHT (a number from 1 to 1000, 400 the
default and 700 bold, or a name: :thin, :extra-light, :light, :regular, :medium, :semibold, :bold,
:extra-bold or :black), with TRACKING points of space after each character (0 if not given): the same
object for the same arguments. The face is the upright one of the family nearest WEIGHT; if the
system has no such font, a system font.")
  (:method ((context context) name size &optional weight tracking)
    (declare (ignore name size weight tracking))
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

;;; --- the clipboard, which the host supplies ---
;;;
;;; Copy and paste go through the context, so that a host with a window uses the system's clipboard
;;; and every other context (a test, a replay, a host made without a window) keeps what was copied
;;; to itself and never touches the person's clipboard.

(defgeneric context-clipboard-text (context)
  (:documentation "The text on the clipboard, or NIL if it holds none.")
  (:method ((context context))
    (context-clipboard context)))

(defgeneric context-set-clipboard-text (context string)
  (:documentation "Put STRING on the clipboard, in place of what it held. Returns STRING.")
  (:method ((context context) string)
    (setf (context-clipboard context) (copy-seq string))))

;;; --- the system's file panels, which the host supplies ---

(defgeneric context-choose-file (context kind types name callback)
  (:documentation "Ask the person for a file: KIND :open, for one to open, or :save, for where to
save, with TYPES the extensions allowed (all if NIL) and NAME the name offered to save as. CALLBACK is
called with the path chosen, or NIL if the person cancelled, later (a host with a window shows the
system's panel after the frame) or at once. A context with no window answers with its FILE-CHOOSER,
or cancels.")
  (:method ((context context) kind types name callback)
    (funcall callback (let ((chooser (context-file-chooser context)))
                        (and chooser (funcall chooser kind types name))))))
