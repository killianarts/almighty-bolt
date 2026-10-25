(defpackage #:almighty-bolt-core/event
  (:use #:cl)
  (:import-from #:almighty-bolt-core/clock
                #:nanoseconds)
  (:export #:event
           #:event-timestamp
           #:+key-up+
           #:+key-down+
           #:+key-left+
           #:+key-right+
           #:+key-home+
           #:+key-end+
           #:+key-page-up+
           #:+key-page-down+
           #:+key-delete-forward+
           #:+modifier-shift+
           #:+modifier-control+
           #:+modifier-alt+
           #:+modifier-super+
           #:modifier-down-p
           #:pointer-event
           #:make-pointer-event
           #:pointer-event-kind
           #:pointer-event-x
           #:pointer-event-y
           #:pointer-event-button
           #:pointer-event-buttons
           #:pointer-event-modifiers
           #:pointer-event-click-count
           #:wheel-event
           #:make-wheel-event
           #:wheel-event-x
           #:wheel-event-y
           #:wheel-event-dx
           #:wheel-event-dy
           #:wheel-event-precise-p
           #:wheel-event-modifiers
           #:+points-per-line+
           #:wheel-event-lines
           #:key-event
           #:make-key-event
           #:key-event-kind
           #:key-event-physical-key
           #:key-event-logical-key
           #:key-event-modifiers
           #:key-event-repeat-p
           #:text-input-event
           #:make-text-input-event
           #:text-input-event-text
           #:composition-event
           #:make-composition-event
           #:composition-event-kind
           #:composition-event-text
           #:composition-event-selection-start
           #:composition-event-selection-length
           #:resize-event
           #:make-resize-event
           #:resize-event-width
           #:resize-event-height
           #:scale-event
           #:make-scale-event
           #:scale-event-scale
           #:focus-event
           #:make-focus-event
           #:focus-event-focused-p))

(in-package #:almighty-bolt-core/event)

;;; Events are plain, immutable structures: numbers, keywords and strings, so a
;;; script of them can be written with PRIN1 and read back. Every event carries
;;; the core clock's time in nanoseconds. Positions are in logical pixels,
;;; measured from the top left of the window's content.
;;;
;;; The platform layer creates events outside the frame, when the operating
;;; system delivers them, so their allocation is not charged to a variant.

(defun required (name)
  (error "~s is required." name))

;;; Keys that make no character have no character of their own, so a key event names
;;; them with the characters that AppKit gives them, which are in the Unicode private
;;; use area (NSUpArrowFunctionKey and the rest), and a port gives the same ones. The
;;; other keys a variant needs have characters: #\Backspace, #\Tab, #\Return, #\Escape
;;; and #\Space.

(defconstant +key-up+ (code-char #xF700))
(defconstant +key-down+ (code-char #xF701))
(defconstant +key-left+ (code-char #xF702))
(defconstant +key-right+ (code-char #xF703))
(defconstant +key-delete-forward+ (code-char #xF728))
(defconstant +key-home+ (code-char #xF729))
(defconstant +key-end+ (code-char #xF72B))
(defconstant +key-page-up+ (code-char #xF72C))
(defconstant +key-page-down+ (code-char #xF72D))

;;; Modifier keys are a small bit set of our own; the platform layer maps the
;;; operating system's flags onto it.

(defconstant +modifier-shift+ 1)
(defconstant +modifier-control+ 2)
(defconstant +modifier-alt+ 4)
(defconstant +modifier-super+ 8)

(deftype modifiers () '(unsigned-byte 8))

(declaim (inline modifier-down-p))
(defun modifier-down-p (modifiers flag)
  "True if the modifier FLAG is set in the bit set MODIFIERS."
  (logtest modifiers flag))

(defstruct (event (:constructor nil))
  "Base of every event."
  (timestamp (required 'timestamp) :type nanoseconds :read-only t))

(defstruct (pointer-event (:include event))
  "The pointer went down, went up, moved, entered or left the window.
BUTTON is the button that changed (0 left, 1 right, 2 middle) or NIL for moves;
BUTTONS is the bit set of buttons now down, so a drag is a :MOVE with BUTTONS."
  (kind (required 'kind) :type (member :down :up :move :enter :leave) :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (button nil :type (or null (unsigned-byte 8)) :read-only t)
  (buttons 0 :type (unsigned-byte 8) :read-only t)
  (modifiers 0 :type modifiers :read-only t)
  (click-count 0 :type (unsigned-byte 8) :read-only t))

(defstruct (wheel-event (:include event))
  "A scroll wheel or trackpad scroll at (X, Y). PRECISE-P is true for pixel
deltas (trackpads) and false for line deltas (wheels)."
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (dx 0d0 :type double-float :read-only t)
  (dy 0d0 :type double-float :read-only t)
  (precise-p nil :type boolean :read-only t)
  (modifiers 0 :type modifiers :read-only t))

(defconstant +points-per-line+ 24d0
  "How many points a line of the scroll wheel is worth: the height of a row of a list
or a grid. A wheel gives its deltas in lines and a trackpad in points (the event's
PRECISE-P says which), and a variant that scrolls by rows reads both in lines.")

(defun wheel-event-lines (event)
  "The scroll of the wheel EVENT as two values, across and down, in lines: a wheel's
deltas as they are, and a trackpad's (precise ones, in points) divided by
+POINTS-PER-LINE+. A positive DY is the content moving down."
  (if (wheel-event-precise-p event)
      (values (/ (wheel-event-dx event) +points-per-line+)
              (/ (wheel-event-dy event) +points-per-line+))
      (values (wheel-event-dx event) (wheel-event-dy event))))

(defstruct (key-event (:include event))
  "A key went down or up. PHYSICAL-KEY is the platform's key code for the key's
position; LOGICAL-KEY is the character it produces, or NIL for keys that
produce none."
  (kind (required 'kind) :type (member :down :up) :read-only t)
  (physical-key 0 :type (unsigned-byte 16) :read-only t)
  (logical-key nil :type (or null character) :read-only t)
  (modifiers 0 :type modifiers :read-only t)
  (repeat-p nil :type boolean :read-only t))

(defstruct (text-input-event (:include event))
  "Text to insert, after the operating system's input handling."
  (text (required 'text) :type string :read-only t))

(defstruct (composition-event (:include event))
  "Input method composition: it started, its preedit text changed, or it was
committed. SELECTION-START and SELECTION-LENGTH are in characters of TEXT."
  (kind (required 'kind) :type (member :started :changed :committed) :read-only t)
  (text "" :type string :read-only t)
  (selection-start 0 :type (unsigned-byte 32) :read-only t)
  (selection-length 0 :type (unsigned-byte 32) :read-only t))

(defstruct (resize-event (:include event))
  "The window's content is now WIDTH by HEIGHT logical pixels."
  (width (required 'width) :type double-float :read-only t)
  (height (required 'height) :type double-float :read-only t))

(defstruct (scale-event (:include event))
  "The window's backing scale factor changed (2d0 on a Retina display)."
  (scale (required 'scale) :type double-float :read-only t))

(defstruct (focus-event (:include event))
  "The window gained or lost keyboard focus."
  (focused-p (required 'focused-p) :type boolean :read-only t))
