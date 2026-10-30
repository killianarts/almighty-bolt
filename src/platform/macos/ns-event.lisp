(defpackage #:almighty-bolt/platform/macos/ns-event
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:lisp-string)
  (:import-from #:almighty-bolt/clock
                #:now-ns)
  (:import-from #:almighty-bolt/event
                #:+modifier-shift+
                #:+modifier-control+
                #:+modifier-alt+
                #:+modifier-super+
                #:make-pointer-event
                #:make-wheel-event
                #:make-key-event)
  (:export #:pointer-event-from-ns-event
           #:wheel-event-from-ns-event
           #:key-event-from-ns-event
           #:logical-key))

(in-package #:almighty-bolt/platform/macos/ns-event)

;;; Turn AppKit's NSEvent into the core's plain event structures. Positions are
;;; reported from the top left of the view, which the view arranges by claiming
;;; to be flipped.

(declaim (inline modifiers-from-flags))
(defun modifiers-from-flags (flags)
  "Map NSEventModifierFlags onto the core's modifier bit set."
  (logior (if (logtest flags (ash 1 17)) +modifier-shift+ 0)
          (if (logtest flags (ash 1 18)) +modifier-control+ 0)
          (if (logtest flags (ash 1 19)) +modifier-alt+ 0)
          (if (logtest flags (ash 1 20)) +modifier-super+ 0)))

(declaim (inline event-time-ns))
(defun event-time-ns (ns-event)
  "The time of NS-EVENT on the core clock. NSEvent counts seconds since boot,
not counting sleep, which is the base of the core clock."
  (let ((seconds (msg-send :double ns-event "timestamp")))
    (declare (type (double-float 0d0 1d12) seconds))
    (values (round (* seconds 1d9)))))

(defun view-position (ns-event view)
  "The position of NS-EVENT in VIEW's coordinates, as two values."
  (multiple-value-bind (window-x window-y) (msg-send :point ns-event "locationInWindow")
    (msg-send :point view "convertPoint:fromView:" (:point window-x window-y) (:id 0))))

(defun pointer-event-from-ns-event (ns-event view kind)
  "A pointer event of KIND (:down :up :move :enter :leave) for NS-EVENT in VIEW."
  (multiple-value-bind (x y) (view-position ns-event view)
    (let ((press-p (member kind '(:down :up))))
      (make-pointer-event
       :timestamp (event-time-ns ns-event)
       :kind kind
       :x x
       :y y
       :button (when press-p
                 (the (unsigned-byte 8) (msg-send :long ns-event "buttonNumber")))
       :buttons (ldb (byte 8 0) (msg-send :ulong (objc-class "NSEvent") "pressedMouseButtons"))
       :modifiers (modifiers-from-flags (msg-send :ulong ns-event "modifierFlags"))
       :click-count (if press-p
                        (ldb (byte 8 0) (msg-send :long ns-event "clickCount"))
                        0)))))

(defun wheel-event-from-ns-event (ns-event view)
  "A wheel event for the scroll wheel or trackpad event NS-EVENT in VIEW."
  (multiple-value-bind (x y) (view-position ns-event view)
    (make-wheel-event
     :timestamp (event-time-ns ns-event)
     :x x
     :y y
     :dx (msg-send :double ns-event "scrollingDeltaX")
     :dy (msg-send :double ns-event "scrollingDeltaY")
     :precise-p (msg-send :bool ns-event "hasPreciseScrollingDeltas")
     :modifiers (modifiers-from-flags (msg-send :ulong ns-event "modifierFlags")))))

(defun logical-key (text)
  "The logical key of a key event whose characters are TEXT: its first character, or
NIL if it has none. A few keys are not what AppKit calls them, and a variant should
not need to know AppKit's names: Shift-Tab is U+0019, its back-tab character, and is
the Tab key with the shift modifier; the Delete key, which deletes backwards, is
U+007F and is #\Backspace; and the Enter key of the keypad is U+0003 and is Return."
  (when (plusp (length text))
    (let ((character (char text 0)))
      (case (char-code character)
        (#x19 #\Tab)
        (#x7F #\Backspace)
        (#x03 #\Return)
        (t character)))))

(defun key-event-from-ns-event (ns-event kind)
  "A key event of KIND (:down or :up) for NS-EVENT."
  (let* ((characters (msg-send :id ns-event "characters"))
         (text (if (zerop characters) "" (lisp-string characters))))
    (make-key-event
     :timestamp (event-time-ns ns-event)
     :kind kind
     :physical-key (msg-send :ushort ns-event "keyCode")
     :logical-key (logical-key text)
     :modifiers (modifiers-from-flags (msg-send :ulong ns-event "modifierFlags"))
     :repeat-p (msg-send :bool ns-event "isARepeat"))))
