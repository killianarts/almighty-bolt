(defpackage #:almighty-bolt/platform/macos/ns-event
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:clock #:almighty-bolt/clock)
                    (#:event #:almighty-bolt/event))
  (:export #:pointer-event-from-ns-event
           #:wheel-event-from-ns-event
           #:key-event-from-ns-event
           #:logical-key))

(in-package #:almighty-bolt/platform/macos/ns-event)

;;; Turn AppKit's NSEvent into the library's plain event structures. Positions are
;;; reported from the top left of the view, which the view arranges by claiming
;;; to be flipped.

(declaim (inline modifiers-from-flags))
(defun modifiers-from-flags (flags)
  "Map NSEventModifierFlags onto the library's modifier bit set."
  (logior (if (logtest flags (ash 1 17)) event:+modifier-shift+ 0)
          (if (logtest flags (ash 1 18)) event:+modifier-control+ 0)
          (if (logtest flags (ash 1 19)) event:+modifier-alt+ 0)
          (if (logtest flags (ash 1 20)) event:+modifier-super+ 0)))

(declaim (inline event-time-ns))
(defun event-time-ns (ns-event)
  "The time of NS-EVENT on the library's clock. NSEvent counts seconds since boot,
not counting sleep, which is the base of the library's clock."
  (let ((seconds (objc:msg-send :double ns-event "timestamp")))
    (declare (type (double-float 0d0 1d12) seconds))
    (values (round (* seconds 1d9)))))

(defun view-position (ns-event view)
  "The position of NS-EVENT in VIEW's coordinates, as two values."
  (multiple-value-bind (window-x window-y) (objc:msg-send :point ns-event "locationInWindow")
    (objc:msg-send :point view "convertPoint:fromView:" (:point window-x window-y) (:id 0))))

(defun pointer-event-from-ns-event (ns-event view kind)
  "A pointer event of KIND (:down :up :move :enter :leave) for NS-EVENT in VIEW."
  (multiple-value-bind (x y) (view-position ns-event view)
    (let ((press-p (member kind '(:down :up))))
      (event:make-pointer-event
       :timestamp (event-time-ns ns-event)
       :kind kind
       :x x
       :y y
       :button (when press-p
                 (the (unsigned-byte 8) (objc:msg-send :long ns-event "buttonNumber")))
       :buttons (ldb (byte 8 0) (objc:msg-send :ulong (objc:objc-class "NSEvent") "pressedMouseButtons"))
       :modifiers (modifiers-from-flags (objc:msg-send :ulong ns-event "modifierFlags"))
       :click-count (if press-p
                        (ldb (byte 8 0) (objc:msg-send :long ns-event "clickCount"))
                        0)))))

(defun wheel-event-from-ns-event (ns-event view)
  "A wheel event for the scroll wheel or trackpad event NS-EVENT in VIEW."
  (multiple-value-bind (x y) (view-position ns-event view)
    (event:make-wheel-event
     :timestamp (event-time-ns ns-event)
     :x x
     :y y
     :dx (objc:msg-send :double ns-event "scrollingDeltaX")
     :dy (objc:msg-send :double ns-event "scrollingDeltaY")
     :precise-p (objc:msg-send :bool ns-event "hasPreciseScrollingDeltas")
     :modifiers (modifiers-from-flags (objc:msg-send :ulong ns-event "modifierFlags")))))

(defun logical-key (text)
  "The logical key of a key event whose characters are TEXT: its first character, or
NIL if it has none. A few keys are not what AppKit calls them, and an app should
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
  "A key event of KIND (:down or :up) for NS-EVENT. With Command held the key is a command (a
shortcut), and its logical key is the key's character with Shift only: with Option as well, the
characters typed are what Option makes of it (Option-I is the dead key of the circumflex, and gives
no I), and Command-Option-I is a shortcut on I."
  (let* ((flags (objc:msg-send :ulong ns-event "modifierFlags"))
         (characters (objc:msg-send :id ns-event (if (logtest flags (ash 1 20))
                                                      "charactersIgnoringModifiers"
                                                      "characters")))
         (text (if (zerop characters) "" (objc:lisp-string characters))))
    (event:make-key-event
     :timestamp (event-time-ns ns-event)
     :kind kind
     :physical-key (objc:msg-send :ushort ns-event "keyCode")
     :logical-key (logical-key text)
     :modifiers (modifiers-from-flags flags)
     :repeat-p (objc:msg-send :bool ns-event "isARepeat"))))
