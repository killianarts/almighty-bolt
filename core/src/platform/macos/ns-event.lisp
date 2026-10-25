(defpackage #:almighty-bolt-core/platform/macos/ns-event
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:objc-class
                #:lisp-string)
  (:import-from #:almighty-bolt-core/clock
                #:now-ns)
  (:import-from #:almighty-bolt-core/event
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

(in-package #:almighty-bolt-core/platform/macos/ns-event)

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
NIL if it has none. AppKit reports Shift-Tab as U+0019, its back-tab character. That
is the Tab key with the shift modifier, so that is what it is made here, and a variant
does not need to know AppKit's name for it."
  (when (plusp (length text))
    (let ((character (char text 0)))
      (if (= (char-code character) #x19)
          #\Tab
          character))))

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
