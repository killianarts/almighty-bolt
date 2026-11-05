(defpackage #:almighty-bolt/platform/macos/pasteboard
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc))
  (:export #:general-pasteboard
           #:make-private-pasteboard
           #:discard-pasteboard
           #:pasteboard-string
           #:set-pasteboard-string))

(in-package #:almighty-bolt/platform/macos/pasteboard)

;;; The clipboard of macOS, which AppKit calls a pasteboard: plain text in and out. The general
;;; pasteboard is the one that a person copies to and pastes from in every app; a private one has a
;;; name of its own that nothing else knows, so that a test can copy and paste through the real
;;; pasteboard server without touching what the person copied.

(defun general-pasteboard ()
  "The pasteboard that Copy and Paste use in every app."
  (objc:msg-send :id (objc:objc-class "NSPasteboard") "generalPasteboard"))

(defun make-private-pasteboard ()
  "A new pasteboard with a name of its own. Give it back with DISCARD-PASTEBOARD."
  (objc:msg-send :id (objc:objc-class "NSPasteboard") "pasteboardWithUniqueName"))

(defun discard-pasteboard (pasteboard)
  "Tell the pasteboard server that PASTEBOARD, made by MAKE-PRIVATE-PASTEBOARD, is not wanted."
  (objc:msg-send :void pasteboard "releaseGlobally"))

(defun pasteboard-string (pasteboard)
  "The plain text on PASTEBOARD, or NIL if it holds none."
  (objc:with-autorelease-pool
    (let ((string (objc:msg-send :id pasteboard "stringForType:"
                                 (:id (objc:foreign-constant "NSPasteboardTypeString")))))
      (if (zerop string) nil (objc:lisp-string string)))))

(defun set-pasteboard-string (pasteboard string)
  "Put STRING on PASTEBOARD as plain text, in place of what it held. Returns STRING."
  (objc:with-autorelease-pool
    (let ((text (objc:ns-string string)))
      (unwind-protect
           (progn (objc:msg-send :long pasteboard "clearContents")
                  (objc:msg-send :bool pasteboard "setString:forType:"
                                 (:id text) (:id (objc:foreign-constant "NSPasteboardTypeString"))))
        (objc:release text))))
  string)
