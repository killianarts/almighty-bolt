(defpackage #:almighty-bolt/platform/macos/pasteboard
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:foreign-constant
                #:ns-string
                #:lisp-string
                #:release
                #:with-autorelease-pool)
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
  (msg-send :id (objc-class "NSPasteboard") "generalPasteboard"))

(defun make-private-pasteboard ()
  "A new pasteboard with a name of its own. Give it back with DISCARD-PASTEBOARD."
  (msg-send :id (objc-class "NSPasteboard") "pasteboardWithUniqueName"))

(defun discard-pasteboard (pasteboard)
  "Tell the pasteboard server that PASTEBOARD, made by MAKE-PRIVATE-PASTEBOARD, is not wanted."
  (msg-send :void pasteboard "releaseGlobally"))

(defun pasteboard-string (pasteboard)
  "The plain text on PASTEBOARD, or NIL if it holds none."
  (with-autorelease-pool
    (let ((string (msg-send :id pasteboard "stringForType:"
                            (:id (foreign-constant "NSPasteboardTypeString")))))
      (if (zerop string) nil (lisp-string string)))))

(defun set-pasteboard-string (pasteboard string)
  "Put STRING on PASTEBOARD as plain text, in place of what it held. Returns STRING."
  (with-autorelease-pool
    (let ((text (ns-string string)))
      (unwind-protect
           (progn (msg-send :long pasteboard "clearContents")
                  (msg-send :bool pasteboard "setString:forType:"
                            (:id text) (:id (foreign-constant "NSPasteboardTypeString"))))
        (release text))))
  string)
