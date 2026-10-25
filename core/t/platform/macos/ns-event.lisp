(defpackage #:almighty-bolt-core/t/platform/macos/ns-event
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:ns-event #:almighty-bolt-core/platform/macos/ns-event)))

(in-package #:almighty-bolt-core/t/platform/macos/ns-event)

;;; The part of the translation of AppKit's events that needs no AppKit object. What
;;; a real key press gives is checked by running a variant in a window and reading its
;;; log (`make retained-counter`).

(define-test the-logical-key-is-the-first-character-of-the-event ()
  (assert-eql #\a (ns-event:logical-key "a"))
  (assert-eql #\Return (ns-event:logical-key (string #\Return)))
  (assert-eql #\Space (ns-event:logical-key " "))
  (assert-eql #\Tab (ns-event:logical-key (string #\Tab)))
  (assert-eql #\a (ns-event:logical-key "abc")))

(define-test a-key-with-no-characters-has-no-logical-key ()
  (assert-false (ns-event:logical-key "")))

(define-test shift-tab-is-the-tab-key ()
  ;; AppKit gives the character U+0019, which the window log showed as #\Em.
  (assert-eql #\Tab (ns-event:logical-key (string (code-char #x19)))))
