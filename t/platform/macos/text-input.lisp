(defpackage #:almighty-bolt/t/platform/macos/text-input
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:view #:almighty-bolt/platform/macos/view)
                    (#:window #:almighty-bolt/platform/macos/window)
                    (#:event #:almighty-bolt/event)))

(in-package #:almighty-bolt/t/platform/macos/text-input)

;;; The view as an NSTextInputClient: what the methods that an input method calls turn into. They are
;;; called here as the input method calls them, by sending the messages. What a real input method
;;; does (the preedit of a Japanese or Chinese one, the dead keys) is for a person to try; the
;;; keys here are made up with NSEvent and go to the view's keyDown: as AppKit sends them.

(defconstant +not-found+ (1- (expt 2 63)))

(defmacro with-view ((view object events &key (active t)) &body body)
  "Run BODY with VIEW (a Lisp view) and OBJECT (its Objective-C object) in a window that is not
shown, and EVENTS a function that returns the events that reached its handler, oldest first, and
clears them. The app takes text if ACTIVE, with its caret at 10, 20, 2 by 16."
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication")
       (let* ((seen '())
              (,view (view:make-metal-view :width 200 :height 100
                                           :event-handler (lambda (event) (push event seen))
                                           :text-input (lambda ()
                                                         (values ,active 10d0 20d0 2d0 16d0))))
              (,object (view:metal-view-object ,view))
              (window (window:make-window :title "test" :width 200 :height 100)))
         (declare (ignorable ,object window))
         (window:set-content-view window ,object)
         (flet ((,events () (prog1 (reverse seen) (setf seen '()))))
           (declare (ignorable #',events))
           ,@body)))))

(defun ns (string) (objc:ns-string string))

(defun insert (object text)
  (objc:msg-send :void object "insertText:replacementRange:"
                 (:id (ns text)) (:ulong +not-found+) (:ulong 0)))

(defun mark (object text selected-start selected-length)
  (objc:msg-send :void object "setMarkedText:selectedRange:replacementRange:"
                 (:id (ns text)) (:ulong selected-start) (:ulong selected-length)
                 (:ulong +not-found+) (:ulong 0)))

(defun key-event (characters key-code &optional (flags 0))
  "An NSEvent of a key going down."
  (objc:msg-send :id (objc:objc-class "NSEvent")
                 "keyEventWithType:location:modifierFlags:timestamp:windowNumber:context:characters:charactersIgnoringModifiers:isARepeat:keyCode:"
                 (:ulong 10) (:point 0 0) (:ulong flags) (:double 0d0) (:long 0) (:id 0)
                 (:id (ns characters)) (:id (ns characters)) (:bool nil) (:ushort key-code)))

;;; --- text ---

(define-test inserted-text-is-a-text-input-event ()
  (with-view (view object events)
    (insert object "x")
    (let ((seen (events)))
      (assert-eql 1 (length seen))
      (assert-true (typep (first seen) 'event:text-input-event))
      (assert-equal "x" (event:text-input-event-text (first seen))))))

(define-test text-of-several-characters-arrives-whole ()
  (with-view (view object events)
    (insert object "日本語")
    (assert-equal "日本語" (event:text-input-event-text (first (events))))))

(define-test an-attributed-string-gives-its-text ()
  (with-view (view object events)
    (let ((attributed (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSAttributedString") "alloc")
                                     "initWithString:" (:id (ns "plain")))))
      (objc:msg-send :void object "insertText:replacementRange:"
                     (:id attributed) (:ulong +not-found+) (:ulong 0))
      (assert-equal "plain" (event:text-input-event-text (first (events)))))))

;;; --- composition ---

(define-test a-composition-starts-changes-and-is-committed-by-text ()
  (with-view (view object events)
    (assert-false (objc:msg-send :bool object "hasMarkedText"))
    (mark object "にほ" 2 0)
    (mark object "にほん" 3 0)
    (assert-true (objc:msg-send :bool object "hasMarkedText"))
    (insert object "日本")
    (assert-false (objc:msg-send :bool object "hasMarkedText"))
    (let ((seen (events)))
      (assert-equal '(:started :changed :committed) (mapcar #'event:composition-event-kind seen))
      (assert-equal '("にほ" "にほん" "日本") (mapcar #'event:composition-event-text seen))
      ;; A composition that is committed is not text as well.
      (assert-false (find-if (lambda (e) (typep e 'event:text-input-event)) seen)))))

(define-test the-selection-of-a-composition-is-in-characters ()
  (with-view (view object events)
    ;; "a", an emoji that is two UTF-16 units, and "b"; the selection is the emoji.
    (mark object (format nil "a~cb" (code-char #x1F600)) 1 2)
    (let ((e (first (events))))
      (assert-eql 1 (event:composition-event-selection-start e))
      (assert-eql 1 (event:composition-event-selection-length e)))))

(define-test the-marked-range-is-as-long-as-the-composition-in-utf-16-units ()
  (with-view (view object events)
    (multiple-value-bind (location length) (objc:msg-send :range object "markedRange")
      (assert-eql +not-found+ location)
      (assert-eql 0 length))
    (mark object (format nil "a~cb" (code-char #x1F600)) 0 0)
    (multiple-value-bind (location length) (objc:msg-send :range object "markedRange")
      (assert-eql 0 location)
      (assert-eql 4 length))))

(define-test an-emptied-composition-is-over ()
  (with-view (view object events)
    (mark object "に" 1 0)
    (mark object "" 0 0)
    (assert-false (objc:msg-send :bool object "hasMarkedText"))
    (let ((seen (events)))
      (assert-equal '(:started :committed) (mapcar #'event:composition-event-kind seen))
      (assert-equal "" (event:composition-event-text (second seen))))
    ;; Emptying it again does nothing.
    (mark object "" 0 0)
    (assert-equal '() (events))))

(define-test unmarking-commits-the-text-as-it-is ()
  (with-view (view object events)
    (mark object "かな" 2 0)
    (objc:msg-send :void object "unmarkText")
    (let ((seen (events)))
      (assert-equal '(:started :committed) (mapcar #'event:composition-event-kind seen))
      (assert-equal "かな" (event:composition-event-text (second seen))))
    (assert-false (objc:msg-send :bool object "hasMarkedText"))
    (objc:msg-send :void object "unmarkText")
    (assert-equal '() (events))))

;;; --- where the caret is ---

(define-test the-input-method-is-told-where-the-caret-is-on-the-screen ()
  (with-view (view object events)
    (multiple-value-bind (x y width height)
        (objc:msg-send :rect object "firstRectForCharacterRange:actualRange:"
                       (:range 0 0) (:id 0))
      (assert-true (< (abs (- width 2)) 1e-6))
      (assert-true (< (abs (- height 16)) 1e-6))
      ;; Somewhere on the screen, not at the origin that a failed conversion would give.
      (assert-true (or (/= x 0) (/= y 0))))))

(define-test the-caret-moves-with-what-the-app-says ()
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let* ((rect (list 10d0 20d0))
             (view (view:make-metal-view :width 200 :height 100
                                         :text-input (lambda () (values t (first rect) (second rect)
                                                                        2d0 16d0))))
             (object (view:metal-view-object view))
             (window (window:make-window :title "test" :width 200 :height 100)))
        (window:set-content-view window object)
        (let ((x1 (objc:msg-send :rect object "firstRectForCharacterRange:actualRange:"
                                 (:range 0 0) (:id 0))))
          (setf (first rect) 60d0)
          (let ((x2 (objc:msg-send :rect object "firstRectForCharacterRange:actualRange:"
                                   (:range 0 0) (:id 0))))
            (assert-true (< (abs (- (- x2 x1) 50)) 1e-6))))))))

(define-test the-answers-about-the-text-are-the-plain-ones ()
  (with-view (view object events)
    (multiple-value-bind (location length) (objc:msg-send :range object "selectedRange")
      (assert-eql +not-found+ location)
      (assert-eql 0 length))
    (assert-eql 0 (objc:msg-send :ulong object "characterIndexForPoint:" (:point 5 5)))
    (assert-eql 0 (objc:msg-send :id object "attributedSubstringForProposedRange:actualRange:"
                                 (:range 0 1) (:id 0)))
    (assert-eql 0 (objc:msg-send :ulong
                                 (objc:msg-send :id object "validAttributesForMarkedText") "count"))))

(define-test the-view-conforms-to-the-protocol ()
  (with-view (view object events)
    (assert-true (objc:msg-send :bool object "conformsToProtocol:"
                                (:id (sb-alien:alien-funcall
                                      (sb-alien:extern-alien "objc_getProtocol"
                                                             (function sb-alien:unsigned-long
                                                                       sb-alien:c-string))
                                      "NSTextInputClient"))))))

;;; --- keys ---

(define-test a-character-key-is-text-while-the-app-takes-text ()
  (with-view (view object events)
    (objc:msg-send :void object "keyDown:" (:id (key-event "a" 0)))
    (let ((seen (events)))
      (assert-equal 1 (length seen))
      (assert-true (typep (first seen) 'event:text-input-event))
      (assert-equal "a" (event:text-input-event-text (first seen))))))

(define-test a-command-key-is-a-key-event-while-the-app-takes-text ()
  (with-view (view object events)
    (objc:msg-send :void object "keyDown:" (:id (key-event (string (code-char #xF702)) 123)))
    (let ((seen (events)))
      (assert-equal 1 (length seen))
      (assert-true (typep (first seen) 'event:key-event))
      (assert-eql #xF702 (char-code (event:key-event-logical-key (first seen)))))))

(define-test return-and-backspace-are-key-events-too ()
  (with-view (view object events)
    (objc:msg-send :void object "keyDown:" (:id (key-event (string #\Return) 36)))
    (objc:msg-send :void object "keyDown:" (:id (key-event (string (code-char #x7F)) 51)))
    (assert-equal '(#\Return #\Backspace)
                  (mapcar #'event:key-event-logical-key (events)))))

(define-test a-key-with-command-is-a-key-event-and-not-text ()
  (with-view (view object events)
    ;; Command is bit 20 of the modifier flags.
    (objc:msg-send :void object "keyDown:" (:id (key-event "a" 0 (ash 1 20))))
    (let ((seen (events)))
      (assert-equal 1 (length seen))
      (assert-true (typep (first seen) 'event:key-event)))))

(define-test a-key-is-a-key-event-while-the-app-does-not-take-text ()
  (with-view (view object events :active nil)
    (objc:msg-send :void object "keyDown:" (:id (key-event "a" 0)))
    (let ((seen (events)))
      (assert-equal 1 (length seen))
      (assert-true (typep (first seen) 'event:key-event))
      (assert-eql #\a (event:key-event-logical-key (first seen))))))
