(defpackage #:almighty-bolt/t/platform/macos/objc
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:main-queue #:almighty-bolt/platform/macos/main-queue)))

(in-package #:almighty-bolt/t/platform/macos/objc)

;;; These tests use the Objective-C runtime and a few AppKit classes, so they
;;; run on macOS only, and need to run on the main thread (as `make test` does).
;;; They create windows without showing them.

(defmacro with-appkit (&body body)
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication")
       ,@body)))

(defun make-window (rect-style)
  "A window that is never shown. Style 0 is borderless, so its frame is its content."
  (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSWindow") "alloc")
                 "initWithContentRect:styleMask:backing:defer:"
                 (:rect 0 0 100 100) (:ulong rect-style) (:ulong 2) (:bool nil)))

;;; --- classes, selectors and strings ---

(define-test a-global-of-a-framework-is-looked-up-once-and-looked-up-again-at-the-start-of-an-image ()
  ;; CoreText's kCTFontAttributeName is a reference held in a global; the dictionary callbacks are
  ;; a structure whose address is wanted. Both are cells, which an image that starts fills in again.
  (flet ((cell-of (kind name)
           (find-if (lambda (cell) (and (eq (objc::cell-kind cell) kind)
                                        (string= (objc::cell-name cell) name)))
                    objc::*cells*)))
    (let* ((value (objc:foreign-constant "kCTFontAttributeName"))
           (address (objc:foreign-address "kCFTypeDictionaryKeyCallBacks"))
           (constant-cell (cell-of :constant "kCTFontAttributeName"))
           (address-cell (cell-of :address "kCFTypeDictionaryKeyCallBacks")))
      (assert-true (plusp value))
      (assert-eql (sb-sys:sap-ref-64 (sb-sys:int-sap (sb-sys:find-dynamic-foreign-symbol-address
                                                      "kCTFontAttributeName"))
                                     0)
                  value)
      (assert-eql (sb-sys:find-dynamic-foreign-symbol-address "kCFTypeDictionaryKeyCallBacks")
                  address)
      (assert-true constant-cell)
      (assert-true address-cell)
      ;; An image that has started has the old process's addresses until the cells are resolved.
      (setf (objc::cell-value constant-cell) 0
            (objc::cell-value address-cell) 0)
      (objc::resolve-cell constant-cell)
      (objc::resolve-cell address-cell)
      (assert-eql value (objc:foreign-constant "kCTFontAttributeName"))
      (assert-eql address (objc:foreign-address "kCFTypeDictionaryKeyCallBacks"))
      (assert-error 'error (objc::resolve-cell (objc::make-cell :constant "NoSuchGlobalAtAll"))))))

(define-test classes-are-integers-and-missing-ones-are-reported ()
  (assert-true (typep (objc:find-objc-class "NSObject") '(unsigned-byte 64)))
  (assert-true (plusp (objc:find-objc-class "NSObject")))
  (assert-false (objc:find-objc-class "NoSuchClassAtAll" nil))
  (assert-error 'error (objc:find-objc-class "NoSuchClassAtAll")))

(define-test selectors-are-integers ()
  (assert-eql (objc:register-selector "setFrame:display:")
              (objc:register-selector "setFrame:display:"))
  (assert-false (eql (objc:register-selector "setFrame:display:")
                     (objc:register-selector "frame"))))

(define-test strings-round-trip-through-nsstring ()
  (with-appkit
    (dolist (text '("" "hello" "héllo かな 😀"))
      (let ((string (objc:ns-string text)))
        (assert-equal text (objc:lisp-string string))
        (objc:release string)))))

;;; --- structures and numbers ---

(define-test a-rect-argument-is-passed-as-four-doubles ()
  ;; If the callee saw the doubles in the wrong registers the frame would not
  ;; read back as it was set. The numbers are whole because AppKit rounds the
  ;; frame of a window to whole points.
  (with-appkit
    (let ((window (make-window 0)))
      (objc:msg-send :void window "setFrame:display:" (:rect 10d0 20d0 300d0 200d0) (:bool nil))
      (assert-equal '(10d0 20d0 300d0 200d0)
                    (multiple-value-list (objc:msg-send :rect window "frame"))))))

(define-test a-size-goes-in-and-comes-out ()
  (with-appkit
    (let ((window (make-window 0)))
      (objc:msg-send :void window "setMinSize:" (:size 200d0 150d0))
      (assert-equal '(200d0 150d0)
                    (multiple-value-list (objc:msg-send :size window "minSize"))))))

(define-test a-point-goes-in-and-comes-out ()
  (with-appkit
    (let ((event (objc:msg-send :id (objc:objc-class "NSEvent")
                                "mouseEventWithType:location:modifierFlags:timestamp:windowNumber:context:eventNumber:clickCount:pressure:"
                                (:long 1) (:point 12.5d0 34.25d0) (:ulong 0) (:double 0d0)
                                (:long 0) (:id 0) (:long 0) (:long 1) (:float 1f0))))
      (assert-equal '(12.5d0 34.25d0)
                    (multiple-value-list (objc:msg-send :point event "locationInWindow")))
      (assert-eql 1 (objc:msg-send :long event "clickCount")))))

(define-test booleans-and-doubles-are-converted ()
  (with-appkit
    (let ((window (make-window 0)))
      (assert-false (objc:msg-send :bool window "isVisible"))
      (assert-true (member (objc:msg-send :double window "backingScaleFactor")
                           '(1d0 2d0 3d0))))))

;;; --- classes made at run time ---

(objc:define-callback test-answer sb-alien:unsigned-long
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  42)

(objc:define-callback test-add-one sb-alien:double
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (x sb-alien:double))
  (+ x 1d0))

(objc:define-callback test-explode sb-alien:unsigned-long
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  (error "This callback fails on purpose."))

(defvar *test-class*
  (objc:ensure-class "AlmightyBoltTestClass" "NSObject"
                     :ivars '(("lispSlot" 8 3 "Q"))
                     :methods '(("answer" "Q@:" test-answer)
                                ("addOne:" "d@:d" test-add-one)
                                ("explode" "Q@:" test-explode))))

(define-test a-class-made-in-lisp-answers-messages ()
  (with-appkit
    (let ((object (objc:msg-send :id (objc:msg-send :id *test-class* "alloc") "init")))
      (assert-eql 42 (objc:msg-send :ulong object "answer"))
      (assert-eql 3.5d0 (objc:msg-send :double object "addOne:" (:double 2.5d0))))))

(define-test ensure-class-returns-the-existing-class ()
  (assert-eql *test-class*
              (objc:ensure-class "AlmightyBoltTestClass" "NSObject")))

(define-test an-instance-variable-holds-a-word ()
  (with-appkit
    (let* ((offset (objc:ivar-offset *test-class* "lispSlot"))
           (object (objc:msg-send :id (objc:msg-send :id *test-class* "alloc") "init")))
      (setf (objc:instance-slot object offset) 123456789)
      (assert-eql 123456789 (objc:instance-slot object offset))
      (assert-error 'error (objc:ivar-offset *test-class* "noSuchVariable")))))

(define-test an-error-in-a-callback-is-reported-and-contained ()
  (with-appkit
    (let* ((object (objc:msg-send :id (objc:msg-send :id *test-class* "alloc") "init"))
           (report (make-string-output-stream))
           (result (let ((*error-output* report))
                     (objc:msg-send :ulong object "explode"))))
      ;; It returned instead of unwinding through the Objective-C frames.
      (assert-eql 0 result)
      (assert-true (search "This callback fails on purpose" (get-output-stream-string report))))))

;;; --- the table of Lisp objects ---

(define-test registered-objects-are-found-by-index ()
  (let* ((a (objc:register-object :a))
         (b (objc:register-object :b)))
    (assert-true (plusp a))
    (assert-false (eql a b))
    (assert-eql :a (objc:registered-object a))
    (assert-eql :b (objc:registered-object b))
    (objc:unregister-object a)
    (assert-false (objc:registered-object a))
    ;; The freed index is reused, and the table grows when it is full.
    (let ((indices (loop :repeat 200 :collect (objc:register-object :x))))
      (assert-eql 200 (length (remove-duplicates indices)))
      (assert-true (every (lambda (i) (eq :x (objc:registered-object i))) indices))
      (dolist (i indices) (objc:unregister-object i)))
    (objc:unregister-object b)))

;;; --- allocation ---

;;; The loops live in functions compiled with this file, not in the test bodies:
;;; lisp-unit2 evaluates test bodies with a debug-heavy policy, which keeps every
;;; unused structure result alive and so allocates where compiled code does not.

(defun send-callbacks (object n)
  (dotimes (i n)
    (objc:msg-send :ulong object "answer")
    (objc:msg-send :double object "addOne:" (:double 1d0))))

(defun send-with-structs (window n)
  (let ((sum 0d0))
    (declare (double-float sum))
    (dotimes (i n)
      (objc:msg-send :void window "setViewsNeedDisplay:" (:bool nil))
      (multiple-value-bind (x y width height) (objc:msg-send :rect window "frame")
        (incf sum (+ x y width height)))
      (multiple-value-bind (width height) (objc:msg-send :size window "minSize")
        (incf sum (+ width height)))
      (incf sum (objc:msg-send :double window "backingScaleFactor")))
    sum))

(defun bytes-consed-by (thunk)
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall thunk)
    (- (sb-ext:get-bytes-consed) before)))

(define-test calling-into-lisp-allocates-nothing ()
  (with-appkit
    (let* ((object (objc:msg-send :id (objc:msg-send :id *test-class* "alloc") "init"))
           (delta (bytes-consed-by (lambda () (send-callbacks object 500000)))))
      ;; A round trip through a callback, a million of them. A boxed value per
      ;; call would add 16 MB.
      (assert-true (< delta 1000000) delta))))

(define-test sending-with-structures-allocates-nothing ()
  (with-appkit
    (let* ((window (make-window 0))
           (delta (bytes-consed-by (lambda () (send-with-structs window 200000)))))
      ;; Four sends a loop, structures going out and coming back, 800,000 sends.
      (assert-true (< delta 1000000) delta))))

;;; --- the main queue ---

(defun pump-main-queue (predicate &key (timeout 2d0))
  "Run the main thread's run loop until PREDICATE is true, as AppKit would, so
that work given to the main queue runs."
  (let ((mode (sb-alien:extern-alien "kCFRunLoopDefaultMode" sb-alien:unsigned-long))
        (deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop :until (or (funcall predicate) (> (get-internal-real-time) deadline))
          :do (sb-alien:alien-funcall
               (sb-alien:extern-alien "CFRunLoopRunInMode"
                                      (function sb-alien:int sb-alien:unsigned-long
                                                sb-alien:double (sb-alien:unsigned 8)))
               mode 0.01d0 0))
    (funcall predicate)))

(define-test work-given-from-another-thread-runs-on-the-main-thread ()
  (when (main-queue:on-main-thread-p)
    (let* ((ran-on nil)
           (thread (sb-thread:make-thread
                    (lambda ()
                      (main-queue:run-on-main
                       (lambda () (setf ran-on sb-thread:*current-thread*)))))))
      (sb-thread:join-thread thread)
      (assert-true (pump-main-queue (lambda () ran-on)))
      (assert-eql (sb-thread:main-thread) ran-on))))

(define-test calling-on-the-main-thread-waits-for-the-values ()
  (when (main-queue:on-main-thread-p)
    (let* ((result nil)
           (thread (sb-thread:make-thread
                    (lambda ()
                      (setf result (multiple-value-list
                                    (main-queue:call-on-main (lambda () (values 1 2 3)))))))))
      (assert-true (pump-main-queue (lambda () result)))
      (sb-thread:join-thread thread)
      (assert-equal '(1 2 3) result))))

(define-test on-the-main-thread-work-runs-at-once ()
  (when (main-queue:on-main-thread-p)
    (let ((ran nil))
      (main-queue:run-on-main (lambda () (setf ran t)))
      (assert-true ran)
      (assert-equal '(7) (multiple-value-list (main-queue:call-on-main (lambda () 7)))))))
