(defpackage #:almighty-bolt/t/platform/macos/file-panel
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:panel #:almighty-bolt/platform/macos/file-panel)
                    (#:main-queue #:almighty-bolt/platform/macos/main-queue)))

(in-package #:almighty-bolt/t/platform/macos/file-panel)

;;; The system's Open and Save panels as the host makes them, set up but not shown: showing one waits
;;; for a person, which is for a window (try it in the gallery).

(defmacro with-appkit (&body body)
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication")
       ,@body)))

(define-test an-open-panel-allows-the-extensions-it-is-given ()
  (with-appkit
    (let ((open (panel:make-file-panel :open :types '("txt" "md"))))
      (assert-true (objc:msg-send :bool open "isKindOfClass:" (:id (objc:objc-class "NSOpenPanel"))))
      (assert-equal '("txt" "md") (panel:file-panel-types open))
      (assert-false (objc:msg-send :bool open "allowsMultipleSelection"))
      (assert-false (objc:msg-send :bool open "canChooseDirectories")))
    (assert-eq nil (panel:file-panel-types (panel:make-file-panel :open)))))

(define-test a-save-panel-offers-the-name-it-is-given ()
  (with-appkit
    (let ((save (panel:make-file-panel :save :types '("txt") :name "Untitled.txt")))
      (assert-false (objc:msg-send :bool save "isKindOfClass:" (:id (objc:objc-class "NSOpenPanel"))))
      (assert-equal "Untitled.txt" (panel:file-panel-name save))
      (assert-equal '("txt") (panel:file-panel-types save)))))

;;; A panel runs a loop of its own until the person answers. The host starts it from a turn of the
;;; main run loop, so that what else is given to the main queue meanwhile (the REPL's work, an agent's)
;;; still runs; started from the queue itself, nothing would until the panel closed.

(defun run-loop-until (predicate mode seconds)
  "Run the main thread's run loop in MODE (a string) until PREDICATE is true or SECONDS have passed."
  (let ((name (objc:ns-string mode))
        (deadline (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (unwind-protect
         (loop :until (or (funcall predicate) (> (get-internal-real-time) deadline))
               :do (sb-alien:alien-funcall
                    (sb-alien:extern-alien "CFRunLoopRunInMode"
                                           (function sb-alien:int sb-alien:unsigned-long
                                                     sb-alien:double (sb-alien:unsigned 8)))
                    name 0.05d0 1))
      (objc:release name))))

(define-test work-started-in-a-turn-of-the-run-loop-leaves-the-main-queue-served ()
  (with-appkit
    (let ((started nil) (served nil) (served-while-it-ran nil))
      (main-queue:run-on-main-in-a-turn
       (lambda ()
         (setf started t)
         (main-queue:run-on-main-later (lambda () (setf served t)))
         ;; The loop a modal panel runs, in the mode it runs it in.
         (run-loop-until (lambda () served) "NSModalPanelRunLoopMode" 1)
         (setf served-while-it-ran served)))
      ;; Not at once: at the next turn of the run loop.
      (assert-false started)
      (run-loop-until (lambda () started) "kCFRunLoopDefaultMode" 2)
      (assert-true started)
      (assert-true served-while-it-ran))))
