(defpackage #:almighty-bolt/t/platform/macos/pasteboard
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:pb #:almighty-bolt/platform/macos/pasteboard)
                    (#:host #:almighty-bolt/platform/macos/host)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:app #:almighty-bolt/app)
                    (#:todo #:almighty-bolt-examples/todo))
  (:import-from #:almighty-bolt/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt/render/metal/target
                #:+pixel-format-bgra8-unorm+))

(in-package #:almighty-bolt/t/platform/macos/pasteboard)

;;; Copy and paste through the real pasteboard server. A test never touches the general pasteboard,
;;; which holds what the person copied: it makes a private one, and checks that a host made without a
;;; window leaves the general one as it was.

(defmacro with-private-pasteboard ((pasteboard) &body body)
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let ((,pasteboard (pb:make-private-pasteboard)))
         (unwind-protect (progn ,@body)
           (pb:discard-pasteboard ,pasteboard))))))

(defun change-count (pasteboard)
  (objc:msg-send :long pasteboard "changeCount"))

(defun todo-on-host (host)
  "A machine session on the to-do example, built by HOST's context as a window's frames are."
  (let* ((context (host:host-context host))
         (instance (core:with-kept-state ((core:make-kept-state))
                    (app:make-app-instance (quote todo:make-todo))))
         (list (core:make-display-list :capacity 16384)))
    (setf (core:context-width context) 360f0
          (core:context-height context) 320f0)
    (flet ((draw () (core:settle-frame instance context list)))
      (draw)
      (m:make-session :app (lambda () instance)
                      :deliver (lambda (event) (core:handle-event instance event) (draw))
                      :settle (lambda () (dotimes (i 4) (draw)))
                      :on-main #'funcall))))

(define-test text-goes-onto-a-pasteboard-and-comes-back-as-it-was ()
  (with-private-pasteboard (pasteboard)
    (dolist (text (list "Water the plants" "Grüße, 日本語, 🌱" (format nil "two~%lines")))
      (pb:set-pasteboard-string pasteboard text)
      (assert-equal text (pb:pasteboard-string pasteboard)))))

(define-test a-field-in-a-host-with-a-pasteboard-copies-to-it-and-pastes-from-it ()
  (with-private-pasteboard (pasteboard)
    (let* ((host (host:make-host (make-instance 'core:app-instance) (default-device)
                                 +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
           (session (progn (setf (host:mac-context-pasteboard (host:host-context host)) pasteboard)
                           (todo-on-host host))))
      (m:type-text session "Feed the cat" :id :draft)
      (m:press-key session :a :modifiers '(:super))
      (m:press-key session :x :modifiers '(:super))
      (assert-equal "Feed the cat" (pb:pasteboard-string pasteboard))
      (pb:set-pasteboard-string pasteboard "Water the plants")
      (m:press-key session :v :modifiers '(:super))
      (assert-equal "Water the plants"
                    (s:node-value (m:find-node session :id :draft))))))

(define-test a-host-made-without-a-window-leaves-the-persons-clipboard-alone ()
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let* ((general (pb:general-pasteboard))
             (before (change-count general))
             (host (host:make-host (make-instance 'core:app-instance) (default-device)
                                   +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
             (session (todo-on-host host)))
        (m:type-text session "Feed the cat" :id :draft)
        (m:press-key session :a :modifiers '(:super))
        (m:press-key session :c :modifiers '(:super))
        (assert-equal "Feed the cat" (core:context-clipboard-text (host:host-context host)))
        (assert-eql before (change-count general))))))
