(defpackage #:almighty-bolt-7guis/launcher
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:now-ns
                #:handle-event
                #:hit-target
                #:variant
                #:frame-ring-total
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:key-event
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-physical-key
                #:key-event-modifiers
                #:key-event-repeat-p
                #:modifier-down-p
                #:+modifier-shift+
                #:+modifier-control+
                #:+modifier-alt+
                #:+modifier-super+)
  (:import-from #:almighty-bolt-core/platform/macos/host
                #:run-variant
                #:host-ring
                #:close-host-window)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:find-app
                #:app-constructor
                #:app-width
                #:app-height
                #:app-title)
  (:export #:run))

(in-package #:almighty-bolt-7guis/launcher)

;;; Open a task of a variant in a window. With LOG, the variant is changed into a subclass
;;; that prints each press and release of the pointer with what is under it, and each key
;;; with its character code, so that what a window really delivers can be seen. The
;;; printing allocates, so it is for a person to read and not for measuring.

(defclass logging-mixin () ()
  (:documentation "Prints the events that matter, before the variant handles them."))

(defun modifier-names (modifiers)
  (let ((names (loop :for (flag name) :in (list (list +modifier-shift+ "shift")
                                                  (list +modifier-control+ "control")
                                                  (list +modifier-alt+ "option")
                                                  (list +modifier-super+ "command"))
                     :when (modifier-down-p modifiers flag)
                       :collect name)))
    (if names (format nil "~{~a~^+~}" names) "none")))

(defmethod handle-event :before ((variant logging-mixin) event)
  (typecase event
    (pointer-event
     (when (member (pointer-event-kind event) '(:down :up))
       (let ((x (float (pointer-event-x event) 1f0))
             (y (float (pointer-event-y event) 1f0)))
         (format t "~&pointer ~(~4a~) button ~a at ~,1f, ~,1f   under it: ~s~%"
                 (pointer-event-kind event) (pointer-event-button event) x y
                 (hit-target variant x y)))))
    (key-event
     (when (eq (key-event-kind event) :down)
       (let ((key (key-event-logical-key event)))
         (format t "~&key down  ~s (code ~a) physical ~d modifiers ~a~:[~; (repeat)~]~%"
                 key (and key (char-code key)) (key-event-physical-key event)
                 (modifier-names (key-event-modifiers event)) (key-event-repeat-p event))))))
  (finish-output))

(defun logged (variant)
  "VARIANT, changed into an instance of a subclass of its class that logs."
  (let* ((class (class-of variant))
         (name (intern (format nil "LOGGED-~a" (class-name class)) '#:almighty-bolt-7guis/launcher)))
    (unless (find-class name nil)
      (sb-mop:ensure-class name :direct-superclasses (list 'logging-mixin (class-name class))))
    (change-class variant (find-class name))))

(defun smoke-check (host)
  "Wait for the app to draw, then see that it draws nothing while nothing happens."
  (sleep 1)
  (let ((first (frame-ring-total (host-ring host))))
    (sleep 1.5)
    (format t "~&drew ~d frames in the first second; ~d more in 1.5 s of nobody at the window~%"
            first (- (frame-ring-total (host-ring host)) first))
    (finish-output)
    (close-host-window host)))

(defun run (variant task &key log smoke)
  "Open the app of TASK in VARIANT (keywords) in a window, and run until it is closed."
  (let ((app (or (find-app variant task)
                 (error "There is no ~(~a~) app for ~(~a~)." variant task))))
    (let ((object (funcall (app-constructor app))))
      (when log
        (setf object (logged object))
        (format t "~&~a: press, drag, type; what arrives is printed. Close the window to finish.~%"
                (app-title app))
        (finish-output))
      (run-variant object :title (app-title app) :width (app-width app) :height (app-height app)
                          :driver (and smoke #'smoke-check)))))
