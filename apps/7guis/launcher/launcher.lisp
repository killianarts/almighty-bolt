(defpackage #:almighty-bolt-7guis/launcher
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:now-ns
                #:make-recorder
                #:record-event
                #:recorder-script
                #:save-script
                #:handle-event
                #:hit-target
                #:variant
                #:frame-ring-total
                #:frame-end-ns
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:wheel-event
                #:wheel-event-x
                #:wheel-event-y
                #:wheel-event-dx
                #:wheel-event-dy
                #:wheel-event-precise-p
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
                #:host-keyboard-state
                #:close-host-window)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:find-app
                #:app-constructor
                #:app-width
                #:app-height
                #:app-title)
  (:import-from #:almighty-bolt-7guis/common/sources
                #:watch-sources
                #:stop-watching-files)
  (:export #:run
           #:image-main))

(in-package #:almighty-bolt-7guis/launcher)

;;; Open a task of a variant in a window. With LOG, the variant is changed into a subclass
;;; that prints each press and release of the pointer with what is under it, each key with
;;; its character code, and each step of the wheel with its deltas and whether they are in
;;; lines or points, so that what a window really delivers can be seen. The printing
;;; allocates, so it is for a person to read and not for measuring.

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
    (wheel-event
     (let ((x (float (wheel-event-x event) 1f0))
           (y (float (wheel-event-y event) 1f0)))
       (format t "~&wheel dx ~,2f dy ~,2f in ~:[lines~;points~] at ~,1f, ~,1f   under it: ~s~%"
               (wheel-event-dx event) (wheel-event-dy event) (wheel-event-precise-p event)
               x y (hit-target variant x y))))
    (key-event
     (when (eq (key-event-kind event) :down)
       (let ((key (key-event-logical-key event)))
         (format t "~&key down  ~s (code ~a) physical ~d modifiers ~a~:[~; (repeat)~]~%"
                 key (and key (char-code key)) (key-event-physical-key event)
                 (modifier-names (key-event-modifiers event)) (key-event-repeat-p event))))))
  (finish-output))

(defclass recording-mixin ()
  ((recorder :initform nil :accessor mixin-recorder)
   (path :initform nil :accessor mixin-path))
  (:documentation "Notes the events for a replay script, before the variant handles them, and
keeps the script in a file."))

(defun saves-script-p (event)
  "True for the events after which the script is written: a press or a release, a key, text.
A window that closes ends the process from AppKit without returning to Lisp, so the script is
written as the session goes and not when it ends; the moves and the wheel in between wait for
the next of these."
  (typecase event
    (pointer-event (member (pointer-event-kind event) '(:down :up)))
    (key-event t)
    (t nil)))

(defmethod handle-event :before ((variant recording-mixin) event)
  (when (mixin-recorder variant)
    (record-event (mixin-recorder variant) variant event)
    (when (saves-script-p event)
      (save-script (recorder-script (mixin-recorder variant)) (mixin-path variant)))))

(defun with-mixins (variant &rest mixins)
  "VARIANT, changed into an instance of a subclass of its class that has the MIXINS."
  (let* ((class (class-of variant))
         (name (intern (format nil "~{~a~^+~}+~a" mixins (class-name class))
                       '#:almighty-bolt-7guis/launcher)))
    (unless (find-class name nil)
      (sb-mop:ensure-class name :direct-superclasses (append mixins (list (class-name class)))))
    (change-class variant (find-class name))))

(defun logged (variant)
  "VARIANT, changed into an instance of a subclass of its class that logs."
  (with-mixins variant 'logging-mixin))

(defun smoke-check (host)
  "Wait for the app to draw, then see that it draws nothing while nothing happens."
  (sleep 1)
  (let ((first (frame-ring-total (host-ring host))))
    (sleep 1.5)
    (format t "~&drew ~d frames in the first second; ~d more in 1.5 s of nobody at the window~%"
            first (- (frame-ring-total (host-ring host)) first))
    (multiple-value-bind (responder key active) (host-keyboard-state host)
      (format t "~&keyboard: the view is the first responder ~a, the window is the key window ~a, ~
                 the application is active ~a~%" responder key active)
      (unless (and responder key active)
        (format t "~&WARNING: a key typed now would not reach the app until the window is clicked~%")))
    (finish-output)
    (close-host-window host)))

(defun measure-driver (host)
  "For a measurement from outside (bench/app-metrics.lisp): print the end time of the first frame
on the core clock, the one an outside process can read too, and then once a second the frames
drawn so far and the bytes in use in the Lisp heap, until the process is killed. Everything is a
line of words and numbers."
  (let ((ring (host-ring host)))
    (loop :until (plusp (frame-ring-total ring))
          :do (sleep 0.001))
    (format t "~&first-frame-ns ~d~%" (frame-end-ns ring 0))
    (finish-output)
    (loop
      (sleep 1)
      (format t "~&tick ~d ~d~%" (frame-ring-total ring) (sb-kernel:dynamic-usage))
      (finish-output))))

(defun make-app-variant (variant task log)
  "A new variant of the app of TASK in VARIANT, made by the constructor that is registered now:
the app may have been defined again since the window opened. With LOG it logs."
  (let* ((app (or (find-app variant task)
                  (error "There is no ~(~a~) app for ~(~a~)." variant task)))
         (object (funcall (app-constructor app))))
    (if log (logged object) object)))

(defun run (variant task &key log smoke record measure hot)
  "Open the app of TASK in VARIANT (keywords) in a window, and run until it is closed. With
RECORD, a pathname, the events are written to it as a replay script as they come. With MEASURE
the process reports to whoever started it and runs until it is killed. With HOT, the app is made
again, in the same window, whenever the code that makes it is compiled, and a source file that is
saved is compiled and loaded (decision record 0030); the new app starts from nothing."
  (let ((app (or (find-app variant task)
                 (error "There is no ~(~a~) app for ~(~a~)." variant task)))
        (recorder nil)
        (sources nil))
    (when (and hot record)
      (error "HOT and RECORD do not go together: a new app would not be recorded."))
    (let ((object (make-app-variant variant task log)))
      (when log
        (format t "~&~a: press, drag, type; what arrives is printed. Close the window to finish.~%"
                (app-title app))
        (finish-output))
      (when record
        (setf object (with-mixins object 'recording-mixin)
              recorder (make-recorder :header (list :variant variant :app task
                                                    :width (app-width app)
                                                    :height (app-height app)))
              (mixin-recorder object) recorder
              (mixin-path object) record)
        (save-script (recorder-script recorder) record)
        (format t "~&~a: what you do is recorded to ~a as you go (after each press, release and key).~%"
                (app-title app) record)
        (finish-output))
      (when hot
        (setf sources (watch-sources))
        (format t "~&~a: hot reload is on. Save a source file, or compile a definition, and the ~
                   app is made again here.~%" (app-title app))
        (finish-output))
      (unwind-protect
           (run-variant object :title (app-title app) :width (app-width app)
                               :height (app-height app)
                               :driver (cond (smoke #'smoke-check)
                                             (measure #'measure-driver))
                               :rebuild (and hot
                                             (lambda () (make-app-variant variant task log))))
        (when sources
          (stop-watching-files sources))))))

(defun environment (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun image-main (variant)
  "What a saved executable of VARIANT does when it starts: open the app of the task that the
environment's TASK names, in a window. MEASURE=1 makes it report to the process that started
it, for bench/app-metrics.lisp; RECORD is as for make 7guis."
  (sb-ext:disable-debugger)
  (let ((task (environment "TASK")))
    (unless task
      (format *error-output* "Say which task with TASK=counter, for example.~%")
      (sb-ext:exit :code 2 :abort t))
    (run variant (intern (string-upcase task) :keyword)
         :log (environment "LOG") :record (environment "RECORD") :measure (environment "MEASURE"))
    (sb-ext:exit :code 0)))
