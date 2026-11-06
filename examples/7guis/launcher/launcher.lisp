(defpackage #:almighty-bolt-7guis/launcher
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:host #:almighty-bolt/platform/macos/host)
                    (#:app #:almighty-bolt/app)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:sources #:almighty-bolt/sources))
  (:export #:run
           #:open-task
           #:close-task
           #:image-main))

(in-package #:almighty-bolt-7guis/launcher)

;;; Open a task in a window. With LOG, the app is changed into a subclass
;;; that prints each press and release of the pointer with what is under it, each key with
;;; its character code, and each step of the wheel with its deltas and whether they are in
;;; lines or points, so that what a window really delivers can be seen. The printing
;;; allocates, so it is for a person to read and not for measuring.

(defclass logging-mixin () ()
  (:documentation "Prints the events that matter, before the app handles them."))

(defun modifier-names (modifiers)
  (let ((names (loop :for (flag name) :in (list (list core:+modifier-shift+ "shift")
                                                  (list core:+modifier-control+ "control")
                                                  (list core:+modifier-alt+ "option")
                                                  (list core:+modifier-super+ "command"))
                     :when (core:modifier-down-p modifiers flag)
                       :collect name)))
    (if names (format nil "~{~a~^+~}" names) "none")))

(defmethod core:handle-event :before ((instance logging-mixin) event)
  (typecase event
    (core:pointer-event
     (when (member (core:pointer-event-kind event) '(:down :up))
       (let ((x (float (core:pointer-event-x event) 1f0))
             (y (float (core:pointer-event-y event) 1f0)))
         (format t "~&pointer ~(~4a~) button ~a at ~,1f, ~,1f   under it: ~s~%"
                 (core:pointer-event-kind event) (core:pointer-event-button event) x y
                 (core:hit-target instance x y)))))
    (core:wheel-event
     (let ((x (float (core:wheel-event-x event) 1f0))
           (y (float (core:wheel-event-y event) 1f0)))
       (format t "~&wheel dx ~,2f dy ~,2f in ~:[lines~;points~] at ~,1f, ~,1f   under it: ~s~%"
               (core:wheel-event-dx event) (core:wheel-event-dy event) (core:wheel-event-precise-p event)
               x y (core:hit-target instance x y))))
    (core:key-event
     (when (eq (core:key-event-kind event) :down)
       (let ((key (core:key-event-logical-key event)))
         (format t "~&key down  ~s (code ~a) physical ~d modifiers ~a~:[~; (repeat)~]~%"
                 key (and key (char-code key)) (core:key-event-physical-key event)
                 (modifier-names (core:key-event-modifiers event)) (core:key-event-repeat-p event))))))
  (finish-output))

(defclass recording-mixin ()
  ((recorder :initform nil :accessor mixin-recorder)
   (path :initform nil :accessor mixin-path))
  (:documentation "Notes the events for a replay script, before the app handles them, and
keeps the script in a file."))

(defun saves-script-p (event)
  "True for the events after which the script is written: a press or a release, a key, text.
A window that closes ends the process from AppKit without returning to Lisp, so the script is
written as the session goes and not when it ends; the moves and the wheel in between wait for
the next of these."
  (typecase event
    (core:pointer-event (member (core:pointer-event-kind event) '(:down :up)))
    (core:key-event t)
    (t nil)))

(defmethod core:handle-event :before ((instance recording-mixin) event)
  (when (mixin-recorder instance)
    (core:record-event (mixin-recorder instance) instance event)
    (when (saves-script-p event)
      (core:save-script (core:recorder-script (mixin-recorder instance)) (mixin-path instance)))))

(defun with-mixins (instance &rest mixins)
  "INSTANCE, changed into an instance of a subclass of its class that has the MIXINS."
  (let* ((class (class-of instance))
         (name (intern (format nil "~{~a~^+~}+~a" mixins (class-name class))
                       '#:almighty-bolt-7guis/launcher)))
    (unless (find-class name nil)
      (sb-mop:ensure-class name :direct-superclasses (append mixins (list (class-name class)))))
    (change-class instance (find-class name))))

(defun logged (instance)
  "INSTANCE, changed into an instance of a subclass of its class that logs."
  (with-mixins instance 'logging-mixin))

(defun smoke-check (host)
  "Wait for the app to draw, then see that it draws nothing while nothing happens."
  (sleep 1)
  (let ((first (core:frame-ring-total (host:host-ring host))))
    (sleep 1.5)
    (format t "~&drew ~d frames in the first second; ~d more in 1.5 s of nobody at the window~%"
            first (- (core:frame-ring-total (host:host-ring host)) first))
    (multiple-value-bind (responder key active) (host:host-keyboard-state host)
      (format t "~&keyboard: the view is the first responder ~a, the window is the key window ~a, ~
                 the application is active ~a~%" responder key active)
      (unless (and responder key active)
        (format t "~&WARNING: a key typed now would not reach the app until the window is clicked~%")))
    (finish-output)
    (host:close-host-window host)))

(defun measure-driver (host)
  "For a measurement from outside (bench/app-metrics.lisp): print the end time of the first frame
on the library's clock, the one an outside process can read too, and then once a second the frames
drawn so far and the bytes in use in the Lisp heap, until the process is killed. Everything is a
line of words and numbers."
  (let ((ring (host:host-ring host)))
    (loop :until (plusp (core:frame-ring-total ring))
          :do (sleep 0.001))
    (format t "~&first-frame-ns ~d~%" (core:frame-end-ns ring 0))
    (finish-output)
    (loop
      (sleep 1)
      (format t "~&tick ~d ~d~%" (core:frame-ring-total ring) (sb-kernel:dynamic-usage))
      (finish-output))))

(defun task-app (task)
  "The app registered for TASK, or an error."
  (or (registry:find-app task)
      (error "There is no app for ~(~a~)." task)))

(defun make-task-app (task log)
  "A new instance of the app of TASK, made by the constructor that is registered now: the app may
have been defined again since the window opened. With LOG it logs."
  (let* ((app (task-app task))
         (object (registry:make-app-object app)))
    (if log (logged object) object)))

(defun loaded-system-names ()
  "The names of the loaded systems that belong to this project."
  (remove-if-not (lambda (name) (eql 0 (search "almighty-bolt" name)))
                 (asdf:already-loaded-systems)))

(defun run (task &key log smoke record measure hot)
  "Open the app of TASK (a keyword) in a window, and run until it is closed. With
RECORD, a pathname, the events are written to it as a replay script as they come. With MEASURE
the process reports to whoever started it and runs until it is killed. With HOT, the app is made
again, in the same window, whenever the code that makes it is compiled, and a source file that is
saved is compiled and loaded; the state that the app keeps, and that its
widgets hold, is kept."
  (let ((app (task-app task))
        (recorder nil)
        (sources nil))
    (when (and hot record)
      (error "HOT and RECORD do not go together: a new app would not be recorded."))
    (let* ((state (core:make-kept-state))
           (object (core:with-kept-state (state)
                     (make-task-app task log))))
      (when log
        (format t "~&~a: press, drag, type; what arrives is printed. Close the window to finish.~%"
                (registry:app-title app))
        (finish-output))
      (when record
        (setf object (with-mixins object 'recording-mixin)
              recorder (core:make-recorder :header (list :app task
                                                         :width (registry:app-width app)
                                                         :height (registry:app-height app)))
              (mixin-recorder object) recorder
              (mixin-path object) record)
        (core:save-script (core:recorder-script recorder) record)
        (format t "~&~a: what you do is recorded to ~a as you go (after each press, release and key).~%"
                (registry:app-title app) record)
        (finish-output))
      (when hot
        (setf sources (sources:watch-sources (loaded-system-names)))
        (format t "~&~a: hot reload is on. Save a source file, or compile a definition, and the ~
                   app is made again here.~%" (registry:app-title app))
        (finish-output))
      (unwind-protect
           (host:run-host object :title (registry:app-title app) :width (registry:app-width app)
                               :height (registry:app-height app)
                               :kept-state state
                               :driver (cond (smoke #'smoke-check)
                                             (measure #'measure-driver))
                               :rebuild (and hot
                                             (lambda () (make-task-app task log))))
        (when sources
          (sources:stop-watching-files sources))))))

(defun open-task (task)
  "Open the app of TASK (a keyword) in a window and return its host at once, with the
REPL free: the library's OPEN-APP, for an app that is registered here. Whenever code is compiled
into the image the app is made again in the same window, with the state it keeps (STATE-LET);
closing the window leaves the Lisp running."
  (let ((app (task-app task)))
    (app:open-app (registry:app-constructor app)
                  :title (registry:app-title app) :width (registry:app-width app) :height (registry:app-height app))))

(defun close-task ()
  "Close the window that OPEN-TASK opened. Returns its host, or NIL if there was none."
  (app:close-app))

(defun environment (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun image-main ()
  "What a saved executable of the apps does when it starts: open the app of the task that the
environment's TASK names, in a window. MEASURE=1 makes it report to the process that started
it, for bench/app-metrics.lisp; RECORD is as for make 7guis."
  (sb-ext:disable-debugger)
  (let ((task (environment "TASK")))
    (unless task
      (format *error-output* "Say which task with TASK=counter, for example.~%")
      (sb-ext:exit :code 2 :abort t))
    (run (intern (string-upcase task) :keyword)
         :log (environment "LOG") :record (environment "RECORD") :measure (environment "MEASURE"))
    (sb-ext:exit :code 0)))
