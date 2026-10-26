(defpackage #:almighty-bolt-scripts/mcclim-measure
  (:use #:cl)
  (:local-nicknames (#:dashboard #:almighty-bolt-stress/dashboard/mcclim)
                    (#:gadgets #:almighty-bolt-stress/dashboard/mcclim-gadgets)
                    (#:rows #:almighty-bolt-stress/rows/mcclim)
                    (#:editor #:almighty-bolt-stress/editor/mcclim)
                    (#:model #:almighty-bolt-stress/dashboard/model)
                    (#:bt #:bordeaux-threads)))

(in-package #:almighty-bolt-scripts/mcclim-measure)

;;;; What the McCLIM versions of the stress apps cost, by McCLIM only (not comparable with a frame
;;;; of ours, which is a display list for Metal, nor between the null port, which makes the output
;;;; records and sends them nowhere, and the CLX port, which sends them to XQuartz). One task, one
;;;; port, one size at a time, from the repository root, after loading almighty-bolt-stress/mcclim;
;;;; the result is one RESULT line per number:
;;;;   TASK=dashboard PORT=null sbcl ... --load scripts/mcclim-measure.lisp
;;;; TASK is dashboard, board, chart (the panes of the dashboard alone), gadgets, rows or editor;
;;;; PORT is null or clx (clx needs DISPLAY); N is the number of rows or of lines; for the editor,
;;;; MODE is after (the text is set once the window is up) or construct (the text is given as the
;;;; frame is made) and CHARS is the number of characters typed. A run that takes more than TIMEOUT
;;;; seconds (900) is ended with a RESULT TIMEOUT line.

(defun env (name default)
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value))) value default)))

(defun now ()
  (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

(defun result (&rest fields)
  (format t "~&RESULT~{ ~a~}~%" fields)
  (finish-output))

(defun make-frame (maker port-name &rest initargs)
  "A frame made by MAKER, on the null port if PORT-NAME is null, and on the default port if not
(then the frame has its panes when it is run)."
  (if (string= port-name "null")
      (apply maker
             :frame-manager (clim:find-frame-manager :port (clim:find-port :server-path '(:null)))
             initargs)
      (apply maker initargs)))

(defvar *failure* nil "The error that the thread of a frame ended with, if one did.")

(defun frame-thread (frame)
  "A thread that runs FRAME and notes the error that ends it, which a window that the display
cannot make (X11 has 16 bits for a window's size) does."
  (bt:make-thread (lambda ()
                    (handler-case (clim:run-frame-top-level frame)
                      (error (condition)
                        (setf *failure* (remove #\Newline (princ-to-string condition))))))
                  :name "frame"))

(defun await-enabled (frame)
  "True once FRAME is enabled, and false if its thread has ended with an error first."
  (loop :until (or (eq (clim:frame-state frame) :enabled) *failure*) :do (sleep 0.005))
  (not *failure*))

(defun run-frame (frame)
  "Run FRAME in a thread, and return the thread once the frame is enabled and has had a second to
display itself."
  (let ((thread (frame-thread frame)))
    (await-enabled frame)
    (sleep 1)
    thread))

(defun stop-frame (frame thread)
  (let ((sheet (clim:frame-top-level-sheet frame)))
    (clim:queue-event sheet (make-instance 'clim:window-manager-delete-event :sheet sheet)))
  (bt:join-thread thread))

(defun retained-bytes ()
  (sb-ext:gc :full t)
  (sb-kernel:dynamic-usage))

(defun measure-steps (frame model step-function port-name tag)
  "The time and the bytes consed of one step of an animated dashboard: the model brought to a new
step and what is done to show it."
  (dotimes (i 10)
    (model:advance-dashboard model (+ 1 i))
    (funcall step-function frame))
  (let ((start (now)) (bytes (sb-ext:get-bytes-consed)) (steps 100))
    (dotimes (i steps)
      (model:advance-dashboard model (+ 20 i))
      (funcall step-function frame))
    (let ((seconds (/ (- (now) start) steps)))
      (result tag port-name "step-ms" (format nil "~,2f" (* 1000 seconds))
              "bytes-per-step" (floor (- (sb-ext:get-bytes-consed) bytes) steps)
              "steps-per-second-flat-out" (format nil "~,1f" (/ 1 seconds))))))

(defun measure-dashboard (port-name)
  (let* ((frame (make-frame #'dashboard:make-dashboard-frame port-name))
         (thread (run-frame frame)))
    (measure-steps frame (dashboard:frame-model frame)
                   (lambda (frame) (clim:redisplay-frame-panes frame :force-p t))
                   port-name "dashboard")
    (stop-frame frame thread)))

(defun measure-pane (port-name pane)
  "One pane of the display-function dashboard, shown again and again: the board of 36 tiles or the
chart of 721 rectangles."
  (let* ((frame (make-frame #'dashboard:make-dashboard-frame port-name))
         (thread (run-frame frame))
         (model (dashboard:frame-model frame)))
    (measure-steps frame model
                   (lambda (frame) (clim:redisplay-frame-pane frame pane :force-p t))
                   port-name (string-downcase (symbol-name pane)))
    (stop-frame frame thread)))

(defun measure-gadgets (port-name)
  (let* ((frame (make-frame #'gadgets:make-gadget-dashboard-frame port-name))
         (thread (run-frame frame)))
    (measure-steps frame (gadgets:frame-model frame) #'gadgets:update-gadgets port-name "gadgets")
    (stop-frame frame thread)))

(defun measure-rows (port-name count)
  (let* ((before (retained-bytes))
         (consed (sb-ext:get-bytes-consed))
         (start (now))
         (frame (make-frame #'rows:make-rows-frame port-name :row-count count))
         (thread (bt:make-thread (lambda () (clim:run-frame-top-level frame)) :name "frame")))
    (loop :until (eq (clim:frame-state frame) :enabled) :do (sleep 0.005))
    (let ((enabled (now)))
      (sleep 1)
      (result "rows" port-name count "seconds-to-enabled" (format nil "~,2f" (- enabled start))
              "bytes-consed-making" (- (sb-ext:get-bytes-consed) consed)
              "bytes-retained" (- (retained-bytes) before))
      (let ((pane (clim:find-pane-named frame 'rows::rows)))
        (let ((start (now)))
          (dotimes (i 20)
            (let ((clim:*application-frame* frame))
              (rows::move-selection frame almighty-bolt-core:+key-page-down+)))
          (result "rows" port-name count "page-down-ms"
                  (format nil "~,2f" (/ (* 1000 (- (now) start)) 20))))
        (let ((start (now)))
          (dotimes (i 20)
            (clim:handle-repaint pane clim:+everywhere+))
          (result "rows" port-name count "repaint-visible-ms"
                  (format nil "~,2f" (/ (* 1000 (- (now) start)) 20))))))
    (stop-frame frame thread)))

(defun focus (pane)
  (setf (clim:port-keyboard-input-focus (clim:port pane)) pane))

(defun measure-editor (port-name lines characters mode)
  "MODE is construct, which gives the text to the gadget as the frame is made (the way a McCLIM
program does it), or after, which makes the frame with ten lines and sets the text once it is up."
  (let* ((construct (string= mode "construct"))
         (before (retained-bytes))
         (start (now))
         (frame (make-frame #'editor:make-editor-frame port-name
                            :line-count (if construct lines 10)))
         (thread (frame-thread frame)))
    (cond ((not (await-enabled frame))
           (result "editor" port-name lines mode "FAILED" *failure*))
          (t
           (let ((enabled (now)))
             (sleep 1)
             (let ((pane (clim:find-pane-named frame 'editor::editor)))
               (focus pane)
               (unless construct
                 (let ((text (almighty-bolt-stress/editor/model:generate-text lines))
                       (start (now)))
                   (setf (clim:gadget-value pane) text)
                   (result "editor" port-name lines mode "set-text-seconds"
                           (format nil "~,2f" (- (now) start)))))
               (when construct
                 (result "editor" port-name lines mode "seconds-to-enabled"
                         (format nil "~,2f" (- enabled start))))
               (result "editor" port-name lines mode "bytes-retained" (- (retained-bytes) before))
               (let ((start (now)))
                 (clim:gadget-value pane)
                 (result "editor" port-name lines mode "gadget-value-ms"
                         (format nil "~,1f" (* 1000 (- (now) start)))))
               (clim:handle-repaint pane clim:+everywhere+)
               (let ((start (now)))
                 (clim:handle-repaint pane clim:+everywhere+)
                 (result "editor" port-name lines mode "repaint-ms"
                         (format nil "~,1f" (* 1000 (- (now) start)))))
               (let ((start (now)) (bytes (sb-ext:get-bytes-consed)))
                 (dotimes (i characters)
                   (clim:handle-event pane (make-instance 'clim:key-press-event
                                                          :sheet pane :key-name :x
                                                          :key-character #\x :modifier-state 0)))
                 (result "editor" port-name lines mode "typed-character-ms"
                         (format nil "~,1f" (/ (* 1000 (- (now) start)) characters))
                         "bytes-per-character" (floor (- (sb-ext:get-bytes-consed) bytes)
                                                      characters)))))))
    (if *failure*
        (result "editor" port-name lines mode "FAILED-LATER" *failure*)
        (stop-frame frame thread))))

(let ((task (env "TASK" "dashboard"))
      (port-name (env "PORT" "null"))
      (n (parse-integer (env "N" "1000")))
      (timeout (parse-integer (env "TIMEOUT" "900"))))
  (result "machine" (lisp-implementation-type) (lisp-implementation-version)
          (machine-type) "port" port-name "task" task "n" n)
  ;; A run that does not end in TIMEOUT seconds says so and stops (one of the first runs of the rows
  ;; hung with its thread busy, once, and was not found again).
  (bt:make-thread (lambda ()
                    (sleep timeout)
                    (result "TIMEOUT" task port-name n timeout "seconds")
                    (sb-ext:exit :code 3 :abort t))
                  :name "watchdog")
  (cond ((string= task "dashboard") (measure-dashboard port-name))
        ((string= task "gadgets") (measure-gadgets port-name))
        ((string= task "board") (measure-pane port-name 'dashboard::board))
        ((string= task "chart") (measure-pane port-name 'dashboard::chart))
        ((string= task "rows") (measure-rows port-name n))
        ((string= task "editor") (measure-editor port-name n (parse-integer (env "CHARS" "5"))
                                                (env "MODE" "after")))
        (t (error "TASK is dashboard, gadgets, rows or editor, not ~a." task)))
  (sb-ext:exit :code 0 :abort t))
