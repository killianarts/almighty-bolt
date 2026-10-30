;;;; Check what an editor's REPL does: open a 7GUIs task in a window and get the REPL back, see
;;;; the app made again when code is compiled, close the window and keep the Lisp, open it again.
;;;; From the repository root:
;;;;   make 7guis-repl-smoke VARIANT=immediate TASK=crud
;;;; The main thread waits, as a REPL's waits for the editor, and another thread does what the
;;;; editor's forms do. It says "The REPL check passed" when every check holds, and the last one
;;;; is that the Lisp exits with a window open.

(defpackage #:almighty-bolt-7guis/repl-smoke
  (:use #:cl))

(in-package #:almighty-bolt-7guis/repl-smoke)

(defun keyword-from-environment (name)
  (let ((value (uiop:getenv name)))
    (unless (and value (plusp (length value)))
      (error "Say which with ~a=..., for example VARIANT=immediate TASK=crud." name))
    (intern (string-upcase value) :keyword)))

(defun symbol-in (package name)
  (find-symbol name package))

(defun call (package name &rest arguments)
  (apply (or (symbol-in package name) (error "No ~a in ~a." name package)) arguments))

(defun host-call (name &rest arguments)
  (apply #'call "ALMIGHTY-BOLT/PLATFORM/MACOS/HOST" name arguments))

(defun frames (host)
  (call "ALMIGHTY-BOLT/INTERNALS" "FRAME-RING-TOTAL" (host-call "HOST-RING" host)))

(defun wait-until (predicate &optional (seconds 5))
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop :until (funcall predicate)
          :do (when (> (get-internal-real-time) end)
                (return-from wait-until nil))
              (sleep 0.02))
    t))

(defvar *failed* nil)

(defun check (what ok)
  (format t "~&~:[FAIL~;ok  ~] ~a~%" ok what)
  (finish-output)
  (unless ok
    (setf *failed* t))
  ok)

(defun what-the-editor-does (variant task)
  (let* ((started (get-internal-real-time))
         (host (call "ALMIGHTY-BOLT-7GUIS/LAUNCHER" "OPEN-TASK" variant task))
         (took (/ (- (get-internal-real-time) started) internal-time-units-per-second 1.0)))
    (check "the window is open and the REPL has its thread back" (and host (host-call "RUNNING-HOST")))
    (check (format nil "that took ~,2f s, which is not a second" took) (< took 1))
    (check "the REPL's thread is not the main thread"
           (not (eq sb-thread:*current-thread* (sb-thread:main-thread))))
    (check "the REPL prints what it returned in a line, not the parts of a window"
           (< (length (prin1-to-string host)) 60))
    (check "the window draws" (wait-until (lambda () (plusp (frames host)))))
    ;; What compiling a definition does. Any function does: the app is made again when code
    ;; changes, not when a particular function does.
    (let ((app (host-call "HOST-VARIANT" host))
          (before (frames host)))
      (eval '(defun almighty-bolt-7guis/repl-smoke::compiled-in-the-editor () 1))
      (check "compiling a definition makes the app again, in the same window"
             (wait-until (lambda () (not (eq app (host-call "HOST-VARIANT" host))))))
      (check "and the window draws it" (wait-until (lambda () (> (frames host) before)))))
    (check "closing the window leaves the Lisp running" (eq host (call "ALMIGHTY-BOLT-7GUIS/LAUNCHER" "CLOSE-TASK")))
    (check "and no window is open" (null (host-call "RUNNING-HOST")))
    (check "closing it again does nothing" (null (call "ALMIGHTY-BOLT-7GUIS/LAUNCHER" "CLOSE-TASK")))
    (let ((again (call "ALMIGHTY-BOLT-7GUIS/LAUNCHER" "OPEN-TASK" variant task)))
      (check "a window can be opened again" (and again (not (eq again host))
                                                 (wait-until (lambda () (plusp (frames again))))))
      (call "ALMIGHTY-BOLT-7GUIS/LAUNCHER" "CLOSE-TASK"))
    (not *failed*)))

(defun the-lisp-can-exit-with-a-window-open (variant task)
  "What quitting the editor's Lisp does. It is the last check, because it ends the process, so
it says that the check passed before the exit that proves it, and a watchdog says that it did not
if the exit hangs. AppKit's own quit ends the process with code 0, which is why the Makefile
looks for the words and not for the code."
  (let ((host (call "ALMIGHTY-BOLT-7GUIS/LAUNCHER" "OPEN-TASK" variant task)))
    (wait-until (lambda () (plusp (frames host))))
    (format t "~&The REPL check passed, if the Lisp now exits with a window open.~%")
    (finish-output)
    (sb-thread:make-thread
     (lambda ()
       (sleep 15)
       (format t "~&The REPL check FAILED: the Lisp did not exit with a window open.~%")
       (finish-output)
       (sb-ext:exit :code 1 :abort t))
     :name "watchdog")
    (sb-ext:exit :code 0)))

(let ((variant (keyword-from-environment "VARIANT"))
      (task (keyword-from-environment "TASK")))
  (asdf:load-system "almighty-bolt-7guis/launcher")
  (asdf:load-system (format nil "almighty-bolt-7guis/~(~a~)" variant))
  (asdf:load-system (format nil "almighty-bolt-stress/~(~a~)" variant))
  (let* ((output *standard-output*)
         (editor (sb-thread:make-thread
                  (lambda ()
                    (handler-case (and (what-the-editor-does variant task)
                                       (the-lisp-can-exit-with-a-window-open variant task))
                      (error (condition)
                        (format output "~&FAIL the REPL signalled an error: ~a~%" condition)
                        nil)))
                  :name "repl")))
    ;; The main thread has nothing to do but wait, like a REPL for its next form. The window's
    ;; loop is run on it from the other thread.
    (sb-thread:join-thread editor :default nil)
    ;; Only a check that failed gets here: the last one ends the process.
    (format t "~&The REPL check FAILED.~%")
    (finish-output)
    (sb-ext:exit :code 1 :abort t)))
