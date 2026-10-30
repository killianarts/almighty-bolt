(defpackage #:almighty-bolt/app
  (:use #:cl)
  (:import-from #:almighty-bolt/kept
                #:make-kept-state
                #:with-kept-state)
  (:import-from #:almighty-bolt/platform/macos/host
                #:run-variant
                #:open-variant
                #:close-variant)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant)
  (:import-from #:almighty-bolt/sources
                #:watch-sources
                #:stop-watching-files)
  (:export #:make-app-instance
           #:run-app
           #:open-app
           #:close-app))

(in-package #:almighty-bolt/app)

;;; How an app is started. An app is a function of no arguments, its constructor, that makes the
;;; function that describes the screen (and holds the app's state in KEPT-LET). The constructor is
;;; given by name, so that compiling it again is what the window runs: when code is compiled the
;;; window calls the constructor again and swaps what it makes in, with the state that KEPT-LET
;;; keeps, the caret and selection of fields, the scroll of lists and the keyboard focus.

(defun make-app-instance (constructor)
  "Call CONSTRUCTOR, a function or the name of one, and make what the host runs from the function
that describes the screen that it returns."
  (make-immediate-variant (funcall constructor)))

(defun run-app (constructor &key (title "almighty-bolt") (width 800) (height 600)
                              hot watch-systems driver)
  "Open a window of WIDTH by HEIGHT points that shows the app that CONSTRUCTOR makes. This must be
called on the main thread, which the window then has, and closing the window ends the process: it is
for a program that is only the window (sbcl --load app.lisp). For a REPL, which has to be free and
keeps running, see OPEN-APP.

With HOT, the app is made again, in the window, whenever code is compiled into the image. With
WATCH-SYSTEMS, a list of system names, the systems are also loaded again when one of their files is
saved, which is for editing in a program that does not compile into the running image.
DRIVER, if given, is called with the host on a new thread once the window is up (it is how the
smoke checks and the benchmarks use a window)."
  (let* ((state (make-kept-state))
         (variant (with-kept-state (state) (make-app-instance constructor)))
         (watcher (and watch-systems (watch-sources watch-systems))))
    (unwind-protect
         (run-variant variant :title title :width width :height height
                              :kept-state state :driver driver
                              :rebuild (and (or hot watch-systems)
                                            (lambda () (make-app-instance constructor))))
      (when watcher
        (stop-watching-files watcher)))))

(defun open-app (constructor &key (title "almighty-bolt") (width 800) (height 600))
  "Open a window of WIDTH by HEIGHT points that shows the app that CONSTRUCTOR makes, and return at
once, with the REPL free. Whenever code is compiled into the image, from the editor or by loading
a file, the app is made again in the same window, with the state it keeps. Closing the window (its
button, Command-W or Command-Q) or CLOSE-APP leaves Lisp running, and OPEN-APP can be called again;
the app then starts from the beginning.

It is called from a thread other than the main one, as the forms of an editor's REPL are in Slynk or
SLIME, and needs the main thread to be waiting for something that can be interrupted, as it is when
an editor starts Lisp with a pipe for its input. In a Lisp whose REPL runs on the main thread it is an
error; RUN-APP does what is wanted there."
  (let ((state (make-kept-state)))
    (open-variant (with-kept-state (state) (make-app-instance constructor))
                  :title title :width width :height height
                  :kept-state state
                  :rebuild (lambda () (make-app-instance constructor)))))

(defun close-app ()
  "Close the window that OPEN-APP opened. Returns its host, or NIL if there was none."
  (close-variant))
