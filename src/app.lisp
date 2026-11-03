(defpackage #:almighty-bolt/app
  (:use #:cl)
  (:import-from #:almighty-bolt/kept
                #:make-kept-state
                #:with-kept-state
                #:kept-value)
  (:import-from #:almighty-bolt/platform/macos/host
                #:run-variant
                #:open-variant
                #:close-variant
                #:close-host-window
                #:host-open-p
                #:running-host
                #:request-all-frames
                #:host-kept-state)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme-defined-hook*)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant)
  (:import-from #:almighty-bolt/sources
                #:watch-sources
                #:stop-watching-files)
  (:export #:make-app-instance
           #:run-app
           #:open-app
           #:close-app
           #:open-window
           #:close-window
           #:*development*
           #:state))

(in-package #:almighty-bolt/app)

;;; How an app is started. An app is a function of no arguments, its constructor, that makes the
;;; function that describes the screen (and holds the app's state in STATE-LET). The constructor is
;;; given by name, so that compiling it again is what the window runs: when code is compiled the
;;; window calls the constructor again and swaps what it makes in, with the state that STATE-LET
;;; keeps, the caret and selection of fields, the scroll of lists and the keyboard focus.

;;; A theme defined again (DEFINE-THEME, compiled in the editor) is drawn at once in the open windows:
;;; its new id is in the hashes of the boxes drawn in them, so a frame is all that is needed.
(setf *theme-defined-hook* #'request-all-frames)

(defvar *development* nil
  "True while the app is being developed: opened by OPEN-APP, or by RUN-APP with :HOT or
:WATCH-SYSTEMS. Then the windows it opens are made again when code is compiled, as its own is, and
the tools for developing it can be opened. Never true in an app that is delivered (RUN-APP alone).")

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
    (setf *development* (and (or hot watch-systems) t))
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
    (setf *development* t)
    (open-variant (with-kept-state (state) (make-app-instance constructor))
                  :title title :width width :height height
                  :kept-state state
                  :rebuild (lambda () (make-app-instance constructor)))))

(defun close-app ()
  "Close the window that OPEN-APP opened. Returns its host, or NIL if there was none. The other
windows stay open."
  (close-variant))

(defun open-window (constructor &key (title "almighty-bolt") (width 800) (height 600))
  "Open another window, WIDTH by HEIGHT points, that shows the app that CONSTRUCTOR makes, and return
it (for CLOSE-WINDOW). It is called from an app, in its function (say, when a button is pressed),
and the window opens after the frame; or from the REPL while a window is open, or before, when it
opens the first window as OPEN-APP does but is not the app's window.

The window's app is an app of its own, with its own STATE-LET. What two windows share is the
ordinary Lisp they share: a closure that CONSTRUCTOR makes over the first app's variables, or a
special variable. A frame in which one window took input (a press, a key, the wheel) draws the
other windows again, so that each shows what the other changed. While the app is being developed
(*DEVELOPMENT*), compiling code makes the window's app again, as it does the app's own; the window
stays open when the app's window closes, and closing the last window stops the windows' loop."
  (let ((state (make-kept-state)))
    (open-variant (with-kept-state (state) (make-app-instance constructor))
                  :title title :width width :height height
                  :kept-state state
                  :main nil
                  :rebuild (and *development* (lambda () (make-app-instance constructor))))))

(defun close-window (window)
  "Close WINDOW, which OPEN-WINDOW returned, as its close button would. Returns at once, and the
window is closed by the next frame; closing one that is closed does nothing."
  (when (host-open-p window)
    (close-host-window window))
  nil)

(defun state (name)
  "The value of the kept variable NAME (a symbol: the one STATE-LET was given, in the package it was
read in) of the app that is open, for looking at it from the REPL. NIL if there is no window or the
app keeps no such variable."
  (let ((host (running-host)))
    (and host (kept-value (host-kept-state host) name))))
