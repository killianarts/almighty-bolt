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
                #:host-variant
                #:host-on-close
                #:open-hosts
                #:request-host-frame
                #:running-host
                #:request-all-frames
                #:host-kept-state)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme-defined-hook*)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant
                #:immediate-variant-ui
                #:immediate-variant-tools)
  (:import-from #:almighty-bolt/ui/ui
                #:ui-inspection)
  (:import-from #:almighty-bolt/ui/inspection
                #:*inspector-opener*)
  (:import-from #:almighty-bolt/ui/inspector
                #:make-inspector)
  (:import-from #:almighty-bolt/machine
                #:perform-actions)
  (:import-from #:almighty-bolt/machine/host-session
                #:host-session)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:run-on-main)
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
that describes the screen that it returns. While the app is developed (*DEVELOPMENT*), its window can
open the inspector."
  (let ((variant (make-immediate-variant (funcall constructor))))
    (setf (immediate-variant-tools variant) *development*)
    variant))

(defun placement (top left bottom right beside side gap)
  "The placement (see PLACE-WINDOW in the platform's window) that the keyword arguments give."
  (append (and top (list :top top)) (and left (list :left left))
          (and bottom (list :bottom bottom)) (and right (list :right right))
          (and beside (list :beside beside :side side :gap gap))))

(defun run-app (constructor &key (title "almighty-bolt") (width 800) (height 600)
                              hot watch-systems driver top left bottom right)
  "Open a window of WIDTH by HEIGHT points that shows the app that CONSTRUCTOR makes. This must be
called on the main thread, which the window then has, and closing the window ends the process: it is
for a program that is only the window (sbcl --load app.lisp). For a REPL, which has to be free and
keeps running, see OPEN-APP.

With HOT, the app is made again, in the window, whenever code is compiled into the image. With
WATCH-SYSTEMS, a list of system names, the systems are also loaded again when one of their files is
saved, which is for editing in a program that does not compile into the running image.
DRIVER, if given, is called with the host on a new thread once the window is up (it is how the
smoke checks and the benchmarks use a window). TOP, LEFT, BOTTOM and RIGHT say where the window
opens, as for OPEN-APP."
  (setf *development* (and (or hot watch-systems) t))
  (let* ((state (make-kept-state))
         (variant (with-kept-state (state) (make-app-instance constructor)))
         (watcher (and watch-systems (watch-sources watch-systems))))
    (unwind-protect
         (run-variant variant :title title :width width :height height
                              :kept-state state :driver driver
                              :placement (placement top left bottom right nil nil nil)
                              :rebuild (and (or hot watch-systems)
                                            (lambda () (make-app-instance constructor))))
      (when watcher
        (stop-watching-files watcher)))))

(defun open-app (constructor &key (title "almighty-bolt") (width 800) (height 600)
                               top left bottom right beside (side :right) (gap 12))
  "Open a window of WIDTH by HEIGHT points that shows the app that CONSTRUCTOR makes, and return at
once, with the REPL free. Whenever code is compiled into the image, from the editor or by loading
a file, the app is made again in the same window, with the state it keeps. Closing the window (its
button, Command-W or Command-Q) or CLOSE-APP leaves Lisp running, and OPEN-APP can be called again;
the app then starts from the beginning.

It is called from a thread other than the main one, as the forms of an editor's REPL are in Slynk or
SLIME, and needs the main thread to be waiting for something that can be interrupted, as it is when
an editor starts Lisp with a pipe for its input. In a Lisp whose REPL runs on the main thread it is an
error; RUN-APP does what is wanted there.

Where the window opens: TOP, LEFT, BOTTOM and RIGHT are points from that edge of the screen's usable
area (below the menu bar and beside the Dock), as CSS's absolute positioning has them: LEFT before
RIGHT and TOP before BOTTOM when both are given, and the middle along an axis that has neither.
BESIDE is a window (one that OPEN-APP or OPEN-WINDOW returned), next to which it opens on its SIDE
(:right, :left, :above or :below), GAP points away, with the edges lined up, or on the other side
if there is no room there. With none of them, the first window opens in the middle of the screen and
the others below and to the right of the last one opened. Wherever it goes, it is kept on the
screen."
  (let ((state (make-kept-state)))
    (setf *development* t)
    (open-variant (with-kept-state (state) (make-app-instance constructor))
                  :title title :width width :height height
                  :kept-state state
                  :placement (placement top left bottom right beside side gap)
                  :rebuild (lambda () (make-app-instance constructor)))))

(defun close-app ()
  "Close the window that OPEN-APP opened. Returns its host, or NIL if there was none. The other
windows stay open."
  (close-variant))

(defun open-window (constructor &key (title "almighty-bolt") (width 800) (height 600)
                                  top left bottom right beside (side :right) (gap 12))
  "Open another window, WIDTH by HEIGHT points, that shows the app that CONSTRUCTOR makes, and return
it (for CLOSE-WINDOW). It is called from an app, in its function (say, when a button is pressed),
and the window opens after the frame; or from the REPL while a window is open, or before, when it
opens the first window as OPEN-APP does but is not the app's window.

The window's app is an app of its own, with its own STATE-LET. What two windows share is the
ordinary Lisp they share: a closure that CONSTRUCTOR makes over the first app's variables, or a
special variable. A frame in which one window took input (a press, a key, the wheel) draws the
other windows again, so that each shows what the other changed. While the app is being developed
(*DEVELOPMENT*), compiling code makes the window's app again, as it does the app's own; the window
stays open when the app's window closes, and closing the last window stops the windows' loop.

Where it opens is as for OPEN-APP: BESIDE the window that OPEN-APP returned, say, or at a corner of
the screen with TOP and RIGHT."
  (let ((state (make-kept-state)))
    (open-variant (with-kept-state (state) (make-app-instance constructor))
                  :title title :width width :height height
                  :kept-state state
                  :main nil
                  :placement (placement top left bottom right beside side gap)
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

;;; --- the inspector ---
;;;
;;; Command-Option-I in the window of an app that is developed opens the inspector (inspector.lisp)
;;; in a window of its own, which belongs to the app's window and closes with it; the keys again, in
;;; the app's window, close it. What it asks to be done to the app is done on a thread of its own,
;;; through a session on the app's window, as an agent's would be. When it closes, what it asked of
;;; the app's window (a pick, an outline) goes with it.

(defvar *inspectors* '()
  "The inspectors' hosts that are open, each with the host of the window it looks at: (app . inspector).")

(defun act-on (host)
  "What the inspector of HOST's window does with an action it asks for: do it after the frame, on a
thread of its own, through a session on the window, and report what went wrong on the main thread."
  (lambda (action report)
    (sb-thread:make-thread
     (lambda ()
       (let ((message (handler-case (perform-actions (host-session host) (list action))
                        (error (condition) (princ-to-string condition)))))
         (when message
           (run-on-main (lambda () (funcall report message))))))
     :name "inspector action")))

(defun toggle-inspector (variant)
  "Open the inspector of the window whose app is VARIANT, or close it if it is open. Called on the
main thread, in that window's frame."
  (setf *inspectors* (remove-if-not (lambda (pair) (host-open-p (cdr pair))) *inspectors*))
  (let ((host (find variant (open-hosts) :key #'host-variant)))
    (when host
      (let ((open (cdr (assoc host *inspectors*))))
        (if open
            (close-host-window open)
            (let* ((closed nil)
                   (inspector
                    (open-variant (make-immediate-variant
                                   ;; A frame of the inspector's window may come after it has
                                   ;; closed; it must not put its pick in the app's ui again.
                                   (make-inspector (lambda () (and (not closed) (host-open-p host)
                                                                   (host-variant host)))
                                                   (act-on host)))
                                  :title "Inspector" :width 360 :height 600 :main nil :owner host
                                  :placement (list :beside host :side :right))))
              (setf (host-on-close inspector)
                    (lambda ()
                      (setf closed t)
                      (when (host-open-p host)
                        (setf (ui-inspection (immediate-variant-ui (host-variant host))) nil)
                        (request-host-frame host))))
              (push (cons host inspector) *inspectors*)))))))

(setf *inspector-opener* #'toggle-inspector)
