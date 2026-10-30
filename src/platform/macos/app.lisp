(defpackage #:almighty-bolt/platform/macos/app
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:ns-string
                #:release
                #:register-selector
                #:define-callback
                #:define-runtime-class
                #:with-autorelease-pool
                #:with-masked-float-traps)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:run-on-main)
  (:export #:run-application
           #:quit-application
           #:stop-application))

(in-package #:almighty-bolt/platform/macos/app)

;;; The application: AppKit's event loop, which has to run on the main thread
;;; and does not return until the application quits. Everything else (windows,
;;; views, timers) runs inside it, called back by AppKit.

;;; The application delegate has one job: end the application when its last
;;; window closes. What ending means depends on who started it. A program that is
;;; only the window (make 7guis) ends the process, as AppKit does by default. A Lisp
;;; that a person is working in (a REPL in an editor) must not die with its window, so
;;; there the loop stops and RUN-APPLICATION returns to the thread that was running it,
;;; which goes back to what it was doing.

(defvar *ends-process* t
  "True if the application ends the process when its last window closes, or when it is told
to quit. False if it only stops the event loop. Bound by RUN-APPLICATION.")

(defun shared-application ()
  (msg-send :id (objc-class "NSApplication") "sharedApplication"))

(defun stop-event-loop (app)
  "Make the loop that RUN-APPLICATION is in return. -stop: takes effect when the event that is
being handled is done, and one that is called from outside an event (the main queue's work,
a window closed by a program) is not in one, so an event that does nothing is posted to wake the
loop, as GLFW does."
  (msg-send :void app "stop:" (:id 0))
  (let ((event (msg-send :id (objc-class "NSEvent")
                         "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"
                         (:ulong 15)    ; NSEventTypeApplicationDefined
                         (:point 0 0) (:ulong 0) (:double 0d0) (:long 0) (:id 0)
                         (:int 0) (:long 0) (:long 0))))
    (msg-send :void app "postEvent:atStart:" (:id event) (:bool t))))

(define-callback app-should-terminate-after-last-window-closed sb-alien:unsigned-char
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (app sb-alien:unsigned-long))
  (cond (*ends-process* 1)
        (t (stop-event-loop app)
           0)))

(define-runtime-class *app-delegate-class* "AlmightyBoltAppDelegate" "NSObject"
  :methods '(("applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
              app-should-terminate-after-last-window-closed)))

(defun menu-item (title action key)
  "A menu item that sends ACTION (a selector name) to whatever has the keyboard, with the
shortcut KEY."
  (let ((title (ns-string title))
        (key (ns-string key)))
    (prog1 (msg-send :id (msg-send :id (objc-class "NSMenuItem") "alloc")
                     "initWithTitle:action:keyEquivalent:"
                     (:id title)
                     (:ulong (register-selector action))
                     (:id key))
      (release title)
      (release key))))

(defun install-menu (app)
  "A main menu with Quit, so that Command-Q works and the menu bar has the process's name. When
the application does not end the process, Quit closes the window instead, and so does Command-W:
neither may take the Lisp that a person is working in with them."
  (let* ((menu-bar (msg-send :id (msg-send :id (objc-class "NSMenu") "alloc") "init"))
         (app-item (msg-send :id (msg-send :id (objc-class "NSMenuItem") "alloc") "init"))
         (app-menu (msg-send :id (msg-send :id (objc-class "NSMenu") "alloc") "init")))
    (cond (*ends-process*
           (msg-send :void app-menu "addItem:" (:id (menu-item "Quit" "terminate:" "q"))))
          (t
           (msg-send :void app-menu "addItem:" (:id (menu-item "Close Window" "performClose:" "w")))
           (msg-send :void app-menu "addItem:" (:id (menu-item "Quit" "performClose:" "q")))))
    (msg-send :void app-item "setSubmenu:" (:id app-menu))
    (msg-send :void menu-bar "addItem:" (:id app-item))
    (msg-send :void app "setMainMenu:" (:id menu-bar))))

(defun activate (app)
  "Make the application the active one, whoever was, so that its window is the key window and
keys reach it. macOS 14 added -activate and deprecated -activateIgnoringOtherApps:, but -activate
is cooperative: for a program that a terminal started it leaves the application inactive, the
window not the key window and the keys going to the terminal until the window is clicked (found
on macOS 26 with =make 7guis-smoke=). The old message still does it, so it is the one to use
where it exists."
  (if (msg-send :bool app "respondsToSelector:"
                (:ulong (register-selector "activateIgnoringOtherApps:")))
      (msg-send :void app "activateIgnoringOtherApps:" (:bool t))
      (msg-send :void app "activate")))

(defun run-application (setup &key (ends-process t))
  "Start the application and run its event loop on the calling thread, which must
be the main thread. SETUP is called once, inside the application and on the main
thread, to make the windows. Returns when the loop stops. With ENDS-PROCESS, which is what
a program that is only the window wants, closing the last window ends the process and
this does not return; without it closing the last window (or quitting) stops the loop and
this returns, and it can be called again for another window."
  (with-masked-float-traps
    (with-autorelease-pool
      (let ((app (shared-application))
            (delegate (msg-send :id (msg-send :id *app-delegate-class* "alloc") "init"))
            (*ends-process* ends-process))
        ;; A plain executable such as sbcl needs this to show windows and a menu.
        (msg-send :bool app "setActivationPolicy:" (:long 0))
        (msg-send :void app "setDelegate:" (:id delegate))
        (install-menu app)
        (funcall setup)
        (activate app)
        (msg-send :void app "run")))))

(defun quit-application ()
  "End the application, and the process with it. Safe from any thread."
  (run-on-main (lambda ()
                 (msg-send :void (shared-application) "terminate:" (:id 0)))))

(defun stop-application ()
  "Make RUN-APPLICATION return, whatever started it, leaving the process alone and the windows
as they are. Safe from any thread."
  (run-on-main (lambda ()
                 (stop-event-loop (shared-application)))))
