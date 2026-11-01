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

(defconstant +command-key+ (ash 1 20) "NSEventModifierFlagCommand.")
(defconstant +option-key+ (ash 1 19) "NSEventModifierFlagOption.")
(defconstant +shift-key+ (ash 1 17) "NSEventModifierFlagShift.")

(defun menu-item (title action key &optional (modifiers +command-key+))
  "A menu item that sends ACTION (a selector name) to whatever has the keyboard, with the
shortcut KEY (\"\" for none) and MODIFIERS (an NSEventModifierFlags mask) held with it."
  (let ((title (ns-string title))
        (key (ns-string key)))
    (prog1 (let ((item (msg-send :id (msg-send :id (objc-class "NSMenuItem") "alloc")
                                 "initWithTitle:action:keyEquivalent:"
                                 (:id title)
                                 (:ulong (register-selector action))
                                 (:id key))))
             (msg-send :void item "setKeyEquivalentModifierMask:" (:ulong modifiers))
             item)
      (release title)
      (release key))))

(defun make-menu (title &rest items)
  "A menu called TITLE holding ITEMS, in order; :separator is a line between them."
  (let* ((name (ns-string title))
         (menu (msg-send :id (msg-send :id (objc-class "NSMenu") "alloc") "initWithTitle:"
                         (:id name))))
    (release name)
    (dolist (item items menu)
      (msg-send :void menu "addItem:"
                (:id (if (eq item :separator)
                         (msg-send :id (objc-class "NSMenuItem") "separatorItem")
                         item))))))

(defun add-menu (menu-bar menu)
  "Put MENU in MENU-BAR, under its own title."
  (let ((item (msg-send :id (msg-send :id (objc-class "NSMenuItem") "alloc") "init")))
    (msg-send :void item "setTitle:" (:id (msg-send :id menu "title")))
    (msg-send :void item "setSubmenu:" (:id menu))
    (msg-send :void menu-bar "addItem:" (:id item))
    menu))

(defun install-menu (app)
  "The main menu every Mac app has: the application's menu (hiding it and the others, and Quit),
Edit (undo, redo, cut, copy, paste and select all, which reach the app as their shortcuts' keys do)
and Window (minimize, zoom and close). When the application does not end the process, Quit closes
the window instead: it may not take the Lisp that a person is working in with it."
  (let ((menu-bar (msg-send :id (msg-send :id (objc-class "NSMenu") "alloc") "init")))
    (add-menu menu-bar
              (make-menu "Application"
                         (menu-item "Hide" "hide:" "h")
                         (menu-item "Hide Others" "hideOtherApplications:" "h"
                                    (logior +command-key+ +option-key+))
                         (menu-item "Show All" "unhideAllApplications:" "" 0)
                         :separator
                         (menu-item "Quit" (if *ends-process* "terminate:" "performClose:") "q")))
    (add-menu menu-bar
              (make-menu "Edit"
                         (menu-item "Undo" "undo:" "z")
                         (menu-item "Redo" "redo:" "z" (logior +command-key+ +shift-key+))
                         :separator
                         (menu-item "Cut" "cut:" "x")
                         (menu-item "Copy" "copy:" "c")
                         (menu-item "Paste" "paste:" "v")
                         (menu-item "Select All" "selectAll:" "a")))
    (let ((window (add-menu menu-bar
                            (make-menu "Window"
                                       (menu-item "Minimize" "performMiniaturize:" "m")
                                       (menu-item "Zoom" "performZoom:" "" 0)
                                       :separator
                                       (menu-item "Close" "performClose:" "w")))))
      (msg-send :void app "setWindowsMenu:" (:id window)))
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
