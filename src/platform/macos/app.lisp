(defpackage #:almighty-bolt/platform/macos/app
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:main-queue #:almighty-bolt/platform/macos/main-queue))
  (:export #:run-application
           #:quit-application
           #:stop-application
           #:*may-stop-p*
           #:set-app-menus))

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

(defvar *may-stop-p* (lambda () t)
  "A function of no arguments, called on the main thread when the last window has closed, that
says whether the application may end (or its loop stop) now. False when a window is about to be
opened from another thread, whose work is already on its way to the main thread.")

(defun shared-application ()
  (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication"))

(defun stop-event-loop (app)
  "Make the loop that RUN-APPLICATION is in return. -stop: takes effect when the event that is
being handled is done, and one that is called from outside an event (the main queue's work,
a window closed by a program) is not in one, so an event that does nothing is posted to wake the
loop, as GLFW does."
  (objc:msg-send :void app "stop:" (:id 0))
  (let ((event (objc:msg-send :id (objc:objc-class "NSEvent")
                              "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"
                              (:ulong 15)    ; NSEventTypeApplicationDefined
                              (:point 0 0) (:ulong 0) (:double 0d0) (:long 0) (:id 0)
                              (:int 0) (:long 0) (:long 0))))
    (objc:msg-send :void app "postEvent:atStart:" (:id event) (:bool t))))

(objc:define-callback app-should-terminate-after-last-window-closed sb-alien:unsigned-char
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (app sb-alien:unsigned-long))
  (cond ((not (funcall *may-stop-p*)) 0)
        (*ends-process* 1)
        (t (stop-event-loop app)
           0)))

;;; Quit (Command-Q). A program that is only its windows ends. A Lisp that a person is working in
;;; does not: its windows are all closed instead, and closing the last one stops the loop.
(objc:define-callback app-should-terminate sb-alien:unsigned-long
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (app sb-alien:unsigned-long))
  (cond (*ends-process* 1)              ; NSTerminateNow
        (t (let* ((windows (objc:msg-send :id app "windows"))
                  (count (objc:msg-send :ulong windows "count")))
             ;; -windows is a copy, so closing them as it is read is safe.
             (dotimes (i count)
               (let ((window (objc:msg-send :id windows "objectAtIndex:" (:ulong i))))
                 (when (objc:msg-send :bool window "isVisible")
                   (objc:msg-send :void window "performClose:" (:id 0))))))
           0)))                         ; NSTerminateCancel

(objc:define-runtime-class *app-delegate-class* "AlmightyBoltAppDelegate" "NSObject"
  :methods '(("applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
              app-should-terminate-after-last-window-closed)
             ("applicationShouldTerminate:" "Q@:@" app-should-terminate)))

(defconstant +command-key+ (ash 1 20) "NSEventModifierFlagCommand.")
(defconstant +option-key+ (ash 1 19) "NSEventModifierFlagOption.")
(defconstant +shift-key+ (ash 1 17) "NSEventModifierFlagShift.")

(defun menu-item (title action key &optional (modifiers +command-key+))
  "A menu item that sends ACTION (a selector name) to whatever has the keyboard, with the
shortcut KEY (\"\" for none) and MODIFIERS (an NSEventModifierFlags mask) held with it."
  (let ((title (objc:ns-string title))
        (key (objc:ns-string key)))
    (prog1 (let ((item (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSMenuItem") "alloc")
                                      "initWithTitle:action:keyEquivalent:"
                                      (:id title)
                                      (:ulong (objc:register-selector action))
                                      (:id key))))
             (objc:msg-send :void item "setKeyEquivalentModifierMask:" (:ulong modifiers))
             item)
      (objc:release title)
      (objc:release key))))

(defun make-menu (title &rest items)
  "A menu called TITLE holding ITEMS, in order; :separator is a line between them."
  (let* ((name (objc:ns-string title))
         (menu (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSMenu") "alloc") "initWithTitle:"
                              (:id name))))
    (objc:release name)
    (dolist (item items menu)
      (objc:msg-send :void menu "addItem:"
                     (:id (if (eq item :separator)
                              (objc:msg-send :id (objc:objc-class "NSMenuItem") "separatorItem")
                              item))))))

(defun add-menu (menu-bar menu)
  "Put MENU in MENU-BAR, under its own title."
  (let ((item (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSMenuItem") "alloc") "init")))
    (objc:msg-send :void item "setTitle:" (:id (objc:msg-send :id menu "title")))
    (objc:msg-send :void item "setSubmenu:" (:id menu))
    (objc:msg-send :void menu-bar "addItem:" (:id item))
    menu))

(defun app-menu-item (spec)
  "A menu item of the app's own (see APP-MENU): :SEPARATOR, a string, or (string &key key shift
option). It sends appMenuItem: to what has the keyboard, the view of the key window."
  (if (eq spec :separator)
      :separator
      (destructuring-bind (title &key key shift option) (if (stringp spec) (list spec) spec)
        (menu-item title "appMenuItem:" (if key (string (char-downcase key)) "")
                   (if key
                       (logior +command-key+ (if shift +shift-key+ 0) (if option +option-key+ 0))
                       0)))))

(defvar *installed-menus* :none
  "The app's own menus in the menu bar now, as SET-APP-MENUS was last given them, or :NONE.")

(defun set-app-menus (menus)
  "Put the app's own MENUS (a vector of each menu's title then its items) in the menu bar, between
the application's menu and Edit, if they are not the ones there. Called on the main thread after a
frame of the key window."
  (let ((menus (coerce (or menus #()) 'list)))
    (unless (equal menus *installed-menus*)
      (setf *installed-menus* (copy-tree menus))
      (install-menu (shared-application)
                    (loop :for (title items) :on menus :by #'cddr
                          :collect (apply #'make-menu title (mapcar #'app-menu-item items)))))))

(defun install-menu (app &optional own)
  "The main menu every Mac app has: the application's menu (hiding it and the others, and Quit),
the app's OWN menus (NSMenu objects), Edit (undo, redo, cut, copy, paste and select all, which reach
the app as their shortcuts' keys do) and Window (minimize, zoom and close). When the application does not end the process, Quit closes
the windows instead: it may not take the Lisp that a person is working in with it."
  (let ((menu-bar (objc:msg-send :id (objc:msg-send :id (objc:objc-class "NSMenu") "alloc") "init")))
    (add-menu menu-bar
              (make-menu "Application"
                         (menu-item "Hide" "hide:" "h")
                         (menu-item "Hide Others" "hideOtherApplications:" "h"
                                    (logior +command-key+ +option-key+))
                         (menu-item "Show All" "unhideAllApplications:" "" 0)
                         :separator
                         (menu-item "Quit" "terminate:" "q")))
    (dolist (menu own)
      (add-menu menu-bar menu))
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
      (objc:msg-send :void app "setWindowsMenu:" (:id window)))
    (objc:msg-send :void app "setMainMenu:" (:id menu-bar))))

(defun activate (app)
  "Make the application the active one, whoever was, so that its window is the key window and
keys reach it. macOS 14 added -activate and deprecated -activateIgnoringOtherApps:, but -activate
is cooperative: for a program that a terminal started it leaves the application inactive, the
window not the key window and the keys going to the terminal until the window is clicked (found
on macOS 26 with =make 7guis-smoke=). The old message still does it, so it is the one to use
where it exists."
  (if (objc:msg-send :bool app "respondsToSelector:"
                     (:ulong (objc:register-selector "activateIgnoringOtherApps:")))
      (objc:msg-send :void app "activateIgnoringOtherApps:" (:bool t))
      (objc:msg-send :void app "activate")))

(defun run-application (setup &key (ends-process t))
  "Start the application and run its event loop on the calling thread, which must
be the main thread. SETUP is called once, inside the application and on the main
thread, to make the windows. Returns when the loop stops. With ENDS-PROCESS, which is what
a program that is only the window wants, closing the last window ends the process and
this does not return; without it closing the last window (or quitting) stops the loop and
this returns, and it can be called again for another window."
  (objc:with-masked-float-traps
    (objc:with-autorelease-pool
      (let ((app (shared-application))
            (delegate (objc:msg-send :id (objc:msg-send :id *app-delegate-class* "alloc") "init"))
            (*ends-process* ends-process))
        ;; A plain executable such as sbcl needs this to show windows and a menu.
        (objc:msg-send :bool app "setActivationPolicy:" (:long 0))
        (objc:msg-send :void app "setDelegate:" (:id delegate))
        (install-menu app)
        (funcall setup)
        (activate app)
        (objc:msg-send :void app "run")))))

(defun quit-application ()
  "End the application, and the process with it. Safe from any thread."
  (main-queue:run-on-main (lambda ()
                            (objc:msg-send :void (shared-application) "terminate:" (:id 0)))))

(defun stop-application ()
  "Make RUN-APPLICATION return, whatever started it, leaving the process alone and the windows
as they are. Safe from any thread."
  (main-queue:run-on-main (lambda ()
                            (stop-event-loop (shared-application)))))
