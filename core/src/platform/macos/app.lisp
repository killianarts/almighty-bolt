(defpackage #:almighty-bolt-core/platform/macos/app
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:objc-class
                #:ns-string
                #:release
                #:register-selector
                #:define-callback
                #:define-runtime-class
                #:with-autorelease-pool
                #:with-masked-float-traps)
  (:import-from #:almighty-bolt-core/platform/macos/main-queue
                #:run-on-main)
  (:export #:run-application
           #:quit-application))

(in-package #:almighty-bolt-core/platform/macos/app)

;;; The application: AppKit's event loop, which has to run on the main thread
;;; and does not return until the application quits. Everything else (windows,
;;; views, timers) runs inside it, called back by AppKit.

;;; The application delegate has one job: end the application when its last
;;; window closes.

(define-callback app-should-terminate-after-last-window-closed sb-alien:unsigned-char
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (app sb-alien:unsigned-long))
  1)

(define-runtime-class *app-delegate-class* "AlmightyBoltAppDelegate" "NSObject"
  :methods '(("applicationShouldTerminateAfterLastWindowClosed:" "B@:@"
              app-should-terminate-after-last-window-closed)))

(defun shared-application ()
  (msg-send :id (objc-class "NSApplication") "sharedApplication"))

(defun install-menu (app)
  "A main menu with only Quit, so that Command-Q works and the menu bar has the
process's name."
  (let* ((menu-bar (msg-send :id (msg-send :id (objc-class "NSMenu") "alloc") "init"))
         (app-item (msg-send :id (msg-send :id (objc-class "NSMenuItem") "alloc") "init"))
         (app-menu (msg-send :id (msg-send :id (objc-class "NSMenu") "alloc") "init"))
         (title (ns-string "Quit"))
         (key (ns-string "q"))
         (quit-item (msg-send :id (msg-send :id (objc-class "NSMenuItem") "alloc")
                              "initWithTitle:action:keyEquivalent:"
                              (:id title)
                              (:ulong (register-selector "terminate:"))
                              (:id key))))
    (msg-send :void app-menu "addItem:" (:id quit-item))
    (msg-send :void app-item "setSubmenu:" (:id app-menu))
    (msg-send :void menu-bar "addItem:" (:id app-item))
    (msg-send :void app "setMainMenu:" (:id menu-bar))
    (release title)
    (release key)))

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

(defun run-application (setup)
  "Start the application and run its event loop on the calling thread, which must
be the main thread. SETUP is called once, inside the application and on the main
thread, to make the windows. Returns only if the loop stops."
  (with-masked-float-traps
    (with-autorelease-pool
      (let ((app (shared-application))
            (delegate (msg-send :id (msg-send :id *app-delegate-class* "alloc") "init")))
        ;; A plain executable such as sbcl needs this to show windows and a menu.
        (msg-send :bool app "setActivationPolicy:" (:long 0))
        (msg-send :void app "setDelegate:" (:id delegate))
        (install-menu app)
        (funcall setup)
        (activate app)
        (msg-send :void app "run")))))

(defun quit-application ()
  "End the application. Safe from any thread."
  (run-on-main (lambda ()
                 (msg-send :void (shared-application) "terminate:" (:id 0)))))
