(defpackage #:almighty-bolt/machine/host-session
  (:use #:cl)
  (:local-nicknames (#:m #:almighty-bolt/machine)
                    (#:png #:almighty-bolt/machine/png)
                    (#:core #:almighty-bolt/internals)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:main-queue #:almighty-bolt/platform/macos/main-queue)
                    (#:host #:almighty-bolt/platform/macos/host)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:target #:almighty-bolt/render/metal/target)
                    (#:renderer #:almighty-bolt/render/metal/renderer))
  (:export #:host-session
           #:open-session
           #:host-screenshot))

(in-package #:almighty-bolt/machine/host-session)

;;; The machine interface's view of a window: events go in on the main thread as the window's own
;;; do, and the wait for the app to answer is the host's, which is told by the frame that has nothing
;;; more to do and does not poll.

(defun host-screenshot (host)
  "What HOST last drew, as a PNG in a vector of bytes. The display list of the last frame is drawn
again, with the host's own renderer (the glyphs it names are in that renderer's atlas), into a
texture that nobody sees, between frames, on the main thread."
  (main-queue:call-on-main
   (lambda ()
     (objc:with-masked-float-traps
       (objc:with-autorelease-pool
         (let* ((context (host:host-context host))
                (width (core:context-width context))
                (height (core:context-height context))
                (scale (core:context-scale context))
                (pixel-width (max 1 (round (* width scale))))
                (pixel-height (max 1 (round (* height scale))))
                (target (target:make-offscreen-target (device:default-device) pixel-width pixel-height)))
           (renderer:render-display-list (host:host-renderer host) (host:host-display-list host)
                                         (target:offscreen-target-descriptor target) 0 width height scale)
           (renderer:wait-for-last-frame (host:host-renderer host))
           (png:png-octets pixel-width pixel-height (target:read-pixels target))))))))

(defun host-session (host)
  "A session on the window of HOST (the machine interface's operations work on one)."
  (m:make-session :app (lambda () (host:host-app host))
                  :deliver (lambda (event)
                           (main-queue:call-on-main (lambda () (host:host-deliver-event host event))))
                  :settle (lambda () (host:wait-for-idle host))
                  :on-main #'main-queue:call-on-main
                  :screenshot (lambda () (host-screenshot host))))

(defun open-session (&optional window)
  "A session on WINDOW (one that OPEN-WINDOW returned), or on the window that OPEN-APP or RUN-APP has
open; an error if there is none. It is returned once the window has made its first frame, so that
what is seen and photographed through it is the window and not the nothing before it."
  (let ((host (or window (host:running-host))))
    (unless host
      (error 'm:cannot :message "There is no window open: call open-app or run-app first."))
    (host:wait-for-first-frame host)
    (host-session host)))
