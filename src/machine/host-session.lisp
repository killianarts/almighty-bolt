(defpackage #:almighty-bolt/machine/host-session
  (:use #:cl)
  (:import-from #:almighty-bolt/machine
                #:make-session
                #:cannot
                #:machine-error-message)
  (:import-from #:almighty-bolt/machine/png
                #:png-octets)
  (:import-from #:almighty-bolt/internals
                #:context-width
                #:context-height
                #:context-scale)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt/platform/macos/host
                #:running-host
                #:host-variant
                #:host-context
                #:host-renderer
                #:host-display-list
                #:host-deliver-event
                #:wait-for-idle)
  (:import-from #:almighty-bolt/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt/render/metal/target
                #:make-offscreen-target
                #:offscreen-target-descriptor
                #:read-pixels)
  (:import-from #:almighty-bolt/render/metal/renderer
                #:render-display-list
                #:wait-for-last-frame)
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
  (call-on-main
   (lambda ()
     (with-masked-float-traps
       (with-autorelease-pool
         (let* ((context (host-context host))
                (width (context-width context))
                (height (context-height context))
                (scale (context-scale context))
                (pixel-width (max 1 (round (* width scale))))
                (pixel-height (max 1 (round (* height scale))))
                (target (make-offscreen-target (default-device) pixel-width pixel-height)))
           (render-display-list (host-renderer host) (host-display-list host)
                                (offscreen-target-descriptor target) 0 width height scale)
           (wait-for-last-frame (host-renderer host))
           (png-octets pixel-width pixel-height (read-pixels target))))))))

(defun host-session (host)
  "A session on the window of HOST (the machine interface's operations work on one)."
  (make-session :variant (lambda () (host-variant host))
                :deliver (lambda (event)
                           (call-on-main (lambda () (host-deliver-event host event))))
                :settle (lambda () (wait-for-idle host))
                :on-main #'call-on-main
                :screenshot (lambda () (host-screenshot host))))

(defun open-session ()
  "A session on the window that OPEN-APP or RUN-APP has open, or an error if there is none."
  (let ((host (running-host)))
    (unless host
      (error 'cannot :message "There is no window open: call open-app or run-app first."))
    (host-session host)))
