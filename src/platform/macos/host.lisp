(defpackage #:almighty-bolt/platform/macos/host
  (:use #:cl)
  (:import-from #:almighty-bolt/clock
                #:real-time-source)
  (:import-from #:almighty-bolt/ring
                #:make-frame-ring
                #:record-frame
                #:record-frame-details)
  (:import-from #:almighty-bolt/display-list
                #:make-display-list)
  (:import-from #:almighty-bolt/kept
                #:make-kept-state
                #:with-kept-state)
  (:import-from #:almighty-bolt/variant
                #:inherit-state
                #:retire-variant
                #:context
                #:context-width
                #:context-height
                #:context-scale
                #:context-frame-number
                #:context-requester
                #:context-frame-requested
                #:context-text-input-p
                #:context-text-input-rect
                #:context-phases-p
                #:context-phase-ns
                #:reset-phases
                #:+phase-update+
                #:+phase-layout+
                #:+phase-paint+
                #:+phase-first-build+
                #:+phase-all-builds+
                #:take-frame-request
                #:deliver-event
                #:take-events
                #:context-font
                #:context-measure
                #:context-break-lines
                #:context-caret-x
                #:context-index-at-x
                #:context-caret-rect
                #:context-push-text)
  (:import-from #:almighty-bolt/settle
                #:settle-frame)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:on-main-thread-p
                #:run-on-main
                #:call-on-main)
  (:import-from #:almighty-bolt/redefinition
                #:watch-redefinitions
                #:stop-watching-redefinitions)
  (:import-from #:almighty-bolt/platform/macos/app
                #:run-application)
  (:import-from #:almighty-bolt/platform/macos/window
                #:make-window
                #:set-content-view
                #:show-window
                #:close-window
                #:window-object
                #:window-scale-factor)
  (:import-from #:almighty-bolt/platform/macos/view
                #:make-metal-view
                #:metal-view-object
                #:request-draw)
  (:import-from #:almighty-bolt/text/font
                #:make-font)
  (:import-from #:almighty-bolt/text/line
                #:get-line
                #:measure-text
                #:break-lines
                #:caret-x
                #:index-at-x
                #:caret-rect)
  (:import-from #:almighty-bolt/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt/render/metal/renderer
                #:make-renderer
                #:renderer-atlas
                #:render-display-list
                #:wait-for-last-frame
                #:last-frame-gpu-ns)
  (:import-from #:almighty-bolt/render/metal/draw-text
                #:push-line)
  (:export #:host
           #:make-host
           #:host-variant
           #:host-context
           #:host-display-list
           #:host-renderer
           #:host-ring
           #:host-window
           #:host-view
           #:host-acquire
           #:host-wake
           #:host-idle-p
           #:wait-for-idle
           #:host-kept-state
           #:swap-variant
           #:reload-host
           #:running-host
           #:run-frame
           #:host-deliver-event
           #:close-host-window
           #:host-keyboard-state
           #:run-variant
           #:open-variant
           #:close-variant))

(in-package #:almighty-bolt/platform/macos/host)

;;; The macOS host: it owns the window, the view, the
;;; renderer and the display list, gives a variant its context, delivers events to
;;; it, and builds and draws a frame whenever the view draws.

;;; --- the context, with text from Core Text and the glyph atlas ---

(defstruct (mac-context (:include context)
                        (:constructor make-mac-context
                            (&key width height scale requester renderer)))
  "A context whose text operations are answered by Core Text and the renderer's
glyph atlas. FONTS keeps the fonts made, by name and then by size."
  (renderer nil :read-only t)
  (fonts (make-hash-table :test 'equal) :read-only t))

(defmethod context-font ((context mac-context) name size)
  (let ((fonts (mac-context-fonts context)))
    (multiple-value-bind (entries present) (gethash name fonts)
      (or (cdr (assoc size entries :test #'=))
          (let ((font (make-font name size)))
            ;; The caller's string may change later, so keep a copy as the key.
            (setf (gethash (if present name (copy-seq name)) fonts)
                  (acons size font entries))
            font)))))

(defmethod context-measure ((context mac-context) font string)
  (measure-text font string))

(defmethod context-break-lines ((context mac-context) font string width)
  (break-lines font string width))

(defmethod context-caret-x ((context mac-context) font string index)
  (caret-x (get-line font string) index))

(defmethod context-index-at-x ((context mac-context) font string x)
  (index-at-x (get-line font string) x))

(defmethod context-caret-rect ((context mac-context) font string index)
  (caret-rect (get-line font string) index))

(defmethod context-push-text ((context mac-context) display-list font string x baseline color)
  (push-line display-list (renderer-atlas (mac-context-renderer context))
             (get-line font string) (float x 1f0) (float baseline 1f0) color))

;;; --- the host ---

(defstruct (host (:constructor %make-host))
  ;; The variant the host builds. A replay that draws sets it for each run, so that one host's
  ;; renderer and glyph atlas stay warm from run to run.
  (variant nil)
  ;; The variant that SWAP-VARIANT replaced, until the one that replaced it has made a frame
  ;; without an error. If that frame fails, this is the one that is put back.
  (previous nil)
  ;; What the app keeps between the times it is made (KEPT-LET), which is bound while the app is
  ;; made again and so outlives the variant.
  (kept-state (make-kept-state) :read-only t)
  (context nil :read-only t)
  (display-list nil :read-only t)
  (renderer nil :read-only t)
  (ring nil :read-only t)
  ;; Guards DRAWING-P and the context's frame-requested flag, so that a request
  ;; made from another thread while a frame ends is never lost.
  (lock (sb-thread:make-mutex :name "host") :read-only t)
  (drawing-p nil :type boolean)
  (window nil)
  (view nil)
  ;; The watcher of definitions that makes the app again, if the window was opened to do that.
  (watcher nil)
  ;; Returns the render pass and the drawable to draw into, as two values, or zero
  ;; for both if there is nothing to draw into just now.
  (acquire (lambda () (values 0 0)) :type function)
  ;; Asks the view for one more frame. It runs on the main thread and does not
  ;; allocate, which REQUEST-DRAW (it makes a closure to hand to the main thread)
  ;; does. NIL for a host with no view, which leaves requests as a flag for whoever
  ;; drives it.
  (wake nil :type (or null function))
  ;; True from the moment a draw is asked for until the frame that answers it starts, so that "no
  ;; frame is being made and none is wanted" (HOST-IDLE-P) has no gap in it.
  (draw-pending nil :type boolean)
  ;; Callers of WAIT-FOR-IDLE, and the queue they wait on. A frame that ends with nothing more
  ;; wanted wakes them if there are any; with none it reads one number.
  (waiters 0 :type fixnum)
  (idle-queue (sb-thread:make-waitqueue :name "host idle")))

(defmethod print-object ((host host) stream)
  ;; What a REPL shows after (OPEN-APP ...), which returns the host: not its parts.
  (print-unreadable-object (host stream :type t :identity t)))

(defun request-host-frame (host)
  "What REQUEST-FRAME does for HOST. While a frame is being made, a request only
sets a flag, and the host asks for the next frame once this one is presented. At
any other time it asks at once: directly on the main thread, and through the main
queue from another thread. A host with no wake function, as in tests, leaves the
flag set."
  (sb-thread:with-mutex ((host-lock host))
    (let ((wake (host-wake host))
          (view (host-view host)))
      (cond ((or (host-drawing-p host) (null wake))
             (setf (context-frame-requested (host-context host)) t))
            ((on-main-thread-p) (setf (host-draw-pending host) t) (funcall wake))
            (view (setf (host-draw-pending host) t) (request-draw view))
            (t (setf (context-frame-requested (host-context host)) t))))))

(defun make-host (variant device pixel-format
                  &key (capacity 65536) (ring-capacity 4096) (scale 2) phases
                    (kept-state (make-kept-state)))
  "A host for VARIANT drawing with DEVICE into targets of PIXEL-FORMAT, with no
window yet. CAPACITY is the most commands a frame may hold. SCALE is the pixels a
point that the glyph atlas is rasterized at, which is the scale text must be drawn
at. With PHASES, the instrumentation is on: the variant's update, layout and paint
times, the first build, garbage collection and the GPU's time go into the ring as well
(see WITH-PHASE). KEPT-STATE is what the app keeps when it is made again (KEPT-LET); the one that
made VARIANT had it bound. RUN-VARIANT makes one with a window; tests make one and call RUN-FRAME."
  (let* ((renderer (make-renderer device pixel-format :capacity capacity :scale scale))
         (context (let ((context (make-mac-context :width 0f0 :height 0f0 :scale 1f0
                                                    :renderer renderer)))
                    (setf (context-phases-p context) (and phases t))
                    context))
         (host (%make-host :variant variant
                           :kept-state kept-state
                           :context context
                           :renderer renderer
                           :display-list (make-display-list :capacity capacity)
                           :ring (make-frame-ring :capacity ring-capacity))))
    (setf (context-requester context)
          (lambda (context)
            (declare (ignore context))
            (request-host-frame host)))
    host))

(defun host-deliver-event (host event)
  "Offer EVENT to the host's variant, requesting a frame if it wants one. Called on
the main thread, once for each event, in order."
  (when host
    (deliver-event (host-variant host) (host-context host) event)))

(defun start-drawing (host)
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-drawing-p host) t
          (host-draw-pending host) nil)))

(defun stop-drawing (host)
  "End a frame. Returns true if a frame was requested while it was made and there is
a way to ask for another."
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-drawing-p host) nil)
    (let ((again (and (host-wake host)
                      (take-frame-request (host-context host)))))
      (cond (again
             ;; The caller wakes the view next, outside the lock.
             (setf (host-draw-pending host) t))
            ((plusp (host-waiters host))
             (sb-thread:condition-broadcast (host-idle-queue host))))
      again)))

(defun idle-p (host)
  (and (not (host-drawing-p host))
       (not (host-draw-pending host))
       (not (context-frame-requested (host-context host)))))

(defun host-idle-p (host)
  "True if no frame is being made and none is wanted: what the app was asked is answered on the
screen."
  (sb-thread:with-mutex ((host-lock host))
    (idle-p host)))

(defun wait-for-idle (host &key (timeout 5))
  "Wait until HOST-IDLE-P, at most TIMEOUT seconds. Returns true if it is idle. The frame that
ends with nothing more wanted wakes the waiter; nothing polls, and a window that nobody waits for pays
a read of one number a frame."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (sb-thread:with-mutex ((host-lock host))
      (incf (host-waiters host))
      (unwind-protect
           (loop
             (when (idle-p host)
               (return t))
             (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
               (when (<= left 0)
                 (return nil))
               (let ((woken (sb-thread:condition-wait (host-idle-queue host) (host-lock host)
                                                      :timeout left)))
                 ;; A wait that times out may return without the lock.
                 (unless (sb-thread:holding-mutex-p (host-lock host))
                   (sb-thread:grab-mutex (host-lock host)))
                 (unless woken
                   (return (idle-p host))))))
        (decf (host-waiters host))))))

(defun report-hot-reload (what condition)
  "Say on the error output that WHAT happened, and why. Printed without the pretty printer, which
breaks the lines of an error's text at a narrow margin."
  (let ((*print-pretty* nil))
    (format *error-output* "~&Hot reload: ~a: ~a~%" what condition))
  (finish-output *error-output*))

(defun retire (variant)
  "RETIRE-VARIANT, for a variant that is no longer used. A failure to let go of it is reported and
does not stop what is going on."
  (handler-case (retire-variant variant)
    (error (condition)
      (report-hot-reload "an app that is no longer used could not be let go of" condition))))

(defun swap-variant (host variant)
  "Make HOST run VARIANT from the next frame, in place of the one it runs. The window, the
renderer and the glyph atlas stay, and the new variant is given what the old one kept that is not
the app's own (INHERIT-STATE): the focus, the carets, the scrolls. If that signals an error, the
old one stays and the error is signalled again. If the new variant's first frame signals an error,
the old one is put back and the error is reported (SETTLE-HOST), so that code that does not work
yet does not take the window with it. Call it on the thread that draws, the main thread, and ask
for a frame after (RELOAD-HOST does both)."
  (let ((old (or (host-previous host) (host-variant host))))
    (handler-case (inherit-state variant old)
      (error (condition)
        (retire variant)
        (error condition)))
    ;; A variant that was swapped in and has not drawn yet is replaced by this one, and goes.
    (when (host-previous host)
      (retire (host-variant host)))
    (setf (host-previous host) old
          (host-variant host) variant))
  variant)

(defun settle-host (host context)
  "SETTLE-FRAME with HOST's variant. A variant that SWAP-VARIANT has just put in has its first
frame made with the old one ready: if it signals an error, the old one is put back, the
error is reported, and the frame is made again with that one. Whichever of the two is not used
after that is retired."
  (let ((previous (host-previous host))
        (display-list (host-display-list host)))
    (cond ((null previous)
           (settle-frame (host-variant host) context display-list))
          (t
           (setf (host-previous host) nil)
           (let ((builds nil) (capped nil) (failure nil))
             (handler-case (multiple-value-setq (builds capped)
                             (settle-frame (host-variant host) context display-list))
               (error (condition)
                 (setf failure condition)))
             (cond ((null failure)
                    (retire previous)
                    (values builds capped))
                   (t
                    (report-hot-reload "the new app failed in its first frame, so the old one is back"
                                       failure)
                    (let ((failed (host-variant host)))
                      (setf (host-variant host) previous)
                      (retire failed))
                    (settle-frame previous context display-list))))))))

(defun run-frame (host width height scale)
  "Make and draw one frame: the size and scale go into the context, the variant
builds until the frame settles, the drawable is asked for, and the frame is
encoded, committed and presented. The time of each part goes into the ring: the
build, the wait for the display (the GPU finishing the frame before, and then
getting the drawable) and the encoding. A frame requested while this one was made
is asked for after it is presented, with the host's wake function, which does not
allocate. WIDTH and HEIGHT are in points. Does nothing past the build if there is
no drawable. Must run inside an autorelease pool."
  (declare (type single-float width height scale))
  (let ((context (host-context host))
        (ring (host-ring host))
        (again nil)
        (start (real-time-source))
        (consed-before (the (unsigned-byte 64) (sb-ext:get-bytes-consed)))
        (gc-before sb-ext:*gc-run-time*)
        (previous-gpu-ns 0))
    (declare (type (unsigned-byte 64) start consed-before previous-gpu-ns)
             (type fixnum gc-before))
    (when (context-phases-p context)
      (reset-phases context))
    (start-drawing host)
    (unwind-protect
         (progn
           (setf (context-width context) width
                 (context-height context) height
                 (context-scale context) scale)
           (incf (context-frame-number context))
           (multiple-value-bind (builds capped)
               (settle-host host context)
             (let ((built (real-time-source)))
               (wait-for-last-frame (host-renderer host))
               ;; The frame before has finished, and its command buffer says how long it took.
               (when (context-phases-p context)
                 (setf previous-gpu-ns (last-frame-gpu-ns (host-renderer host))))
               (multiple-value-bind (pass drawable) (funcall (host-acquire host))
                 (let ((acquired (real-time-source)))
                   (unless (zerop pass)
                     (render-display-list (host-renderer host) (host-display-list host)
                                          pass drawable width height scale))
                   (let ((done (real-time-source)))
                     (record-frame ring start done consed-before
                                   (the (unsigned-byte 64) (sb-ext:get-bytes-consed))
                                   (- built start) (- acquired built) (- done acquired)
                                   builds capped)
                     (when (context-phases-p context)
                       (let ((slots (context-phase-ns context)))
                         (multiple-value-bind (events-ns events) (take-events context)
                           (record-frame-details
                            ring
                            :gc-ns (* (max 0 (- sb-ext:*gc-run-time* gc-before))
                                      #.(floor 1000000000 internal-time-units-per-second))
                            :first-build-ns (aref slots +phase-first-build+)
                            :update-ns (aref slots +phase-update+)
                            :layout-ns (aref slots +phase-layout+)
                            :paint-ns (aref slots +phase-paint+)
                            :all-builds-ns (aref slots +phase-all-builds+)
                            :gpu-ns previous-gpu-ns
                            :events-ns events-ns
                            :events events))))))))))
      (setf again (stop-drawing host)))
    (when again
      (funcall (the function (host-wake host))))
    nil))

;;; --- a window ---

(defun view-acquirer (object)
  "A function that asks the MTKView OBJECT for its render pass and drawable."
  (lambda ()
    (let ((pass (msg-send :id object "currentRenderPassDescriptor"))
          (drawable (msg-send :id object "currentDrawable")))
      (if (or (zerop pass) (zerop drawable))
          (values 0 0)
          (values pass drawable)))))

(defun draw-view (host view)
  "The view's draw callback: read its size and scale, and make a frame."
  (when host
    (let ((object (metal-view-object view)))
      (multiple-value-bind (x y width height) (msg-send :rect object "bounds")
        (declare (ignore x y))
        (let ((pixel-width (msg-send :size object "drawableSize")))
          (run-frame host
                     (float width 1f0) (float height 1f0)
                     (if (plusp width) (float (/ pixel-width width) 1f0) 1f0)))))))

(defun close-host-window (host)
  "Close the host's window as the user would, which ends the application. Safe from
any thread."
  (close-window (host-window host)))

(defun host-keyboard-state (host)
  "Where a key would go, as three values that are true or false: whether the host's view is the
first responder of its window, whether the window is the key window, and whether the application
is the active one. A person can type into the variant only when all three are true. Safe from any
thread."
  (call-on-main
   (lambda ()
     (let ((window (window-object (host-window host)))
           (view (metal-view-object (host-view host)))
           (app (msg-send :id (objc-class "NSApplication") "sharedApplication")))
       (values (eql (msg-send :id window "firstResponder") view)
               (and (msg-send :bool window "isKeyWindow") t)
               (and (msg-send :bool app "isActive") t))))))

(defvar *running-host* nil
  "The host of the window that RUN-VARIANT or OPEN-VARIANT has open, if there is one.")

(defun running-host ()
  "The host of the window that RUN-VARIANT or OPEN-VARIANT has open, or NIL. It is what a REPL has
to ask for a frame with, or to make the app again with (RELOAD-HOST), while the window is open."
  *running-host*)

(defun reload-host (host rebuild)
  "Call REBUILD, a function of no arguments that returns a variant, and put what it returns in
place of the variant HOST runs (SWAP-VARIANT), and ask for a frame. It is done on the main
thread, so this is safe from any thread, and from any other thread it returns at once. If
REBUILD signals an error the error is reported and the old variant stays."
  (run-on-main
   (lambda ()
     (handler-case
         (progn (swap-variant host (with-kept-state ((host-kept-state host))
                                     (funcall rebuild)))
                (request-host-frame host)
                (format t "~&Hot reload: made the app again.~%")
                (finish-output))
       (error (condition)
         (report-hot-reload "the app could not be made again, so the old one stays"
                            condition))))))

(defun make-variant-window (variant title width height capacity rebuild driver kept-state)
  "Make the window for VARIANT, show it, and return the host. Called inside the application, on
the main thread (RUN-VARIANT and OPEN-VARIANT see to that). The arguments are theirs."
  (let* ((host nil)
         (device (default-device))
         (window (make-window :title title :width width :height height
                              :event-handler (lambda (event)
                                               (host-deliver-event host event))))
         (view (make-metal-view :device device :width width :height height
                                :on-draw (lambda (view) (draw-view host view))
                                :event-handler (lambda (event)
                                                 (host-deliver-event host event))
                                :text-input (lambda ()
                                              (let* ((context (host-context host))
                                                     (rect (context-text-input-rect context)))
                                                (values (context-text-input-p context)
                                                        (float (aref rect 0) 1d0)
                                                        (float (aref rect 1) 1d0)
                                                        (float (aref rect 2) 1d0)
                                                        (float (aref rect 3) 1d0))))))
         (object (metal-view-object view)))
    (setf host (make-host variant device (msg-send :ulong object "colorPixelFormat")
                          :capacity capacity
                          :scale (window-scale-factor window)
                          :kept-state (or kept-state (make-kept-state))))
    (setf (host-window host) window
          (host-view host) view
          (host-acquire host) (view-acquirer object)
          (host-wake host) (lambda ()
                             (msg-send :void object "setNeedsDisplay:" (:bool t))))
    (setf *running-host* host)
    (set-content-view window object)
    (show-window window)
    (request-draw view)
    (when rebuild
      (setf (host-watcher host)
            (watch-redefinitions (lambda () (reload-host host rebuild)))))
    (when driver
      (sb-thread:make-thread
       (lambda ()
         (handler-case (funcall driver host)
           (error (condition)
             (format t "~&The driver failed: ~a~%" condition)
             (finish-output)
             (sb-ext:exit :code 1 :abort t))))
       :name "variant driver"))
    host))

(defun close-down (host)
  "What is left to do when the event loop of HOST's window has stopped: stop watching
definitions, and forget the host."
  (when host
    (let ((watcher (host-watcher host)))
      (when watcher
        (setf (host-watcher host) nil)
        (stop-watching-redefinitions watcher)))
    (when (eq *running-host* host)
      (setf *running-host* nil))))

(defun run-variant (variant &key (title "almighty-bolt") (width 800) (height 600)
                              driver (capacity 65536) rebuild kept-state)
  "Open a window of WIDTH by HEIGHT points, run VARIANT in it, and return only when
the application quits. This thread must be the main thread, and it is the window's until the
window closes, which ends the process: it is for a program that is only the window (make 7guis).
For a REPL, which must be free, see OPEN-VARIANT. DRIVER, if given, is called on a new thread with
the host once the window is up; a driver that finishes should close the window.

REBUILD, if given, is a function of no arguments that returns a variant. It is called again,
on the main thread, whenever functions are defined or redefined and the definitions have
stopped for a moment (WATCH-REDEFINITIONS), and what it returns takes the place of the
variant that runs (RELOAD-HOST). So code that is compiled in a running
image, from an editor or from a file, changes the window with nothing else done. The new
variant starts from nothing; if its first frame fails the old one is put back."
  (let ((host nil))
    (unwind-protect
         (run-application
          (lambda ()
            (setf host (make-variant-window variant title width height capacity rebuild driver
                                            kept-state))))
      (close-down host))))

(defun open-variant (variant &key (title "almighty-bolt") (width 800) (height 600)
                               (capacity 65536) rebuild kept-state (timeout 10))
  "Open a window of WIDTH by HEIGHT points that runs VARIANT, and return its host at once, with
the calling thread free: for a REPL, which goes on being one while the window is open. The
window's event loop runs on the main thread, which has to be doing nothing that cannot be
interrupted (a REPL in an editor that starts Lisp itself leaves it waiting for input, as one
does when it is the standard input), so this is called from another thread, as a REPL's
forms are in Slynk or SLIME with threads. Closing the window, with its button, Command-W or
Command-Q, or CLOSE-VARIANT, stops the loop and leaves the Lisp running; OPEN-VARIANT can then be
called again.

REBUILD is as for RUN-VARIANT, which is what compiling a definition in the editor needs: with
it the window changes when code is compiled. What it prints, and what the window's callbacks
print when they fail, goes to the output of the thread that called this.

Signals an error if this is the main thread, if a window is open, if the window could not be
made, or if the main thread had not started it in TIMEOUT seconds."
  (when (on-main-thread-p)
    (error "OPEN-VARIANT is called from the main thread, which it needs for the window's event ~
            loop and which would then never come back. Call it from another thread (a REPL in an ~
            editor is on one), or use RUN-VARIANT to give this thread to the window."))
  (when *running-host*
    (error "A window is already open. Close it first with CLOSE-VARIANT."))
  (let ((out *standard-output*)
        (err *error-output*)
        (ready (sb-thread:make-semaphore))
        (lock (sb-thread:make-mutex :name "open-variant"))
        (host nil)
        (failure nil)
        (cancelled nil))
    (sb-thread:interrupt-thread
     (sb-thread:main-thread)
     ;; An interruption runs with interrupts off, and this one is the window's event loop, which
     ;; goes on for as long as the window is open. With them off the Lisp cannot exit while a
     ;; window is open (Slynk's quit-lisp leaves the process hung), because EXIT unwinds the main
     ;; thread by interrupting it.
     (lambda ()
       (sb-sys:with-interrupts
         (unless (sb-thread:with-mutex (lock) cancelled)
           (let ((*standard-output* out)
                 (*error-output* err))
             (handler-case
                 (run-application
                  (lambda ()
                    (setf host (make-variant-window variant title width height capacity
                                                    rebuild nil kept-state))
                    (sb-thread:signal-semaphore ready))
                  :ends-process nil)
               (error (condition)
                 (setf failure condition)))
             (close-down host)
             (finish-output out)
             (finish-output err)
             ;; Setup failed before it said it was done.
             (unless host
               (sb-thread:signal-semaphore ready)))))))
    (unless (sb-thread:wait-on-semaphore ready :timeout timeout)
      (sb-thread:with-mutex (lock)
        (setf cancelled t))
      (error "The main thread did not open the window in ~d seconds. Is it busy?" timeout))
    (when failure
      (error "The window could not be opened: ~a" failure))
    host))

(defun close-variant (&key (timeout 5))
  "Close the window that OPEN-VARIANT opened, as its close button would, and return the host once
the event loop has stopped, or NIL if no window was open. Safe from any thread but the main one."
  (let ((host *running-host*))
    (when host
      (close-host-window host)
      (let ((end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
        (loop :while (eq *running-host* host)
              :do (when (> (get-internal-real-time) end)
                    (error "The window did not close in ~d seconds." timeout))
                  (sleep 0.01))))
    host))
