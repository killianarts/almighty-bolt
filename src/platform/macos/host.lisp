(defpackage #:almighty-bolt/platform/macos/host
  (:use #:cl)
  (:local-nicknames (#:clock #:almighty-bolt/clock)
                    (#:event #:almighty-bolt/event)
                    (#:ring #:almighty-bolt/ring)
                    (#:dl #:almighty-bolt/display-list)
                    (#:kept #:almighty-bolt/kept)
                    (#:app-instance #:almighty-bolt/app-instance)
                    (#:settle #:almighty-bolt/settle)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:main-queue #:almighty-bolt/platform/macos/main-queue)
                    (#:redefinition #:almighty-bolt/redefinition)
                    (#:macos-app #:almighty-bolt/platform/macos/app)
                    (#:window #:almighty-bolt/platform/macos/window)
                    (#:a11y #:almighty-bolt/platform/macos/accessibility)
                    (#:view #:almighty-bolt/platform/macos/view)
                    (#:panel #:almighty-bolt/platform/macos/file-panel)
                    (#:pb #:almighty-bolt/platform/macos/pasteboard)
                    (#:font #:almighty-bolt/text/font)
                    (#:line #:almighty-bolt/text/line)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:renderer #:almighty-bolt/render/metal/renderer)
                    (#:draw #:almighty-bolt/render/metal/draw-text)
                    (#:atlas #:almighty-bolt/render/metal/glyph-atlas)
                    (#:image-atlas #:almighty-bolt/render/metal/image-atlas))
  (:export #:host
           #:make-host
           #:host-app
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
           #:wait-for-first-frame
           #:host-kept-state
           #:mac-context-pasteboard
           #:swap-app
           #:reload-host
           #:running-host
           #:request-host-frame
           #:run-frame
           #:host-deliver-event
           #:close-host-window
           #:host-keyboard-state
           #:run-host
           #:open-host
           #:close-host
           #:open-hosts
           #:host-open-p
           #:host-owner
           #:host-on-close
           #:request-all-frames))

(in-package #:almighty-bolt/platform/macos/host)

;;; The macOS host: it owns the window, the view, the
;;; renderer and the display list, gives an app its context, delivers events to
;;; it, and builds and draws a frame whenever the view draws.

;;; --- the context, with text from Core Text and the glyph atlas ---

(defstruct (mac-context (:include app-instance:context)
                        (:constructor make-mac-context
                            (&key width height scale requester renderer)))
  "A context whose text operations are answered by Core Text and the renderer's
glyph atlas. FONTS keeps the fonts made, by name and then by size. PASTEBOARD is the macOS
pasteboard that copy and paste use; with none (a host made without a window, as in tests), what is
copied is kept in the context and the person's clipboard is never touched."
  (renderer nil :read-only t)
  (fonts (make-hash-table :test 'equal) :read-only t)
  (pasteboard nil)
  ;; True for the context of a host with a window, which shows the system's file panels.
  (windowed nil))

(defmethod app-instance:context-clipboard-text ((context mac-context))
  (let ((pasteboard (mac-context-pasteboard context)))
    (if pasteboard
        (pb:pasteboard-string pasteboard)
        (call-next-method))))

(defmethod app-instance:context-choose-file ((context mac-context) kind types name callback)
  ;; The system's panel, for a context with a window; after the frame, since the panel runs a loop
  ;; of its own.
  (if (mac-context-windowed context)
      (main-queue:run-on-main-in-a-turn (lambda ()
                                      (funcall callback (panel:run-file-panel (panel:make-file-panel kind :types types
                                                                                                          :name name)))))
      (call-next-method)))

(defmethod app-instance:context-set-clipboard-text ((context mac-context) string)
  (let ((pasteboard (mac-context-pasteboard context)))
    (if pasteboard
        (pb:set-pasteboard-string pasteboard string)
        (call-next-method))))

(defmethod app-instance:context-font ((context mac-context) name size &optional weight tracking)
  (let ((fonts (mac-context-fonts context))
        (weight (or weight :regular))
        (tracking (or tracking 0)))
    (multiple-value-bind (entries present) (gethash name fonts)
      ;; By name, then by size, weight (as given: :bold and 700 are kept apart, which costs a font)
      ;; and tracking, so that asking again allocates nothing.
      (or (loop :for (key . font) :in entries
                :when (and (= (first key) size) (eql (second key) weight) (= (third key) tracking))
                  :return font)
          (let ((font (font:make-font name size weight tracking)))
            ;; The caller's string may change later, so keep a copy as the key.
            (setf (gethash (if present name (copy-seq name)) fonts)
                  (acons (list size weight tracking) font entries))
            font)))))

(defmethod app-instance:context-measure ((context mac-context) font string)
  (line:measure-text font string))

(defmethod app-instance:context-break-lines ((context mac-context) font string width)
  (line:break-lines font string width))

(defmethod app-instance:context-caret-x ((context mac-context) font string index)
  (line:caret-x (line:get-line font string) index))

(defmethod app-instance:context-index-at-x ((context mac-context) font string x)
  (line:index-at-x (line:get-line font string) x))

(defmethod app-instance:context-caret-rect ((context mac-context) font string index)
  (line:caret-rect (line:get-line font string) index))

(defmethod app-instance:context-forget-glyphs ((context mac-context))
  ;; The frame before may still be drawing from the atlas.
  (let ((renderer (mac-context-renderer context)))
    (renderer:wait-for-last-frame renderer)
    (atlas:clear-glyph-atlas (renderer:renderer-atlas renderer))
    (image-atlas:clear-image-atlas (renderer:renderer-images renderer))))

(defmethod app-instance:context-push-text ((context mac-context) display-list font string x baseline color
                                           &optional turn)
  (draw:push-line display-list (renderer:renderer-atlas (mac-context-renderer context))
                  (line:get-line font string) (float x 1f0) (float baseline 1f0) color turn))

(defmethod app-instance:context-push-image ((context mac-context) display-list image x y width height
                                            &key tint (opacity 1))
  ;; The image's top left is put on a whole pixel, as a glyph's is, so that one drawn at its own
  ;; size is its pixels one for one and not filtered soft.
  (let* ((renderer (mac-context-renderer context))
         (scale (atlas:glyph-atlas-scale (renderer:renderer-atlas renderer))))
    (dl:push-image display-list (/ (fround (* x scale)) scale) (/ (fround (* y scale)) scale)
                   width height (image-atlas:atlas-image-index (renderer:renderer-images renderer) image)
                   (or tint (dl:pack-color 255 255 255 (round (* 255 (max 0 (min 1 opacity))))))
                   (and tint t))))

;;; --- the host ---

(defstruct (host (:constructor %make-host))
  ;; The app the host builds. A replay that draws sets it for each run, so that one host's
  ;; renderer and glyph atlas stay warm from run to run.
  (app nil)
  ;; The app that SWAP-APP replaced, until the one that replaced it has made a frame
  ;; without an error. If that frame fails, this is the one that is put back.
  (previous nil)
  ;; What the app keeps between the times it is made (STATE-LET), which is bound while the app is
  ;; made again and so outlives the app.
  (kept-state (kept:make-kept-state) :read-only t)
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
  (idle-queue (sb-thread:make-waitqueue :name "host idle"))
  ;; True once a frame has been made: until then the window has no size, and a host that has
  ;; done nothing yet is idle without having shown anything.
  (drawn nil :type boolean)
  ;; The host whose window this one's belongs to (a tool's window belongs to the app's it looks at),
  ;; which it closes with; or NIL.
  (owner nil)
  ;; Called with no arguments, on the main thread, when the window closes, or NIL.
  (on-close nil))

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
             (setf (app-instance:context-frame-requested (host-context host)) t))
            ((main-queue:on-main-thread-p) (setf (host-draw-pending host) t) (funcall wake))
            (view (setf (host-draw-pending host) t) (view:request-draw view))
            (t (setf (app-instance:context-frame-requested (host-context host)) t))))))

(defun make-host (instance device pixel-format
                  &key (capacity 65536) (ring-capacity 4096) (scale 2) (atlas-size 1024)
                    (image-atlas-size 2048) phases
                    (kept-state (kept:make-kept-state)))
  "A host for INSTANCE drawing with DEVICE into targets of PIXEL-FORMAT, with no
window yet. CAPACITY is the most commands a frame may hold. SCALE is the pixels a
point that the glyph atlas is rasterized at, which is the scale text must be drawn
at, and ATLAS-SIZE the pixels square the atlas is; IMAGE-ATLAS-SIZE is the pixels square the image
atlas is. With PHASES, the instrumentation is on: the app's update, layout and paint
times, the first build, garbage collection and the GPU's time go into the ring as well
(see WITH-PHASE). KEPT-STATE is what the app keeps when it is made again (STATE-LET); the one that
made INSTANCE had it bound. RUN-HOST makes one with a window; tests make one and call RUN-FRAME."
  (let* ((renderer (renderer:make-renderer device pixel-format :capacity capacity :scale scale
                                             :atlas-size atlas-size
                                             :image-atlas-size image-atlas-size))
         (context (let ((context (make-mac-context :width 0f0 :height 0f0 :scale 1f0
                                                    :renderer renderer)))
                    (setf (app-instance:context-phases-p context) (and phases t))
                    context))
         (host (%make-host :app instance
                           :kept-state kept-state
                           :context context
                           :renderer renderer
                           :display-list (dl:make-display-list :capacity capacity)
                           :ring (ring:make-frame-ring :capacity ring-capacity))))
    (setf (app-instance:context-requester context)
          (lambda (context)
            (declare (ignore context))
            (request-host-frame host))
          (app-instance:context-window context) host)
    host))

(defun host-deliver-event (host event)
  "Offer EVENT to the host's app, requesting a frame if it wants one. Called on
the main thread, once for each event, in order."
  (when host
    (app-instance:deliver-event (host-app host) (host-context host) event)))

(defun click-at (host x y)
  "Press and release the primary button at X, Y (points from the top left of the window), the
pointer having moved there first: the events that a person's hand makes. Called on the main thread;
the window answers on the frames that follow."
  (flet ((pointer (kind button buttons count)
           (host-deliver-event host (event:make-pointer-event :timestamp (clock:now-ns) :kind kind
                                                              :x (float x 1d0) :y (float y 1d0)
                                                              :button button :buttons buttons
                                                              :click-count count))))
    (pointer :move nil 0 0)
    (pointer :down 0 1 1)
    (pointer :up 0 0 1)))

(defun secondary-click-at (host x y)
  "Press and release the secondary button at X, Y, as CLICK-AT does the primary: what opens a context
menu there."
  (flet ((pointer (kind button buttons count)
           (host-deliver-event host (event:make-pointer-event :timestamp (clock:now-ns) :kind kind
                                                              :x (float x 1d0) :y (float y 1d0)
                                                              :button button :buttons buttons
                                                              :click-count count))))
    (pointer :move nil 0 0)
    (pointer :down 1 2 1)
    (pointer :up 1 0 1)))

(defun start-drawing (host)
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-drawing-p host) t
          (host-draw-pending host) nil)))

(defun stop-drawing (host)
  "End a frame. Returns true if a frame was requested while it was made and there is
a way to ask for another."
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-drawing-p host) nil
          (host-drawn host) t)
    (let ((again (and (host-wake host)
                      (app-instance:take-frame-request (host-context host)))))
      (when again
        ;; The caller wakes the view next, outside the lock.
        (setf (host-draw-pending host) t))
      ;; Every waiter is woken at the end of a frame, and looks again at what it waits for: idle, or
      ;; (WAIT-FOR-FIRST-FRAME) a frame at all, which an animation that never rests also makes.
      (when (plusp (host-waiters host))
        (sb-thread:condition-broadcast (host-idle-queue host)))
      again)))

(defun idle-p (host)
  (and (not (host-drawing-p host))
       (not (host-draw-pending host))
       (not (app-instance:context-frame-requested (host-context host)))))

(defun host-idle-p (host)
  "True if no frame is being made and none is wanted: what the app was asked is answered on the
screen."
  (sb-thread:with-mutex ((host-lock host))
    (idle-p host)))

(defun wait-until (host test timeout)
  "Wait until TEST, called with HOST's lock held, is true, at most TIMEOUT seconds; the end of each
frame wakes the waiter to look again. Returns what TEST last returned."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (sb-thread:with-mutex ((host-lock host))
      (incf (host-waiters host))
      (unwind-protect
           (loop
             (when (funcall test)
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
                   (return (funcall test))))))
        (decf (host-waiters host))))))

(defun wait-for-first-frame (host &key (timeout 5))
  "Wait until HOST has made a frame (and so the window has its size), at most TIMEOUT seconds.
Returns true if it has."
  (wait-until host (lambda () (host-drawn host)) timeout))

(defun wait-for-idle (host &key (timeout 5))
  "Wait until HOST-IDLE-P, at most TIMEOUT seconds. Returns true if it is idle. The frame that
ends wakes the waiter; nothing polls, and a window that nobody waits for pays a read of one number a
frame."
  (wait-until host (lambda () (idle-p host)) timeout))

(defun report-hot-reload (what condition)
  "Say on the error output that WHAT happened, and why. Printed without the pretty printer, which
breaks the lines of an error's text at a narrow margin."
  (let ((*print-pretty* nil))
    (format *error-output* "~&Hot reload: ~a: ~a~%" what condition))
  (finish-output *error-output*))

(defun retire (instance)
  "RETIRE-APP, for an app that is no longer used. A failure to let go of it is reported and
does not stop what is going on."
  (handler-case (app-instance:retire-app instance)
    (error (condition)
      (report-hot-reload "an app that is no longer used could not be let go of" condition))))

(defun swap-app (host instance)
  "Make HOST run INSTANCE from the next frame, in place of the one it runs. The window, the
renderer and the glyph atlas stay, and the new app is given what the old one kept that is not
the app's own (INHERIT-STATE): the focus, the carets, the scrolls. If that signals an error, the
old one stays and the error is signalled again. If the new app's first frame signals an error,
the old one is put back and the error is reported (SETTLE-HOST), so that code that does not work
yet does not take the window with it. Call it on the thread that draws, the main thread, and ask
for a frame after (RELOAD-HOST does both)."
  (let ((old (or (host-previous host) (host-app host))))
    (handler-case (app-instance:inherit-state instance old)
      (error (condition)
        (retire instance)
        (error condition)))
    ;; An app that was swapped in and has not drawn yet is replaced by this one, and goes.
    (when (host-previous host)
      (retire (host-app host)))
    (setf (host-previous host) old
          (host-app host) instance))
  instance)

(defun settle-host (host context)
  "SETTLE-FRAME with HOST's app. An app that SWAP-APP has just put in has its first
frame made with the old one ready: if it signals an error, the old one is put back, the
error is reported, and the frame is made again with that one. Whichever of the two is not used
after that is retired."
  (let ((previous (host-previous host))
        (display-list (host-display-list host)))
    (cond ((null previous)
           ;; A frame of the app that fails shows the last one that did not, with the error over it:
           ;; the window answers still, and a press on what was there can put the app right.
           (let ((app-instance:*show-frame-errors* t))
             (settle:settle-frame (host-app host) context display-list)))
          (t
           (setf (host-previous host) nil)
           (let ((builds nil) (capped nil) (failure nil))
             (handler-case (multiple-value-setq (builds capped)
                             (settle:settle-frame (host-app host) context display-list))
               (error (condition)
                 (setf failure condition)))
             (cond ((null failure)
                    (retire previous)
                    (values builds capped))
                   (t
                    (report-hot-reload "the new app failed in its first frame, so the old one is back"
                                       failure)
                    (let ((failed (host-app host)))
                      (setf (host-app host) previous)
                      (retire failed))
                    (settle:settle-frame previous context display-list))))))))

(defun run-frame (host width height scale)
  "Make and draw one frame: the size and scale go into the context, the app
builds until the frame settles, the drawable is asked for, and the frame is
encoded, committed and presented. The time of each part goes into the ring: the
build, the wait for the display (the GPU finishing the frame before, and then
getting the drawable) and the encoding. A frame requested while this one was made
is asked for after it is presented, with the host's wake function, which does not
allocate. WIDTH and HEIGHT are in points. Does nothing past the build if there is
no drawable. Must run inside an autorelease pool. Returns how many times the app built the
frame: more than once if what it was given (a press, a key) changed what the app holds."
  (declare (type single-float width height scale))
  (let ((context (host-context host))
        (ring (host-ring host))
        (again nil)
        (start (clock:real-time-source))
        (consed-before (the (unsigned-byte 64) (sb-ext:get-bytes-consed)))
        (gc-before sb-ext:*gc-run-time*)
        (previous-gpu-ns 0)
        (made 0))
    (declare (type (unsigned-byte 64) start consed-before previous-gpu-ns)
             (type fixnum gc-before))
    (when (app-instance:context-phases-p context)
      (app-instance:reset-phases context))
    (start-drawing host)
    (unwind-protect
         (progn
           (setf (app-instance:context-width context) width
                 (app-instance:context-height context) height
                 (app-instance:context-scale context) scale)
           (incf (app-instance:context-frame-number context))
           (multiple-value-bind (builds capped)
               (settle-host host context)
             (setf made builds)
             ;; Nothing takes text now: a composition that was going on is over.
             (when (and (host-view host) (not (app-instance:context-text-input-p context)))
               (view:end-composition (host-view host)))
             (let ((built (clock:real-time-source)))
               (renderer:wait-for-last-frame (host-renderer host))
               ;; The frame before has finished, and its command buffer says how long it took.
               (when (app-instance:context-phases-p context)
                 (setf previous-gpu-ns (renderer:last-frame-gpu-ns (host-renderer host))))
               (multiple-value-bind (pass drawable) (funcall (host-acquire host))
                 (let ((acquired (clock:real-time-source)))
                   (unless (zerop pass)
                     (renderer:render-display-list (host-renderer host) (host-display-list host)
                                                   pass drawable width height scale))
                   (let ((done (clock:real-time-source)))
                     (ring:record-frame ring start done consed-before
                                        (the (unsigned-byte 64) (sb-ext:get-bytes-consed))
                                        (- built start) (- acquired built) (- done acquired)
                                        builds capped)
                     (when (app-instance:context-phases-p context)
                       (let ((slots (app-instance:context-phase-ns context)))
                         (multiple-value-bind (events-ns events) (app-instance:take-events context)
                           (ring:record-frame-details
                            ring
                            :gc-ns (* (max 0 (- sb-ext:*gc-run-time* gc-before))
                                      #.(floor 1000000000 internal-time-units-per-second))
                            :first-build-ns (aref slots app-instance:+phase-first-build+)
                            :update-ns (aref slots app-instance:+phase-update+)
                            :layout-ns (aref slots app-instance:+phase-layout+)
                            :paint-ns (aref slots app-instance:+phase-paint+)
                            :all-builds-ns (aref slots app-instance:+phase-all-builds+)
                            :gpu-ns previous-gpu-ns
                            :events-ns events-ns
                            :events events))))))))))
      (setf again (stop-drawing host)))
    (when again
      (funcall (the function (host-wake host))))
    made))

;;; --- a window ---

(defun view-acquirer (object)
  "A function that asks the MTKView OBJECT for its render pass and drawable."
  (lambda ()
    (let ((pass (objc:msg-send :id object "currentRenderPassDescriptor"))
          (drawable (objc:msg-send :id object "currentDrawable")))
      (if (or (zerop pass) (zerop drawable))
          (values 0 0)
          (values pass drawable)))))

;;; --- the windows that are open ---
;;;
;;; An app can have more than one window, each with a host of its own: its own app, made by its own
;;; constructor, with its own STATE-LET, view, renderer and glyph atlas. They share the main thread
;;; and AppKit's one event loop, which the first window starts and the last one, closing, stops.
;;;
;;; What one window's app holds may be what another shows (a preferences window and the window whose
;;; preferences they are), and an immediate-mode window draws only when it is asked. So a frame in
;;; which a window took input (a press, a key, the wheel: it built more than once) asks every other
;;; window for a frame. That frame takes no input, builds once, and asks nothing of the others, so
;;; windows do not wake each other for ever.

(defvar *hosts-lock* (sb-thread:make-mutex :name "open windows")
  "Guards *HOSTS*, *LOOP* and *PENDING-OPENS*.")

(defvar *hosts* '()
  "The hosts whose windows are open, in the order they were opened.")

(defvar *loop* nil
  "The state of the application's event loop: NIL before it starts and after it stops, :STARTING
while the main thread is being asked to run it, :RUNNING, and :STOPPING once the last window has
closed and it is about to return.")

(defvar *pending-opens* 0
  "How many windows other threads have asked the running loop to open, and it has not opened yet.
While there are any, the loop does not stop when the last window closes.")

(defun open-hosts ()
  "The hosts whose windows are open, in the order they were opened."
  (sb-thread:with-mutex (*hosts-lock*)
    (copy-list *hosts*)))

(defun host-open-p (host)
  "True while HOST's window is open."
  (sb-thread:with-mutex (*hosts-lock*)
    (and (member host *hosts*) t)))

(defun request-all-frames ()
  "Ask every open window for a frame: what they all show has changed (a theme was defined again)."
  (dolist (host (open-hosts))
    (request-host-frame host)))

(defun may-stop-p ()
  "Whether the loop may stop now that the last window has closed (see *MAY-STOP-P*): not while a
window is on its way. If it may, it is marked as stopping, so that no other thread gives it work."
  (sb-thread:with-mutex (*hosts-lock*)
    (cond ((plusp *pending-opens*) nil)
          (t (setf *loop* :stopping)
             t))))

(setf macos-app:*may-stop-p* #'may-stop-p)

(defun draw-view (host view)
  "The view's draw callback: read its size and scale, and make a frame. A frame that took input asks
the other windows for one (see above)."
  (when host
    (let ((object (view:metal-view-object view)))
      (multiple-value-bind (x y width height) (objc:msg-send :rect object "bounds")
        (declare (ignore x y))
        (let* ((pixel-width (objc:msg-send :size object "drawableSize"))
               (builds (run-frame host
                                  (float width 1f0) (float height 1f0)
                                  (if (plusp width) (float (/ pixel-width width) 1f0) 1f0))))
          ;; The pointer takes the shape the frame said, for what is under it.
          (view:set-view-cursor view (app-instance:context-cursor (host-context host)))
          ;; The menu bar has the menus of the window that has the keyboard.
          (when (objc:msg-send :bool (window:window-object (host-window host)) "isKeyWindow")
            (macos-app:set-app-menus (app-instance:app-menus (host-app host))))
          ;; What a screen reader reads is out of date now, and is made when it is asked for.
          (let ((bridge (view:metal-view-bridge view)))
            (when bridge
              (a11y:frame-presented bridge)))
          (when (> builds 1)
            (dolist (other (open-hosts))
              (unless (eq other host)
                (request-host-frame other)))))))))

(defun close-host-window (host)
  "Close the host's window as the user would. Safe from any thread."
  (window:close-window (host-window host)))

(defun host-keyboard-state (host)
  "Where a key would go, as three values that are true or false: whether the host's view is the
first responder of its window, whether the window is the key window, and whether the application
is the active one. A person can type into the app only when all three are true. Safe from any
thread."
  (main-queue:call-on-main
   (lambda ()
     (let ((window (window:window-object (host-window host)))
           (view (view:metal-view-object (host-view host)))
           (app (objc:msg-send :id (objc:objc-class "NSApplication") "sharedApplication")))
       (values (eql (objc:msg-send :id window "firstResponder") view)
               (and (objc:msg-send :bool window "isKeyWindow") t)
               (and (objc:msg-send :bool app "isActive") t))))))

(defvar *running-host* nil
  "The host of the app's window that RUN-HOST or OPEN-HOST opened, while it is open.")

(defun running-host ()
  "The host of the app's window that RUN-HOST or OPEN-HOST opened (the first, not one that the
app or a tool opened beside it), or NIL. It is what a REPL has to ask for a frame with, or to make
the app again with (RELOAD-HOST), while the window is open."
  *running-host*)

(defun reload-host (host rebuild)
  "Call REBUILD, a function of no arguments that returns an app, and put what it returns in
place of the app HOST runs (SWAP-APP), and ask for a frame. It is done on the main
thread, so this is safe from any thread, and from any other thread it returns at once. If
REBUILD signals an error the error is reported and the old app stays."
  (main-queue:run-on-main
   (lambda ()
     (handler-case
         (progn (swap-app host (kept:with-kept-state ((host-kept-state host))
                                     (funcall rebuild)))
                (request-host-frame host)
                (format t "~&Hot reload: made the app again.~%")
                (finish-output))
       (error (condition)
         (report-hot-reload "the app could not be made again, so the old one stays"
                            condition))))))

(defun forget-host (host)
  "What is left to do when HOST's window has closed: stop watching definitions, let the screen
reader go, and take it from the windows that are open."
  (let ((watcher (host-watcher host)))
    (when watcher
      (setf (host-watcher host) nil)
      (redefinition:stop-watching-redefinitions watcher)))
  (let* ((view (host-view host))
         (bridge (and view (view:metal-view-bridge view))))
    (when bridge
      (setf (view:metal-view-bridge view) nil)
      (a11y:release-bridge bridge)))
  (sb-thread:with-mutex (*hosts-lock*)
    (setf *hosts* (remove host *hosts*))
    (when (eq *running-host* host)
      (setf *running-host* nil))))

(defun window-closing (host)
  "HOST's window is closing: the windows that belong to it close first, and then it is forgotten."
  (dolist (other (open-hosts))
    (when (eq (host-owner other) host)
      (objc:msg-send :void (window:window-object (host-window other)) "close")))
  (forget-host host)
  (let ((on-close (host-on-close host)))
    (when on-close
      (funcall on-close))))

(defun remove-key (plist key)
  (loop :for (k v) :on plist :by #'cddr
        :unless (eq k key) :append (list k v)))

(defun make-app-window (instance title width height capacity rebuild driver kept-state
                            &key owner main (show t) placement)
  "Make the window for INSTANCE and return its host, shown at once with SHOW, or else after what
the main thread is doing now (a frame, in which the window is asked for). Called inside the
application, on the main thread. OWNER is the host the window belongs to and closes with, and with
MAIN it is the app's window (RUNNING-HOST). PLACEMENT says where it opens (see PLACE-WINDOW; a host
given as :BESIDE stands for its window); with none, it opens below and to the right of the last
window opened, or in the middle of the screen if it is the first. The other arguments are
RUN-HOST's."
  (let* ((host nil)
         (device (device:default-device))
         (window (window:make-window :title title :width width :height height
                                     :event-handler (lambda (event)
                                                      (host-deliver-event host event))
                                     :on-close (lambda () (window-closing host))))
         (view (view:make-metal-view :device device :width width :height height
                                     ;; Read when asked, from the app the host has then: the app
                                     ;; may have been made again.
                                     :accessibility-tree (lambda ()
                                                           (app-instance:accessibility-tree (host-app host)))
                                     :accessibility-press (lambda (x y) (click-at host x y))
                                     :accessibility-secondary-press
                                     (lambda (x y) (secondary-click-at host x y))
                                     :on-draw (lambda (view) (draw-view host view))
                                     ;; The system went dark or light: the window's theme follows.
                                     :on-appearance (lambda (view)
                                                      (setf (app-instance:context-appearance (host-context host))
                                                            (view:view-appearance (view:metal-view-object view)))
                                                      (request-host-frame host))
                                     :event-handler (lambda (event)
                                                      (host-deliver-event host event))
                                     :text-input (lambda ()
                                                   (let* ((context (host-context host))
                                                          (rect (app-instance:context-text-input-rect context)))
                                                     (values (app-instance:context-text-input-p context)
                                                             (float (aref rect 0) 1d0)
                                                             (float (aref rect 1) 1d0)
                                                             (float (aref rect 2) 1d0)
                                                             (float (aref rect 3) 1d0))))))
         (object (view:metal-view-object view)))
    (setf host (make-host instance device (objc:msg-send :ulong object "colorPixelFormat")
                          :capacity capacity
                          :scale (window:window-scale-factor window)
                          :kept-state (or kept-state (kept:make-kept-state))))
    (setf (mac-context-pasteboard (host-context host)) (pb:general-pasteboard)
          (mac-context-windowed (host-context host)) t
          (app-instance:context-appearance (host-context host)) (view:view-appearance object))
    (setf (host-window host) window
          (host-view host) view
          (host-owner host) owner
          (host-acquire host) (view-acquirer object)
          (host-wake host) (lambda ()
                             (objc:msg-send :void object "setNeedsDisplay:" (:bool t))))
    (let ((beside (getf placement :beside))
          (last (car (last (open-hosts)))))
      (setf placement
            (cond (beside (list* :beside (host-window beside) (remove-key placement :beside)))
                  ((or placement (null last)) placement)
                  (t (list :cascade (host-window last))))))
    (sb-thread:with-mutex (*hosts-lock*)
      (setf *hosts* (append *hosts* (list host)))
      (when main
        (setf *running-host* host)))
    (window:set-content-view window object)
    (flet ((show ()
             (when (host-open-p host)
               (window:show-window window placement)
               (view:request-draw view))))
      ;; A window asked for in a frame is shown after it: it takes the keyboard from the window
      ;; whose frame it is, which must not be told of that while it is building.
      (if show (show) (main-queue:run-on-main-later #'show)))
    (when rebuild
      (setf (host-watcher host)
            (redefinition:watch-redefinitions (lambda () (reload-host host rebuild)))))
    (when driver
      (sb-thread:make-thread
       (lambda ()
         (handler-case (funcall driver host)
           (error (condition)
             (format t "~&The driver failed: ~a~%" condition)
             (finish-output)
             (sb-ext:exit :code 1 :abort t))))
       :name "app driver"))
    host))

(defun loop-ended ()
  "The event loop has returned: forget every window that is left, and say the loop is not running."
  (dolist (host (open-hosts))
    (forget-host host))
  (sb-thread:with-mutex (*hosts-lock*)
    (setf *loop* nil
          *pending-opens* 0)))

(defun run-host (instance &key (title "almighty-bolt") (width 800) (height 600)
                              driver (capacity 65536) rebuild kept-state placement)
  "Open a window of WIDTH by HEIGHT points, run INSTANCE in it, and return only when
the application quits. This thread must be the main thread, and it is the windows' until the
last of them closes, which ends the process: it is for a program that is only its windows (make
7guis). For a REPL, which must be free, see OPEN-HOST. DRIVER, if given, is called on a new thread
with the host once the window is up; a driver that finishes should close the window.

REBUILD, if given, is a function of no arguments that returns an app. It is called again,
on the main thread, whenever functions are defined or redefined and the definitions have
stopped for a moment (WATCH-REDEFINITIONS), and what it returns takes the place of the
app that runs (RELOAD-HOST). So code that is compiled in a running
image, from an editor or from a file, changes the window with nothing else done. The new
app starts from nothing; if its first frame fails the old one is put back."
  (sb-thread:with-mutex (*hosts-lock*)
    (when *loop*
      (error "The application is running already; open another window with OPEN-HOST."))
    (setf *loop* :starting))
  (unwind-protect
       (macos-app:run-application
        (lambda ()
          (sb-thread:with-mutex (*hosts-lock*)
            (setf *loop* :running))
          (make-app-window instance title width height capacity rebuild driver kept-state
                               :main t :placement placement)))
    (loop-ended)))

(defun open-host (instance &key (title "almighty-bolt") (width 800) (height 600)
                               (capacity 65536) rebuild kept-state (timeout 10) owner (main t)
                               placement)
  "Open a window of WIDTH by HEIGHT points that runs INSTANCE, and return its host. With MAIN it is
the app's window (RUNNING-HOST), of which there is one at a time; without it, another window beside
it, which closes with OWNER if one is given. PLACEMENT says where it opens (see PLACE-WINDOW).

The first window starts the application's event loop on the main thread and returns at once, with
the calling thread free: for a REPL, which goes on being one while the window is open. The main
thread has to be doing nothing that cannot be interrupted (a REPL in an editor that starts Lisp
itself leaves it waiting for input, as one does when it is the standard input), so the first window
is opened from another thread, as a REPL's forms are in Slynk or SLIME with threads. A window opened
while the loop runs (from the REPL, or from an app's frame, on the main thread) joins it. Closing the
last window, with its button, Command-W, Command-Q (which closes them all) or CLOSE-HOST, stops the
loop and leaves the Lisp running; OPEN-HOST can then be called again.

REBUILD is as for RUN-HOST, which is what compiling a definition in the editor needs: with
it the window changes when code is compiled. What it prints, and what the window's callbacks
print when they fail, goes to the output of the thread that started the loop.

Signals an error if MAIN and the app's window is open, if the loop has to be started and this is the
main thread, if the window could not be made, or if the main thread had not made it in TIMEOUT
seconds."
  (when (and main *running-host*)
    (error "A window is already open. Close it first with CLOSE-HOST."))
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second)))
        (state nil))
    ;; A loop that is starting or stopping is waited for: then it runs, or it has stopped.
    (loop
      (setf state (sb-thread:with-mutex (*hosts-lock*)
                    (case *loop*
                      (:running (incf *pending-opens*) :running)
                      ((nil) (setf *loop* :starting) :start)
                      (t :wait))))
      (unless (eq state :wait)
        (return))
      (when (> (get-internal-real-time) deadline)
        (error "The application's event loop did not start or stop in ~d seconds." timeout))
      (sleep 0.01))
    (if (eq state :running)
        (flet ((make (show)
                 (unwind-protect
                      (make-app-window instance title width height capacity rebuild nil
                                           kept-state :owner owner :main main :show show
                                           :placement placement)
                   (sb-thread:with-mutex (*hosts-lock*)
                     (decf *pending-opens*)))))
          (if (main-queue:on-main-thread-p)
              (make nil)
              (main-queue:call-on-main (lambda () (make t)))))
        (start-loop instance title width height capacity rebuild kept-state timeout main
                    placement))))

(defun start-loop (instance title width height capacity rebuild kept-state timeout main placement)
  "Start the application's event loop on the main thread, with a window for INSTANCE, and return its
host (see OPEN-HOST, which has said that the loop is :STARTING)."
  (when (main-queue:on-main-thread-p)
    (sb-thread:with-mutex (*hosts-lock*)
      (setf *loop* nil))
    (error "OPEN-HOST is called from the main thread, which it needs for the window's event ~
            loop and which would then never come back. Call it from another thread (a REPL in an ~
            editor is on one), or use RUN-HOST to give this thread to the window."))
  (let ((out *standard-output*)
        (err *error-output*)
        (ready (sb-thread:make-semaphore))
        (lock (sb-thread:make-mutex :name "open-host"))
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
         (if (sb-thread:with-mutex (lock) cancelled)
             (loop-ended)
             (let ((*standard-output* out)
                   (*error-output* err))
               (handler-case
                   (macos-app:run-application
                    (lambda ()
                      (sb-thread:with-mutex (*hosts-lock*)
                        (setf *loop* :running))
                      (setf host (make-app-window instance title width height capacity
                                                      rebuild nil kept-state :main main
                                                      :placement placement))
                      (sb-thread:signal-semaphore ready))
                    :ends-process nil)
                 (error (condition)
                   (setf failure condition)))
               (loop-ended)
               (finish-output out)
               (finish-output err)
               ;; Setup failed before it said it was done.
               (unless host
                 (sb-thread:signal-semaphore ready)))))))
    (unless (sb-thread:wait-on-semaphore ready :timeout timeout)
      (sb-thread:with-mutex (lock)
        (setf cancelled t))
      (sb-thread:with-mutex (*hosts-lock*)
        (setf *loop* nil))
      (error "The main thread did not open the window in ~d seconds. Is it busy?" timeout))
    (when failure
      (error "The window could not be opened: ~a" failure))
    host))

(defun close-host (&key (host *running-host*) (timeout 5))
  "Close HOST's window (the app's window that OPEN-HOST opened, if not given), as its close button
would, and return the host once it has closed, or NIL if it was not open. Safe from any thread but
the main one."
  (when (and host (host-open-p host))
    (close-host-window host)
    (let ((end (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
      (loop :while (host-open-p host)
            :do (when (> (get-internal-real-time) end)
                  (error "The window did not close in ~d seconds." timeout))
                (sleep 0.01)))
    host))
