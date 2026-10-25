(defpackage #:almighty-bolt-core/platform/macos/host
  (:use #:cl)
  (:import-from #:almighty-bolt-core/clock
                #:now-ns)
  (:import-from #:almighty-bolt-core/ring
                #:make-frame-ring
                #:record-frame)
  (:import-from #:almighty-bolt-core/display-list
                #:make-display-list)
  (:import-from #:almighty-bolt-core/variant
                #:context
                #:context-width
                #:context-height
                #:context-scale
                #:context-frame-number
                #:context-requester
                #:context-frame-requested
                #:take-frame-request
                #:deliver-event
                #:context-font
                #:context-measure
                #:context-break-lines
                #:context-caret-x
                #:context-index-at-x
                #:context-caret-rect
                #:context-push-text)
  (:import-from #:almighty-bolt-core/settle
                #:settle-frame)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send)
  (:import-from #:almighty-bolt-core/platform/macos/main-queue
                #:on-main-thread-p)
  (:import-from #:almighty-bolt-core/platform/macos/app
                #:run-application)
  (:import-from #:almighty-bolt-core/platform/macos/window
                #:make-window
                #:set-content-view
                #:show-window
                #:close-window
                #:window-scale-factor)
  (:import-from #:almighty-bolt-core/platform/macos/view
                #:make-metal-view
                #:metal-view-object
                #:request-draw)
  (:import-from #:almighty-bolt-core/text/font
                #:make-font)
  (:import-from #:almighty-bolt-core/text/line
                #:get-line
                #:measure-text
                #:break-lines
                #:caret-x
                #:index-at-x
                #:caret-rect)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt-core/render/metal/renderer
                #:make-renderer
                #:renderer-atlas
                #:render-display-list
                #:wait-for-last-frame)
  (:import-from #:almighty-bolt-core/render/metal/draw-text
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
           #:run-frame
           #:host-deliver-event
           #:close-host-window
           #:run-variant))

(in-package #:almighty-bolt-core/platform/macos/host)

;;; The macOS host of decision record 0009: it owns the window, the view, the
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
  (variant nil :read-only t)
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
  ;; Returns the render pass and the drawable to draw into, as two values, or zero
  ;; for both if there is nothing to draw into just now.
  (acquire (lambda () (values 0 0)) :type function)
  ;; Asks the view for one more frame. It runs on the main thread and does not
  ;; allocate, which REQUEST-DRAW (it makes a closure to hand to the main thread)
  ;; does. NIL for a host with no view, which leaves requests as a flag for whoever
  ;; drives it.
  (wake nil :type (or null function)))

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
            ((on-main-thread-p) (funcall wake))
            (view (request-draw view))
            (t (setf (context-frame-requested (host-context host)) t))))))

(defun make-host (variant device pixel-format
                  &key (capacity 65536) (ring-capacity 4096) (scale 2))
  "A host for VARIANT drawing with DEVICE into targets of PIXEL-FORMAT, with no
window yet. CAPACITY is the most commands a frame may hold. SCALE is the pixels a
point that the glyph atlas is rasterized at, which is the scale text must be drawn
at. RUN-VARIANT makes one with a window; tests make one and call RUN-FRAME."
  (let* ((renderer (make-renderer device pixel-format :capacity capacity :scale scale))
         (context (make-mac-context :width 0f0 :height 0f0 :scale 1f0 :renderer renderer))
         (host (%make-host :variant variant
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
    (setf (host-drawing-p host) t)))

(defun stop-drawing (host)
  "End a frame. Returns true if a frame was requested while it was made and there is
a way to ask for another."
  (sb-thread:with-mutex ((host-lock host))
    (setf (host-drawing-p host) nil)
    (and (host-wake host)
         (take-frame-request (host-context host)))))

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
        (start (now-ns))
        (consed-before (the (unsigned-byte 64) (sb-ext:get-bytes-consed))))
    (declare (type (unsigned-byte 64) start consed-before))
    (start-drawing host)
    (unwind-protect
         (progn
           (setf (context-width context) width
                 (context-height context) height
                 (context-scale context) scale)
           (incf (context-frame-number context))
           (multiple-value-bind (builds capped)
               (settle-frame (host-variant host) context (host-display-list host))
             (let ((built (now-ns)))
               (wait-for-last-frame (host-renderer host))
               (multiple-value-bind (pass drawable) (funcall (host-acquire host))
                 (let ((acquired (now-ns)))
                   (unless (zerop pass)
                     (render-display-list (host-renderer host) (host-display-list host)
                                          pass drawable width height scale))
                   (let ((done (now-ns)))
                     (record-frame ring start done consed-before
                                   (the (unsigned-byte 64) (sb-ext:get-bytes-consed))
                                   (- built start) (- acquired built) (- done acquired)
                                   builds capped)))))))
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

(defun run-variant (variant &key (title "almighty-bolt") (width 800) (height 600)
                              driver (capacity 65536))
  "Open a window of WIDTH by HEIGHT points, run VARIANT in it, and return only when
the application quits. DRIVER, if given, is called on a new thread with the host
once the window is up; a driver that finishes should close the window."
  (let ((host nil))
    (run-application
     (lambda ()
       (let* ((device (default-device))
              (window (make-window :title title :width width :height height
                                   :event-handler (lambda (event)
                                                    (host-deliver-event host event))))
              (view (make-metal-view :device device :width width :height height
                                     :on-draw (lambda (view) (draw-view host view))
                                     :event-handler (lambda (event)
                                                      (host-deliver-event host event))))
              (object (metal-view-object view)))
         (setf host (make-host variant device (msg-send :ulong object "colorPixelFormat")
                               :capacity capacity
                               :scale (window-scale-factor window)))
         (setf (host-window host) window
               (host-view host) view
               (host-acquire host) (view-acquirer object)
               (host-wake host) (lambda ()
                                  (msg-send :void object "setNeedsDisplay:" (:bool t))))
         (set-content-view window object)
         (show-window window)
         (request-draw view)
         (when driver
           (sb-thread:make-thread
            (lambda ()
              (handler-case (funcall driver host)
                (error (condition)
                  (format t "~&The driver failed: ~a~%" condition)
                  (finish-output)
                  (sb-ext:exit :code 1 :abort t))))
            :name "variant driver")))))))
