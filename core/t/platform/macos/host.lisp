(defpackage #:almighty-bolt-core/t/platform/macos/host
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt-core/platform/macos/objc)
                    (#:host #:almighty-bolt-core/platform/macos/host)
                    (#:main-queue #:almighty-bolt-core/platform/macos/main-queue)
                    (#:device #:almighty-bolt-core/render/metal/device)
                    (#:target #:almighty-bolt-core/render/metal/target)
                    (#:renderer #:almighty-bolt-core/render/metal/renderer)
                    (#:variant #:almighty-bolt-core/variant)
                    (#:settle #:almighty-bolt-core/settle)
                    (#:ring #:almighty-bolt-core/ring)
                    (#:event #:almighty-bolt-core/event)
                    (#:dl #:almighty-bolt-core/display-list)))

(in-package #:almighty-bolt-core/t/platform/macos/host)

;;; These run the macOS host without a window. The host draws into an off-screen
;;; texture instead of a view's drawable, so they need a Mac with a GPU. A window
;;; and the events AppKit delivers are checked by running the null variant.

(defmacro with-host ((host target variant &key (width 64) (height 48) (scale 2)
                                            (capacity 1024) phases)
                     &body body)
  "Run BODY with HOST, a host for VARIANT, drawing into TARGET, an off-screen texture
WIDTH by HEIGHT points at SCALE pixels a point."
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let* ((device (device:default-device))
              (,host (host:make-host ,variant device target:+pixel-format-bgra8-unorm+
                                     :capacity ,capacity :scale ,scale
                                     :phases ,phases))
              (,target (target:make-offscreen-target device (* ,width ,scale)
                                                     (* ,height ,scale))))
         (let ((pass (target:offscreen-target-descriptor ,target)))
           (setf (host:host-acquire ,host) (lambda () (values pass 0))))
         ,@body))))

(defun draw (host &key (width 64f0) (height 48f0) (scale 2f0))
  (host:run-frame host width height scale)
  (renderer:wait-for-last-frame (host:host-renderer host)))

(defun pixel (pixels width x y)
  "The (red green blue alpha) of the pixel at X, Y."
  (let ((i (* 4 (+ (* y width) x))))
    (list (aref pixels (+ i 2)) (aref pixels (+ i 1)) (aref pixels i) (aref pixels (+ i 3)))))

;;; --- variants to run ---

(defclass shape-variant (variant:variant) ())

(defmethod variant:frame ((variant shape-variant) context list)
  (declare (ignore context))
  (dl:push-rect list 10 10 20 20 (dl:pack-color 255 0 0))
  nil)

;;; Answers each build from a script, true (state changed) or false (settled), and
;;; repeats the last answer.
(defclass scripted-variant (variant:variant)
  ((script :initarg :script :accessor script)
   (builds :initform 0 :accessor builds)))

(defmethod variant:frame ((variant scripted-variant) context list)
  (declare (ignore context))
  (incf (builds variant))
  (dl:push-rect list 0 0 4 4 (dl:pack-color 0 255 0))
  (let ((script (script variant)))
    (if (rest script)
        (pop (script variant))
        (first script))))

(defclass requesting-variant (variant:variant) ())

(defmethod variant:frame ((variant requesting-variant) context list)
  (declare (ignore list))
  (variant:request-frame context)
  nil)

(defclass wanting-variant (variant:variant) ())

(defmethod variant:handle-event ((variant wanting-variant) event)
  (declare (ignore event))
  t)

(defclass text-variant (variant:variant)
  ((added :initform 0 :accessor added)))

(defmethod variant:frame ((variant text-variant) context list)
  (let ((font (variant:context-font context "Helvetica" 14)))
    (setf (added variant)
          (variant:context-push-text context list font "Hello" 4f0 20f0
                                     (dl:pack-color 255 255 255))))
  nil)

;;; --- a frame ---

(define-test a-frame-is-built-by-the-variant-and-drawn ()
  (with-host (host target (make-instance 'shape-variant) :scale 1)
    (draw host :scale 1f0)
    (let ((pixels (target:read-pixels target)))
      (assert-equal '(255 0 0 255) (pixel pixels 64 15 15))
      (assert-equal '(0 0 0 255) (pixel pixels 64 50 40)))
    (assert-eql 1 (dl:display-list-count (host:host-display-list host)))))

(define-test the-context-gets-the-size-the-scale-and-the-frame-number ()
  (with-host (host target (make-instance 'shape-variant))
    (let ((context (host:host-context host)))
      (draw host)
      (assert-eql 64f0 (variant:context-width context))
      (assert-eql 48f0 (variant:context-height context))
      (assert-eql 2f0 (variant:context-scale context))
      (assert-eql 1 (variant:context-frame-number context))
      (draw host :width 80f0)
      (assert-eql 80f0 (variant:context-width context))
      (assert-eql 2 (variant:context-frame-number context)))))

;;; --- the ring ---

(define-test the-ring-records-a-frame-and-its-phases ()
  (with-host (host target (make-instance 'shape-variant))
    (draw host)
    (let ((ring (host:host-ring host)))
      (assert-eql 1 (ring:frame-ring-count ring))
      (assert-eql 1 (ring:frame-builds ring 0))
      (assert-false (ring:frame-capped-p ring 0))
      (assert-true (plusp (ring:frame-encode-ns ring 0)))
      ;; The phases are the frame, divided.
      (assert-eql (ring:frame-duration-ns ring 0)
                  (+ (ring:frame-build-ns ring 0)
                     (ring:frame-drawable-wait-ns ring 0)
                     (ring:frame-encode-ns ring 0))))))

(define-test every-frame-adds-a-record ()
  (with-host (host target (make-instance 'shape-variant))
    (dotimes (i 5)
      (draw host))
    (assert-eql 5 (ring:frame-ring-count (host:host-ring host)))))

;;; --- settling ---

(define-test a-frame-that-changed-state-is-built-again ()
  (with-host (host target (make-instance 'scripted-variant :script '(t nil)))
    (draw host)
    (assert-eql 2 (builds (host:host-variant host)))
    (assert-eql 2 (ring:frame-builds (host:host-ring host) 0))
    (assert-false (ring:frame-capped-p (host:host-ring host) 0))
    ;; Only the settled build is left in the list, and drawn.
    (assert-eql 1 (dl:display-list-count (host:host-display-list host)))))

(define-test a-frame-that-never-settles-is-recorded-as-capped ()
  (with-host (host target (make-instance 'scripted-variant :script '(t)))
    (let ((warnings 0))
      (handler-bind ((settle:settle-cap-reached
                       (lambda (condition)
                         (incf warnings)
                         (muffle-warning condition))))
        (draw host))
      (assert-eql 1 warnings))
    (assert-eql 3 (ring:frame-builds (host:host-ring host) 0))
    (assert-true (ring:frame-capped-p (host:host-ring host) 0))))

;;; --- asking for frames ---

(define-test a-request-made-while-building-is-kept-for-after ()
  ;; With no view to ask, the flag stays set for whoever drives the host.
  (with-host (host target (make-instance 'requesting-variant))
    (let ((context (host:host-context host)))
      (assert-false (variant:context-frame-requested context))
      (draw host)
      (assert-true (variant:take-frame-request context)))))

(defun counting-wake (host)
  "Give HOST a wake function that only counts its calls. Returns a function that
reads the count."
  (let ((count 0))
    (declare (type fixnum count))
    (setf (host:host-wake host) (lambda () (incf count)))
    (lambda () count)))

(define-test a-request-made-while-building-wakes-the-view-after-the-frame ()
  (with-host (host target (make-instance 'requesting-variant))
    (let ((wakes (counting-wake host))
          (context (host:host-context host)))
      (draw host)
      (assert-eql 1 (funcall wakes))
      ;; The host took the flag when it woke the view.
      (assert-false (variant:take-frame-request context))
      (draw host)
      (assert-eql 2 (funcall wakes)))))

(define-test a-frame-nobody-asked-for-wakes-nothing ()
  (with-host (host target (make-instance 'shape-variant))
    (let ((wakes (counting-wake host)))
      (draw host)
      (draw host)
      (assert-eql 0 (funcall wakes)))))

(define-test a-request-made-outside-a-frame-wakes-the-view-at-once ()
  ;; On the main thread the wake function is called directly. From a REPL thread
  ;; the host would go through the main queue, which needs a view.
  (when (main-queue:on-main-thread-p)
    (with-host (host target (make-instance 'shape-variant))
      (let ((wakes (counting-wake host))
            (context (host:host-context host)))
        (variant:request-frame context)
        (assert-eql 1 (funcall wakes))
        (assert-false (variant:take-frame-request context))))))

(define-test an-event-that-wants-a-frame-requests-one ()
  (with-host (host target (make-instance 'wanting-variant))
    (host:host-deliver-event host (event:make-focus-event :timestamp 0 :focused-p t))
    (assert-true (variant:take-frame-request (host:host-context host)))))

(define-test a-request-from-another-thread-is-not-lost ()
  (with-host (host target (make-instance 'shape-variant))
    (let ((context (host:host-context host)))
      (sb-thread:join-thread
       (sb-thread:make-thread (lambda () (variant:request-frame context))))
      (assert-true (variant:take-frame-request context)))))

;;; --- text through the context ---

(define-test the-same-font-is-the-same-object ()
  (with-host (host target (make-instance 'shape-variant))
    (let ((context (host:host-context host)))
      (assert-true (eq (variant:context-font context "Helvetica" 12)
                       (variant:context-font context "Helvetica" 12)))
      (assert-true (eq (variant:context-font context "Helvetica" 12)
                       (variant:context-font context "Helvetica" 12.0)))
      (assert-false (eq (variant:context-font context "Helvetica" 12)
                        (variant:context-font context "Helvetica" 14)))
      (assert-false (eq (variant:context-font context "Helvetica" 12)
                        (variant:context-font context "Menlo" 12))))))

(define-test the-context-measures-and-finds-carets ()
  (with-host (host target (make-instance 'shape-variant))
    (let* ((context (host:host-context host))
           (font (variant:context-font context "Helvetica" 14)))
      (multiple-value-bind (width ascent descent) (variant:context-measure context font "Hello")
        (assert-true (plusp width))
        (assert-true (plusp ascent))
        (assert-true (plusp descent))
        (assert-true (< (abs (- width (variant:context-caret-x context font "Hello" 5))) 0.1))
        (assert-eql 0f0 (variant:context-caret-x context font "Hello" 0))
        (assert-eql 0 (variant:context-index-at-x context font "Hello" 0d0))
        (assert-equal '(1f0)
                      (list (nth-value 2 (variant:context-caret-rect context font "Hello" 2)))))
      (assert-true (> (length (variant:context-break-lines
                               context font "a few words that will not fit" 40d0))
                      1)))))

(define-test text-is-drawn-through-the-context ()
  (with-host (host target (make-instance 'text-variant))
    (draw host)
    (let ((list (host:host-display-list host)))
      (assert-eql 5 (added (host:host-variant host)))
      (assert-eql 5 (dl:display-list-count list))
      (assert-eql dl:+command-glyph+ (dl:display-list-kind list 0)))
    ;; Something white came out.
    (let ((pixels (target:read-pixels target)))
      (assert-true (loop :for i :from 0 :below (length pixels) :by 4
                         :thereis (> (aref pixels i) 128))))))

;;; --- no allocation ---

;;; Shapes and text, in two builds a frame. Compiled with this file, because
;;; lisp-unit2 evaluates test bodies under a debugging policy that allocates.

(defclass busy-variant (variant:variant)
  ((n :initform 0 :type fixnum :accessor n)))

(defmethod variant:frame ((variant busy-variant) context list)
  (dotimes (i 50)
    (dl:push-rounded-rect list (float (* 2 i) 1f0) 4f0 8f0 8f0 3f0 #xFF8000FF))
  (let ((font (variant:context-font context "Helvetica" 14)))
    (variant:context-push-text context list font "Hello, world" 4f0 40f0 #xFFFFFFFF))
  (oddp (incf (n variant))))

(defun draw-frames (host count)
  (declare (type fixnum count))
  (dotimes (i count)
    (objc:with-autorelease-pool
      (host:run-frame host 64f0 48f0 2f0)))
  (renderer:wait-for-last-frame (host:host-renderer host)))

(define-test running-frames-allocates-nothing ()
  (with-host (host target (make-instance 'busy-variant) :capacity 256)
    (draw-frames host 50)                           ; warm up: glyphs, fonts, dispatch
    (let ((before (sb-ext:get-bytes-consed)))
      (draw-frames host 5000)
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        ;; 5,000 frames, 10,000 builds. Allocating 32 bytes a frame would be
        ;; 160 KB and a region is about 64 KB, so allow 300 KB.
        (assert-true (< delta 300000) delta)))
    (assert-eql 5050 (ring:frame-ring-total (host:host-ring host)))))

;;; A variant that animates asks for the next frame in every frame. The request
;;; is made, and the view woken, after the frame is recorded, so the ring cannot
;;; see what it costs; this counts it with the whole process's allocation.

(define-test animating-frames-allocate-nothing ()
  (with-host (host target (make-instance 'requesting-variant) :capacity 256)
    (let ((wakes (counting-wake host)))
      (draw-frames host 50)
      (let ((before (sb-ext:get-bytes-consed)))
        (draw-frames host 5000)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          (assert-true (< delta 300000) delta)))
      (assert-eql 5050 (funcall wakes)))))

;;; --- the instrumentation of Phase 3 ---

;;; A variant that spends known times in its phases.
(defclass phased-variant (variant:variant) ())

(defmethod variant:frame ((variant phased-variant) context list)
  (variant:with-phase (context variant:+phase-update+) (sleep 0.002))
  (variant:with-phase (context variant:+phase-layout+) (sleep 0.004))
  (variant:with-phase (context variant:+phase-paint+)
    (sleep 0.001)
    (dl:push-rect list 10 10 20 20 (dl:pack-color 255 0 0)))
  nil)

(define-test with-the-instrumentation-on-the-ring-has-the-phases-of-the-variant ()
  (with-host (host target (make-instance 'phased-variant) :phases t)
    (dotimes (i 3)
      (draw host))
    (let ((ring (host:host-ring host)))
      (assert-eql 3 (ring:frame-ring-count ring))
      (dotimes (i 3)
        (assert-true (>= (ring:frame-update-ns ring i) 1500000) i)
        (assert-true (>= (ring:frame-layout-ns ring i) 3500000) i)
        (assert-true (>= (ring:frame-paint-ns ring i) 800000) i)
        ;; One build: its time is the first build's, and the parts are inside it.
        (assert-true (<= (abs (- (ring:frame-first-build-ns ring i) (ring:frame-build-ns ring i)))
                         200000)
                     i)
        (assert-true (<= (+ (ring:frame-update-ns ring i) (ring:frame-layout-ns ring i)
                            (ring:frame-paint-ns ring i))
                         (ring:frame-first-build-ns ring i))
                     i))
      ;; The GPU has no frame before the first one to report on.
      (assert-eql 0 (ring:frame-gpu-ns ring 0))
      (assert-true (plusp (ring:frame-gpu-ns ring 2))))))

(define-test with-the-instrumentation-off-the-ring-has-no-details ()
  (with-host (host target (make-instance 'phased-variant))
    (dotimes (i 3)
      (draw host))
    (let ((ring (host:host-ring host)))
      (dotimes (i 3)
        (assert-eql 0 (ring:frame-update-ns ring i))
        (assert-eql 0 (ring:frame-layout-ns ring i))
        (assert-eql 0 (ring:frame-paint-ns ring i))
        (assert-eql 0 (ring:frame-first-build-ns ring i))
        (assert-eql 0 (ring:frame-gpu-ns ring i))))))

;;; A variant that does its work in HANDLE-EVENT, as the retained and reactive variants do (the
;;; immediate one does it in the frame): about two milliseconds for an event.
(defclass slow-event-variant (variant:variant) ())

(defmethod variant:handle-event ((variant slow-event-variant) event)
  (declare (ignore event))
  (sleep 0.002)
  t)

(defmethod variant:frame ((variant slow-event-variant) context list)
  (dl:push-rect list 10 10 20 20 (dl:pack-color 255 0 0))
  nil)

(define-test with-the-instrumentation-on-a-frame-has-the-events-delivered-before-it ()
  (with-host (host target (make-instance 'slow-event-variant) :phases t)
    (draw host)
    (dotimes (i 3)
      (host:host-deliver-event host (event:make-focus-event :timestamp 0 :focused-p t)))
    (draw host)
    (draw host)
    (let ((ring (host:host-ring host)))
      (assert-equal '(0 3 0) (loop :for i :below 3 :collect (ring:frame-events ring i)))
      (assert-true (>= (ring:frame-events-ns ring 1) 5400000) (ring:frame-events-ns ring 1))
      ;; What the events took is not in what the build of the frame took.
      (assert-true (< (ring:frame-build-ns ring 1) 1800000) (ring:frame-build-ns ring 1))
      (assert-eql 0 (ring:frame-events-ns ring 0))
      (assert-eql 0 (ring:frame-events-ns ring 2)))))

(define-test with-the-instrumentation-off-a-frame-has-no-events ()
  (with-host (host target (make-instance 'slow-event-variant))
    (host:host-deliver-event host (event:make-focus-event :timestamp 0 :focused-p t))
    (draw host)
    (let ((ring (host:host-ring host)))
      (assert-eql 0 (ring:frame-events ring 0))
      (assert-eql 0 (ring:frame-events-ns ring 0)))))

(define-test the-builds-of-a-frame-that-settles-show-the-cost-of-settling ()
  (with-host (host target (make-instance 'scripted-variant :script '(t t nil)) :phases t)
    (draw host)
    (let ((ring (host:host-ring host)))
      (assert-eql 3 (ring:frame-builds ring 0))
      (assert-true (plusp (ring:frame-first-build-ns ring 0)))
      (assert-true (> (ring:frame-build-ns ring 0) (ring:frame-first-build-ns ring 0)))
      ;; All three builds, which the settle loop's own work is not part of.
      (assert-true (> (ring:frame-all-builds-ns ring 0) (ring:frame-first-build-ns ring 0)))
      (assert-true (<= (ring:frame-all-builds-ns ring 0) (ring:frame-build-ns ring 0))))))

(define-test running-frames-with-the-instrumentation-on-allocates-nothing ()
  (with-host (host target (make-instance 'busy-variant) :capacity 256 :phases t)
    (draw-frames host 50)
    (let ((before (sb-ext:get-bytes-consed)))
      (draw-frames host 5000)
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        (assert-true (< delta 300000) delta)))))

