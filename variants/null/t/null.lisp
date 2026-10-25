(defpackage #:almighty-bolt-variant-null/t/null
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:null #:almighty-bolt-variant-null/null)
                    (#:host #:almighty-bolt-core/platform/macos/host)
                    (#:objc #:almighty-bolt-core/platform/macos/objc)
                    (#:device #:almighty-bolt-core/render/metal/device)
                    (#:target #:almighty-bolt-core/render/metal/target)
                    (#:renderer #:almighty-bolt-core/render/metal/renderer)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-null/t/null)

;;; These run the null variant in the macOS host without a window, drawing into an
;;; off-screen texture, so they need a Mac with a GPU. The window itself, the events
;;; AppKit delivers and the timings are checked by `make bench-null`.

(defmacro with-host ((host target variant) &body body)
  "Run BODY with HOST, a host for VARIANT, drawing into TARGET: a 200 by 150 point
texture at two pixels a point."
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let* ((device (device:default-device))
              (,host (host:make-host ,variant device target:+pixel-format-bgra8-unorm+
                                     :capacity 4096 :scale 2))
              (,target (target:make-offscreen-target device 400 300)))
         (let ((pass (target:offscreen-target-descriptor ,target)))
           (setf (host:host-acquire ,host) (lambda () (values pass 0))))
         ,@body))))

(defun draw (host)
  (host:run-frame host 200f0 150f0 2f0)
  (renderer:wait-for-last-frame (host:host-renderer host)))

;;; --- the list it replays ---

(define-test the-first-frame-builds-the-scene-and-draws-it ()
  (with-host (host target (null:make-null-variant))
    (let ((variant (host:host-variant host)))
      (assert-false (null:null-variant-scene variant))
      (draw host)
      (assert-true (null:null-variant-scene variant))
      (let ((count (core:display-list-count (host:host-display-list host))))
        (assert-eql (core:display-list-count (null:null-variant-scene variant)) count)
        ;; A panel, eight shapes, and a good many glyphs.
        (assert-true (> count 100) count)))
    ;; The panel is not the clear colour.
    (let ((pixels (target:read-pixels target)))
      (assert-true (loop :for i :from 0 :below (length pixels) :by 4
                         :thereis (/= (aref pixels i) 0))))))

(define-test every-frame-replays-the-same-list ()
  (with-host (host target (null:make-null-variant))
    (draw host)
    (let ((hash (core:display-list-hash (host:host-display-list host)))
          (scene (null:null-variant-scene (host:host-variant host))))
      (dotimes (i 4)
        (draw host)
        (assert-eql hash (core:display-list-hash (host:host-display-list host))))
      (assert-eql hash (core:display-list-hash scene)))))

(define-test the-null-variant-settles-in-one-build-every-time ()
  (with-host (host target (null:make-null-variant))
    (dotimes (i 5)
      (draw host))
    (let ((ring (host:host-ring host)))
      (assert-eql 5 (core:frame-ring-count ring))
      (dotimes (i 5)
        (assert-eql 1 (core:frame-builds ring i))
        (assert-false (core:frame-capped-p ring i))))))

;;; --- events and requests ---

(define-test events-are-counted-and-ask-for-no-frame ()
  (with-host (host target (null:make-null-variant))
    (dotimes (i 3)
      (host:host-deliver-event host (core:make-focus-event :timestamp i :focused-p t)))
    (assert-eql 3 (null:null-variant-events-seen (host:host-variant host)))
    (assert-false (core:take-frame-request (host:host-context host)))))

(define-test an-animating-null-variant-asks-for-a-frame-after-each-frame ()
  (with-host (host target (null:make-null-variant :animate-frames 3))
    (let ((context (host:host-context host)))
      (dotimes (i 3)
        (draw host)
        (assert-true (core:take-frame-request context)))
      ;; The fourth frame has nothing left to ask for.
      (draw host)
      (assert-false (core:take-frame-request context))
      (assert-eql 0 (null:null-variant-animate-frames (host:host-variant host))))))

;;; --- no allocation ---

(defun draw-frames (host count)
  "Compiled with this file, because lisp-unit2 evaluates test bodies under a
debugging policy that allocates."
  (declare (type fixnum count))
  (dotimes (i count)
    (objc:with-autorelease-pool
      (host:run-frame host 200f0 150f0 2f0)))
  (renderer:wait-for-last-frame (host:host-renderer host)))

(define-test replaying-frames-allocates-nothing ()
  (with-host (host target (null:make-null-variant))
    (draw-frames host 50)                           ; builds the scene, fills the atlas
    (let ((before (sb-ext:get-bytes-consed)))
      (draw-frames host 5000)
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        ;; Allocating 32 bytes a frame would be 160 KB, and a region is about
        ;; 64 KB, so allow 300 KB.
        (assert-true (< delta 300000) delta)))))

;;; Asking for the next frame in every frame, as an animation does. The request is
;;; made, and the view woken, after the frame is recorded in the ring, so the ring
;;; cannot see what it costs; this counts it with the whole process's allocation.

(define-test an-animating-null-variant-allocates-nothing ()
  (with-host (host target (null:make-null-variant :animate-frames 1000000))
    (let ((wakes 0))
      (declare (type fixnum wakes))
      (setf (host:host-wake host) (lambda () (incf wakes)))
      (draw-frames host 50)
      (let ((before (sb-ext:get-bytes-consed)))
        (draw-frames host 5000)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          (assert-true (< delta 300000) delta)))
      ;; Every frame asked for the next, and the host woke the view each time.
      (assert-eql 5050 wakes))))
