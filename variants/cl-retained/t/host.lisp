(defpackage #:almighty-bolt-variant-retained/t/host
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:host #:almighty-bolt-core/platform/macos/host)
                    (#:objc #:almighty-bolt-core/platform/macos/objc)
                    (#:device #:almighty-bolt-core/render/metal/device)
                    (#:target #:almighty-bolt-core/render/metal/target)
                    (#:renderer #:almighty-bolt-core/render/metal/renderer)
                    (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:counter #:almighty-bolt-variant-retained/t/counter)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-retained/t/host)

;;; The retained variant in the macOS host, drawing into an off-screen texture, so
;;; that its text comes from Core Text and the glyph atlas and its allocation can be
;;; counted. They need a Mac with a GPU. A window, and the events AppKit delivers, are
;;; checked by `make retained-counter`.

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

(define-test the-counter-draws-with-real-text ()
  (multiple-value-bind (variant number button) (counter:make-counter)
    (with-host (host target variant)
      (draw host)
      ;; The text of the real engine gives sizes that are single-floats of a sensible size.
      (assert-true (typep (n:node-w number) 'single-float))
      (assert-true (< 5f0 (n:node-w number) 40f0) (n:node-w number))
      (assert-true (< 40f0 (n:node-w button) 200f0) (n:node-w button))
      (assert-true (> (core:display-list-count (host:host-display-list host)) 5))
      (let ((pixels (target:read-pixels target)))
        (assert-true (loop :for i :from 0 :below (length pixels) :by 4
                           :thereis (/= (aref pixels i) 0)))))))

(define-test one-click-through-the-host-changes-the-number ()
  (multiple-value-bind (variant number) (counter:make-counter)
    (with-host (host target variant)
      (draw host)
      (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
        (flet ((send (kind)
                 (host:host-deliver-event
                  host (core:make-pointer-event :timestamp 0 :kind kind
                                                :x (float (+ x (/ width 2)) 1d0)
                                                :y (float (+ y (/ height 2)) 1d0)
                                                :button (if (eq kind :move) nil 0)
                                                :buttons (if (eq kind :down) 1 0)))))
          (send :move)
          (send :down)
          (send :up)))
      (assert-equal "1" (w:label-text number))
      ;; The click asked for a frame, which the host leaves as a request.
      (assert-true (core:take-frame-request (host:host-context host))))))

;;; --- no allocation ---

(defun draw-frames (host count)
  "Compiled with this file, because lisp-unit2 evaluates test bodies under a
debugging policy that allocates."
  (declare (type fixnum count))
  (dotimes (i count)
    (objc:with-autorelease-pool
      (host:run-frame host 200f0 150f0 2f0)))
  (renderer:wait-for-last-frame (host:host-renderer host)))

(define-test repainting-an-unchanged-counter-allocates-nothing ()
  (multiple-value-bind (variant) (counter:make-counter)
    (with-host (host target variant)
      (draw-frames host 50)
      (let ((before (sb-ext:get-bytes-consed)))
        (draw-frames host 5000)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          (assert-true (< delta 300000) delta))))))

(defun button-middle (variant)
  "The middle of the Counter's button, as two double-floats."
  (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
    (values (float (+ x (/ width 2)) 1d0) (float (+ y (/ height 2)) 1d0))))

(defun move-pointer (variant x y count)
  "Move the pointer over the button, at X, Y, and over empty space COUNT times, with
events made before the count, since events are made by the platform and not by a
variant."
  (declare (type fixnum count) (type double-float x y))
  (let ((inside (core:make-pointer-event :timestamp 0 :kind :move :x x :y y))
        (inside-again (core:make-pointer-event :timestamp 0 :kind :move :x (+ x 2d0) :y y))
        (outside (core:make-pointer-event :timestamp 0 :kind :move :x 190d0 :y 140d0))
        (outside-again (core:make-pointer-event :timestamp 0 :kind :move :x 191d0 :y 141d0)))
    (let ((before (sb-ext:get-bytes-consed)))
      (dotimes (i count)
        (core:handle-event variant inside)
        (core:handle-event variant inside-again)
        (core:handle-event variant outside)
        (core:handle-event variant outside-again))
      (- (sb-ext:get-bytes-consed) before))))

(define-test hover-events-allocate-nothing ()
  (multiple-value-bind (variant) (counter:make-counter)
    (with-host (host target variant)
      (draw host)
      (multiple-value-bind (x y) (button-middle variant)
        (move-pointer variant x y 100)
        (let ((delta (move-pointer variant x y 50000)))
          (assert-true (< delta 300000) delta))))))

(defun click-and-draw (host variant x y count)
  "COUNT times: click the button at X, Y, so that the number changes and the next
frame has to lay out, and draw that frame. Returns the bytes consed."
  (declare (type fixnum count) (type double-float x y))
  (let ((down (core:make-pointer-event :timestamp 0 :kind :down :x x :y y
                                       :button 0 :buttons 1))
        (up (core:make-pointer-event :timestamp 0 :kind :up :x x :y y :button 0)))
    (let ((before (sb-ext:get-bytes-consed)))
      (dotimes (i count)
        (core:handle-event variant down)
        (core:handle-event variant up)
        (objc:with-autorelease-pool
          (host:run-frame host 200f0 150f0 2f0)))
      (renderer:wait-for-last-frame (host:host-renderer host))
      (- (sb-ext:get-bytes-consed) before))))

(define-test a-click-and-the-frame-it-causes-cost-only-what-the-number-costs ()
  ;; Each click makes a new number string and so a new line to shape, which the text
  ;; engine keeps. That is the app's allocation and the engine's cache, not the
  ;; layout's; the bound is a generous one a click, to catch anything much worse.
  (multiple-value-bind (variant) (counter:make-counter)
    (with-host (host target variant)
      (draw host)
      (multiple-value-bind (x y) (button-middle variant)
        (click-and-draw host variant x y 10)
        (let ((delta (click-and-draw host variant x y 200)))
          (format t "~&A click and the frame it causes: ~,1f KB~%" (/ delta 200 1024.0))
          (assert-true (< delta (* 200 20000)) delta))))))

(defun toggle-and-draw (host label count)
  "COUNT times: change the number's colour, which makes the number paint again, and
draw the frame. Compiled with this file, as DRAW-FRAMES is."
  (declare (type fixnum count))
  (let ((red (core:pack-color 255 64 64))
        (white (core:pack-color 255 255 255)))
    (dotimes (i count)
      (setf (w:label-color label) (if (evenp i) red white))
      (objc:with-autorelease-pool
        (host:run-frame host 200f0 150f0 2f0)))
    (renderer:wait-for-last-frame (host:host-renderer host))))

(define-test painting-a-node-again-into-its-cache-allocates-nothing ()
  (multiple-value-bind (variant number) (counter:make-counter)
    (with-host (host target variant)
      (toggle-and-draw host number 50)
      (let ((before (sb-ext:get-bytes-consed)))
        (toggle-and-draw host number 5000)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          (assert-true (< delta 300000) delta))))))

;;; --- the caches and the pixel grid ---

(defun painted-hash (variant context caching)
  "The hash of what painting the whole tree gives, with or without the caches."
  (let ((list (core:make-display-list :capacity 4096))
        (n:*paint-caching* caching))
    (n:paint-node (retained:retained-variant-root variant) context list 0f0 0f0)
    (core:display-list-hash list)))

(defun click-button (host variant)
  (multiple-value-bind (x y width height) (core:lookup-target variant :count-button)
    (flet ((send (kind)
             (host:host-deliver-event
              host (core:make-pointer-event :timestamp 0 :kind kind
                                            :x (float (+ x (/ width 2)) 1d0)
                                            :y (float (+ y (/ height 2)) 1d0)
                                            :button (if (eq kind :move) nil 0)
                                            :buttons (if (eq kind :down) 1 0)))))
      (send :move)
      (send :down)
      (send :up))))

(define-test the-caches-give-what-painting-from-nothing-gives-with-real-text ()
  ;; Glyphs are put on whole pixels by the text drawing, from their position in the
  ;; window, so a cache painted at the top left and moved must come out on the same
  ;; pixels as text painted where it is. The real text engine gives positions that are
  ;; not whole numbers, which the fake context in the other tests does not.
  (multiple-value-bind (variant number button) (counter:make-counter)
    (with-host (host target variant)
      (draw host)
      (let ((context (host:host-context host)))
        (assert-true (/= (n:node-x button) (ffloor (n:node-x button)))
                     "the button should be at a fractional place" (n:node-x button))
        (assert-eql (painted-hash variant context nil) (painted-hash variant context t))
        (dotimes (i 10)
          (click-button host variant)
          (draw host))
        ;; The number is 10 now, wider, and the button has moved.
        (assert-equal "10" (w:label-text number))
        (assert-eql (painted-hash variant context nil) (painted-hash variant context t))))))
