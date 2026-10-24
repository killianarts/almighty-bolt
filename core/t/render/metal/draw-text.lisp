(defpackage #:almighty-bolt-core/t/render/metal/draw-text
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt-core/platform/macos/objc)
                    (#:device #:almighty-bolt-core/render/metal/device)
                    (#:target #:almighty-bolt-core/render/metal/target)
                    (#:renderer #:almighty-bolt-core/render/metal/renderer)
                    (#:atlas #:almighty-bolt-core/render/metal/glyph-atlas)
                    (#:draw #:almighty-bolt-core/render/metal/draw-text)
                    (#:font #:almighty-bolt-core/text/font)
                    (#:line #:almighty-bolt-core/text/line)
                    (#:dl #:almighty-bolt-core/display-list)))

(in-package #:almighty-bolt-core/t/render/metal/draw-text)

;;; Text drawn through the whole path: Core Text shapes it, Core Graphics
;;; rasterizes the glyphs into the atlas, the display list holds one command per
;;; glyph, and the renderer draws them. The checks are against where the text was
;;; measured to be, not against exact pixels, which differ between versions of
;;; macOS.

(defconstant +scale+ 2)

(defmacro with-text-scene ((device renderer font-var) &body body)
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let* ((,device (device:default-device))
              (,renderer (renderer:make-renderer ,device target:+pixel-format-bgra8-unorm+
                                                 :capacity 1024 :scale +scale+))
              (,font-var (font:make-font "Helvetica" 24)))
         (declare (ignorable ,device ,renderer ,font-var))
         ,@body))))

(defun render-text (device renderer display-list width height)
  "Draw DISPLAY-LIST at the scale the atlas was made for, on black, and return the pixels."
  (let ((target (target:make-offscreen-target device (* width +scale+) (* height +scale+)
                                              :clear-color '(0d0 0d0 0d0 1d0))))
    (renderer:render-display-list renderer display-list
                                  (target:offscreen-target-descriptor target)
                                  0 width height +scale+)
    (renderer:wait-for-last-frame renderer)
    (target:read-pixels target)))

(defun lit-box (pixels width height)
  "The box (left top right bottom, in pixels) that holds every pixel that is not
black, or NIL if there is none."
  (let ((left width) (top height) (right -1) (bottom -1))
    (dotimes (y height)
      (dotimes (x width)
        (let ((i (* 4 (+ (* y width) x))))
          (when (or (plusp (aref pixels i)) (plusp (aref pixels (+ i 1))) (plusp (aref pixels (+ i 2))))
            (setf left (min left x) right (max right x)
                  top (min top y) bottom (max bottom y))))))
    (and (>= right 0) (list left top right bottom))))

(defun lit-count (pixels)
  (loop for i from 0 below (length pixels) by 4
        count (or (plusp (aref pixels i)) (plusp (aref pixels (+ i 1))) (plusp (aref pixels (+ i 2))))))

;;; --- where the text lands ---

(define-test text-lands-where-it-was-measured ()
  (with-text-scene (device renderer font)
    (let* ((list (dl:make-display-list :capacity 64))
           (line (line:get-line font "Hello"))
           (width (line:text-line-width line))
           (x 20f0)
           (baseline 50f0))
      (assert-eql 5 (draw:push-line list (renderer:renderer-atlas renderer) line x baseline
                                    (dl:pack-color 255 255 255)))
      (destructuring-bind (left top right bottom)
          (lit-box (render-text device renderer list 160 80) (* 160 +scale+) (* 80 +scale+))
        (let ((margin 3))               ; pixels: bearings and antialiasing
          (assert-true (>= left (- (* x +scale+) margin)) left)
          (assert-true (<= right (+ (* (+ x width) +scale+) margin)) right)
          (assert-true (>= top (- (* (- baseline (line:text-line-ascent line)) +scale+) margin)) top)
          (assert-true (<= bottom (+ (* (+ baseline (line:text-line-descent line)) +scale+) margin))
                       bottom)
          ;; And it is not a speck: it fills most of the measured width.
          (assert-true (> (- right left) (* 0.8 width +scale+)) (- right left)))))))

(define-test text-is-drawn-in-its-colour-with-full-coverage-in-the-stems ()
  (with-text-scene (device renderer font)
    (let ((list (dl:make-display-list :capacity 64)))
      (draw:push-line list (renderer:renderer-atlas renderer) (line:get-line font "HI") 10f0 40f0
                      (dl:pack-color 255 128 0))
      (let* ((pixels (render-text device renderer list 80 60))
             (full (loop for i from 0 below (length pixels) by 4
                         thereis (and (= 255 (aref pixels (+ i 2)))     ; red
                                      (= 128 (aref pixels (+ i 1)))     ; green
                                      (= 0 (aref pixels i))))))         ; blue
        (assert-true full)))))

(define-test text-in-another-script-draws-with-a-font-that-has-it ()
  (with-text-scene (device renderer font)
    (let ((list (dl:make-display-list :capacity 64)))
      (assert-eql 5 (draw:push-line list (renderer:renderer-atlas renderer)
                                    (line:get-line font "こんにちは") 10f0 40f0
                                    (dl:pack-color 255 255 255)))
      (assert-true (> (lit-count (render-text device renderer list 160 60)) 200)))))

;;; --- the atlas ---

(define-test a-space-adds-no-command ()
  (with-text-scene (device renderer font)
    (let ((list (dl:make-display-list :capacity 64)))
      (assert-eql 2 (draw:push-line list (renderer:renderer-atlas renderer)
                                    (line:get-line font "a b") 0f0 20f0 #xFFFFFFFF))
      (assert-eql 2 (dl:display-list-count list)))))

(define-test glyphs-are-rasterized-once ()
  (with-text-scene (device renderer font)
    (let ((list (dl:make-display-list :capacity 64))
          (atlas (renderer:renderer-atlas renderer))
          (line (line:get-line font "Hello")))
      (draw:push-line list atlas line 0f0 20f0 #xFFFFFFFF)
      (let ((count (atlas:glyph-atlas-count atlas)))
        ;; H e l l o: the two l are one glyph.
        (assert-eql 4 count)
        (draw:push-line list atlas line 0f0 50f0 #xFFFFFFFF)
        (assert-eql count (atlas:glyph-atlas-count atlas))))))

(define-test the-same-glyph-has-the-same-index ()
  (with-text-scene (device renderer font)
    (let ((list (dl:make-display-list :capacity 64)))
      (draw:push-line list (renderer:renderer-atlas renderer) (line:get-line font "ll") 0f0 20f0
                      #xFFFFFFFF)
      (assert-eql (dl:display-list-parameter list 0) (dl:display-list-parameter list 1))
      (assert-eql dl:+command-glyph+ (dl:display-list-kind list 0)))))

(define-test a-full-atlas-is-an-error ()
  (with-text-scene (device renderer font)
    (let ((tiny (atlas:make-glyph-atlas device :width 24 :height 24 :capacity 64 :scale +scale+))
          (line (line:get-line font "WMBQ@")))
      (assert-error 'atlas:glyph-atlas-full
                    (dotimes (i (line:text-line-glyph-count line))
                      (atlas:atlas-glyph-index tiny
                                               (aref (line:text-line-glyph-fonts line) i)
                                               (aref (line:text-line-glyphs line) i)))))))

(define-test an-atlas-with-no-room-for-more-glyphs-says-so ()
  (with-text-scene (device renderer font)
    (let ((tiny (atlas:make-glyph-atlas device :width 1024 :height 1024 :capacity 2 :scale +scale+))
          (line (line:get-line font "abc")))
      (assert-error 'atlas:glyph-atlas-full
                    (dotimes (i 3)
                      (atlas:atlas-glyph-index tiny
                                               (aref (line:text-line-glyph-fonts line) i)
                                               (aref (line:text-line-glyphs line) i)))))))

;;; --- allocation ---
;;;
;;; The frame loop is a function compiled with this file, not a test body (see the
;;; note in the renderer tests).

(defun draw-text-frames (renderer display-list target lines count)
  (let ((pass (target:offscreen-target-descriptor target))
        (atlas (renderer:renderer-atlas renderer)))
    (dotimes (frame count)
      (objc:with-autorelease-pool
        (dl:clear-display-list display-list)
        (let ((y 24f0))
          (declare (single-float y))
          (dolist (line lines)
            (draw:push-line display-list atlas line 10f0 y #xFFFFFFFF)
            (incf y 22f0)))
        (renderer:render-display-list renderer display-list pass 0 400 200 +scale+)))
    (renderer:wait-for-last-frame renderer)))

(define-test drawing-text-already-seen-allocates-nothing ()
  (with-text-scene (device renderer font)
    (let* ((target (target:make-offscreen-target device (* 400 +scale+) (* 200 +scale+)))
           (display-list (dl:make-display-list :capacity 512))
           (lines (list (line:get-line font "The quick brown fox")
                        (line:get-line font "jumps over the lazy dog")
                        (line:get-line font "0123456789 !@#$%"))))
      ;; Warm up: the first frame rasterizes every glyph.
      (draw-text-frames renderer display-list target lines 5)
      (let ((glyphs (atlas:glyph-atlas-count (renderer:renderer-atlas renderer)))
            (before (sb-ext:get-bytes-consed)))
        (draw-text-frames renderer display-list target lines 5000)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          (assert-eql glyphs (atlas:glyph-atlas-count (renderer:renderer-atlas renderer)))
          ;; 5000 frames of about 60 glyphs: 300,000 glyph commands. Even one byte
          ;; each would be 300 KB.
          (assert-true (< delta 300000) delta))))))
