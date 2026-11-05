(defpackage #:almighty-bolt/t/render/metal/draw-text
  (:use #:cl #:lisp-unit2)
  (:import-from #:almighty-bolt/text/core-foundation
                #:ccall)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:target #:almighty-bolt/render/metal/target)
                    (#:renderer #:almighty-bolt/render/metal/renderer)
                    (#:atlas #:almighty-bolt/render/metal/glyph-atlas)
                    (#:draw #:almighty-bolt/render/metal/draw-text)
                    (#:font #:almighty-bolt/text/font)
                    (#:line #:almighty-bolt/text/line)
                    (#:dl #:almighty-bolt/display-list)))

(in-package #:almighty-bolt/t/render/metal/draw-text)

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
  (loop :for i :from 0 :below (length pixels) :by 4
        :count (or (plusp (aref pixels i)) (plusp (aref pixels (+ i 1))) (plusp (aref pixels (+ i 2))))))

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

;;; --- the baseline ---

(defun core-graphics-coverage (font-address glyph x baseline width height)
  "What Core Graphics itself draws for GLYPH of the font at FONT-ADDRESS, with its origin at X and
BASELINE (points, y down), in a WIDTH by HEIGHT points bitmap at +SCALE+ pixels a point: a byte of
coverage for each pixel, row by row from the top. The atlas is held to this."
  (let* ((pixel-width (* width +scale+))
         (pixel-height (* height +scale+))
         (pixels (make-array (* pixel-width pixel-height)
                             :element-type '(unsigned-byte 8) :initial-element 0)))
    (sb-alien:with-alien ((glyph-id (sb-alien:unsigned 16))
                          (origin (array sb-alien:double 2)))
      (setf glyph-id glyph
            (sb-alien:deref origin 0) (float x 1d0)
            (sb-alien:deref origin 1) (float (- height baseline) 1d0))
      (sb-sys:with-pinned-objects (pixels)
        (let ((context (ccall "CGBitmapContextCreate" :ulong
                              (:pointer (sb-sys:sap-int (sb-sys:vector-sap pixels)))
                              (:ulong pixel-width) (:ulong pixel-height)
                              (:ulong 8) (:ulong pixel-width)
                              (:ulong 0) (:uint 7))))       ; alpha only
          (ccall "CGContextScaleCTM" :void (:ulong context) (:double (float +scale+ 1d0))
                 (:double (float +scale+ 1d0)))
          (ccall "CGContextSetShouldAntialias" :void (:ulong context) (:bool t))
          (ccall "CGContextSetAllowsFontSmoothing" :void (:ulong context) (:bool nil))
          (ccall "CGContextSetShouldSmoothFonts" :void (:ulong context) (:bool nil))
          (ccall "CTFontDrawGlyphs" :void
                 (:ulong font-address)
                 (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr glyph-id))))
                 (:pointer (sb-sys:sap-int (sb-alien:alien-sap origin)))
                 (:ulong 1)
                 (:ulong context))
          (ccall "CGContextRelease" :void (:ulong context)))))
    pixels))

(define-test every-glyph-is-drawn-where-core-graphics-draws-it ()
  ;; A glyph cut to the box of its own ink and set on a whole pixel is up to half a pixel off,
  ;; a different fraction for each glyph, so that some letters of a line stand a little higher
  ;; or lower than the rest. Drawn one at a time with the pen and the baseline on whole pixels,
  ;; each has to come out as Core Graphics draws it there. White on black, so the red of a pixel
  ;; is the coverage.
  (with-text-scene (device renderer font)
    (let ((atlas (renderer:renderer-atlas renderer))
          (width 60) (height 60))
      (loop :for char :across "oeHxpgyjQ,;acsfl&@"
            :do (let* ((list (dl:make-display-list :capacity 8))
                       (line (line:get-line font (string char))))
                  (draw:push-line list atlas line 20f0 40f0 (dl:pack-color 255 255 255))
                  (let* ((pixels (render-text device renderer list width height))
                         (truth (core-graphics-coverage (aref (line:text-line-glyph-fonts line) 0)
                                                        (aref (line:text-line-glyphs line) 0)
                                                        20 40 width height))
                         (worst (loop :for i :below (length truth)
                                      :maximize (abs (- (aref pixels (+ (* 4 i) 2))
                                                        (aref truth i))))))
                    (assert-true (<= worst 8) char worst)))))))

(define-test a-baseline-between-two-pixel-rows-moves-the-whole-line-together ()
  ;; A line whose baseline is not on a pixel row is set on one row or the next, all of it: every
  ;; glyph is rounded the same way, even when the baseline is exactly half way between the rows,
  ;; where rounding each glyph's own sum would send some of them each way.
  (with-text-scene (device renderer font)
    (let ((atlas (renderer:renderer-atlas renderer))
          (line (line:get-line font "Hxopgy,jQ")))
      (flet ((drawn (baseline)
               (let ((list (dl:make-display-list :capacity 32)))
                 (draw:push-line list atlas line 20f0 (float baseline 1f0)
                                 (dl:pack-color 255 255 255))
                 (render-text device renderer list 160 60))))
        (dolist (baseline '(40.25 40.75 40.1 40.3 40.6 40.9))
          (let ((here (drawn baseline))
                (above (drawn (/ (floor (* baseline +scale+)) (float +scale+))))
                (below (drawn (/ (ceiling (* baseline +scale+)) (float +scale+)))))
            (assert-true (or (equalp here above) (equalp here below)) baseline)))))))

(define-test text-is-drawn-in-its-colour-with-full-coverage-in-the-stems ()
  (with-text-scene (device renderer font)
    (let ((list (dl:make-display-list :capacity 64)))
      (draw:push-line list (renderer:renderer-atlas renderer) (line:get-line font "HI") 10f0 40f0
                      (dl:pack-color 255 128 0))
      (let* ((pixels (render-text device renderer list 80 60))
             (full (loop :for i :from 0 :below (length pixels) :by 4
                         :thereis (and (= 255 (aref pixels (+ i 2)))     ; red
                                       (= 128 (aref pixels (+ i 1)))     ; green
                                       (= 0 (aref pixels i))))))         ; blue
        (assert-true full)))))

;;; --- text turned a quarter ---

(defun lit-crop (pixels width height)
  "The red of every pixel in the lit box of PIXELS (WIDTH by HEIGHT pixels), as a 2D array of rows,
and the lit box."
  (destructuring-bind (left top right bottom) (lit-box pixels width height)
    (let ((crop (make-array (list (1+ (- bottom top)) (1+ (- right left))))))
      (dotimes (row (array-dimension crop 0))
        (dotimes (column (array-dimension crop 1))
          (setf (aref crop row column)
                (aref pixels (+ 2 (* 4 (+ (* (+ top row) width) left column)))))))
      (values crop (list left top right bottom)))))

(defun turned-crop (crop turn)
  "CROP turned a quarter: to the left if TURN is :up, to the right if it is :down."
  (destructuring-bind (rows columns) (array-dimensions crop)
    (let ((turned (make-array (list columns rows))))
      (dotimes (row columns turned)
        (dotimes (column rows)
          (setf (aref turned row column)
                (ecase turn
                  (:up (aref crop column (- columns 1 row)))
                  (:down (aref crop (- rows 1 column) row)))))))))

(define-test turned-text-is-the-upright-texts-pixels-turned ()
  ;; Each glyph of a turned line is the bitmap of the upright one, read turned in the shader: on whole
  ;; pixels, what is drawn is exactly the upright line's pixels turned a quarter, and it starts at the
  ;; point it was given, reading up (the letters' tops to the left) or down (to the right).
  (with-text-scene (device renderer font)
    (let* ((atlas (renderer:renderer-atlas renderer))
           (line (line:get-line font "Hgy,"))
           (size 160)
           (pixels (* size +scale+)))
      (flet ((drawn (x baseline turn)
               (let ((list (dl:make-display-list :capacity 16)))
                 (draw:push-line list atlas line x baseline (dl:pack-color 255 255 255) turn)
                 (lit-crop (render-text device renderer list size size) pixels pixels))))
        (let ((upright (drawn 20f0 50f0 nil))
              (ascent (line:text-line-ascent line))
              (descent (line:text-line-descent line))
              (long (line:text-line-width line))
              (margin 3))
          (dolist (turn '(:up :down))
            (multiple-value-bind (crop box) (drawn 80f0 80f0 turn)
              (assert-equalp (turned-crop upright turn) crop turn)
              (destructuring-bind (left top right bottom) box
                (multiple-value-bind (above below) (if (eq turn :up)
                                                       (values ascent descent)
                                                       (values descent ascent))
                  (assert-true (>= left (- (* (- 80 above) +scale+) margin)) turn left)
                  (assert-true (<= right (+ (* (+ 80 below) +scale+) margin)) turn right))
                (if (eq turn :up)
                    (assert-true (and (<= bottom (+ (* 80 +scale+) margin))
                                      (>= top (- (* (- 80 long) +scale+) margin)))
                                 turn top bottom)
                    (assert-true (and (>= top (- (* 80 +scale+) margin))
                                      (<= bottom (+ (* (+ 80 long) +scale+) margin)))
                                 turn top bottom))))))))))

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
