(defpackage #:almighty-bolt/t/render/metal/renderer
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:device #:almighty-bolt/render/metal/device)
                    (#:target #:almighty-bolt/render/metal/target)
                    (#:renderer #:almighty-bolt/render/metal/renderer)
                    (#:dl #:almighty-bolt/display-list)))

(in-package #:almighty-bolt/t/render/metal/renderer)

;;; These draw with the GPU into an off-screen texture and read the pixels back,
;;; so they need a Mac with a GPU but no window. Most check exact pixels worked
;;; out by hand; one compares with a golden image.

(defmacro with-metal ((device renderer &key (capacity 1024)) &body body)
  `(objc:with-masked-float-traps
     (objc:with-autorelease-pool
       (let* ((,device (device:default-device))
              (,renderer (renderer:make-renderer ,device target:+pixel-format-bgra8-unorm+
                                                 :capacity ,capacity)))
         ,@body))))

(defparameter *clear* '(0.1d0 0.1d0 0.15d0 1d0)
  "The background: 26, 26, 38 as bytes.")

(defun render-scene (device renderer display-list width height scale)
  "Draw DISPLAY-LIST at SCALE pixels per point into a fresh target and return
its pixels."
  (let ((target (target:make-offscreen-target device (* width scale) (* height scale)
                                              :clear-color *clear*)))
    (renderer:render-display-list renderer display-list
                                  (target:offscreen-target-descriptor target)
                                  0 width height scale)
    (renderer:wait-for-last-frame renderer)
    (target:read-pixels target)))

(defun pixel (pixels width x y)
  "The (red green blue alpha) of the pixel at X, Y."
  (let ((i (* 4 (+ (* y width) x))))
    (list (aref pixels (+ i 2)) (aref pixels (+ i 1)) (aref pixels i) (aref pixels (+ i 3)))))

(defparameter +background+ '(26 26 38 255))
(defparameter +red+ '(255 0 0 255))
(defparameter +blue+ '(0 0 255 255))

(defun near (expected actual &optional (tolerance 2))
  (every (lambda (e a) (<= (abs (- e a)) tolerance)) expected actual))

;;; --- exact pixels ---

(define-test an-empty-display-list-draws-only-the-background ()
  (with-metal (device renderer)
    (let ((pixels (render-scene device renderer (dl:make-display-list :capacity 4) 16 8 1)))
      (assert-equal +background+ (pixel pixels 16 0 0))
      (assert-equal +background+ (pixel pixels 16 15 7)))))

(define-test a-rectangle-edge-lands-on-the-pixel-boundary ()
  ;; A rectangle at 10, 10 of 40 by 30 points. At one pixel a point its left
  ;; pixel is 10 and its right is 49; at two pixels a point they are 20 and 99.
  ;; This checks both the geometry and the scale.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rect list 10 10 40 30 (dl:pack-color 255 0 0))
      (let ((pixels (render-scene device renderer list 128 96 1)))
        (assert-equal +background+ (pixel pixels 128 9 20))
        (assert-equal +red+ (pixel pixels 128 10 20))
        (assert-equal +red+ (pixel pixels 128 49 20))
        (assert-equal +background+ (pixel pixels 128 50 20))
        (assert-equal +background+ (pixel pixels 128 20 9))
        (assert-equal +red+ (pixel pixels 128 20 10))
        (assert-equal +red+ (pixel pixels 128 20 39))
        (assert-equal +background+ (pixel pixels 128 20 40)))
      (let ((pixels (render-scene device renderer list 128 96 2)))
        (assert-equal +background+ (pixel pixels 256 19 40))
        (assert-equal +red+ (pixel pixels 256 20 40))
        (assert-equal +red+ (pixel pixels 256 99 40))
        (assert-equal +background+ (pixel pixels 256 100 40))
        (assert-equal +red+ (pixel pixels 256 40 79))
        (assert-equal +background+ (pixel pixels 256 40 80))))))

(define-test a-rounded-corner-is-cut-away ()
  ;; 50 by 40 points at 60, 10 with a radius of 12: the very corner is not
  ;; covered, the middle of each straight edge and the centre are.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rounded-rect list 60 10 50 40 12 (dl:pack-color 0 0 255))
      (let ((pixels (render-scene device renderer list 128 96 1)))
        (assert-equal +background+ (pixel pixels 128 60 10))
        (assert-equal +background+ (pixel pixels 128 109 10))
        (assert-equal +background+ (pixel pixels 128 60 49))
        (assert-equal +background+ (pixel pixels 128 109 49))
        (assert-equal +blue+ (pixel pixels 128 85 10))
        (assert-equal +blue+ (pixel pixels 128 60 30))
        (assert-equal +blue+ (pixel pixels 128 85 30))))))

(define-test a-stroke-is-the-edge-of-a-rounded-rectangle-inside-it ()
  ;; 50 by 40 points at 60, 10, a stroke 2 points wide: the two rows and columns at each edge are
  ;; covered, what is inside them is not, and nothing outside.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rounded-stroke list 60 10 50 40 6 2 (dl:pack-color 255 0 0))
      (let ((pixels (render-scene device renderer list 128 96 1)))
        (assert-equal +red+ (pixel pixels 128 85 10))
        (assert-equal +red+ (pixel pixels 128 85 11))
        (assert-equal +red+ (pixel pixels 128 60 30))
        (assert-equal +red+ (pixel pixels 128 109 30))
        (assert-equal +red+ (pixel pixels 128 85 49))
        (assert-equal +background+ (pixel pixels 128 85 13))
        (assert-equal +background+ (pixel pixels 128 85 30))
        (assert-equal +background+ (pixel pixels 128 85 8))
        (assert-eql 2f0 (dl:stroke-width-of (dl:display-list-kind list 0)))))))

(define-test a-radius-larger-than-the-shape-is-limited-to-the-shape ()
  ;; A 20 by 20 square with a radius of 100 becomes a circle, not nothing.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rounded-rect list 10 10 20 20 100 (dl:pack-color 255 0 0))
      (let ((pixels (render-scene device renderer list 64 48 1)))
        (assert-equal +red+ (pixel pixels 64 20 20))
        (assert-equal +background+ (pixel pixels 64 10 10))))))

;;; --- shadows ---

(defun normal-below (z)
  "The share of a standard normal distribution below Z (Abramowitz and Stegun 7.1.26, to 1.5e-7)."
  (let* ((x (/ (abs z) (sqrt 2d0)))
         (k (/ 1 (+ 1 (* 0.3275911d0 x))))
         (erf (- 1 (* k (+ 0.254829592d0
                           (* k (+ -0.284496736d0
                                   (* k (+ 1.421413741d0
                                           (* k (+ -1.453152027d0 (* k 1.061405429d0))))))))
                      (exp (- (* x x)))))))
    (if (minusp z) (* 0.5d0 (- 1 erf)) (* 0.5d0 (+ 1 erf)))))

(defun coverage (pixel)
  "How much of a white shadow covers PIXEL, over the background, from its red."
  (/ (- (first pixel) 26) (- 255 26d0)))

(define-test a-shadow-is-its-rectangle-blurred-as-a-design-tool-blurs-it ()
  ;; A white shadow of the square 40, 40 of 120, blurred by 16: a Gaussian whose standard deviation is
  ;; half the blur, 8. Across the middle of a side, far from the corners, the coverage a point d
  ;; outside the edge is the normal distribution's share below -d/8: a half at the edge, and about
  ;; 0.16 a deviation out. It reaches no further than a blur and a half, and every side is alike.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-shadow list 40 40 120 120 0 16 (dl:pack-color 255 255 255))
      (let ((pixels (render-scene device renderer list 200 200 1)))
        (flet ((at (x y) (coverage (pixel pixels 200 x y))))
          (loop :for column :from 24 :to 56
                ;; The middle of the pixel, from the left edge, outward.
                :for d = (- 40 (+ column 0.5d0))
                :do (assert-true (< (abs (- (at column 100) (normal-below (/ (- d) 8)))) 0.02)
                                 column (at column 100) (normal-below (/ (- d) 8))))
          (assert-true (< (abs (- 0.5 (/ (+ (at 39 100) (at 40 100)) 2))) 0.01))
          ;; The other sides, the same at the same distance.
          (loop :for k :from 0 :to 20
                :do (let ((left (at (- 40 k) 100)))
                      (assert-true (< (abs (- left (at (+ 159 k) 100))) 0.01) k)
                      (assert-true (< (abs (- left (at 100 (- 40 k)))) 0.01) k)
                      (assert-true (< (abs (- left (at 100 (+ 159 k)))) 0.01) k)))
          ;; Past a blur and a half (24) and a pixel, nothing at all.
          (assert-equal +background+ (pixel pixels 200 14 100))
          (assert-equal +background+ (pixel pixels 200 100 185)))))))

(define-test a-shadow-has-its-rectangles-round-corners-and-unblurred-is-the-rectangle ()
  (with-metal (device renderer)
    ;; With corners of 40, little reaches the corner of the square.
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-shadow list 40 40 120 120 40 4 (dl:pack-color 255 255 255))
      (let ((pixels (render-scene device renderer list 200 200 1)))
        (assert-true (< (coverage (pixel pixels 200 44 44)) 0.05) (pixel pixels 200 44 44))
        (assert-true (> (coverage (pixel pixels 200 100 44)) 0.95) (pixel pixels 200 100 44))))
    ;; Blurred by nothing it is the rounded rectangle itself; by less than a pixel, the same but
    ;; for a level of an edge's antialiasing, its quad being a little larger.
    (dolist (blur '(0 0.4))
      (let ((shadow (dl:make-display-list :capacity 4))
            (shape (dl:make-display-list :capacity 4)))
        (dl:push-shadow shadow 20 20 60 40 12 blur (dl:pack-color 255 0 0))
        (dl:push-rounded-rect shape 20 20 60 40 12 (dl:pack-color 255 0 0))
        (let ((a (render-scene device renderer shape 100 80 1))
              (b (render-scene device renderer shadow 100 80 1)))
          (if (zerop blur)
              (assert-equalp a b)
              (assert-true (every (lambda (x y) (<= (abs (- x y)) 2)) a b))))))))

;;; --- dashed strokes ---

(define-test a-dashed-stroke-is-on-for-its-dash-and-off-for-its-gap-clockwise-round-the-edge ()
  ;; The square 40, 40 of 120, a stroke 2 wide dashed 10 on and 10 off from the left end of its top
  ;; side, clockwise. Without corners each side is 120, so every side starts on a dash.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rounded-stroke list 40 40 120 120 0 2 (dl:pack-color 255 0 0) 10 10)
      (assert-equal '(10f0 10f0) (multiple-value-list
                                  (dl:stroke-dash-of (dl:display-list-extra list 0))))
      (let ((pixels (render-scene device renderer list 200 200 1)))
        (flet ((on (x y) (equal +red+ (pixel pixels 200 x y)))
               (off (x y) (equal +background+ (pixel pixels 200 x y))))
          ;; Along the top, left to right; down the right; along the bottom, right to left; up the
          ;; left. A dash ends sharply on its pixel's edge.
          (assert-true (and (on 45 40) (on 49 41) (off 50 40) (off 55 40) (on 65 40)))
          (assert-true (and (on 159 45) (off 159 55) (on 158 65)))
          (assert-true (and (on 154 159) (off 144 159) (on 134 158)))
          (assert-true (and (on 40 154) (off 40 144) (on 41 134)))
          ;; Inside the stroke, nothing.
          (assert-true (off 45 43))))))
  ;; With corners of 20 each side is 80 and each corner a quarter circle, 31.4: the dashes carry on
  ;; round them, so where a later side is on is where the arcs' lengths put it.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rounded-stroke list 40 40 120 120 20 2 (dl:pack-color 255 0 0) 10 10)
      (let ((pixels (render-scene device renderer list 200 200 1)))
        (flet ((on (x y) (equal +red+ (pixel pixels 200 x y)))
               (off (x y) (equal +background+ (pixel pixels 200 x y))))
          (assert-true (and (on 159 74) (off 159 84)))
          (assert-true (and (on 100 159) (on 95 159) (off 90 159) (off 85 159)))
          (assert-true (and (off 40 100) (on 40 105) (off 40 96) (on 40 108)))))))
  ;; A gap of its own, 10.75 after a dash of 10: the second dash starts at 20.75, a quarter of the
  ;; way into the pixel whose middle is at 20.5, which is a quarter covered.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rounded-stroke list 40 40 120 120 0 2 (dl:pack-color 255 0 0) 10 10.75)
      (let ((pixels (render-scene device renderer list 200 200 1)))
        (assert-equal +red+ (pixel pixels 200 45 40))
        (assert-equal +background+ (pixel pixels 200 55 40))
        (assert-equal +background+ (pixel pixels 200 59 40))
        (assert-true (near (list (round (+ 26 (* 0.25 229))) 0 0) (butlast (pixel pixels 200 60 40)) 30)
                     (pixel pixels 200 60 40))
        (assert-equal +red+ (pixel pixels 200 66 40))))))

;;; --- blending and antialiasing ---

(define-test an-edge-in-the-middle-of-a-pixel-is-half-covered ()
  ;; The rectangle starts half way through pixel 10, so that pixel is half red
  ;; over the background: 0.5 * 255 + 0.5 * 26 = 140 red, 13 green, 19 blue.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rect list 10.5 10 40 30 (dl:pack-color 255 0 0))
      (let ((pixels (render-scene device renderer list 128 96 1)))
        (assert-true (near '(140 13 19 255) (pixel pixels 128 10 20)) (pixel pixels 128 10 20))
        (assert-equal +red+ (pixel pixels 128 11 20))
        (assert-equal +background+ (pixel pixels 128 9 20))))))

(define-test translucent-colours-blend-with-what-is-under-them ()
  ;; Green at half alpha over the background, and over solid blue.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rect list 0 0 32 16 (dl:pack-color 0 0 255))
      (dl:push-rect list 16 0 32 16 (dl:pack-color 0 255 0 128))
      (let ((pixels (render-scene device renderer list 64 16 1)))
        (assert-true (near '(0 128 127 255) (pixel pixels 64 20 8)) (pixel pixels 64 20 8))
        (assert-true (near '(13 141 19 255) (pixel pixels 64 40 8)) (pixel pixels 64 40 8))))))

(define-test later-commands-draw-over-earlier-ones ()
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rect list 0 0 16 16 (dl:pack-color 255 0 0))
      (dl:push-rect list 8 0 16 16 (dl:pack-color 0 0 255))
      (let ((pixels (render-scene device renderer list 32 16 1)))
        (assert-equal +red+ (pixel pixels 32 4 8))
        (assert-equal +blue+ (pixel pixels 32 12 8))
        (assert-equal +blue+ (pixel pixels 32 20 8))))))

;;; --- the renderer ---

(define-test commands-beyond-the-renderers-capacity-are-an-error ()
  (with-metal (device renderer :capacity 2)
    (let ((list (dl:make-display-list :capacity 4)))
      (dotimes (i 3) (dl:push-rect list 0 0 4 4 (dl:pack-color 255 0 0)))
      (assert-error 'error (render-scene device renderer list 16 16 1)))))

(define-test the-renderer-times-itself ()
  (with-metal (device renderer)
    (assert-true (plusp (renderer:renderer-shader-compile-ns renderer)))
    (assert-true (plusp (renderer:renderer-pipeline-ns renderer)))
    (let ((list (dl:make-display-list :capacity 4)))
      (dl:push-rect list 0 0 4 4 (dl:pack-color 255 0 0))
      (render-scene device renderer list 16 16 1)
      (assert-true (plusp (renderer:last-frame-gpu-ns renderer))))))

;;; --- a golden image ---
;;;
;;; The picture in t/golden/shapes.ppm was made by REGENERATE-GOLDEN and looked
;;; at. Another GPU may antialias a pixel a little differently, so the comparison
;;; allows a small difference in each channel.

(defun golden-path ()
  (asdf:system-relative-pathname "almighty-bolt/tests" "t/golden/shapes.ppm"))

(defun shapes-scene ()
  "A red rectangle, a blue rounded one, and a translucent green pill over both."
  (let ((list (dl:make-display-list :capacity 8)))
    (dl:push-rect list 10 10 40 30 (dl:pack-color 255 0 0))
    (dl:push-rounded-rect list 60 10 50 40 12 (dl:pack-color 0 0 255))
    (dl:push-rounded-rect list 30 40 60 40 20 (dl:pack-color 0 255 0 128))
    list))

(defun write-ppm (path width height pixels)
  "Write BGRA PIXELS as a binary PPM (red, green, blue, no alpha)."
  (with-open-file (out path :direction :output :if-exists :supersede
                            :element-type '(unsigned-byte 8))
    (write-sequence (map '(vector (unsigned-byte 8)) #'char-code
                         (format nil "P6~%~d ~d~%255~%" width height))
                    out)
    (dotimes (i (* width height))
      (write-byte (aref pixels (+ (* 4 i) 2)) out)
      (write-byte (aref pixels (+ (* 4 i) 1)) out)
      (write-byte (aref pixels (* 4 i)) out))))

(defun read-ppm (path)
  "The width, height and red-green-blue bytes of a binary PPM."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((data (make-array (file-length in) :element-type '(unsigned-byte 8)))
           (text (progn (read-sequence data in)
                        (map 'string #'code-char (subseq data 0 (min 32 (length data)))))))
      ;; The header is "P6", the size and 255, each on its own line.
      (let* ((first-newline (position #\Newline text))
             (second-newline (position #\Newline text :start (1+ first-newline)))
             (third-newline (position #\Newline text :start (1+ second-newline)))
             (size (subseq text (1+ first-newline) second-newline))
             (space (position #\Space size)))
        (values (parse-integer size :end space)
                (parse-integer size :start (1+ space))
                (subseq data (1+ third-newline)))))))

(defun regenerate-golden ()
  "Draw the shapes scene and store it as the golden image. Look at it first."
  (with-metal (device renderer)
    (write-ppm (golden-path) 128 96 (render-scene device renderer (shapes-scene) 128 96 1))))

(define-test the-shapes-scene-matches-the-golden-image ()
  (assert-true (probe-file (golden-path))
               "The golden image is missing. Run REGENERATE-GOLDEN in this package.")
  (when (probe-file (golden-path))
    (with-metal (device renderer)
      (multiple-value-bind (width height expected) (read-ppm (golden-path))
        (let ((pixels (render-scene device renderer (shapes-scene) width height 1))
              (worst 0))
          (dotimes (i (* width height))
            (loop :for channel :from 0 :below 3
                  :do (setf worst (max worst
                                       (abs (- (aref expected (+ (* 3 i) channel))
                                               ;; The pixels are blue, green, red; the
                                               ;; picture is red, green, blue.
                                               (aref pixels (+ (* 4 i) (- 2 channel)))))))))
          (assert-true (<= worst 3) worst))))))

;;; --- allocation ---
;;;
;;; The frame loop is a function compiled with this file, not a test body:
;;; lisp-unit2 evaluates test bodies under a debug-heavy policy that allocates
;;; where compiled code does not.

(defun draw-frames (renderer display-list target count)
  (let ((pass (target:offscreen-target-descriptor target)))
    (dotimes (frame count)
      (objc:with-autorelease-pool
        (dl:clear-display-list display-list)
        (dotimes (i 200)
          (dl:push-rounded-rect display-list
                                (float (* 2 (mod i 50)) 1f0) (float (* 3 (floor i 50)) 1f0)
                                8f0 8f0 3f0 #xFF8000FF))
        (renderer:render-display-list renderer display-list pass 0 128 96 1)))
    (renderer:wait-for-last-frame renderer)))

(define-test building-and-drawing-frames-allocates-nothing ()
  (with-metal (device renderer :capacity 256)
    (let* ((target (target:make-offscreen-target device 128 96))
           (list (dl:make-display-list :capacity 256)))
      ;; Warm up, so that one-time work does not count.
      (draw-frames renderer list target 20)
      (let ((before (sb-ext:get-bytes-consed)))
        (draw-frames renderer list target 10000)
        (let ((delta (- (sb-ext:get-bytes-consed) before)))
          ;; 10,000 frames of 200 commands. Allocating 32 bytes a frame would be
          ;; 320 KB, and a region is about 64 KB, so allow 300 KB.
          (assert-true (< delta 300000) delta))))))

;;; --- clipping ---

(define-test a-clip-keeps-what-is-drawn-inside-it-and-stretches-after-it-are-drawn-in-order ()
  ;; A red rectangle across the top is drawn first; then, clipped to a box, a blue one
  ;; that is larger than the box; then, after the clip, a green one.
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 16)))
      (dl:push-rect list 0 0 100 10 (dl:pack-color 255 0 0))
      (dl:push-clip list 20 20 30 30)
      (dl:push-rect list 0 15 100 60 (dl:pack-color 0 0 255))
      (dl:push-unclip list)
      (dl:push-rect list 60 60 20 20 (dl:pack-color 0 255 0))
      (let ((pixels (render-scene device renderer list 100 100 1)))
        (assert-equal +red+ (pixel pixels 100 5 5))
        ;; The blue is only inside the box.
        (assert-equal +blue+ (pixel pixels 100 25 25))
        (assert-equal +blue+ (pixel pixels 100 49 49))
        (assert-equal +background+ (pixel pixels 100 19 25))
        (assert-equal +background+ (pixel pixels 100 25 19))
        (assert-equal +background+ (pixel pixels 100 50 25))
        (assert-equal +background+ (pixel pixels 100 25 50))
        (assert-equal +background+ (pixel pixels 100 90 30))
        ;; After the clip, drawing is unclipped again, and in the right place.
        (assert-equal '(0 255 0 255) (pixel pixels 100 70 70))))))

(define-test clips-inside-clips-are-the-overlap-and-an-end-restores-the-outer-one ()
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 16)))
      (dl:push-clip list 10 10 40 40)
      (dl:push-clip list 30 30 40 40)
      (dl:push-rect list 0 0 100 100 (dl:pack-color 255 0 0))
      (dl:push-unclip list)
      (dl:push-rect list 0 0 100 100 (dl:pack-color 0 0 255))
      (dl:push-unclip list)
      (let ((pixels (render-scene device renderer list 100 100 1)))
        ;; Red where the two boxes overlap (30 to 49), then blue over the first box
        ;; (10 to 49) which hides it, and nothing outside the first box.
        (assert-equal +blue+ (pixel pixels 100 35 35))
        (assert-equal +blue+ (pixel pixels 100 15 15))
        (assert-equal +background+ (pixel pixels 100 55 55))
        (assert-equal +background+ (pixel pixels 100 5 5))))))

(define-test a-clip-is-in-points-and-scales-with-the-display ()
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 8)))
      (dl:push-clip list 10 10 20 20)
      (dl:push-rect list 0 0 100 100 (dl:pack-color 255 0 0))
      (dl:push-unclip list)
      (let ((pixels (render-scene device renderer list 50 50 2)))
        (assert-equal +background+ (pixel pixels 100 19 30))
        (assert-equal +red+ (pixel pixels 100 20 30))
        (assert-equal +red+ (pixel pixels 100 59 30))
        (assert-equal +background+ (pixel pixels 100 60 30))))))

(define-test a-clip-outside-the-target-or-of-no-area-draws-nothing ()
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 16)))
      (dl:push-clip list 200 200 50 50)
      (dl:push-rect list 0 0 100 100 (dl:pack-color 255 0 0))
      (dl:push-unclip list)
      (dl:push-clip list 10 10 0 0)
      (dl:push-rect list 0 0 100 100 (dl:pack-color 255 0 0))
      (dl:push-unclip list)
      (dl:push-rect list 90 90 5 5 (dl:pack-color 0 0 255))
      (let ((pixels (render-scene device renderer list 100 100 1)))
        (assert-equal +background+ (pixel pixels 100 50 50))
        (assert-equal +blue+ (pixel pixels 100 92 92))))))

(define-test an-unmatched-end-of-clip-is-ignored ()
  (with-metal (device renderer)
    (let ((list (dl:make-display-list :capacity 8)))
      (dl:push-unclip list)
      (dl:push-rect list 10 10 20 20 (dl:pack-color 255 0 0))
      (let ((pixels (render-scene device renderer list 50 50 1)))
        (assert-equal +red+ (pixel pixels 50 15 15))))))
