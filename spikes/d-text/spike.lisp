(defpackage #:almighty-bolt-spikes/d-text
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:msg-send
                #:objc-class
                #:with-autorelease-pool
                #:with-masked-float-traps)
  (:import-from #:almighty-bolt-core/platform/macos/view
                #:metal-view-object
                #:metal-view-device)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt-core/render/metal/target
                #:+pixel-format-bgra8-unorm+
                #:make-offscreen-target
                #:offscreen-target-descriptor)
  (:import-from #:almighty-bolt-core/render/metal/renderer
                #:make-renderer
                #:renderer-atlas
                #:render-display-list
                #:wait-for-last-frame
                #:last-frame-gpu-ns)
  (:import-from #:almighty-bolt-core/render/metal/glyph-atlas
                #:glyph-atlas-count
                #:atlas-glyph-index)
  (:import-from #:almighty-bolt-core/render/metal/draw-text
                #:push-line)
  (:import-from #:almighty-bolt-core/display-list
                #:make-display-list
                #:clear-display-list
                #:display-list-count
                #:push-rounded-rect
                #:pack-color)
  (:import-from #:almighty-bolt-core/text/font
                #:make-font
                #:system-font)
  (:import-from #:almighty-bolt-core/text/line
                #:shape-line
                #:get-line
                #:break-lines
                #:text-line-width
                #:text-line-glyph-count
                #:text-line-glyphs
                #:text-line-glyph-fonts)
  (:local-nicknames (#:b #:almighty-bolt-spikes/b-own)
                    (#:clock #:almighty-bolt-core/clock))
  (:export #:main
           #:make-frame-function
           #:benchmark))

(in-package #:almighty-bolt-spikes/d-text)

;;; Spike D: text. In a window, behind the same acceptance script as A, B and C, a
;;; heading, a paragraph broken to a width, text in several scripts and a monospaced
;;; line; and without one, a benchmark of what each stage of text costs.

(defparameter +paragraph+
  "Text is shaped by Core Text, drawn by Core Graphics into a texture atlas, and put on screen by the Metal renderer. A paragraph breaks into lines that fit its width, and every line is shaped once and kept.")

(defun screen-scale ()
  (msg-send :double (msg-send :id (objc-class "NSScreen") "mainScreen") "backingScaleFactor"))

(defun layout-lines (font width)
  "The cached lines of +PARAGRAPH+ broken to WIDTH points."
  (loop :for (start end) :in (break-lines font +paragraph+ width)
        :collect (get-line font (string-right-trim " " (subseq +paragraph+ start end)))))

(defun make-frame-function (rig view)
  (declare (ignore rig))
  (let* ((object (metal-view-object view))
         (scale (screen-scale))
         (renderer (make-renderer (metal-view-device view)
                                  (msg-send :ulong object "colorPixelFormat")
                                  :capacity 4096 :scale scale))
         (atlas (renderer-atlas renderer))
         (display-list (make-display-list :capacity 4096))
         (heading (get-line (make-font "Helvetica" 28) "Text in a window"))
         (body-font (system-font 15))
         (body (layout-lines body-font 440))
         (scripts (list (get-line body-font "Latin: Hello, wörld! ünïcödé")
                        (get-line body-font "日本語: こんにちは、世界")
                        (get-line body-font "Ελληνικά: Γειά σου κόσμε")
                        (get-line body-font "العربية: مرحبا")))
         (code (get-line (make-font "Menlo" 13) "(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))"))
         (white (pack-color 255 255 255))
         (grey (pack-color 190 195 210))
         (amber (pack-color 255 200 80))
         (green (pack-color 120 220 160)))
    (lambda (rig view)
      (declare (ignore rig view))
      (let ((pass (msg-send :id object "currentRenderPassDescriptor"))
            (drawable (msg-send :id object "currentDrawable")))
        (unless (or (zerop pass) (zerop drawable))
          (multiple-value-bind (x y width height) (msg-send :rect object "bounds")
            (declare (ignore x y))
            (clear-display-list display-list)
            (push-rounded-rect display-list 8 8 (- width 16) (- height 16) 10 (pack-color 36 40 56))
            (push-line display-list atlas heading 24f0 46f0 white)
            (let ((y 74f0))
              (declare (single-float y))
              (dolist (line body)
                (push-line display-list atlas line 24f0 y grey)
                (incf y 20f0))
              (incf y 8f0)
              (dolist (line scripts)
                (push-line display-list atlas line 24f0 y amber)
                (incf y 22f0))
              (push-line display-list atlas code 24f0 (+ y 8f0) green))
            (render-display-list renderer display-list pass drawable width height scale)))))))

(defun main (&key (frames 300))
  "Open the window, run the acceptance script with text, and quit."
  (b:main :frames frames
          :title "Spike D: text, paused MTKView"
          :make-frame-function #'make-frame-function))

;;; The benchmark

(defparameter +alphabet+
  "abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789 .,;:!?-_()[]{}<>/\\|@#$%^&*+=~")

(defun make-test-string (length seed)
  "A string of LENGTH characters made of the alphabet, different for each SEED."
  (let ((string (make-string length)))
    (dotimes (i length string)
      (setf (char string i) (char +alphabet+ (mod (+ (* i 7) (* seed 13)) (length +alphabet+)))))))

(defun time-per-call-ns (count thunk)
  (declare (type fixnum count) (type function thunk))
  (let ((start (clock:now-ns)))
    (dotimes (i count) (funcall thunk))
    (/ (- (clock:now-ns) start) count 1.0)))

(defun bytes-per-call (count thunk)
  (declare (type fixnum count) (type function thunk))
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i count) (funcall thunk))
    (/ (- (sb-ext:get-bytes-consed) before) count 1.0)))

(defun push-lines-repeatedly (display-list atlas line count)
  "Build COUNT lists of glyph commands from LINE; returns the commands made."
  (declare (type fixnum count))
  (let ((total 0))
    (declare (type fixnum total))
    (dotimes (i count total)
      (clear-display-list display-list)
      (incf total (push-line display-list atlas line 10f0 30f0 #xFFFFFFFF)))))

(defun benchmark ()
  "Time each stage of text without a window and print a table."
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((font (make-font "Helvetica" 16))
             (device (default-device))
             (renderer (make-renderer device +pixel-format-bgra8-unorm+ :capacity 16384 :scale 2))
             (atlas (renderer-atlas renderer))
             (display-list (make-display-list :capacity 16384)))
        (format t "~&--- Spike D: what text costs, without a window ---~%")
        (format t "~%Shaping a new string (Core Text), and finding one already shaped:~%")
        (format t "~10a ~16a ~16a ~16a~%" "length" "shape (us)" "cache hit (ns)" "hit conses (B)")
        (dolist (length '(10 100 1000))
          (let ((strings (loop :for seed :below 300 :collect (make-test-string length seed)))
                (n 0))
            (let ((shape-ns (time-per-call-ns 300 (lambda () (shape-line font (nth (mod n 300) strings))
                                                       (incf n))))
                  (line (get-line font (first strings))))
              (declare (ignore line))
              (get-line font (first strings))
              (let ((hit-ns (time-per-call-ns 200000 (lambda () (get-line font (first strings)))))
                    (hit-bytes (bytes-per-call 200000 (lambda () (get-line font (first strings))))))
                (format t "~10d ~16,1f ~16,1f ~16,1f~%" length (/ shape-ns 1000) hit-ns hit-bytes)))))
        (format t "~%Rasterizing glyphs into the atlas, at 2 pixels a point:~%")
        (let* ((line (get-line font +alphabet+))
               (start (clock:now-ns))
               (new-glyphs 0))
          (dotimes (i (text-line-glyph-count line))
            (let ((before (glyph-atlas-count atlas)))
              (atlas-glyph-index atlas (aref (text-line-glyph-fonts line) i) (aref (text-line-glyphs line) i))
              (incf new-glyphs (- (glyph-atlas-count atlas) before))))
          (format t "~d glyphs rasterized and uploaded: ~,1f us each~%"
                  new-glyphs (/ (- (clock:now-ns) start) (max 1 new-glyphs) 1000.0)))
        (format t "~%Building glyph commands for a line already in the atlas:~%")
        (format t "~10a ~18a ~16a~%" "glyphs" "per glyph (ns)" "bytes consed")
        (dolist (length '(10 100 1000))
          (let ((line (get-line font (make-test-string length 99))))
            (push-lines-repeatedly display-list atlas line 2)       ; the first time rasterizes
            (let* ((count (max 20 (floor 200000 length)))
                   (before (sb-ext:get-bytes-consed))
                   (start (clock:now-ns))
                   (commands (push-lines-repeatedly display-list atlas line count)))
              (format t "~10d ~18,1f ~16d~%" length
                      (/ (- (clock:now-ns) start) (max 1 commands) 1.0)
                      (- (sb-ext:get-bytes-consed) before)))))
        (format t "~%A frame of text, 960 x 640 pixels, encoded and drawn:~%")
        (format t "~10a ~14a ~14a ~12a~%" "glyphs" "encode (us)" "GPU (us)" "consed/frame")
        (let ((target (make-offscreen-target device 960 640)))
          (dolist (length '(100 1000 5000))
            (let ((line (get-line font (make-test-string length 5))))
              (clear-display-list display-list)
              (let ((glyphs (push-line display-list atlas line 0f0 20f0 #xFFFFFFFF)))
                (flet ((frame ()
                         (with-autorelease-pool
                           (render-display-list renderer display-list (offscreen-target-descriptor target)
                                                0 480 320 2))
                         (wait-for-last-frame renderer)))
                  (dotimes (i 20) (frame))
                  (let ((before (sb-ext:get-bytes-consed)) (encode 0) (gpu 0) (frames 500))
                    (dotimes (i frames)
                      (let ((t0 (clock:now-ns)))
                        (with-autorelease-pool
                          (render-display-list renderer display-list (offscreen-target-descriptor target)
                                               0 480 320 2))
                        (incf encode (- (clock:now-ns) t0))
                        (wait-for-last-frame renderer)
                        (incf gpu (last-frame-gpu-ns renderer))))
                    (format t "~10d ~14,1f ~14,1f ~12,1f~%" glyphs
                            (/ encode frames 1000.0) (/ gpu frames 1000.0)
                            (/ (- (sb-ext:get-bytes-consed) before) frames 1.0))))))))
        (finish-output)))))
