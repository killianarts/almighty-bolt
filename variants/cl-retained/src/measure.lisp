(defpackage #:almighty-bolt-variant-retained/measure
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:now-ns
                #:variant
                #:settle-frame
                #:make-display-list
                #:replace-display-list
                #:append-display-list
                #:display-list-count
                #:display-list-kind
                #:display-list-x
                #:display-list-y
                #:display-list-width
                #:display-list-height
                #:display-list-parameter
                #:display-list-radius
                #:display-list-color
                #:+command-rect+
                #:+command-rounded-rect+
                #:+command-glyph+
                #:push-rect
                #:push-rounded-rect
                #:push-glyph
                #:context-width
                #:context-height)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt-core/platform/macos/host
                #:make-host
                #:host-context)
  (:import-from #:almighty-bolt-core/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt-core/render/metal/target
                #:+pixel-format-bgra8-unorm+)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:label-text
                #:label-color
                #:button
                #:row
                #:column)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-children)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:main))

(in-package #:almighty-bolt-variant-retained/measure)

;;; What painting costs the retained variant, with and without the caches in its nodes.
;;; The variant's frame (its layout and its painting, through the settle loop) is timed
;;; alone, with the real text engine and the real glyph atlas but no window and no GPU
;;; work: the time is what the variant adds to a frame, the "build" of the frame ring.
;;; The tree is a column of rows, each a line of text and a button. The floor is
;;; copying a finished list, as the null variant does.
;;;
;;; A node that has kept its commands replays them with the core's APPEND-DISPLAY-LIST
;;; (decision record 0011). To show what that operation is worth, the middle column
;;; replaces it, while it runs, with a copy a command at a time that uses only the
;;; core's accessors and its PUSH-RECT, PUSH-ROUNDED-RECT and PUSH-GLYPH, which is what
;;; a variant could have written without it.

(defun append-command-by-command (target source &optional (dx 0f0) (dy 0f0))
  "What a variant could do without APPEND-DISPLAY-LIST."
  (declare (type single-float dx dy))
  (dotimes (i (display-list-count source))
    (let ((x (+ (display-list-x source i) dx))
          (y (+ (display-list-y source i) dy))
          (width (display-list-width source i))
          (height (display-list-height source i))
          (color (display-list-color source i)))
      (declare (type single-float x y width height))
      (ecase (display-list-kind source i)
        (#.+command-rect+
         (push-rect target x y width height color))
        (#.+command-rounded-rect+
         (push-rounded-rect target x y width height (display-list-radius source i) color))
        (#.+command-glyph+
         (push-glyph target x y width height (display-list-parameter source i) color)))))
  target)

(defun call-with-copy-in-the-variant (function)
  "Call FUNCTION with APPEND-DISPLAY-LIST replaced by the copy a command at a time."
  (let ((original (fdefinition 'append-display-list)))
    (setf (fdefinition 'append-display-list) #'append-command-by-command)
    (unwind-protect (funcall function)
      (setf (fdefinition 'append-display-list) original))))

(defun make-rows (count)
  "A column of COUNT rows of a label and a button. Returns it and a vector of the labels."
  (let ((rows '())
        (labels '()))
    (dotimes (i count)
      (let ((label (label (format nil "Row ~d: the quick brown fox jumps over the lazy dog" i))))
        (push label labels)
        (push (row label (button "Go") :gap 8 :cross-align :center) rows)))
    (values (apply #'column (append (nreverse rows) (list :gap 2)))
            (coerce (nreverse labels) 'simple-vector))))

(defun time-frames (variant context list frames mutate)
  "Mean nanoseconds of a frame of VARIANT, and bytes consed in a frame, over FRAMES
frames, after warming up. MUTATE, if given, is called before each frame, and is not
timed."
  (flet ((one ()
           (when mutate (funcall mutate))
           (let ((start (now-ns)))
             (settle-frame variant context list)
             (- (now-ns) start))))
    (dotimes (i 50) (one))
    (let ((total 0)
          (consed (sb-ext:get-bytes-consed)))
      (dotimes (i frames)
        (incf total (one)))
      (values (/ total frames 1.0)
              (/ (- (sb-ext:get-bytes-consed) consed) frames 1.0)))))

(defun scenarios (labels)
  "The things that can change between frames, as (name . function), for the label in
the middle of LABELS. The function is NIL for nothing."
  (let* ((target (svref labels (floor (length labels) 2)))
         (original (label-text target))
         (other (substitute #\X #\o original))
         (toggle nil))
    (list (cons "nothing changes" nil)
          (cons "one colour changes"
                (lambda ()
                  (setf toggle (not toggle)
                        (label-color target) (if toggle #xFF8040FF #xE8EAF2FF))))
          (cons "one text changes"
                (lambda ()
                  (setf toggle (not toggle)
                        (label-text target) (if toggle other original)))))))

(defun measure-rows (context rows frames)
  "The mean frame, and the bytes consed in one, for each scenario in each mode, as
lists of (scenario mode nanoseconds bytes commands). The modes are :NONE (no caches),
:VARIANT (caches, replayed with the copy a command at a time) and :CORE (caches, replayed
with the core's operation)."
  (let ((results '()))
    (dolist (mode '(:none :variant :core))
      (multiple-value-bind (tree labels) (make-rows rows)
        (let ((variant (make-retained-variant tree :caching (not (eq mode :none))))
              (list (make-display-list :capacity 65536)))
          (flet ((run ()
                   (dolist (scenario (scenarios labels))
                     (multiple-value-bind (ns bytes)
                         (time-frames variant context list frames (cdr scenario))
                       (push (list (car scenario) mode ns bytes (display-list-count list))
                             results)))))
            (if (eq mode :variant)
                (call-with-copy-in-the-variant #'run)
                (run))))))
    (nreverse results)))

(defun copy-floor (list)
  "Nanoseconds to copy LIST into another, which is all the null variant does."
  (let ((target (make-display-list :capacity 65536)))
    (dotimes (i 20) (replace-display-list target list))
    (let ((start (now-ns)))
      (dotimes (i 1000) (replace-display-list target list))
      (/ (- (now-ns) start) 1000.0))))

(defun node-count (root)
  (1+ (loop :for child :across (node-children root) :sum (node-count child))))

(defun report (context rows frames)
  (multiple-value-bind (tree) (make-rows rows)
    (let* ((results (measure-rows context rows frames))
           (commands (fifth (first results)))
           (list (make-display-list :capacity 65536)))
      (settle-frame (make-retained-variant tree) context list)
      (format t "~&~%--- ~d rows: ~:d nodes, ~:d commands in the list ---~%"
              rows (node-count tree) commands)
      (format t "~28a ~12a ~16a ~16a~%" "" "no caches" "caches, copied" "caches, copied")
      (format t "~28a ~12a ~16a ~16a~%" "" "" "by the variant" "by the core")
      (dolist (name '("nothing changes" "one colour changes" "one text changes"))
        (flet ((cell (mode)
                 (let ((r (find-if (lambda (r) (and (string= (first r) name) (eq (second r) mode)))
                                   results)))
                   (format nil "~9,1f us" (/ (third r) 1000.0)))))
          (format t "~28a ~12a ~16a ~16a~%" name (cell :none) (cell :variant) (cell :core))))
      (format t "~28a ~9,1f us~%" "copying the finished list" (/ (copy-floor list) 1000.0))
      (format t "~28a ~,1f bytes~%" "most consed in any frame"
              (reduce #'max results :key #'fourth)))))

(defun main (&key (rows '(10 100 1000)) (frames 200))
  "Print a table of what a frame costs for each number of rows."
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((host (make-host (make-instance 'variant) (default-device)
                              +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
             (context (host-context host)))
        (setf (context-width context) 800f0
              (context-height context) 600f0)
        (format t "~&The retained variant's frame, mean of ~d frames; bytes are consed in a frame~%"
                frames)
        (dolist (count rows)
          (report context count frames))
        (finish-output)))))
