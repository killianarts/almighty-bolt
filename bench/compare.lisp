(defpackage #:almighty-bolt-bench/compare
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:rw #:almighty-bolt-variant-retained/widgets)
                    (#:rn #:almighty-bolt-variant-retained/node)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:sig #:almighty-bolt-variant-reactive/signals)
                    (#:rv #:almighty-bolt-variant-reactive/views)
                    (#:reactive #:almighty-bolt-variant-reactive/reactive)
                    (#:iw #:almighty-bolt-variant-immediate/widgets)
                    (#:immediate #:almighty-bolt-variant-immediate/immediate))
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
  (:export #:main))

(in-package #:almighty-bolt-bench/compare)

;;; The same screen in each variant, a column of rows each a line of text and a button, one
;;; row's text or colour changed between frames, with the real text engine and glyph atlas
;;; and no window or GPU work: what each variant adds to a frame, which is the "build" of the
;;; frame ring. The retained figures are those of =make bench-retained= (with its caches);
;;; the reactive variant is the retained one plus describing, comparing and tracking, and
;;; changes the middle row through signals read in a component of its own; the immediate
;;; variant calls its function for every frame, with the strings of the rows made once, as an
;;; application that holds its data would, and skips what it can by comparison (0016).

(defconstant +ink+ #xFF8040FF)
(defconstant +plain+ #xE8EAF2FF)

(defun row-text (i)
  (format nil "Row ~d: the quick brown fox jumps over the lazy dog" i))

;;; Each constructor returns a variant and a function of one argument, :COLOR or :TEXT, that
;;; changes the middle row's label the way the scenario does.

(defun retained-screen (rows)
  (let* ((labels (loop :for i :below rows :collect (rw:label (row-text i))))
         (root (apply #'rw:column
                      (append (loop :for label :in labels
                                    :collect (rw:row label (rw:button "Go") :gap 8
                                                                            :cross-align :center))
                              (list :gap 2))))
         (target (nth (floor rows 2) labels))
         (original (rw:label-text target))
         (other (substitute #\X #\o original))
         (toggle nil))
    (values (retained:make-retained-variant root)
            (lambda (what)
              (setf toggle (not toggle))
              (ecase what
                (:color (setf (rw:label-color target) (if toggle +ink+ +plain+)))
                (:text (setf (rw:label-text target) (if toggle other original))))))))

(defun reactive-screen (rows)
  (let* ((middle (floor rows 2))
         (original (row-text middle))
         (other (substitute #\X #\o original))
         (text (sig:make-signal original))
         (color (sig:make-signal +plain+))
         (toggle nil))
    (values (reactive:make-reactive-variant
             (lambda ()
               (apply #'rv:column
                      (append (loop :for i :below rows
                                    :collect (rv:row (if (= i middle)
                                                         (rv:component
                                                          (lambda ()
                                                            (rv:label (sig:value text)
                                                                      :color (sig:value color))))
                                                         (rv:label (row-text i)))
                                                     (rv:button "Go") :gap 8
                                                                      :cross-align :center))
                              (list :gap 2)))))
            (lambda (what)
              (setf toggle (not toggle))
              (ecase what
                (:color (setf (sig:value color) (if toggle +ink+ +plain+)))
                (:text (setf (sig:value text) (if toggle other original))))))))

(defun immediate-screen (rows)
  (let* ((middle (floor rows 2))
         (strings (coerce (loop :for i :below rows :collect (row-text i)) 'simple-vector))
         (original (svref strings middle))
         (other (substitute #\X #\o original))
         (text original)
         (color +plain+)
         (toggle nil))
    (values (immediate:make-immediate-variant
             (lambda ()
               (iw:column (:gap 2)
                 (dotimes (i rows)
                   (iw:row (:gap 8 :cross-align :center)
                     (if (= i middle)
                         (iw:label text :color color)
                         (iw:label (svref strings i)))
                     (iw:button "Go"))))))
            (lambda (what)
              (setf toggle (not toggle))
              (ecase what
                (:color (setf color (if toggle +ink+ +plain+)))
                (:text (setf text (if toggle other original))))))))

(defun time-frames (variant context list frames mutate)
  "Mean nanoseconds of a frame of VARIANT, and bytes consed in a frame, over FRAMES frames,
after warming up. MUTATE, if given, is called before each frame, and is not timed."
  (flet ((one ()
           (when mutate (funcall mutate))
           (let ((start (core:now-ns)))
             (core:settle-frame variant context list)
             (- (core:now-ns) start))))
    (dotimes (i 50) (one))
    (let ((total 0)
          (consed (sb-ext:get-bytes-consed)))
      (dotimes (i frames)
        (incf total (one)))
      (values (/ total frames 1.0)
              (/ (- (sb-ext:get-bytes-consed) consed) frames 1.0)))))

(defun measure (constructor rows context frames what)
  "Microseconds and bytes of a frame in which WHAT (NIL, :COLOR or :TEXT) changes."
  (multiple-value-bind (variant change) (funcall constructor rows)
    (let ((list (core:make-display-list :capacity 65536)))
      (multiple-value-bind (ns bytes)
          (time-frames variant context list frames
                       (and what (lambda () (funcall change what))))
        (values (/ ns 1000.0) bytes (core:display-list-count list))))))

(defun report (context rows frames)
  (format t "~&~%--- ~d rows ---~%" rows)
  (format t "~22a ~18a ~18a ~18a~%" "" "retained" "reactive" "immediate")
  (dolist (scenario '(("nothing changes" nil) ("one colour changes" :color)
                      ("one text changes" :text)))
    (flet ((cell (constructor)
             (multiple-value-bind (us bytes) (measure constructor rows context frames
                                                      (second scenario))
               (format nil "~9,1f us ~6,0f B" us bytes))))
      (format t "~22a ~18a ~18a ~18a~%" (first scenario) (cell #'retained-screen)
              (cell #'reactive-screen) (cell #'immediate-screen)))))

(defun main (&key (rows '(10 100 1000)) (frames 200))
  "Print a table of what a frame costs in each variant for each number of rows."
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((host (make-host (make-instance 'core:variant) (default-device)
                              +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
             (context (host-context host)))
        (setf (core:context-width context) 800f0
              (core:context-height context) 600f0)
        (format t "~&A frame of each variant, mean of ~d frames: microseconds, then bytes~%"
                frames)
        (dolist (count rows)
          (report context count frames))
        (finish-output)))))
