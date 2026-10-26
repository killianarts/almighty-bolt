(defpackage #:almighty-bolt-bench/cursor-spike
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:compare #:almighty-bolt-variant-support/compare)
                    (#:layout #:almighty-bolt-7guis/layout/cursor)
                    (#:dialog #:almighty-bolt-7guis/dialog/cursor))
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
  (:export #:main
           #:sweep-widths
           #:run-sweep
           #:truth-lists))

(in-package #:almighty-bolt-bench/cursor-spike)

;;; The cursor experiment of record 0022: two screens, the layout spike (every size comes from
;;; the window, down) and the dialog (a card in the middle of the window, as wide and as tall
;;; as what is in it), drawn by the cursor front end in each of its modes, in a window that
;;; is made a width wider or narrower for every frame, against what the box tree draws at that
;;; width. A frame is right if it draws what the box tree draws, to a hundredth of a point.

(defparameter +modes+ '(:lag :request :rebuild :measure :fragment))
(defparameter +tasks+ '(:layout :dialog))
(defparameter +tolerance+ 0.01f0)

(defun sweep-widths (order)
  "The widths 280 to 900 by 2, in an ORDER: :up, :down or :jumps (a fixed shuffle, so that a
step is as likely to be large as small)."
  (let ((widths (loop :for width :from 280 :to 900 :by 2 :collect (float width 1f0))))
    (ecase order
      (:up widths)
      (:down (reverse widths))
      (:jumps (let ((vector (coerce widths 'vector))
                    (state 12345))
                (loop :for i :from (1- (length vector)) :downto 1
                      :do (setf state (mod (+ (* state 1103515245) 12345) 2147483648))
                          (rotatef (aref vector i) (aref vector (mod state (1+ i)))))
                (coerce vector 'list))))))

(defun box-tree-app (task)
  (funcall (registry:app-constructor (registry:find-app :immediate task))))

(defun make-app (task mode)
  "The front end for TASK in MODE; :box is the box-tree variant, for its time."
  (if (eq mode :box)
      (box-tree-app task)
      (ecase task
        (:layout (layout:make-layout mode))
        (:dialog (dialog:make-dialog mode)))))

(defun copy-list-of (list)
  (let ((copy (core:make-display-list :capacity (max 1 (core:display-list-count list)))))
    (core:replace-display-list copy list)))

(defun truth-lists (task context widths)
  "What the box tree draws at each of WIDTHS, as a hash table of copies."
  (let ((truths (make-hash-table :test 'eql))
        (list (core:make-display-list :capacity 65536)))
    (setf (core:context-height context) 420f0)
    (dolist (width widths)
      (setf (core:context-width context) width)
      (core:settle-frame (box-tree-app task) context list)
      (setf (gethash width truths) (copy-list-of list)))
    truths))

(defun difference (list truth)
  "How far LIST is from TRUTH: the largest difference in points, or :count or :commands if
they are not even the same commands."
  (multiple-value-bind (difference why) (compare:compare-display-lists list truth)
    (or why difference)))

(defun right-p (list truth)
  (let ((difference (difference list truth)))
    (and (numberp difference) (<= difference +tolerance+))))

(defun run-sweep (task mode context widths truths)
  "Draw TASK in MODE at every one of WIDTHS in turn, with one front end, as a window that is
resized does. Returns a plist: :steps, :first-wrong (steps whose first frame differs from the
box tree's), :final-wrong (steps that still do after the frames asked for), :extra-frames
(frames asked for), :builds (a hash table, builds to count), :most-frames in a step and the
mean microseconds a step took."
  (let ((object (make-app task mode))
        (list (core:make-display-list :capacity 65536))
        (builds (make-hash-table))
        (first-wrong 0) (final-wrong 0) (extra-frames 0) (most-frames 1) (steps 0) (total 0)
        (worst 0f0))
    (setf (core:context-height context) 420f0)
    ;; The first step is the front end's first frame; the steps count from the second.
    (loop :for width :in widths
          :for step :from 0
          :do (setf (core:context-width context) width)
              (let ((frames 0)
                    (timed 0))
                (loop
                  (incf frames)
                  (let ((start (core:now-ns)))
                    (incf (gethash (core:settle-frame object context list) builds 0))
                    (incf timed (- (core:now-ns) start)))
                  (when (and (= frames 1) (plusp step) (not (right-p list (gethash width truths))))
                    (incf first-wrong))
                  (unless (core:take-frame-request context) (return))
                  (when (>= frames 10) (return)))
                (when (plusp step)
                  (incf steps)
                  (incf total timed)
                  (incf extra-frames (1- frames))
                  (setf most-frames (max most-frames frames))
                  (unless (right-p list (gethash width truths))
                    (incf final-wrong)
                    (let ((difference (difference list (gethash width truths))))
                      (setf worst (if (numberp difference) (max worst difference) 1f6)))))))
    (list :steps steps :first-wrong first-wrong :final-wrong final-wrong
          :extra-frames extra-frames :most-frames most-frames :builds builds :worst worst
          :microseconds (/ total (max 1 steps) 1000.0))))

(defun cold-start (task mode context widths truths)
  "How many of WIDTHS a new front end's first frame is wrong at, and how many frames it asks
for after it."
  (let ((wrong 0) (asked 0)
        (list (core:make-display-list :capacity 65536)))
    (setf (core:context-height context) 420f0)
    (dolist (width widths)
      (setf (core:context-width context) width)
      (let ((object (make-app task mode)))
        (core:settle-frame object context list)
        (unless (right-p list (gethash width truths)) (incf wrong))
        (when (core:take-frame-request context) (incf asked))))
    (values wrong asked)))

(defun builds-text (table)
  (let ((total 0) (count 0) (most 0))
    (maphash (lambda (builds n) (incf total (* builds n)) (incf count n) (setf most (max most builds)))
             table)
    (format nil "~,2f/~d" (/ total (max 1 count)) most)))

(defun report (label context)
  (format t "~&~%=== ~a ===~%" label)
  (let ((widths (sweep-widths :up)))
    (dolist (task +tasks+)
      (let ((truths (truth-lists task context widths)))
        (format t "~&~%~(~a~): ~d widths. Cold start, a new front end at each width: wrong first frame / frames asked for~%"
                task (length widths))
        (dolist (mode +modes+)
          (multiple-value-bind (wrong asked) (cold-start task mode context widths truths)
            (format t "~&  ~(~9a~) ~3d / ~3d~%" mode wrong asked)))
        (dolist (order '(:up :down :jumps))
          (format t "~&~(~a~), widths ~(~a~), one front end: first frame wrong / final wrong / frames asked for (most in a step) / builds mean/most / microseconds a step / worst difference, points~%"
                  task order)
          (dolist (mode (cons :box +modes+))
            (let ((result (run-sweep task mode context (sweep-widths order) truths)))
              (format t "~&  ~(~9a~) ~3d / ~3d / ~3d (~d) / ~a / ~7,1f / ~,2f~%" mode
                      (getf result :first-wrong) (getf result :final-wrong)
                      (getf result :extra-frames) (getf result :most-frames)
                      (builds-text (getf result :builds)) (getf result :microseconds)
                      (getf result :worst)))))))))

(defun main ()
  (format t "~&The cursor experiment (record 0022): a step is one window width; right means~%the box tree's display list to ~,2f of a point.~%" +tolerance+)
  (report "fake text, scale 1" (fake:make-fake-context :width 640f0 :height 420f0))
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((host (make-host (make-instance 'core:variant) (default-device)
                              +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2)))
        (dolist (scale '(1f0 2f0))
          (setf (core:context-scale (host-context host)) scale)
          (report (format nil "Core Text, scale ~d" (round scale)) (host-context host))))))
  (finish-output))
