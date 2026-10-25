(defpackage #:almighty-bolt-bench/layout-spike
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:registry #:almighty-bolt-7guis/common/registry))
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

(in-package #:almighty-bolt-bench/layout-spike)

;;; The layout spike of the charter (H6) with the real text engine and no window: the same
;;; screen in the three variants (a nested split panel and paragraphs broken to the width of
;;; their panes, apps/7guis/layout), the window made a width wider or narrower for every frame
;;; so that layout and painting are needed in each, against frames of a window that does not
;;; change. Per variant: the mean frame in microseconds and the bytes consed in one, the most
;;; builds a settled frame needed, and, across variants, at how many of the widths the display
;;; lists differ.

(defparameter +variants+ '(:retained :reactive :immediate))

(defun widths ()
  (loop :for width :from 280 :to 900 :by 2 :collect (float width 1f0)))

(defun measure (variant context sweeps)
  "Mean microseconds of a frame at a new width, of a frame at the same width, bytes consed in a
frame at a new width, the most builds in a frame, and the hashes at each width."
  (let* ((object (funcall (registry:app-constructor (registry:find-app variant :layout))))
         (list (core:make-display-list :capacity 65536))
         (widths (widths))
         (most-builds 0)
         (hashes '()))
    (setf (core:context-height context) 420f0)
    (flet ((frame (width)
             (setf (core:context-width context) width)
             (let ((start (core:now-ns)))
               (let ((builds (core:settle-frame object context list)))
                 (setf most-builds (max most-builds builds))
                 (- (core:now-ns) start)))))
      (dotimes (sweep 3) (dolist (width widths) (frame width)))     ; warm up
      (let ((total 0) (frames 0)
            (consed (sb-ext:get-bytes-consed)))
        (dotimes (sweep sweeps)
          (dolist (width widths)
            (incf total (frame width))
            (incf frames)))
        (let ((bytes (/ (- (sb-ext:get-bytes-consed) consed) frames 1.0))
              (same-total 0))
          (dotimes (i 2000)
            (incf same-total (frame 640f0)))
          (dolist (width widths)
            (frame width)
            (push (core:display-list-hash list) hashes))
          (values (/ total frames 1000.0) (/ same-total 2000 1000.0) bytes most-builds
                  (nreverse hashes)))))))

(defun main (&key (sweeps 20))
  (with-masked-float-traps
    (with-autorelease-pool
      (let* ((host (make-host (make-instance 'core:variant) (default-device)
                              +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
             (context (host-context host))
             (results '()))
        (format t "~&The layout spike, ~d frames a variant at new widths (280 to 900) and 2000 at one~%"
                (* sweeps (length (widths))))
        (format t "~&~12a ~14a ~14a ~12a ~a~%" "" "new width" "same width" "bytes" "most builds")
        (dolist (variant +variants+)
          (multiple-value-bind (new-us same-us bytes builds hashes) (measure variant context sweeps)
            (push (cons variant hashes) results)
            (format t "~&~(~12a~) ~9,1f us    ~9,1f us    ~9,0f    ~d~%" variant new-us same-us bytes
                    builds)))
        (let ((reference (cdr (first (last results)))))
          (dolist (entry (reverse results))
            (format t "~&~(~a~): ~d of ~d widths draw a different list from the retained one~%"
                    (car entry)
                    (count nil (mapcar #'eql (cdr entry) reference))
                    (length reference))))
        (finish-output)))))
