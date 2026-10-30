(defpackage #:almighty-bolt/ring-summary
  (:use #:cl)
  (:import-from #:almighty-bolt/stats
                #:summarize)
  (:import-from #:almighty-bolt/ring
                #:frame-ring-count
                #:frame-duration-ns
                #:frame-bytes-consed
                #:frame-build-ns
                #:frame-drawable-wait-ns
                #:frame-encode-ns
                #:frame-builds
                #:frame-capped-p
                #:frame-gc-ns
                #:frame-first-build-ns
                #:frame-events-ns
                #:frame-events
                #:frame-all-builds-ns
                #:frame-update-ns
                #:frame-layout-ns
                #:frame-paint-ns
                #:frame-gpu-ns)
  (:export #:summarize-ring))

(in-package #:almighty-bolt/ring-summary)

;;; What a ring of frames says, in the shape of the charter's metrics: the frame time and its
;;; parts, the builds in a frame and what settling cost, the garbage collection, and the
;;; bytes consed. A frame's own bytes are only good summed (the counter moves a region at a
;;; time), so they are given as a mean over all the frames.

(defun summarize-ring (ring &key (skip 0))
  "A plist for the frames of RING after the first SKIP of them (the first frame is cold):
:frames, and the summary (see SUMMARIZE) in nanoseconds of :duration, :build, :wait, :encode,
:update, :layout, :paint, :gpu and :events (the variant's handling of the events delivered since
the frame before; :events-per-frame is their number, a mean); :builds with :mean, :max, :histogram (a list of (builds .
frames)) and :cap-hits; :settle with :extra-build-ns (a summary over the frames, of what the
builds after the first took, from the time of all the variant's builds), :frames-with-extra-builds
and :share-of-build (the part of the time of all the builds that was in extra builds); :gc-ns (the sum) and :bytes-per-frame."
  (let* ((count (max 0 (- (frame-ring-count ring) skip)))
         (indices (loop :for i :from skip :below (frame-ring-count ring) :collect i)))
    (when (plusp count)
      (flet ((column (function) (mapcar (lambda (i) (funcall function ring i)) indices)))
        (let* ((builds (column #'frame-builds))
               (builds-histogram (let ((table (make-hash-table)))
                                   (dolist (n builds) (incf (gethash n table 0)))
                                   (sort (loop :for n :being :the :hash-keys :of table
                                                 :using (hash-value frames)
                                               :collect (cons n frames))
                                         #'< :key #'car)))
               (build-ns (column #'frame-build-ns))
               (all-builds-ns (column #'frame-all-builds-ns))
               (first-build-ns (column #'frame-first-build-ns))
               (extra (mapcar (lambda (all first) (if (plusp first) (max 0 (- all first)) 0))
                              all-builds-ns first-build-ns))
               (total-build (reduce #'+ all-builds-ns)))
          (list :frames count
                :duration (summarize (column #'frame-duration-ns))
                :build (summarize build-ns)
                :wait (summarize (column #'frame-drawable-wait-ns))
                :encode (summarize (column #'frame-encode-ns))
                :update (summarize (column #'frame-update-ns))
                :layout (summarize (column #'frame-layout-ns))
                :paint (summarize (column #'frame-paint-ns))
                :gpu (summarize (column #'frame-gpu-ns))
                :events (summarize (column #'frame-events-ns))
                :events-per-frame (/ (reduce #'+ (column #'frame-events)) count 1.0)
                :builds (list :mean (/ (reduce #'+ builds) count 1.0)
                              :max (reduce #'max builds)
                              :histogram builds-histogram
                              :cap-hits (count-if (lambda (i) (frame-capped-p ring i)) indices))
                :settle (list :extra-build-ns (summarize extra)
                              :frames-with-extra-builds (count-if #'plusp extra)
                              :share-of-build (if (plusp total-build)
                                                  (/ (reduce #'+ extra) total-build 1.0)
                                                  0.0))
                :gc-ns (reduce #'+ (column #'frame-gc-ns))
                :bytes-per-frame (/ (reduce #'+ (column #'frame-bytes-consed)) count 1.0)))))))
