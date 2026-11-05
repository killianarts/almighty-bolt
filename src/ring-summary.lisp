(defpackage #:almighty-bolt/ring-summary
  (:use #:cl)
  (:local-nicknames (#:stats #:almighty-bolt/stats)
                    (#:ring #:almighty-bolt/ring))
  (:export #:summarize-ring))

(in-package #:almighty-bolt/ring-summary)

;;; What a ring of frames says, in the shape the benchmarks report: the frame time and its
;;; parts, the builds in a frame and what settling cost, the garbage collection, and the
;;; bytes consed. A frame's own bytes are only good summed (the counter moves a region at a
;;; time), so they are given as a mean over all the frames.

(defun summarize-ring (ring &key (skip 0))
  "A plist for the frames of RING after the first SKIP of them (the first frame is cold):
:frames, and the summary (see SUMMARIZE) in nanoseconds of :duration, :build, :wait, :encode,
:update, :layout, :paint, :gpu and :events (the app's handling of the events delivered since
the frame before; :events-per-frame is their number, a mean); :builds with :mean, :max, :histogram (a list of (builds .
frames)) and :cap-hits; :settle with :extra-build-ns (a summary over the frames, of what the
builds after the first took, from the time of all the app's builds), :frames-with-extra-builds
and :share-of-build (the part of the time of all the builds that was in extra builds); :gc-ns (the sum) and :bytes-per-frame."
  (let* ((count (max 0 (- (ring:frame-ring-count ring) skip)))
         (indices (loop :for i :from skip :below (ring:frame-ring-count ring) :collect i)))
    (when (plusp count)
      (flet ((column (function) (mapcar (lambda (i) (funcall function ring i)) indices)))
        (let* ((builds (column #'ring:frame-builds))
               (builds-histogram (let ((table (make-hash-table)))
                                   (dolist (n builds) (incf (gethash n table 0)))
                                   (sort (loop :for n :being :the :hash-keys :of table
                                                 :using (hash-value frames)
                                               :collect (cons n frames))
                                         #'< :key #'car)))
               (build-ns (column #'ring:frame-build-ns))
               (all-builds-ns (column #'ring:frame-all-builds-ns))
               (first-build-ns (column #'ring:frame-first-build-ns))
               (extra (mapcar (lambda (all first) (if (plusp first) (max 0 (- all first)) 0))
                              all-builds-ns first-build-ns))
               (total-build (reduce #'+ all-builds-ns)))
          (list :frames count
                :duration (stats:summarize (column #'ring:frame-duration-ns))
                :build (stats:summarize build-ns)
                :wait (stats:summarize (column #'ring:frame-drawable-wait-ns))
                :encode (stats:summarize (column #'ring:frame-encode-ns))
                :update (stats:summarize (column #'ring:frame-update-ns))
                :layout (stats:summarize (column #'ring:frame-layout-ns))
                :paint (stats:summarize (column #'ring:frame-paint-ns))
                :gpu (stats:summarize (column #'ring:frame-gpu-ns))
                :events (stats:summarize (column #'ring:frame-events-ns))
                :events-per-frame (/ (reduce #'+ (column #'ring:frame-events)) count 1.0)
                :builds (list :mean (/ (reduce #'+ builds) count 1.0)
                              :max (reduce #'max builds)
                              :histogram builds-histogram
                              :cap-hits (count-if (lambda (i) (ring:frame-capped-p ring i)) indices))
                :settle (list :extra-build-ns (stats:summarize extra)
                              :frames-with-extra-builds (count-if #'plusp extra)
                              :share-of-build (if (plusp total-build)
                                                  (/ (reduce #'+ extra) total-build 1.0)
                                                  0.0))
                :gc-ns (reduce #'+ (column #'ring:frame-gc-ns))
                :bytes-per-frame (/ (reduce #'+ (column #'ring:frame-bytes-consed)) count 1.0)))))))
