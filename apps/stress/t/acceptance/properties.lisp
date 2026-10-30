(defpackage #:almighty-bolt-stress/t/acceptance/properties
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:d #:almighty-bolt-7guis/t/acceptance/driver))
  (:import-from #:almighty-bolt/platform/macos/objc
                #:with-masked-float-traps
                #:with-autorelease-pool)
  (:import-from #:almighty-bolt/platform/macos/host
                #:make-host
                #:host-context)
  (:import-from #:almighty-bolt/render/metal/device
                #:default-device)
  (:import-from #:almighty-bolt/render/metal/target
                #:+pixel-format-bgra8-unorm+))

(in-package #:almighty-bolt-stress/t/acceptance/properties)

;;; What the library promises whatever an app does, checked from outside, as the charter's Testing
;;; section asks: the shipped replay scripts are played into the apps with the real text engine and no
;;; window, as the benchmark protocol plays them, and an app that has nothing going on is left alone.
;;;   - a frame settles: no frame of any script hits the settle cap;
;;;   - determinism: a script played twice, into a new app each time, draws the same frames (the
;;;     hash of every frame is the same), which the charter wants before any measurement is trusted;
;;;   - allocation: after the first frame, three of the scripts allocate nothing in a frame and the
;;;     other three no more than about twice what they did when this was written (bytes consed are
;;;     counted in steps of an allocation region, so a script of a dozen frames cannot show less
;;;     than about a kilobyte a frame);
;;;   - idle: an app that is not animating asks for no frame, at the start and after time passes.
;;; The checkpoints of the shipped scripts name a moment and carry no hash (a hash is of this
;;; machine's text engine, and is frozen locally with make replay FREEZE=), so they check nothing here.

(defparameter +scripts+
  '(("7guis" "replay/counter-clicks.script")
    ("7guis" "replay/cells-edit.script")
    ("7guis" "replay/timer-reset.script")
    ("stress" "replay/dashboard-run.script")
    ("stress" "replay/editor-session.script")
    ("stress" "replay/rows-scroll.script")))

(defparameter +bytes-per-frame-allowed+
  ;; Bytes consed by a frame after the first, by script name. Measured on 2026-10-30 (make
  ;; bench-run): 0, 0, 802, 0, 1102 and 2922.
  '(("counter-clicks" . 0)
    ("cells-edit" . 0)
    ("timer-reset" . 1700)
    ("dashboard-run" . 0)
    ("editor-session" . 2300)
    ("rows-scroll" . 6000)))

(defun script-path (system relative)
  (asdf:system-relative-pathname (format nil "almighty-bolt-~a" system) relative))

(defun play (path &key measure)
  "Play the script at PATH into a new app of the immediate variant, with the real text engine.
Returns the replay result. The first two plays of a script are warm-ups, and the third is
returned: the variant builds its tables in the first frame of its first play."
  (let* ((script (core:load-script path))
         (task (getf (core:script-header script) :app)))
    (with-masked-float-traps
      (with-autorelease-pool
        (let* ((host (make-host (make-instance 'core:variant) (default-device)
                                +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
               (context (host-context host)))
          (flet ((once (measure)
                   (core:replay script
                                (funcall (registry:app-constructor
                                          (registry:find-app :immediate task)))
                                context :measure measure)))
            (once nil)
            (once nil)
            ;; Garbage from what ran before, such as a compile in this process, is not the
            ;; frames' to pay for.
            (when measure (sb-ext:gc :full t))
            (once measure)))))))

(defun mean-bytes-after-the-first-frame (result)
  (let ((frames (rest (coerce (core:replay-result-samples result) 'list))))
    (/ (reduce #'+ (mapcar #'third frames)) (max 1 (length frames)) 1.0)))

(defun least-bytes-per-frame (path)
  "The bytes a frame conses after the first, the least of three plays. What the frames cons they
cons in every play; a stray allocation of something else (a thread of the system, a collection)
lands in one, and counts as a whole allocation region, which a dozen frames cannot tell from a
frame that allocates."
  (loop :repeat 3
        :minimize (mean-bytes-after-the-first-frame (play path :measure t))))

(defun hashes (result)
  (map 'vector #'cdr (core:replay-result-hashes result)))

(define-test every-script-settles ()
  (loop :for (system relative) :in +scripts+
        :do (let ((result (play (script-path system relative))))
              (assert-eql 0 (core:replay-result-capped result) relative)
              (assert-true (core:replay-ok-p result) relative))))

(define-test every-script-replays-to-the-same-frames-every-time ()
  (loop :for (system relative) :in +scripts+
        :do (let ((first (play (script-path system relative)))
                  (second (play (script-path system relative))))
              (assert-true (plusp (length (core:replay-result-hashes first))) relative)
              (assert-equalp (hashes first) (hashes second) relative))))

(define-test the-scripts-allocate-what-they-did-after-the-first-frame ()
  (loop :for (system relative) :in +scripts+
        :do (let* ((name (pathname-name relative))
                   (allowed (cdr (assoc name +bytes-per-frame-allowed+ :test #'string=)))
                   (bytes (least-bytes-per-frame (script-path system relative))))
              (assert-true allowed name)
              (assert-true (<= bytes allowed) name bytes allowed))))

(define-test an-app-that-is-not-animating-asks-for-no-frame ()
  (dolist (task '(:counter :temperature :flight :crud :circles :cells :layout :dialog))
    (dolist (variant (d:available-variants task))
      (let ((driver (d:start variant task)))
        ;; What the first frame asked for.
        (d:frame-requested-p driver)
        (d:redraw driver)
        (assert-false (d:frame-requested-p driver) task variant)
        (d:advance-time driver 5d0)
        (assert-false (d:frame-requested-p driver) task variant)))))
