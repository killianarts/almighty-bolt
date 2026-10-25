(asdf:load-system "almighty-bolt-7guis")

(defpackage #:almighty-bolt-scripts/replay
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
                #:+pixel-format-bgra8-unorm+))

(in-package #:almighty-bolt-scripts/replay)

;;;; Play a replay script back into one of the 7GUIs apps, with the real text engine, and say
;;;; what happened. From the repository root:  make replay SCRIPT=path [VARIANT=..] [FREEZE=path]

(defun environment (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun run-once (script variant task)
  "Play SCRIPT into a new VARIANT app of TASK. Returns the result."
  (let ((object (funcall (registry:app-constructor (registry:find-app variant task)))))
    (with-masked-float-traps
      (with-autorelease-pool
        (let* ((host (make-host (make-instance 'core:variant) (default-device)
                                +pixel-format-bgra8-unorm+ :capacity 1024 :scale 2))
               (context (host-context host)))
          (core:replay script object context))))))

(defun report (result variant task)
  (format t "~&~(~a~) ~(~a~): ~d frames, ~d builds, ~d hit the settle cap, ~d checkpoints~%"
          variant task (core:replay-result-frames result) (core:replay-result-builds result)
          (core:replay-result-capped result) (length (core:replay-result-checkpoints result)))
  (dolist (checkpoint (core:replay-result-checkpoints result))
    (unless (core:checkpoint-passed-p checkpoint)
      (format t "  FAILED ~s at ~,3f s: expected ~a, the frame hashes to ~a~%"
              (core:checkpoint-result-name checkpoint)
              (/ (core:checkpoint-result-time checkpoint) 1d9)
              (core:checkpoint-result-expected checkpoint)
              (core:checkpoint-result-actual checkpoint))))
  (let ((failed (count-if-not #'core:checkpoint-passed-p (core:replay-result-checkpoints result))))
    (format t "~&~[All checkpoints hold.~:;~:*~d checkpoint~:p failed.~]~%" failed)
    (zerop failed)))

(defun main ()
  (let* ((path (or (environment "SCRIPT") (error "Say which script with SCRIPT=path.")))
         (script (core:load-script path))
         (header (core:script-header script))
         (variant (if (environment "VARIANT")
                      (intern (string-upcase (environment "VARIANT")) :keyword)
                      (getf header :variant)))
         (task (getf header :app))
         (freeze (environment "FREEZE")))
    (unless (and variant task)
      (error "The script's header names no :variant and :app, and VARIANT= does not say."))
    (asdf:load-system (format nil "almighty-bolt-7guis/~(~a~)" variant))
    (cond
      (freeze
       (let* ((with (core:script-with-checkpoints script))
              (first-run (run-once with variant task))
              (second-run (run-once with variant task)))
         (unless (equalp (core:replay-result-hashes first-run) (core:replay-result-hashes second-run))
           (format t "~&The two runs of the script give different frames: not writing ~a.~%" freeze)
           (uiop:quit 1))
         (core:save-script (core:freeze-checkpoints with first-run) freeze)
         (report first-run variant task)
         (format t "~&Played twice with the same frames; wrote ~a.~%" freeze)))
      (t
       (let ((result (run-once script variant task)))
         (unless (and (report result variant task) (core:replay-ok-p result))
           (uiop:quit 1)))))))

(main)
