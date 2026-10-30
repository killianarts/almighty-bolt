(defpackage #:almighty-bolt-stress/t/acceptance/properties
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:core #:almighty-bolt/internals)
                    (#:m #:almighty-bolt/machine)
                    (#:sem #:almighty-bolt/ui/semantics)
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
                                (registry:make-app-object (registry:find-app :immediate task))
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

;;; --- state kept ---
;;;
;;; An app that is made again by the same code, as when a definition is compiled, is the same app to
;;; the person using it: what they had done, they have still. For every app that keeps state, the same
;;; things are done to two apps, and one is made again in the middle; both must end showing the same
;;; frame (the same text, the same commands).

(defun the-same-with-and-without-a-reload (task before after)
  (let ((plain (d:start :immediate task))
        (reloaded (d:start :immediate task)))
    (funcall before plain)
    (funcall before reloaded)
    (d:reload reloaded)
    (funcall after plain)
    (funcall after reloaded)
    (values (d:screen-text plain) (d:screen-text reloaded)
            (core:display-list-hash (d:driver-list plain))
            (core:display-list-hash (d:driver-list reloaded)))))

(defun type-into (driver id text)
  (d:click driver id)
  (d:type-text driver text))

(defparameter +things-done-before-and-after-a-reload+
  ;; (task before after): what is done to the app, and what is done after the reload.
  (list
   (list :counter
         (lambda (driver) (d:click driver :count-button) (d:click driver :count-button))
         (lambda (driver) (d:click driver :count-button)))
   (list :temperature
         (lambda (driver) (type-into driver :celsius "100"))
         (lambda (driver) (type-into driver :fahrenheit "5")))
   (list :flight
         (lambda (driver) (d:click-at driver :kind 0.75 0.5) (type-into driver :start "7"))
         (lambda (driver) (type-into driver :return "x")))
   (list :timer
         (lambda (driver) (d:advance-time driver 2.5d0))
         (lambda (driver) (d:advance-time driver 4d0)))
   (list :circles
         (lambda (driver) (d:click-at driver :canvas 0.3 0.3) (d:click-at driver :canvas 0.6 0.5))
         (lambda (driver) (d:click-at driver :canvas 0.5 0.8)))
   (list :cells
         (lambda (driver)
           (d:click-at driver :grid 0.3 0.2)
           (d:type-text driver "12")
           (d:press-key driver #\Return))
         (lambda (driver) (d:type-text driver "7")))
   (list :dashboard
         (lambda (driver) (d:advance-time driver 0.5d0))
         (lambda (driver) (d:advance-time driver 0.25d0)))
   (list :rows
         (lambda (driver) (d:scroll driver :list -10) (d:click-at driver :list 0.2 0.3))
         (lambda (driver) (d:scroll driver :list -4)))
   (list :editor
         (lambda (driver) (d:click-at driver :editor 0.2 0.2) (d:input-text driver "ab"))
         (lambda (driver) (d:input-text driver "c")))))

(define-test every-app-that-keeps-state-shows-the-same-after-a-reload ()
  (loop :for (task before after) :in +things-done-before-and-after-a-reload+
        :do (multiple-value-bind (plain-text reloaded-text plain-hash reloaded-hash)
                (the-same-with-and-without-a-reload task before after)
              (assert-equal plain-text reloaded-text task)
              (assert-eql plain-hash reloaded-hash task))))

;;; --- ids ---
;;;
;;; Two things with one id are one widget to the ui: they share a key, so they share the keyboard, the
;;; caret and the cache of what was drawn, and nothing used to say so (the Timer's two "duration"
;;; labels drew more slowly than they should). The library refuses it as the frame is built
;;; (DUPLICATE-ID), so an app that has two fails wherever it is run. This runs every app, and what is
;;; done to each before and after a reload, and looks at the screen as a program would after each.

(defun things-on-screen (driver)
  (let ((count 0))
    (sem:walk-nodes (lambda (node) (declare (ignore node)) (incf count))
                    (m:see (d:driver-session driver)))
    count))

(define-test no-app-has-two-things-that-the-ui-cannot-tell-apart ()
  (dolist (task registry:+tasks+)
    (assert-true (> (things-on-screen (d:start :immediate task)) 1) task))
  (loop :for (task before after) :in +things-done-before-and-after-a-reload+
        :do (let ((driver (d:start :immediate task)))
              (funcall before driver)
              (assert-true (> (things-on-screen driver) 1) task)
              (funcall after driver)
              (assert-true (> (things-on-screen driver) 1) task))))
