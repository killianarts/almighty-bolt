(defpackage #:almighty-bolt-core/t/replay/recorder
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:event #:almighty-bolt-core/event)
                    (#:script #:almighty-bolt-core/replay/script)
                    (#:player #:almighty-bolt-core/replay/player)
                    (#:recorder #:almighty-bolt-core/replay/recorder)
                    (#:variant #:almighty-bolt-core/variant)
                    (#:settle #:almighty-bolt-core/settle)
                    (#:dl #:almighty-bolt-core/display-list)
                    (#:support #:almighty-bolt-core/t/replay/support)))

(in-package #:almighty-bolt-core/t/replay/recorder)

(defun live-session (events)
  "Give EVENTS to a variant as a window would, recording them, with a frame after each.
Returns the variant, the display list and the recorder."
  (let* ((variant (make-instance 'support:button-variant))
         (context (support:make-test-context))
         (list (dl:make-display-list :capacity 1024))
         (recorder (recorder:make-recorder :header '(:app :button :width 200 :height 100))))
    (settle:settle-frame variant context list)
    (dolist (event events)
      (recorder:record-event recorder variant event)
      (variant:deliver-event variant context event)
      (when (variant:take-frame-request context)
        (settle:settle-frame variant context list)))
    (values variant list recorder)))

(defun session-events ()
  ;; Times as the platform gives them: nanoseconds from some start long ago.
  (let ((start 7000000000000))
    (list (event:make-pointer-event :timestamp (+ start 5000000) :kind :move :x 30d0 :y 20d0)
          (event:make-pointer-event :timestamp (+ start 9000000) :kind :down :x 30d0 :y 20d0
                                    :button 0 :buttons 1 :click-count 1)
          (event:make-pointer-event :timestamp (+ start 90000000) :kind :up :x 30d0 :y 20d0
                                    :button 0)
          (event:make-pointer-event :timestamp (+ start 120000000) :kind :down :x 180d0 :y 90d0
                                    :button 0 :buttons 1 :click-count 1)
          (event:make-key-event :timestamp (+ start 300000000) :kind :down :logical-key #\Space))))

(define-test events-are-noted-with-the-time-since-the-first ()
  (multiple-value-bind (variant list recorder) (live-session (session-events))
    (declare (ignore variant list))
    (let ((entries (script:script-entries (recorder:recorder-script recorder))))
      (assert-equal '(0 4000000 85000000 115000000 295000000) (mapcar #'script:entry-time entries)))))

(define-test an-event-over-a-control-is-written-as-a-place-in-it-and-others-as-places ()
  (multiple-value-bind (variant list recorder) (live-session (session-events))
    (declare (ignore variant list))
    (let ((forms (mapcar #'script:entry-form
                         (script:script-entries (recorder:recorder-script recorder)))))
      ;; The button is at 10, 10, 100 by 30: (30, 20) is 0.2 across and a third down.
      (destructuring-bind (&key target fx fy kind &allow-other-keys) (rest (first forms))
        (assert-eql :button target)
        (assert-eql :move kind)
        (assert-true (< (abs (- fx 0.2d0)) 1d-6))
        (assert-true (< (abs (- fy (/ 1d0 3))) 1d-6)))
      ;; (180, 90) is over nothing.
      (assert-equal '(:pointer :kind :down :x 180d0 :y 90d0 :button 0 :buttons 1 :click-count 1)
                    (fourth forms))
      (assert-eq :key (first (fifth forms))))))

(define-test what-was-recorded-replays-to-the-same-frame ()
  (multiple-value-bind (variant list recorder) (live-session (session-events))
    (let* ((script (recorder:recorder-script recorder))
           (replayed (make-instance 'support:button-variant))
           (result (player:replay script replayed (support:make-test-context))))
      (assert-eql (support:button-count variant) (support:button-count replayed))
      (assert-eql 2 (support:button-count replayed))             ; one click, one space
      (assert-eql (dl:display-list-hash list)
                  (dl:display-list-hash (player:replay-result-list result))))))

(define-test a-recorded-script-written-and-read-back-replays-the-same ()
  (multiple-value-bind (variant list recorder) (live-session (session-events))
    (declare (ignore variant))
    (let* ((text (with-output-to-string (stream)
                   (script:write-script (recorder:recorder-script recorder) stream)))
           (script (with-input-from-string (stream text) (script:read-script stream)))
           (result (player:replay script (make-instance 'support:button-variant)
                                  (support:make-test-context))))
      (assert-eql (dl:display-list-hash list)
                  (dl:display-list-hash (player:replay-result-list result))))))
