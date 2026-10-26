(defpackage #:almighty-bolt-core/t/replay/player
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:script #:almighty-bolt-core/replay/script)
                    (#:player #:almighty-bolt-core/replay/player)
                    (#:variant #:almighty-bolt-core/variant)
                    (#:dl #:almighty-bolt-core/display-list)
                    (#:support #:almighty-bolt-core/t/replay/support)))

(in-package #:almighty-bolt-core/t/replay/player)

;;; The player: events, by place and by control, frames where a window would build them, time
;;; that is the script's and nothing else, and checkpoints looked at in settled frames.

(defun click-script ()
  "Three clicks on the button by its id, 100 ms apart, then a checkpoint."
  (script:make-script
   :header '(:width 200 :height 100 :scale 1.0d0)
   :entries (loop :for i :below 3
                  :for time = (* i 100000000)
                  :append (list (list time '(:pointer :kind :move :target :button))
                                (list (+ time 1000000)
                                      '(:pointer :kind :down :target :button :button 0 :buttons 1
                                        :click-count 1))
                                (list (+ time 2000000)
                                      '(:pointer :kind :up :target :button :button 0))))))

(define-test the-events-of-a-script-reach-the-variant-at-the-control-they-name ()
  (let* ((variant (make-instance 'support:button-variant))
         (result (player:replay (click-script) variant (support:make-test-context))))
    (assert-eql 3 (support:button-count variant))
    ;; The first frame, and one for each press (the other events ask for none).
    (assert-eql 4 (player:replay-result-frames result))
    (assert-true (player:replay-ok-p result))))

(define-test an-event-can-be-given-a-place-instead-of-a-control ()
  (let* ((variant (make-instance 'support:button-variant))
         (script (script:make-script
                  :entries '((0 (:pointer :kind :down :x 20 :y 20 :button 0 :buttons 1))
                             (1000 (:pointer :kind :down :x 150 :y 20 :button 0 :buttons 1))
                             (2000 (:key :kind :down :logical-key #\Space)))))
         (result (player:replay script variant (support:make-test-context))))
    (assert-eql 2 (support:button-count variant))        ; the one outside is not a press
    (assert-eql 3 (player:replay-result-frames result))))

(define-test running-a-script-twice-gives-the-same-hashes ()
  (flet ((run ()
           (player:replay (click-script) (make-instance 'support:button-variant)
                          (support:make-test-context))))
    (let ((a (run)) (b (run)))
      (assert-equalp (player:replay-result-hashes a) (player:replay-result-hashes b))
      (assert-true (plusp (length (player:replay-result-hashes a)))))))

(define-test a-measured-replay-notes-what-each-frame-took-and-an-unmeasured-one-nothing ()
  (let* ((measured (player:replay (click-script) (make-instance 'support:button-variant)
                                  (support:make-test-context) :measure t))
         (plain (player:replay (click-script) (make-instance 'support:button-variant)
                               (support:make-test-context))))
    (assert-eql (player:replay-result-frames measured) (length (player:replay-result-samples measured)))
    (assert-true (every (lambda (sample)
                          (destructuring-bind (nanoseconds builds bytes) sample
                            (and (>= nanoseconds 0) (>= builds 1) (>= bytes 0))))
                        (player:replay-result-samples measured)))
    (assert-true (>= (player:replay-result-bytes measured) 0))
    (assert-true (>= (player:replay-result-gc-ns measured) 0))
    (assert-eql 0 (length (player:replay-result-samples plain)))
    ;; Measuring changes nothing a script can see.
    (assert-equalp (player:replay-result-hashes measured) (player:replay-result-hashes plain))))

(define-test the-frames-differ-when-the-state-does ()
  (let* ((result (player:replay (click-script) (make-instance 'support:button-variant)
                                (support:make-test-context)))
         (hashes (map 'list #'cdr (player:replay-result-hashes result))))
    (assert-eql 4 (length (remove-duplicates hashes)))))

(define-test checkpoints-are-made-frozen-and-then-checked ()
  (let* ((with (player:script-with-checkpoints (click-script)))
         (first-run (player:replay with (make-instance 'support:button-variant)
                                   (support:make-test-context)))
         (golden (player:freeze-checkpoints with first-run)))
    ;; A checkpoint follows each of the 9 events.
    (assert-eql 9 (length (player:replay-result-checkpoints first-run)))
    (assert-true (every (lambda (c) (null (player:checkpoint-result-expected c)))
                        (player:replay-result-checkpoints first-run)))
    (let ((second-run (player:replay golden (make-instance 'support:button-variant)
                                     (support:make-test-context))))
      (assert-true (player:replay-ok-p second-run))
      (assert-true (every #'player:checkpoint-passed-p (player:replay-result-checkpoints second-run))))
    ;; A hash that is not what the variant draws is reported.
    (let* ((entries (copy-list (script:script-entries golden)))
           (index (position :checkpoint entries :key (lambda (e) (first (script:entry-form e)))))
           (bad (script:make-script
                 :header (script:script-header golden)
                 :entries (append (subseq entries 0 index)
                                  (list (list (script:entry-time (nth index entries))
                                              '(:checkpoint :name "wrong" :hash 1)))
                                  (subseq entries (1+ index)))))
           (result (player:replay bad (make-instance 'support:button-variant)
                                  (support:make-test-context))))
      (assert-false (player:replay-ok-p result))
      (assert-equal "wrong" (player:checkpoint-result-name
                             (find-if-not #'player:checkpoint-passed-p
                                          (player:replay-result-checkpoints result)))))))

(define-test while-a-variant-asks-for-frames-they-come-at-the-interval-and-time-is-the-scripts ()
  (let* ((script (script:make-script :entries '((500000000 (:checkpoint :name "half a second")))))
         (result (player:replay script (make-instance 'support:timer-variant)
                                (support:make-test-context))))
    ;; The first frame, 29 of 16.67 ms before the checkpoint, and the one built for it.
    (assert-eql 31 (player:replay-result-frames result))
    (assert-eql 500000000 (player:checkpoint-result-time (first (player:replay-result-checkpoints result))))))

(define-test the-frame-interval-changes-the-frames-and-not-what-a-checkpoint-sees ()
  (flet ((hash-at-half-a-second (interval)
           (let* ((script (script:make-script
                           :entries '((500000000 (:checkpoint :name "half a second")))))
                  (result (player:replay script (make-instance 'support:timer-variant)
                                         (support:make-test-context)
                                         :frame-interval-ns interval)))
             (values (player:checkpoint-result-actual
                      (first (player:replay-result-checkpoints result)))
                     (player:replay-result-frames result)))))
    (multiple-value-bind (hash-60 frames-60) (hash-at-half-a-second 16666667)
      (multiple-value-bind (hash-100 frames-100) (hash-at-half-a-second 10000000)
        (assert-true (/= frames-60 frames-100))
        (assert-eql hash-60 hash-100)))))

(define-test a-control-the-variant-does-not-show-is-an-error-with-its-name-and-time ()
  (let ((script (script:make-script
                 :entries '((250000000 (:pointer :kind :down :target :no-such-control))))))
    (handler-case (player:replay script (make-instance 'support:button-variant)
                                 (support:make-test-context))
      (player:replay-target-missing (condition)
        (assert-eql :no-such-control (player:replay-target-missing-id condition))
        (assert-eql 250000000 (player:replay-target-missing-time condition)))
      (:no-error (&rest values)
        (declare (ignore values))
        (assert-true nil "The missing control was not reported.")))))

(define-test the-header-and-a-resize-set-the-size-the-variant-draws-for ()
  (let* ((context (support:make-test-context))
         (before (variant:context-requester context))
         (script (script:make-script
                  :header '(:width 320 :height 240 :scale 2.0d0)
                  :entries '((1000 (:resize :width 400d0 :height 300d0))))))
    (player:replay script (make-instance 'support:button-variant) context)
    (assert-eql 400f0 (variant:context-width context))
    (assert-eql 300f0 (variant:context-height context))
    (assert-eql 2f0 (variant:context-scale context))
    ;; The context asks the host again for its frames once the replay is over.
    (assert-eq before (variant:context-requester context))))

;;; --- frames made by a function of the caller's ---

(define-test a-frame-function-makes-every-frame-in-place-of-the-settle-loop ()
  (let* ((variant (make-instance 'support:button-variant))
         (context (support:make-test-context))
         (list (dl:make-display-list :capacity 4096))
         (calls 0)
         (result (player:replay (click-script) variant context
                                :display-list list
                                :frame-function (lambda ()
                                                  (incf calls)
                                                  (dl:clear-display-list list)
                                                  (variant:frame variant context list)
                                                  (values 1 nil))))
         (plain (player:replay (click-script) (make-instance 'support:button-variant)
                               (support:make-test-context))))
    (assert-eql (player:replay-result-frames result) calls)
    (assert-eql 4 calls)
    ;; The list that is drawn is the one given, and it holds the last frame.
    (assert-eql (player:replay-result-list result) list)
    (assert-eql (dl:display-list-hash list) (cdr (aref (player:replay-result-hashes result) 3)))
    ;; The frames are those the settle loop makes.
    (assert-equalp (player:replay-result-hashes plain) (player:replay-result-hashes result))))

(define-test the-builds-a-frame-function-reports-are-counted ()
  (let* ((variant (make-instance 'support:button-variant))
         (context (support:make-test-context))
         (list (dl:make-display-list :capacity 4096))
         (result (player:replay (click-script) variant context
                                :display-list list
                                :frame-function (lambda ()
                                                  (dl:clear-display-list list)
                                                  (variant:frame variant context list)
                                                  (values 2 nil)))))
    (assert-eql 8 (player:replay-result-builds result))))

