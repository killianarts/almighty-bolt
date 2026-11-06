(defpackage #:almighty-bolt/replay/player
  (:use #:cl)
  (:local-nicknames (#:clock #:almighty-bolt/clock)
                    (#:dl #:almighty-bolt/display-list)
                    (#:app-instance #:almighty-bolt/app-instance)
                    (#:settle #:almighty-bolt/settle)
                    (#:script #:almighty-bolt/replay/script))
  (:export #:replay
           #:replay-result
           #:replay-result-frames
           #:replay-result-builds
           #:replay-result-capped
           #:replay-result-hashes
           #:replay-result-checkpoints
           #:replay-result-list
           #:replay-result-samples
           #:replay-result-bytes
           #:replay-result-gc-ns
           #:replay-ok-p
           #:checkpoint-result
           #:checkpoint-result-name
           #:checkpoint-result-time
           #:checkpoint-result-expected
           #:checkpoint-result-actual
           #:checkpoint-passed-p
           #:freeze-checkpoints
           #:script-with-checkpoints
           #:replay-target-missing
           #:replay-target-missing-id
           #:replay-target-missing-time
           #:+default-frame-interval-ns+))

(in-package #:almighty-bolt/replay/player)

;;; Replay a script (replay/script.lisp) into an app, with the clock virtual: time is what
;;; the script's entries say and nothing else, and the frames are built where a live window
;;; would build them. After each event the frame it asks for is built at once; while an app
;;; keeps asking for frames (an animation, a timer) they come at a fixed interval, 60 a
;;; second unless the script's header or the caller says otherwise; when it asks for none,
;;; time jumps to the next entry. A checkpoint is looked at in a settled frame, so a frame the
;;; app has asked for is built first. Running a script twice must give the same hashes, and
;;; the tests hold the ui to that (if it does not, the bug is fixed before any measurement is
;;; trusted).

(defconstant +default-frame-interval-ns+ 16666667)

(define-condition replay-target-missing (error)
  ((id :initarg :id :reader replay-target-missing-id)
   (time :initarg :time :reader replay-target-missing-time))
  (:report (lambda (condition stream)
             (format stream "The script needs the control ~s at ~,3f s, and the app shows no such control."
                     (replay-target-missing-id condition)
                     (/ (replay-target-missing-time condition) 1d9)))))

(defstruct (checkpoint-result (:constructor make-checkpoint-result (name time expected actual)))
  "A checkpoint of a script looked at: the hash the script expected (NIL if none) and the hash
of the settled frame."
  name time expected actual)

(defun checkpoint-passed-p (checkpoint)
  (or (null (checkpoint-result-expected checkpoint))
      (eql (checkpoint-result-expected checkpoint) (checkpoint-result-actual checkpoint))))

(defstruct (replay-result (:constructor make-replay-result
                              (frames builds capped hashes checkpoints list samples bytes gc-ns)))
  "What a replay did: frames built, builds in them, frames that hit the settle cap, a vector of
(time . hash) for each frame in order, the checkpoints in order, and the last frame's list.
When it was measured (see REPLAY), also a vector with a list (NANOSECONDS BUILDS BYTES
EVENT-NANOSECONDS EVENT-BYTES EVENTS) for each frame: the real time it took to build, the builds it
needed and the bytes consed while it was built (which, being read from a counter that moves a
region at a time, are only good summed over many frames), and the same for the events delivered
since the frame before, which an app may handle in HANDLE-EVENT or in the frame, and how many
there were; and the bytes consed and nanoseconds of garbage collection in all of the replay."
  frames builds capped hashes checkpoints list samples bytes gc-ns)

(defun replay-ok-p (result)
  "True if every checkpoint passed and no frame hit the settle cap."
  (and (zerop (replay-result-capped result))
       (every #'checkpoint-passed-p (replay-result-checkpoints result))))

(defun remove-keys (plist &rest keys)
  (loop :for (key value) :on plist :by #'cddr
        :unless (member key keys)
          :append (list key value)))

(defun resolve-form (form instance time)
  "FORM with a :TARGET (and :FX, :FY) made into :X and :Y from the control's place now."
  (let* ((options (rest form))
         (target (getf options :target)))
    (if (null target)
        form
        (multiple-value-bind (x y width height) (app-instance:lookup-target instance target)
          (unless x
            (error 'replay-target-missing :id target :time time))
          (let ((fx (or (getf options :fx) 0.5d0))
                (fy (or (getf options :fy) 0.5d0)))
            (list* (first form)
                   :x (+ x (* fx width)) :y (+ y (* fy height))
                   (remove-keys options :target :fx :fy)))))))

(defun replay (script instance context
               &key frame-interval-ns (capacity 65536) measure frame-function display-list)
  "Replay SCRIPT into INSTANCE, whose frames are built with CONTEXT (which must have the text
support the app needs). The context's size and scale are set from the script's header
if it has them. Returns a REPLAY-RESULT. Signals REPLAY-TARGET-MISSING for a target the
app does not show. With MEASURE, the time each frame takes to build is noted, on the
real clock though the script's is virtual, with the time and bytes of delivering the events
that came before it, and the bytes consed and the time spent in garbage collection over the
whole replay; the replay is otherwise the same.

FRAME-FUNCTION, a function of no arguments, makes each frame in place of SETTLE-FRAME and returns
the builds and whether the cap was hit, as SETTLE-FRAME does. The host's RUN-FRAME is what it
is for: a replay that draws, with the real renderer. The frame's display list is then the host's,
which DISPLAY-LIST says; the context's size and scale are the frame function's to use. Everything
the player knows of time is still the script's, and the host's ring is on the real clock."
  (let* ((header (script:script-header script))
         (interval (or frame-interval-ns (getf header :frame-interval-ns)
                       +default-frame-interval-ns+))
         (clock (clock:make-virtual-clock 0))
         (list (or display-list (dl:make-display-list :capacity capacity)))
         (hashes (make-array 64 :adjustable t :fill-pointer 0))
         (checkpoints '())
         (frames 0) (builds 0) (capped 0)
         (event-ns 0) (event-bytes 0) (events 0)
         (samples (make-array 64 :adjustable t :fill-pointer 0))
         (bytes-before (sb-ext:get-bytes-consed))
         (gc-before sb-ext:*gc-run-time*)
         (old-requester (app-instance:context-requester context)))
    (when (getf header :width) (setf (app-instance:context-width context) (float (getf header :width) 1f0)))
    (when (getf header :height) (setf (app-instance:context-height context) (float (getf header :height) 1f0)))
    (when (getf header :scale) (setf (app-instance:context-scale context) (float (getf header :scale) 1f0)))
    (setf (app-instance:context-frame-requested context) nil
          (app-instance:context-requester context) (lambda (context)
                                                     (setf (app-instance:context-frame-requested context) t)))
    (unwind-protect
         (let ((clock:*time-source* (clock:virtual-clock-source clock)))
           (labels ((now () (clock:virtual-clock-ns clock))
                    (build ()
                      (app-instance:take-frame-request context)
                      (multiple-value-bind (n capped-p)
                          (let ((start (and measure (clock:real-time-source)))
                                (consed (and measure (sb-ext:get-bytes-consed))))
                            (multiple-value-bind (n capped-p)
                                (handler-bind ((settle:settle-cap-reached #'muffle-warning))
                                  (if frame-function
                                      (funcall frame-function)
                                      (settle:settle-frame instance context list)))
                              (when measure
                                (vector-push-extend
                                 (list (- (clock:real-time-source) start) n
                                       (- (sb-ext:get-bytes-consed) consed)
                                       event-ns event-bytes events)
                                 samples)
                                (setf event-ns 0 event-bytes 0 events 0))
                              (values n capped-p)))
                        (incf frames)
                        (incf builds n)
                        (when capped-p (incf capped))
                        (vector-push-extend (cons (now) (dl:display-list-hash list)) hashes)))
                    (build-if-asked ()
                      (when (app-instance:context-frame-requested context)
                        (build)))
                    (deliver (event)
                      ;; The event is made before the clock starts: that is the replay's work.
                      (if measure
                          (let ((start (clock:real-time-source))
                                (consed (sb-ext:get-bytes-consed)))
                            (app-instance:deliver-event instance context event)
                            (incf event-ns (- (clock:real-time-source) start))
                            (incf event-bytes (- (sb-ext:get-bytes-consed) consed))
                            (incf events))
                          (app-instance:deliver-event instance context event)))
                    (advance-to (target)
                      (loop :for now = (now)
                            :while (< now target)
                            :do (cond ((and (app-instance:context-frame-requested context)
                                            (<= (+ now interval) target))
                                       (clock:advance-virtual-clock clock interval)
                                       (build))
                                      (t (clock:advance-virtual-clock clock (- target now))))))
                    (do-entry (entry)
                      (advance-to (script:entry-time entry))
                      (let ((form (script:entry-form entry)))
                        (if (eq (first form) :checkpoint)
                            (let ((options (rest form)))
                              (build-if-asked)
                              (push (make-checkpoint-result (getf options :name) (now)
                                                            (getf options :hash)
                                                            (dl:display-list-hash list))
                                    checkpoints))
                            (let* ((form (resolve-form form instance (now)))
                                   (options (rest form)))
                              (case (first form)
                                (:resize
                                 (setf (app-instance:context-width context) (float (getf options :width) 1f0)
                                       (app-instance:context-height context) (float (getf options :height) 1f0)))
                                (:scale
                                 (setf (app-instance:context-scale context) (float (getf options :scale) 1f0))))
                              (deliver (script:form-event form (now)))
                              (when (member (first form) '(:resize :scale))
                                (app-instance:request-frame context))
                              (build-if-asked))))))
             (build)
             (dolist (entry (script:script-entries script))
               (do-entry entry))))
      (setf (app-instance:context-requester context) old-requester))
    (make-replay-result frames builds capped hashes (nreverse checkpoints) list samples
                        (- (sb-ext:get-bytes-consed) bytes-before)
                        (round (* (- sb-ext:*gc-run-time* gc-before) 1000000000)
                               internal-time-units-per-second))))

;;; --- making checkpoints ---

(defun script-with-checkpoints (script)
  "SCRIPT with a checkpoint after each entry that is not one, named for the entry it follows."
  (let ((index 0))
    (script:make-script :header (script:script-header script)
                        :entries (loop :for entry :in (script:script-entries script)
                                       :collect entry
                                       :unless (eq (first (script:entry-form entry)) :checkpoint)
                                         :collect (list (script:entry-time entry)
                                                        (list :checkpoint
                                                              :name (format nil "after entry ~d"
                                                                            (incf index))))))))

(defun freeze-checkpoints (script result)
  "SCRIPT with the hash of each of its checkpoints set to what RESULT, a replay of it, found.
Run on a script made with SCRIPT-WITH-CHECKPOINTS, it makes a golden script."
  (let ((found (replay-result-checkpoints result)))
    (script:make-script :header (script:script-header script)
                        :entries (loop :for entry :in (script:script-entries script)
                                       :collect (if (eq (first (script:entry-form entry)) :checkpoint)
                                                    (let ((actual (checkpoint-result-actual (pop found)))
                                                          (name (getf (rest (script:entry-form entry)) :name)))
                                                      (list (script:entry-time entry)
                                                            (list :checkpoint :name name :hash actual)))
                                                    entry)))))
