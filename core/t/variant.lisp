(defpackage #:almighty-bolt-core/t/variant
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:variant #:almighty-bolt-core/variant)
                    (#:clock #:almighty-bolt-core/clock)
                    (#:event #:almighty-bolt-core/event)
                    (#:dl #:almighty-bolt-core/display-list)))

(in-package #:almighty-bolt-core/t/variant)

;;; A variant that records what it is offered and answers as told.

(defclass recording-variant (variant:variant)
  ((seen :initform '() :accessor seen)
   (wants-frame :initarg :wants-frame :initform nil :reader wants-frame)))

(defmethod variant:handle-event ((variant recording-variant) event)
  (push event (seen variant))
  (wants-frame variant))

;;; One that counts, so that delivering to it allocates nothing.

(defclass counting-variant (variant:variant)
  ((count :initform 0 :type fixnum :accessor event-count)))

(defmethod variant:handle-event ((variant counting-variant) event)
  (declare (ignore event))
  (incf (event-count variant))
  t)

(defun an-event (&optional (timestamp 0))
  (event:make-focus-event :timestamp timestamp :focused-p t))

;;; --- the operations a variant need not implement ---

(define-test a-bare-variant-ignores-events-and-hits-nothing ()
  (let ((variant (make-instance 'variant:variant)))
    (assert-false (variant:handle-event variant (an-event)))
    (assert-false (variant:hit-target variant 10f0 20f0))
    (assert-false (variant:lookup-target variant :save-button))
    (assert-false (variant:accessibility-tree variant))))

(define-test a-variant-must-provide-frame ()
  (assert-error 'error
                (variant:frame (make-instance 'variant:variant)
                               (variant:make-context)
                               (dl:make-display-list :capacity 4))))

;;; --- the context ---

(define-test a-new-context-is-empty-and-has-unit-scale ()
  (let ((context (variant:make-context)))
    (assert-eql 0f0 (variant:context-width context))
    (assert-eql 0f0 (variant:context-height context))
    (assert-eql 1f0 (variant:context-scale context))
    (assert-eql 0 (variant:context-frame-number context))
    (assert-eql 0 (variant:context-build-number context))
    (assert-false (variant:context-frame-requested context))))

(define-test the-host-updates-a-context-in-place ()
  (let ((context (variant:make-context :width 640f0 :height 400f0 :scale 2f0)))
    (assert-eql 640f0 (variant:context-width context))
    (setf (variant:context-width context) 800f0
          (variant:context-height context) 600f0
          (variant:context-scale context) 1f0)
    (incf (variant:context-frame-number context))
    (assert-eql 800f0 (variant:context-width context))
    (assert-eql 600f0 (variant:context-height context))
    (assert-eql 1f0 (variant:context-scale context))
    (assert-eql 1 (variant:context-frame-number context))))

(define-test the-context-reads-the-core-clock ()
  (let ((context (variant:make-context))
        (virtual (clock:make-virtual-clock 100)))
    (clock:with-time-source ((clock:virtual-clock-source virtual))
      (assert-eql 100 (variant:context-now-ns context))
      (clock:advance-virtual-clock virtual 400)
      (assert-eql 500 (variant:context-now-ns context)))))

;;; --- asking for frames ---

(define-test a-request-is-a-flag-until-the-host-takes-it ()
  (let ((context (variant:make-context)))
    (assert-false (variant:take-frame-request context))
    (variant:request-frame context)
    (assert-true (variant:context-frame-requested context))
    (assert-true (variant:take-frame-request context))
    (assert-false (variant:take-frame-request context))))

(define-test many-requests-before-a-frame-make-one ()
  (let ((context (variant:make-context)))
    (dotimes (i 5)
      (variant:request-frame context))
    (assert-true (variant:take-frame-request context))
    (assert-false (variant:take-frame-request context))))

(define-test a-host-can-supply-its-own-requester ()
  (let* ((calls '())
         (context (variant:make-context
                   :requester (lambda (context) (push context calls)))))
    (variant:request-frame context)
    (variant:request-frame context)
    (assert-eql 2 (length calls))
    (assert-true (eq context (first calls)))
    ;; The replacement does not set the default flag.
    (assert-false (variant:take-frame-request context))))

;;; --- events ---

(define-test an-event-that-wants-a-frame-requests-one ()
  (let ((variant (make-instance 'recording-variant :wants-frame t))
        (context (variant:make-context))
        (event (an-event)))
    (assert-true (variant:deliver-event variant context event))
    (assert-true (eq event (first (seen variant))))
    (assert-true (variant:take-frame-request context))))

(define-test an-event-that-wants-nothing-requests-nothing ()
  (let ((variant (make-instance 'recording-variant :wants-frame nil))
        (context (variant:make-context)))
    (assert-false (variant:deliver-event variant context (an-event)))
    (assert-eql 1 (length (seen variant)))
    (assert-false (variant:take-frame-request context))))

(define-test events-reach-the-variant-one-at-a-time-in-order ()
  (let ((variant (make-instance 'recording-variant))
        (context (variant:make-context)))
    (dolist (timestamp '(10 20 30))
      (variant:deliver-event variant context (an-event timestamp)))
    (assert-equal '(10 20 30)
                  (mapcar #'event:event-timestamp (reverse (seen variant))))))

;;; --- text, supplied by the host ---

(define-test a-context-without-a-host-has-no-text ()
  (let ((context (variant:make-context))
        (list (dl:make-display-list :capacity 4)))
    (assert-error 'variant:no-text-support (variant:context-font context "Menlo" 12))
    (assert-error 'variant:no-text-support (variant:context-measure context :font "ab"))
    (assert-error 'variant:no-text-support
                  (variant:context-break-lines context :font "ab cd" 20f0))
    (assert-error 'variant:no-text-support (variant:context-caret-x context :font "ab" 1))
    (assert-error 'variant:no-text-support
                  (variant:context-index-at-x context :font "ab" 3f0))
    (assert-error 'variant:no-text-support (variant:context-caret-rect context :font "ab" 1))
    (assert-error 'variant:no-text-support
                  (variant:context-push-text context list :font "ab" 0f0 10f0 0))))

(define-test the-error-names-the-missing-operation ()
  (handler-case (variant:context-font (variant:make-context) "Menlo" 12)
    (variant:no-text-support (condition)
      (assert-eq 'variant:context-font (variant:no-text-support-operation condition)))))

;;; A host provides text by extending the context and specializing the operations.

(defstruct (text-context (:include variant:context)))

(defmethod variant:context-font ((context text-context) name size)
  (list name size))

(defmethod variant:context-measure ((context text-context) font string)
  (declare (ignore font))
  (values (* 6f0 (length string)) 10f0 3f0))

(define-test a-host-provides-text-by-extending-the-context ()
  (let ((context (make-text-context :width 100f0)))
    (assert-eql 100f0 (variant:context-width context))
    (assert-equal '("Menlo" 12) (variant:context-font context "Menlo" 12))
    (assert-equal '(18f0 10f0 3f0)
                  (multiple-value-list
                   (variant:context-measure context '("Menlo" 12) "abc")))
    ;; What the host did not provide is still an error.
    (assert-error 'variant:no-text-support
                  (variant:context-caret-x context '("Menlo" 12) "abc" 1))))

;;; --- no allocation ---

(defun deliver-many (variant context event count)
  "Compiled with this file, because lisp-unit2 evaluates test bodies under a
debugging policy that allocates."
  (declare (type fixnum count))
  (dotimes (i count)
    (variant:deliver-event variant context event)
    (variant:take-frame-request context)))

(define-test delivering-events-and-requesting-frames-allocates-nothing ()
  (let ((variant (make-instance 'counting-variant))
        (context (variant:make-context))
        (event (an-event)))
    (deliver-many variant context event 1000)       ; warm up the dispatch
    (let ((before (sb-ext:get-bytes-consed)))
      (deliver-many variant context event 200000)
      ;; 16 bytes an event would be 3.2 MB.
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        (assert-eql 201000 (event-count variant))
        (assert-true (< delta 500000) delta)))))

;;; --- the phases of a build ---

(defvar *sink* nil
  "Keeps the work of a test from being optimized away.")

(defun slot-ns (context slot)
  (aref (variant:context-phase-ns context) slot))

(define-test a-phase-adds-the-real-time-of-its-body-when-the-instrumentation-is-on ()
  (let ((context (variant:make-context)))
    (setf (variant:context-phases-p context) t)
    (variant:with-phase (context variant:+phase-layout+) (sleep 0.02))
    (variant:with-phase (context variant:+phase-layout+) (sleep 0.02))
    (assert-true (>= (slot-ns context variant:+phase-layout+) 35000000))
    (assert-eql 0 (slot-ns context variant:+phase-paint+))
    (assert-eql 0 (slot-ns context variant:+phase-update+))))

(define-test a-phase-does-nothing-when-the-instrumentation-is-off ()
  (let ((context (variant:make-context)))
    (variant:with-phase (context variant:+phase-paint+) (sleep 0.01))
    (assert-equalp (make-array variant:+phase-slots+ :element-type '(unsigned-byte 64)
                                                    :initial-element 0)
                   (variant:context-phase-ns context))))

(define-test a-phase-returns-the-values-of-its-body ()
  (let ((context (variant:make-context)))
    (dolist (on '(nil t))
      (setf (variant:context-phases-p context) on)
      (assert-equal '(1 2 3)
                    (multiple-value-list
                     (variant:with-phase (context variant:+phase-update+) (values 1 2 3)))))))

(define-test a-phase-is-timed-on-the-real-clock-whatever-the-core-clock-says ()
  (let ((context (variant:make-context))
        (virtual (clock:make-virtual-clock 1000)))
    (setf (variant:context-phases-p context) t)
    (clock:with-time-source ((clock:virtual-clock-source virtual))
      (variant:with-phase (context variant:+phase-layout+) (sleep 0.02)))
    (assert-true (>= (slot-ns context variant:+phase-layout+) 15000000))))

(define-test resetting-the-phases-clears-them ()
  (let ((context (variant:make-context)))
    (setf (variant:context-phases-p context) t)
    (variant:with-phase (context variant:+phase-layout+) (sleep 0.005))
    (variant:reset-phases context)
    (assert-eql 0 (slot-ns context variant:+phase-layout+))))

(define-test a-phase-allocates-nothing-on-or-off ()
  (let ((context (variant:make-context)))
    (dolist (on '(nil t))
      (setf (variant:context-phases-p context) on)
      (let ((before (sb-ext:get-bytes-consed)))
        (dotimes (i 300000)
          (variant:with-phase (context variant:+phase-layout+) (setf *sink* i)))
        (assert-true (< (- (sb-ext:get-bytes-consed) before) 1000000) on)))))

;;; --- the caret of text input ---

;;; A variant whose canvases are where it says, and that counts how often it is asked.

(defclass placed-variant (variant:variant)
  ((places :initarg :places :initform '() :accessor places)
   (asked :initform 0 :type fixnum :accessor asked)))

(defmethod variant:lookup-target ((variant placed-variant) id)
  (incf (asked variant))
  (let ((place (cdr (assoc id (places variant)))))
    (when place
      (values (first place) (second place) 100f0 50f0))))

(defun caret-rect (context)
  (coerce (variant:context-text-input-rect context) 'list))

(define-test a-caret-said-outside-a-canvas-is-taken-to-be-in-the-window ()
  (let ((variant (make-instance 'placed-variant :places '((:editor 30f0 40f0))))
        (context (variant:make-context)))
    (assert-false (variant:context-text-input-p context))
    (variant:set-text-input-caret context 10f0 20f0 2f0 14f0)
    (assert-true (variant:context-text-input-p context))
    (variant:place-text-input-caret variant context)
    (assert-equal '(10f0 20f0 2f0 14f0) (caret-rect context))
    (assert-eql 0 (asked variant))))

(define-test a-caret-said-in-a-canvas-is-put-where-the-variant-has-the-canvas ()
  (let ((variant (make-instance 'placed-variant :places '((:editor 30f0 40f0))))
        (context (variant:make-context)))
    (variant:with-painting (context :editor)
      (variant:set-text-input-caret context 10f0 20f0 2f0 14f0))
    (variant:place-text-input-caret variant context)
    (assert-equal '(40f0 60f0 2f0 14f0) (caret-rect context))))

(define-test a-canvas-that-moved-and-was-not-painted-again-has-its-caret-moved ()
  (let ((variant (make-instance 'placed-variant :places '((:editor 30f0 40f0))))
        (context (variant:make-context)))
    (variant:with-painting (context :editor)
      (variant:set-text-input-caret context 10f0 20f0 2f0 14f0))
    (variant:place-text-input-caret variant context)
    (setf (places variant) '((:editor 130f0 240f0)))
    (variant:place-text-input-caret variant context)
    (assert-equal '(140f0 260f0 2f0 14f0) (caret-rect context))
    ;; Placing again adds the canvas's place to the canvas's own caret, not to the last sum.
    (variant:place-text-input-caret variant context)
    (assert-equal '(140f0 260f0 2f0 14f0) (caret-rect context))))

(define-test a-canvas-the-variant-does-not-have-keeps-the-last-place ()
  (let ((variant (make-instance 'placed-variant :places '((:editor 30f0 40f0))))
        (context (variant:make-context)))
    (variant:with-painting (context :editor)
      (variant:set-text-input-caret context 10f0 20f0 2f0 14f0))
    (variant:place-text-input-caret variant context)
    (setf (places variant) '())
    (variant:place-text-input-caret variant context)
    (assert-equal '(40f0 60f0 2f0 14f0) (caret-rect context))))

(define-test nothing-is-asked-of-the-variant-until-a-canvas-takes-text-input ()
  (let ((variant (make-instance 'placed-variant :places '((:editor 30f0 40f0))))
        (context (variant:make-context)))
    (variant:place-text-input-caret variant context)
    (variant:with-painting (context :editor) nil)
    (variant:place-text-input-caret variant context)
    (assert-eql 0 (asked variant))
    (assert-false (variant:context-text-input-p context))))

(define-test painting-names-the-canvas-for-the-body-only ()
  (let ((context (variant:make-context)))
    (assert-false (variant:context-painting context))
    (variant:with-painting (context :outer)
      (assert-eql :outer (variant:context-painting context))
      (variant:with-painting (context :inner)
        (assert-eql :inner (variant:context-painting context)))
      (assert-eql :outer (variant:context-painting context)))
    (assert-false (variant:context-painting context))
    (catch :out
      (variant:with-painting (context :cut)
        (throw :out nil)))
    (assert-false (variant:context-painting context))))

(defun say-carets (context count)
  "Compiled with this file, as DELIVER-MANY is."
  (declare (type fixnum count))
  (dotimes (i count)
    (variant:with-painting (context :editor)
      (variant:set-text-input-caret context 10f0 20f0 2f0 14f0))))

(define-test saying-where-the-caret-is-allocates-nothing ()
  (let ((context (variant:make-context)))
    (say-carets context 1000)
    (let ((before (sb-ext:get-bytes-consed)))
      (say-carets context 300000)
      (assert-true (< (- (sb-ext:get-bytes-consed) before) 500000)))))
