(defpackage #:almighty-bolt-core/settle
  (:use #:cl)
  (:import-from #:almighty-bolt-core/display-list
                #:clear-display-list)
  (:import-from #:almighty-bolt-core/clock
                #:real-time-source)
  (:import-from #:almighty-bolt-core/variant
                #:frame
                #:place-text-input-caret
                #:context-frame-number
                #:context-build-number
                #:context-phases-p
                #:context-phase-ns
                #:+phase-first-build+
                #:+phase-all-builds+)
  (:export #:+max-builds+
           #:settle-frame
           #:settle-cap-reached
           #:settle-cap-reached-frame-number))

(in-package #:almighty-bolt-core/settle)

;;; The settle step of the frame sequence in decision record 0009: build until the
;;; frame settles. A frame is settled when its build changed no app state. A
;;; variant may change state while it builds (an immediate-mode button that returns
;;; true when clicked does), so widgets built before the change show old state. The
;;; core builds again, up to a cap, and only the settled build is presented.

(defconstant +max-builds+ 3
  "The most times FRAME is called for one frame.")

(define-condition settle-cap-reached (warning)
  ((frame-number :initarg :frame-number :reader settle-cap-reached-frame-number))
  (:report (lambda (condition stream)
             (format stream "Frame ~d did not settle in ~d builds; presenting the last."
                     (settle-cap-reached-frame-number condition) +max-builds+)))
  (:documentation "A frame whose build changed state every time. That is a defect
in the variant or the app, such as a view that changes state on every build."))

(defun settle-frame (variant context display-list)
  "Call VARIANT's FRAME on an empty DISPLAY-LIST until it returns false, at most
+MAX-BUILDS+ times. The list is cleared before each build, so it holds the last
build when this returns. Sets the context's build number (from 1) for each build.
A frame that reaches the cap signals a SETTLE-CAP-REACHED warning, which the
host logs. Returns the number of builds and true if the cap was reached. With the
context's instrumentation on, the real time of the first build and of all the builds
is added to the context's phase times, so that the cost of settling shows apart. When
the builds are done, a caret that a canvas gave in its own coordinates is put in the
window, where the canvas now is (PLACE-TEXT-INPUT-CARET)."
  (let ((builds 0)
        (timed (context-phases-p context)))
    (declare (type fixnum builds))
    (multiple-value-prog1
        (loop
          (incf builds)
          (setf (context-build-number context) builds)
          (clear-display-list display-list)
          (unless (if timed
                      (let* ((start (real-time-source))
                             (again (frame variant context display-list))
                             (took (- (real-time-source) start))
                             (slots (context-phase-ns context)))
                        (when (= builds 1)
                          (incf (aref slots +phase-first-build+) took))
                        (incf (aref slots +phase-all-builds+) took)
                        again)
                      (frame variant context display-list))
            (return (values builds nil)))
          (when (>= builds +max-builds+)
            (warn 'settle-cap-reached :frame-number (context-frame-number context))
            (return (values builds t))))
      (place-text-input-caret variant context))))
