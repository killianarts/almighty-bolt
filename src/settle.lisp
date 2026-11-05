(defpackage #:almighty-bolt/settle
  (:use #:cl)
  (:local-nicknames (#:dl #:almighty-bolt/display-list)
                    (#:clock #:almighty-bolt/clock)
                    (#:app-instance #:almighty-bolt/app-instance))
  (:export #:+max-builds+
           #:settle-frame
           #:settle-cap-reached
           #:settle-cap-reached-frame-number))

(in-package #:almighty-bolt/settle)

;;; The settle step of the frame sequence: build until the
;;; frame settles. A frame is settled when its build changed no app state. A
;;; app may change state while it builds (an immediate-mode button that returns
;;; true when clicked does), so widgets built before the change show old state. The
;;; host builds again, up to a cap, and only the settled build is presented.

(defconstant +max-builds+ 3
  "The most times FRAME is called for one frame.")

(define-condition settle-cap-reached (warning)
  ((frame-number :initarg :frame-number :reader settle-cap-reached-frame-number))
  (:report (lambda (condition stream)
             (format stream "Frame ~d did not settle in ~d builds; presenting the last."
                     (settle-cap-reached-frame-number condition) +max-builds+)))
  (:documentation "A frame whose build changed state every time. That is a defect
in the app or the library, such as a view that changes state on every build."))

(defun settle-frame (instance context display-list)
  "Call INSTANCE's FRAME on an empty DISPLAY-LIST until it returns false, at most
+MAX-BUILDS+ times. The list is cleared before each build, so it holds the last
build when this returns. Sets the context's build number (from 1) for each build.
A frame that reaches the cap signals a SETTLE-CAP-REACHED warning, which the
host logs. Returns the number of builds and true if the cap was reached. With the
context's instrumentation on, the real time of the first build and of all the builds
is added to the context's phase times, so that the cost of settling shows apart. When
the builds are done, a caret that a canvas gave in its own coordinates is put in the
window, where the canvas now is (PLACE-TEXT-INPUT-CARET). If the context has no room for another
glyph (GLYPHS-FULL), it forgets them all, the app forgets what it painted, and the frame is
built again."
  (flet ((settle ()
           (let ((builds 0)
                 (timed (app-instance:context-phases-p context)))
             (declare (type fixnum builds))
             (loop
               (incf builds)
               (setf (app-instance:context-build-number context) builds)
               (dl:clear-display-list display-list)
               (unless (if timed
                           (let* ((start (clock:real-time-source))
                                  (again (app-instance:frame instance context display-list))
                                  (took (- (clock:real-time-source) start))
                                  (slots (app-instance:context-phase-ns context)))
                             (when (= builds 1)
                               (incf (aref slots app-instance:+phase-first-build+) took))
                             (incf (aref slots app-instance:+phase-all-builds+) took)
                             again)
                           (app-instance:frame instance context display-list))
                 (return (values builds nil)))
               (when (>= builds +max-builds+)
                 (warn 'settle-cap-reached :frame-number (app-instance:context-frame-number context))
                 (return (values builds t)))))))
    (multiple-value-prog1
        ;; With no room for another glyph, the glyphs are forgotten, and so is every command kept
        ;; from frames before (they name the glyphs by where they were), and the frame is built
        ;; again. A frame that does not fit in an empty atlas is an error.
        (handler-case (settle)
          (app-instance:glyphs-full ()
            (app-instance:context-forget-glyphs context)
            (app-instance:forget-painting instance)
            (settle)))
      (app-instance:place-text-input-caret instance context))))
