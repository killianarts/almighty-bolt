(defpackage #:almighty-bolt-7guis/t/acceptance/driver
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:registry #:almighty-bolt-7guis/common/registry))
  (:export #:driver
           #:driver-variant
           #:driver-list
           #:define-acceptance
           #:available-variants
           #:start
           #:redraw
           #:rectangle
           #:click
           #:click-at
           #:right-click
           #:press-at
           #:drag-to
           #:release-at
           #:move-to
           #:type-text
           #:press-key
           #:scroll
           #:scroll-points
           #:swipe-points
           #:text-in
           #:text-within
           #:screen-text
           #:found-p
           #:advance-time
           #:frame-requested-p
           #:colors-in
           #:shapes-in))

(in-package #:almighty-bolt-7guis/t/acceptance/driver)

;;; A user, as a test: it starts an app by variant and task, finds its controls by id
;;; through the variant's HIT-TARGET and LOOKUP-TARGET, drives them with pointer and key
;;; events as a window would deliver them, and reads what the app shows from the display
;;; list, which with the fake context draws a character as a command whose parameter is
;;; its code. Nothing here knows which variant it is driving, so the same tests hold all
;;; of them to the same behaviour.

(defstruct (driver (:constructor %make-driver (variant context clock list)))
  variant
  context
  clock
  list
  ;; How many trackpad gestures have been made, so that each starts a second after the last.
  (gestures 0))

(defmacro define-acceptance (name task &body body)
  "Define a test for each variant that has an app for TASK (those loaded when this is
compiled), named NAME/VARIANT. BODY runs with DRIVER (a symbol in the package of the
caller) bound to a driver that has started the app."
  (let ((driver (intern "DRIVER" *package*)))
    `(progn
       ,@(loop :for variant :in (available-variants task)
               :collect `(lisp-unit2:define-test ,(intern (format nil "~a/~a" name variant)
                                                          *package*)
                             ()
                           (let ((,driver (start ,variant ,task)))
                             (declare (ignorable ,driver))
                             ,@body))))))

(defun available-variants (task)
  "The variants that have an app for TASK."
  (loop :for variant :in registry:+variants+
        :when (registry:find-app variant task)
          :collect variant))

(defun start (variant task)
  "Start the app of TASK in VARIANT and draw its first frame, and return the driver. The
clock the app sees is a virtual one, which ADVANCE-TIME moves."
  (let* ((app (or (registry:find-app variant task)
                  (error "No ~(~a~) app for ~(~a~)." variant task)))
         (context (fake:make-fake-context :width (float (registry:app-width app) 1f0)
                                          :height (float (registry:app-height app) 1f0)))
         (clock (core:make-virtual-clock 1000000000))
         (driver (%make-driver nil context clock (core:make-display-list :capacity 65536))))
    (let ((core:*time-source* (core:virtual-clock-source clock)))
      (setf (driver-variant driver) (funcall (registry:app-constructor app))))
    (redraw driver)
    driver))

(defmacro with-clock ((driver) &body body)
  `(let ((core:*time-source* (core:virtual-clock-source (driver-clock ,driver))))
     ,@body))

(defun redraw (driver &optional (frames 1))
  "Build FRAMES frames, as the host does when it is asked for them, and keep the last
frame's list. A frame that changed state is built again, up to the limit."
  (with-clock (driver)
    (dotimes (i frames)
      (core:settle-frame (driver-variant driver) (driver-context driver) (driver-list driver)))
    (driver-list driver)))

(defun advance-time (driver seconds &key (frames 1))
  "Let SECONDS pass and draw FRAMES frames, the first after the time has passed."
  (core:advance-virtual-clock (driver-clock driver) (round (* seconds 1000000000)))
  (redraw driver frames))

(defun frame-requested-p (driver)
  "True if the app asked for another frame since this was last asked, which an app that
has nothing going on does not."
  (core:take-frame-request (driver-context driver)))

(defun rectangle (driver id)
  "The rectangle of the control ID as a list x, y, width, height, or an error."
  (multiple-value-bind (x y width height)
      (core:lookup-target (driver-variant driver) id)
    (unless x (error "The app shows no control ~s." id))
    (list x y width height)))

(defun found-p (driver id)
  (and (core:lookup-target (driver-variant driver) id) t))

(defun deliver (driver event)
  (with-clock (driver)
    (core:handle-event (driver-variant driver) event))
  ;; The host would draw a frame if one was wanted; a test always does, which also
  ;; shows what an event that asked for nothing left on the screen.
  (redraw driver))

(defun pointer (driver kind x y &key button (buttons 0))
  (deliver driver (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                           :y (float y 1d0) :button button :buttons buttons)))

(defun centre (driver id)
  (destructuring-bind (x y width height) (rectangle driver id)
    (values (+ x (/ width 2)) (+ y (/ height 2)))))

(defun click-at (driver id fx fy &key (button 0))
  "Press and release the pointer at the fraction FX, FY of the way across and down the
control ID."
  (destructuring-bind (x y width height) (rectangle driver id)
    (let ((px (+ x (* fx width)))
          (py (+ y (* fy height))))
      (pointer driver :move px py)
      (pointer driver :down px py :button button :buttons (if (zerop button) 1 2))
      (pointer driver :up px py :button button))))

(defun click (driver id)
  "Click in the middle of the control ID."
  (click-at driver id 0.5 0.5))

(defun right-click (driver id &optional (fx 0.5) (fy 0.5))
  (click-at driver id fx fy :button 1))

(defun move-to (driver id &optional (fx 0.5) (fy 0.5))
  (destructuring-bind (x y width height) (rectangle driver id)
    (pointer driver :move (+ x (* fx width)) (+ y (* fy height)))))

(defun press-at (driver id fx fy)
  (destructuring-bind (x y width height) (rectangle driver id)
    (pointer driver :move (+ x (* fx width)) (+ y (* fy height)))
    (pointer driver :down (+ x (* fx width)) (+ y (* fy height)) :button 0 :buttons 1)))

(defun drag-to (driver id fx fy)
  "Move the pointer with the primary button down to the fraction FX, FY of control ID."
  (destructuring-bind (x y width height) (rectangle driver id)
    (pointer driver :move (+ x (* fx width)) (+ y (* fy height)) :buttons 1)))

(defun release-at (driver id fx fy)
  (destructuring-bind (x y width height) (rectangle driver id)
    (pointer driver :up (+ x (* fx width)) (+ y (* fy height)) :button 0)))

(defun press-key (driver character &key (modifiers 0))
  (deliver driver (core:make-key-event :timestamp 0 :kind :down :logical-key character
                                       :modifiers modifiers)))

(defun type-text (driver string)
  (loop :for character :across string
        :do (press-key driver character)))

(defun scroll (driver id dy)
  "Scroll the wheel by DY over the middle of the control ID."
  (multiple-value-bind (x y) (centre driver id)
    (deliver driver (core:make-wheel-event :timestamp 0 :x (float x 1d0) :y (float y 1d0)
                                           :dx 0d0 :dy (float dy 1d0)))))

(defun swipe-points (driver id steps)
  "Swipe a trackpad over the middle of the control ID: STEPS are lists (dx dy), in points,
delivered 10 ms apart as one gesture, which starts a second after the one before."
  (multiple-value-bind (x y) (centre driver id)
    (let ((start (* 1000000000 (incf (driver-gestures driver)))))
      (loop :for (dx dy) :in steps
            :for i :from 0
            :do (deliver driver (core:make-wheel-event :timestamp (+ start (* i 10000000))
                                                       :x (float x 1d0) :y (float y 1d0)
                                                       :dx (float dx 1d0) :dy (float dy 1d0)
                                                       :precise-p t))))))

(defun scroll-points (driver id &key (dx 0) (dy 0))
  "Scroll a trackpad by DX across and DY down, in points, in one event, as a gesture of its
own."
  (swipe-points driver id (list (list dx dy))))

(defun glyph-string (list &optional (test (constantly t)))
  "The characters of the glyph commands of LIST that TEST (given x and y) accepts, in the
order they were drawn, as a string."
  (coerce (loop :for index :below (core:display-list-count list)
                :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                           (funcall test (core:display-list-x list index)
                                    (core:display-list-y list index)))
                  :collect (code-char (round (core:display-list-parameter list index))))
          'string))

(defun screen-text (driver)
  "Everything the app shows as text, in the order it was drawn."
  (glyph-string (driver-list driver)))

(defun text-in (driver id)
  "The text drawn inside the control ID."
  (destructuring-bind (x y width height) (rectangle driver id)
    (glyph-string (driver-list driver)
                  (lambda (gx gy)
                    (and (<= x gx) (< gx (+ x width)) (<= y gy) (< gy (+ y height)))))))

(defun text-within (driver x y width height)
  "The text drawn inside the rectangle X, Y, WIDTH by HEIGHT of the window."
  (glyph-string (driver-list driver)
                (lambda (gx gy)
                  (and (<= x gx) (< gx (+ x width)) (<= y gy) (< gy (+ y height))))))

(defun colors-in (driver id)
  "The colours of the rectangles and rounded rectangles drawn inside the control ID, in
order, which say how it looks (a fill, a state) without saying how it is made."
  (destructuring-bind (x y width height) (rectangle driver id)
    (let ((list (driver-list driver)))
      (loop :for index :below (core:display-list-count list)
            :for gx = (core:display-list-x list index)
            :for gy = (core:display-list-y list index)
            :when (and (member (core:display-list-kind list index)
                               (list core:+command-rect+ core:+command-rounded-rect+))
                       (<= (- x 4) gx) (< gx (+ x width 4)) (<= (- y 4) gy) (< gy (+ y height 4)))
              :collect (core:display-list-color list index)))))

(defun shapes-in (driver id)
  "The rectangles and rounded rectangles drawn with their middle inside the control ID,
as lists of (kind x y width height color), kind being :rect or :rounded, in the order
drawn. A shape that spills over the edge of the control still counts as in it."
  (destructuring-bind (x y width height) (rectangle driver id)
    (let ((list (driver-list driver)))
      (loop :for index :below (core:display-list-count list)
            :for kind = (core:display-list-kind list index)
            :for gx = (core:display-list-x list index)
            :for gy = (core:display-list-y list index)
            :for gw = (core:display-list-width list index)
            :for gh = (core:display-list-height list index)
            :when (and (or (= kind core:+command-rect+) (= kind core:+command-rounded-rect+))
                       (<= x (+ gx (/ gw 2)) (+ x width)) (<= y (+ gy (/ gh 2)) (+ y height)))
              :collect (list (if (= kind core:+command-rect+) :rect :rounded)
                             gx gy gw gh (core:display-list-color list index))))))
