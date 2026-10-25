(defpackage #:almighty-bolt-core/t/replay/support
  (:use #:cl)
  (:local-nicknames (#:variant #:almighty-bolt-core/variant)
                    (#:event #:almighty-bolt-core/event)
                    (#:clock #:almighty-bolt-core/clock)
                    (#:dl #:almighty-bolt-core/display-list))
  (:export #:button-variant
           #:button-count
           #:timer-variant
           #:make-test-context))

(in-package #:almighty-bolt-core/t/replay/support)

;;; Two small variants for the tests of replay. The button counts the presses of the primary
;;; button over it (and of Space) and draws itself wider for each, on a background as wide
;;; as the window; it answers HIT-TARGET and LOOKUP-TARGET for its one control, :BUTTON at
;;; 10, 10, 100 by 30. The timer draws a bar as long as the time since its first frame, in
;;; centiseconds, and asks for a frame until a second has gone.

(defclass button-variant (variant:variant)
  ((count :initform 0 :accessor button-count)))

(defun over-button-p (x y)
  (and (<= 10 x) (< x 110) (<= 10 y) (< y 40)))

(defmethod variant:handle-event ((variant button-variant) event)
  (typecase event
    (event:pointer-event
     (when (and (eq (event:pointer-event-kind event) :down)
                (over-button-p (event:pointer-event-x event) (event:pointer-event-y event)))
       (incf (button-count variant))
       t))
    (event:key-event
     (when (and (eq (event:key-event-kind event) :down)
                (eql (event:key-event-logical-key event) #\Space))
       (incf (button-count variant))
       t))
    (event:resize-event t)))

(defmethod variant:frame ((variant button-variant) context list)
  (dl:push-rect list 0 0 (variant:context-width context) (variant:context-height context)
                #x202020FF)
  (dl:push-rect list 10 10 (+ 20 (* 10 (button-count variant))) 30 #x4060C0FF)
  nil)

(defmethod variant:hit-target ((variant button-variant) x y)
  (when (over-button-p x y) :button))

(defmethod variant:lookup-target ((variant button-variant) id)
  (when (eq id :button)
    (values 10f0 10f0 100f0 30f0)))

(defclass timer-variant (variant:variant)
  ((start :initform nil :accessor start)))

(defmethod variant:frame ((variant timer-variant) context list)
  (let* ((now (clock:now-ns))
         (start (or (start variant) (setf (start variant) now)))
         (elapsed (- now start)))
    (dl:push-rect list 0 0 (/ elapsed 10000000) 10 #xFF0000FF)
    (when (< elapsed 1000000000)
      (variant:request-frame context))
    nil))

(defun make-test-context ()
  (variant:make-context :width 200f0 :height 100f0 :scale 1f0))
