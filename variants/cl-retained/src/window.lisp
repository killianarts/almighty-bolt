(defpackage #:almighty-bolt-variant-retained/window
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:now-ns
                #:make-pointer-event
                #:make-key-event
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:key-event
                #:key-event-kind
                #:key-event-physical-key
                #:key-event-logical-key
                #:key-event-modifiers
                #:key-event-repeat-p
                #:modifier-down-p
                #:+modifier-shift+
                #:+modifier-control+
                #:+modifier-alt+
                #:+modifier-super+
                #:handle-event
                #:hit-target
                #:frame-ring-total
                #:lookup-target)
  (:import-from #:almighty-bolt-core/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt-core/platform/macos/host
                #:run-variant
                #:host-ring
                #:host-deliver-event
                #:close-host-window)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label-text
                #:button-state)
  (:import-from #:almighty-bolt-variant-retained/node
                #:node-id)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:retained-variant
                #:retained-variant-focus)
  (:import-from #:almighty-bolt-variant-retained/counter
                #:make-counter)
  (:export #:main))

(in-package #:almighty-bolt-variant-retained/window)

;;; The Counter in a window. With SELFCHECK, a thread clicks the button for you by
;;; handing pointer events to the host, and reports what happened. That checks the
;;; path from an event through the variant to a drawn frame. It does not check how the
;;; operating system's own events are turned into positions, or keys: only a real
;;; click in the window does. So without SELFCHECK, the variant logs the presses,
;;; releases and keys that reach it, and what is under the pointer, to say what the
;;; window really delivered. The log prints, which allocates, so it is a subclass made
;;; here and used only by a person; it is not part of the variant or of the check.

(defclass logged-variant (retained-variant) ()
  (:documentation "A retained variant that prints the events that matter."))

(defun modifier-names (modifiers)
  "The names of the modifier keys in the bit set MODIFIERS, as a string."
  (let ((names (loop :for (flag name) :in (list (list +modifier-shift+ "shift")
                                                  (list +modifier-control+ "control")
                                                  (list +modifier-alt+ "option")
                                                  (list +modifier-super+ "command"))
                     :when (modifier-down-p modifiers flag)
                       :collect name)))
    (if names
        (format nil "~{~a~^+~}" names)
        "none")))

(defmethod handle-event :before ((variant logged-variant) event)
  (typecase event
    (pointer-event
     (when (member (pointer-event-kind event) '(:down :up))
       (let ((x (float (pointer-event-x event) 1f0))
             (y (float (pointer-event-y event) 1f0)))
         (format t "~&pointer ~(~4a~) button ~a at ~,1f, ~,1f   under it: ~s~%"
                 (pointer-event-kind event) (pointer-event-button event) x y
                 (hit-target variant x y)))))
    (key-event
     (when (eq (key-event-kind event) :down)
       (format t "~&key down  logical ~s physical ~d modifiers ~a~:[~; (repeat)~]~%"
               (key-event-logical-key event) (key-event-physical-key event)
               (modifier-names (key-event-modifiers event)) (key-event-repeat-p event)))))
  (finish-output))

(defmethod handle-event :after ((variant logged-variant) event)
  (when (and (typep event 'key-event) (eq (key-event-kind event) :down))
    (let ((focus (retained-variant-focus variant)))
      (format t "~&          focus is now on ~s~%" (and focus (node-id focus)))
      (finish-output))))

(defun draws (host)
  (frame-ring-total (host-ring host)))

(defun wait-until (predicate &key (timeout-ns 2000000000))
  (let ((deadline (+ (now-ns) timeout-ns)))
    (loop
      (when (funcall predicate) (return t))
      (when (> (now-ns) deadline) (return nil))
      (sleep 0.001))))

(defun send (host kind x y &key (button nil) (buttons 0))
  "Deliver a pointer event on the main thread, as the operating system's would be,
and wait until the host has drawn the frame it asked for, if it asked for one."
  (let ((before (draws host)))
    (call-on-main (lambda ()
                    (host-deliver-event host
                                        (make-pointer-event :timestamp (now-ns) :kind kind
                                                            :x (float x 1d0) :y (float y 1d0)
                                                            :button button :buttons buttons))))
    ;; Give a requested frame time to be drawn, then see whether there was one.
    (wait-until (lambda () (> (draws host) before)) :timeout-ns 100000000)
    (- (draws host) before)))

(defun send-key (host key &key (modifiers 0))
  "Deliver a key going down on the main thread; returns the frames it caused."
  (let ((before (draws host)))
    (call-on-main (lambda ()
                    (host-deliver-event host (make-key-event :timestamp (now-ns) :kind :down
                                                             :logical-key key
                                                             :modifiers modifiers))))
    (wait-until (lambda () (> (draws host) before)) :timeout-ns 100000000)
    (- (draws host) before)))

(defun self-check (host variant number)
  (sleep 0.5)
  (multiple-value-bind (x y width height) (lookup-target variant :count-button)
    (let ((cx (+ x (/ width 2))) (cy (+ y (/ height 2))))
      (format t "~&--- The retained Counter, clicked by a thread ---~%")
      (format t "the button is at ~,1f, ~,1f and ~,1f by ~,1f points~%" x y width height)
      (let ((frames (list (send host :move cx cy)
                          (send host :down cx cy :button 0 :buttons 1)
                          (send host :up cx cy :button 0))))
        (format t "click on the button:       frames drawn after move, down, up: ~{~d~^, ~}~%"
                frames)
        (format t "                           the label shows ~s~%" (label-text number)))
      ;; Press, drag off the button, release elsewhere: no click.
      (send host :move cx cy)
      (send host :down cx cy :button 0 :buttons 1)
      (send host :move 1 1 :buttons 1)
      (send host :up 1 1 :button 0)
      (format t "press, drag off, release:  the label shows ~s (it must not change)~%"
              (label-text number))
      ;; A move inside the button, and over empty space, asks for no frame.
      (send host :move cx cy)
      (let ((inside (send host :move (+ cx 1) cy))
            (outside (progn (send host :move 1 1) (send host :move 2 2))))
        (format t "moves that change nothing: ~d frames inside the button, ~d over empty space~%"
                inside outside))
      ;; The keyboard: Tab focuses the button, and the space bar and Return click it.
      (let ((shown (label-text number))
            (tab (send-key host #\Tab))
            (space (send-key host #\Space))
            (return (send-key host #\Return)))
        (format t "Tab, Space, Return:        ~d, ~d, ~d frames; the label went from ~s to ~s~%"
                tab space return shown (label-text number)))
      (let ((before (draws host)))
        (sleep 1)
        (format t "idle for a second:         ~d frames drawn~%" (- (draws host) before)))
      (finish-output)
      (close-host-window host))))

(defun main (&key selfcheck)
  "Open the Counter in a window and run until it is closed."
  (multiple-value-bind (variant number) (make-counter)
    (unless selfcheck
      (change-class variant 'logged-variant)
      (format t "~&Click the button. Press it, drag off it and let go. Press Tab, Shift-Tab, ~
                 Space and Return. Close the window to finish.~%")
      (finish-output))
    (run-variant variant :title "Counter (retained)" :width 360 :height 160
                         :driver (and selfcheck
                                      (lambda (host) (self-check host variant number))))))
