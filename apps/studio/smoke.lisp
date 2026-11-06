;;;; Check, in a real window, the studio as a screen reader and a person's hands use it: the counter
;;;; example run on the canvas, read through the system's accessibility API (AXUIElement, as
;;;; VoiceOver reads it), pressed there at 100% and at 200%, made again when its code is compiled, and
;;;; a pane moved by its tab's menu, shown as VoiceOver's Show Menu shows it. From the repository
;;;; root:
;;;;   make studio-smoke
;;;; It says "The studio check passed" when every check holds. The process has to be allowed to use
;;;; the API (System Settings, Privacy & Security, Accessibility: the terminal it was started from);
;;;; if it is not, it says that the check was skipped and why. What is left to a person: an input
;;;; method's candidates by the caret, and how sharp text is at 200%.

(asdf:load-system "almighty-bolt-studio")
(asdf:load-system "almighty-bolt-examples")
(load (merge-pathnames "examples/ax-client.lisp" (uiop:getcwd)))

(defpackage #:almighty-bolt-studio/smoke
  (:use #:cl #:almighty-bolt-examples/ax-client)
  (:local-nicknames (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:bolt #:almighty-bolt)
                    (#:host-session #:almighty-bolt/machine/host-session)
                    (#:core #:almighty-bolt/internals)
                    (#:host #:almighty-bolt/platform/macos/host)))

(in-package #:almighty-bolt-studio/smoke)

(defparameter *counter-file*
  (merge-pathnames "examples/counter.lisp" (uiop:getcwd))
  "The counter example, which is put back as it was after it is changed here.")

(defvar *failures* 0)

(defun check (what ok &optional detail)
  (unless ok (incf *failures*))
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ (~a)~]~%" ok what (and (not ok) detail))
  (finish-output)
  ok)

(defun wait-for (predicate seconds)
  "True as soon as PREDICATE holds, NIL if it does not within SECONDS."
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) end) (return nil))
      (sleep 0.02))))

(defun inside-p (frame outer)
  (and frame outer
       (destructuring-bind (x y width height) frame
         (destructuring-bind (ox oy ow oh) outer
           (and (<= (- ox 0.5) x) (<= (- oy 0.5) y)
                (<= (+ x width) (+ ox ow 0.5)) (<= (+ y height) (+ oy oh 0.5)))))))

(defun redefine-counter (label)
  "Compile the counter example's MAKE-COUNTER with its button saying LABEL, as an edit and C-c C-c do."
  (let ((*package* (find-package "ALMIGHTY-BOLT-EXAMPLES/COUNTER")))
    (eval (read-from-string
           (format nil "(defun make-counter ()
                          (bolt:state-let ((count 0))
                            (lambda ()
                              (bolt:padding (:all 24)
                                (bolt:row (:gap 16 :cross-align :center)
                                  (bolt:label (princ-to-string count) :id :count :size 32 :live t)
                                  (when (bolt:button ~s :id :count-button)
                                    (incf count)))))))"
                   label)))))

(defun checks (host)
  (let ((application (application))
        (session (host-session:open-session)))
    (flet ((find-it (identifier) (anywhere application identifier))
           (settle () (host:wait-for-idle host)))
      ;; The counter, named on the canvas.
      (m:click session :id :canvas-pane)
      (m:set-value session "almighty-bolt-examples/counter:make-counter" :id :app-name)
      (m:set-value session "Run" :id :canvas-mode)
      (settle)
      (check "the counter runs on the canvas, and the system finds its button"
             (wait-for (lambda () (find-it "count-button")) 5))
      (let ((button (find-it "count-button"))
            (pane (find-it "running")))
        (check "its button is a button called by its words, as VoiceOver reads it"
               (and button (equal "AXButton" (said button "AXRole"))
                    (equal "Count" (said button "AXDescription")))
               (and button (list (said button "AXRole") (said button "AXDescription"))))
        (check "and it is inside the pane, where the pane shows it"
               (inside-p (and button (ax-frame button)) (and pane (ax-frame pane)))
               (list (and button (ax-frame button)) (and pane (ax-frame pane))))
        (check "the system presses it, and the counter counts"
               (and button (ax-perform button "AXPress")
                    (progn (settle) (wait-for (lambda ()
                                                (equal "1" (said (find-it "count") "AXValue")))
                                              3))))
        ;; Twice as large: the button is twice as wide, and still pressed where it is.
        (let ((before (and button (ax-frame button))))
          (m:set-value session "200%" :id :zoom)
          (settle)
          (let ((after (ax-frame (find-it "count-button"))))
            (check "at 200% the button is twice as wide on the screen"
                   (and before after (< (abs (- (third after) (* 2 (third before)))) 1.5))
                   (list before after)))
          (check "and the system presses it there"
                 (and (ax-perform (find-it "count-button") "AXPress")
                      (progn (settle) (wait-for (lambda ()
                                                  (equal "2" (said (find-it "count") "AXValue")))
                                                3)))
                 (said (find-it "count") "AXValue"))))
      ;; Its code compiled: the app in the pane is made again from it, with its count.
      (unwind-protect
           (progn
             (redefine-counter "Count up")
             (check "compiling the counter makes the app in the pane again, from the new code"
                    (wait-for (lambda () (equal "Count up"
                                                (said (find-it "count-button") "AXDescription")))
                              10)
                    (said (find-it "count-button") "AXDescription"))
             (check "and its count is where it was"
                    (equal "2" (said (find-it "count") "AXValue"))
                    (said (find-it "count") "AXValue")))
        (load *counter-file*))
      (check "the counter as it is in its file is back in the pane"
             (wait-for (lambda () (equal "Count" (said (find-it "count-button") "AXDescription")))
                       10))
      ;; A tab offers its menu to VoiceOver, and what has no menu does not.
      (let ((tab (find-it "preview-pane")))
        (check "a tab can be pressed and offers its menu"
               (and tab (member "AXPress" (ax-actions tab) :test #'equal)
                    (member "AXShowMenu" (ax-actions tab) :test #'equal))
               (and tab (ax-actions tab)))
        (check "a button with no menu does not offer one"
               (not (member "AXShowMenu" (ax-actions (find-it "count-button")) :test #'equal))
               (ax-actions (find-it "count-button")))
        ;; Shown, and Move to Next Group pressed in it: the pane goes to the group on the right.
        (check "the system shows the tab's menu" (and tab (ax-perform tab "AXShowMenu")))
        (settle)
        (let ((item nil))
          (check "the menu's items are read"
                 (wait-for (lambda ()
                             (setf item (find "Move to Next Group"
                                              (everything-in (window-elements application))
                                              :key (lambda (element)
                                                     (said element "AXDescription"))
                                              :test #'equal)))
                           3))
          (check "and pressing Move to Next Group moves the pane to the next group"
                 (and item (ax-perform item "AXPress")
                      (progn (settle)
                             (wait-for (lambda ()
                                         (let ((moved (ax-frame (find-it "preview-pane")))
                                               (beside (ax-frame (find-it "inspect-pane"))))
                                           (and moved beside (> (first moved) (first beside)))))
                                       3)))
                 (list (ax-frame (find-it "preview-pane")) (ax-frame (find-it "inspect-pane"))))))
      (check "the window is at rest" (settle)))))

(defvar *skipped* nil "Why the checks could not be made, or NIL.")

(defun readable-p (host)
  (wait-for (lambda () (plusp (core:frame-ring-total (host:host-ring host)))) 20)
  (sleep 1.0)
  (cond ((not (trusted-p))
         (setf *skipped* "this process may not use the system's accessibility API. Allow the terminal in System Settings, Privacy & Security, Accessibility.")
         nil)
        ((not (wait-for (lambda ()
                          (let ((window (window-elements (application))))
                            (and window (equal "AXWindow" (said window "AXRole")))))
                        5))
         (setf *skipped* "the system does not list the window (the screen is locked, or nobody is logged in at it).")
         nil)
        (t t)))

(defun drive (host)
  (unwind-protect
       (if (readable-p host)
           (handler-case (checks host)
             (error (condition)
               (check "the checks ran to the end" nil condition)))
           (format t "~&The studio check was skipped: ~a~%" *skipped*))
    (unless *skipped*
      (if (zerop *failures*)
          (format t "~&The studio check passed.~%")
          (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*)))
    (finish-output)
    (host:close-host-window host)))

(bolt:run-app 'almighty-bolt-studio:make-studio :title "almighty-bolt studio" :width 1240 :height 820
                                            :hot t :driver #'drive)
