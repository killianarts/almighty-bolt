;;;; Check, in real windows, what an app with more than one window gets: a second window opened by a
;;;; button of the first, which shows what the first holds and changes it; another opened from the
;;;; REPL and closed; both made again when code is compiled; a window that belongs to another closing
;;;; with it; a window whose frames fail going on, and mended by a press; Quit closing them all and
;;;; leaving the Lisp; and the loop starting again. From the
;;;; repository root:
;;;;   make windows-smoke
;;;; The main thread waits, as a REPL's waits for the editor, and another thread does what the
;;;; editor's forms do. It says "The windows check passed" when every check holds.

(asdf:load-system "almighty-bolt")

(defpackage #:almighty-bolt-examples/windows-smoke
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics))
  (:import-from #:almighty-bolt/machine/host-session
                #:open-session)
  (:import-from #:almighty-bolt/internals
                #:frame-ring-total
                #:make-key-event
                #:make-pointer-event
                #:lookup-target
                #:+modifier-super+
                #:+modifier-alt+)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:import-from #:almighty-bolt/platform/macos/host
                #:host-app
                #:host-ring
                #:host-idle-p
                #:host-owner
                #:host-deliver-event
                #:wait-for-idle
                #:open-hosts
                #:host-open-p
                #:running-host
                #:open-host
                #:close-host))

(in-package #:almighty-bolt-examples/windows-smoke)

(defvar *failures* 0)

(defun check (what ok &optional detail)
  (unless ok (incf *failures*))
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ (~a)~]~%" ok what detail)
  (finish-output)
  ok)

(defun wait-for (predicate &optional (seconds 5))
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) end) (return nil))
      (sleep 0.01))))

(defun frames (host)
  (frame-ring-total (host-ring host)))

;;; The app: a count that both windows show and change. The second window is opened by a button of
;;; the first, in its frame, on the main thread; what they share is a special variable.

(defvar *count* 0)
(defvar *second* nil "The window the first one opened.")
(defvar *below* nil "The window the second one opened below itself.")

(defun make-first ()
  (lambda ()
    (bolt:column (:gap 8)
      (bolt:label (format nil "~d" *count*) :id :count)
      (when (bolt:button "Add" :id :add)
        (incf *count*))
      (when (bolt:button "Another window" :id :open)
        (setf *second* (bolt:open-window 'make-second :title "Second" :width 240 :height 120))))))

(defun make-second ()
  (lambda ()
    (bolt:column (:gap 8)
      (bolt:label (format nil "~d" *count*) :id :count)
      (when (bolt:button "Add ten" :id :add-ten)
        (incf *count* 10))
      (when (bolt:button "Below" :id :below)
        (setf *below* (bolt:open-window 'make-closing :title "Below" :width 200 :height 80
                                                      :beside (bolt:this-window) :side :below))))))

(defun make-closing ()
  (lambda ()
    (when (bolt:button "Close" :id :close)
      (bolt:close-window (bolt:this-window)))))

(defun make-third ()
  (lambda ()
    (bolt:label "Third" :id :third)))

(defun count-in (window)
  (s:node-name (m:find-node (open-session window) :id :count)))

(defun at-rest-p (hosts)
  "True if HOSTS are all idle, and stay so for half a second: no window keeps waking another."
  (and (every #'wait-for-idle hosts)
       (let ((before (mapcar #'frames hosts)))
         (sleep 0.5)
         (and (every #'host-idle-p hosts)
              (equal before (mapcar #'frames hosts))))))

(defun quit ()
  "What Command-Q does."
  (call-on-main (lambda ()
                  (msg-send :void (msg-send :id (objc-class "NSApplication") "sharedApplication")
                            "terminate:" (:id 0)))))

(defun frame-of (host)
  "Where HOST's window is: x, y (down from the top of the screen's usable area), width and height."
  (call-on-main (lambda ()
                  (multiple-value-list
                   (almighty-bolt/platform/macos/window:window-frame
                    (almighty-bolt/platform/macos/host:host-window host))))))

(defun usable-width ()
  "The width and the left of the main screen's usable area."
  (call-on-main (lambda ()
                  (multiple-value-bind (x y w h)
                      (msg-send :rect (msg-send :id (objc-class "NSScreen") "mainScreen") "visibleFrame")
                    (declare (ignore y h))
                    (list x w)))))

(defun near (a b) (< (abs (- a b)) 0.5))

(defun command-option-i (host)
  (call-on-main (lambda ()
                  (host-deliver-event host (make-key-event :timestamp 0 :kind :down :logical-key #\i
                                                           :modifiers (logior +modifier-super+
                                                                              +modifier-alt+))))))

(defun the-inspector-of (host)
  (find host (open-hosts) :key #'host-owner))

(defun says (window)
  "What the inspector's window shows, as the names of what is in it."
  (let ((names '()))
    (s:walk-nodes (lambda (node) (let ((name (s:node-name node))) (when name (push name names))))
                  (almighty-bolt/internals:accessibility-tree (host-app window)))
    (format nil "~{~a~^ | ~}" (nreverse names))))

(defun inspector-checks (first)
  "The inspector, in a window of its own beside the app's FIRST."
  (command-option-i first)
  (let ((inspector (and (wait-for (lambda () (the-inspector-of first))) (the-inspector-of first))))
    (check "Command-Option-I in the app's window opens the inspector's, which belongs to it"
           (and inspector (wait-for (lambda () (plusp (frames inspector))))))
    (destructuring-bind (ax ay aw ah) (frame-of first)
      (declare (ignore ah))
      (destructuring-bind (ix iy iw ih) (frame-of inspector)
        (declare (ignore ih))
        (check "it opens beside the app's window, 12 points away, tops lined up"
               (and (near iy ay) (or (near ix (+ ax aw 12)) (near (+ ix iw 12) ax)))
               (list (frame-of first) (frame-of inspector)))))
    ;; A window that has just come up is told where the real pointer is, which would come between
    ;; the events of a click made here; as the run-app check does, let that pass first.
    (wait-for-idle inspector)
    (sleep 1)
    (let ((before *count*))
      (m:click (open-session inspector) :id :inspector-pick)
      (m:click (open-session first) :id :add)
      (wait-for-idle inspector)
      (check "Pick, then a press in the app's window, picks and does not press"
             (and (= before *count*) (search "Id: :add" (says inspector))) (says inspector))
      (m:click (open-session inspector) :id :inspector-press)
      (check "Press in the inspector presses it in the app, after the frame, on a thread of its own"
             (wait-for (lambda () (= *count* (1+ before)))) *count*))
    (command-option-i first)
    (check "the keys again close it" (wait-for (lambda () (not (host-open-p inspector)))))
    (check "and what it asked of the app's window goes with it"
           (wait-for (lambda () (null (almighty-bolt/ui/ui:ui-inspection
                                       (almighty-bolt/ui/immediate:immediate-app-ui
                                        (host-app first)))))))))

(defun make-breakable ()
  (bolt:state-let ((align :start))
    (lambda ()
      (bolt:column (:id :page :gap 8 :main-align align)
        ;; :stretch is a cross-align; as a main-align, layout fails in every frame.
        (when (bolt:button "Break" :id :break)
          (setf align :stretch))
        (when (bolt:button "Mend" :id :mend)
          (setf align :start))))))

(defun press-on (host id)
  "A press and release of the primary button in the middle of ID, where the last good frame had it."
  (destructuring-bind (x y w h) (call-on-main (lambda ()
                                                (multiple-value-list
                                                 (lookup-target (host-app host) id))))
    (let ((px (float (+ x (/ w 2)) 1d0)) (py (float (+ y (/ h 2)) 1d0)))
      (call-on-main
       (lambda ()
         (dolist (kind '(:move :down :up))
           (host-deliver-event host (make-pointer-event :timestamp 0 :kind kind :x px :y py
                                                        :button 0
                                                        :buttons (if (eq kind :down) 1 0)
                                                        :click-count (if (eq kind :move) 0 1)))))))))

(defun failing-frame-checks ()
  "A window whose app makes every frame fail goes on, showing its last good frame, and a press on
what is shown mends it."
  (let ((window (bolt:open-window 'make-breakable :title "Breakable" :width 240 :height 120)))
    (wait-for (lambda () (plusp (frames window))))
    (wait-for-idle window)
    (sleep 1)
    (press-on window :break)
    (check "a frame that fails leaves the window open, saying what failed"
           (wait-for (lambda () (almighty-bolt/ui/immediate:immediate-app-failure (host-app window))))
           (almighty-bolt/ui/immediate:immediate-app-failure (host-app window)))
    (let ((before (frames window)))
      (press-on window :mend)
      (check "and a press on what it shows mends it"
             (wait-for (lambda () (and (> (frames window) before)
                                       (null (almighty-bolt/ui/immediate:immediate-app-failure
                                              (host-app window)))))))
      (check "and the loop is still running" (host-open-p window)))
    (bolt:close-window window)
    (wait-for (lambda () (not (host-open-p window))))))

(defun this-window-checks ()
  "A window that the second opens beside itself, and that closes itself, both by THIS-WINDOW."
  (m:click (open-session *second*) :id :below)
  (check "a window opened beside THIS-WINDOW opens"
         (wait-for (lambda () (and *below* (plusp (frames *below*))))))
  (destructuring-bind (sx sy sw sh) (frame-of *second*)
    (declare (ignore sw))
    (destructuring-bind (bx by bw bh) (frame-of *below*)
      (declare (ignore bw))
      (check "below the window whose button opened it (or above, with no room), left edges lined up"
             (and (near bx sx) (or (near by (+ sy sh 12)) (near (+ by bh 12) sy)))
             (list (frame-of *second*) (frame-of *below*)))))
  (wait-for-idle *below*)
  (sleep 1)
  (m:click (open-session *below*) :id :close)
  (check "and its own button closes it, by THIS-WINDOW"
         (wait-for (lambda () (not (host-open-p *below*)))))
  (check "and only it" (and (host-open-p *second*) (= 2 (length (open-hosts))))))

(defun checks ()
  (let ((first (bolt:open-app 'make-first :title "First" :width 320 :height 200)))
    (check "the app's window opens" (wait-for (lambda () (plusp (frames first)))))
    (m:click (open-session first) :id :open)
    (check "a button opens a second window, after its frame"
           (wait-for (lambda () (and *second* (plusp (frames *second*))))))
    (check "two windows are open, and the first is still the app's"
           (and (= 2 (length (open-hosts))) (eq first (running-host))))
    (check "the second shows what the first holds" (equal "0" (count-in *second*)))
    (destructuring-bind (fx fy fw fh) (frame-of first)
      (declare (ignore fw fh))
      (destructuring-bind (sx sy sw sh) (frame-of *second*)
        (declare (ignore sw sh))
        (check "a window given no place opens below and to the right of the one before"
               (and (near sx (+ fx 24)) (near sy (+ fy 24)))
               (list (frame-of first) (frame-of *second*)))))
    (m:click (open-session first) :id :add)
    (wait-for-idle *second*)
    (check "a press in the first draws the second with what it changed"
           (equal "1" (count-in *second*)) (count-in *second*))
    (m:click (open-session *second*) :id :add-ten)
    (wait-for-idle first)
    (check "and a press in the second draws the first"
           (equal "11" (count-in first)) (count-in first))
    (check "then both rest: neither keeps waking the other" (at-rest-p (list first *second*)))
    (inspector-checks first)
    (this-window-checks)
    (failing-frame-checks)
    ;; Another, from the REPL, closed again.
    (let ((third (bolt:open-window 'make-third :title "Third" :width 200 :height 80
                                               :top 0 :right 0)))
      (check "a window opened from the REPL draws" (wait-for (lambda () (plusp (frames third)))))
      (destructuring-bind (tx ty tw th) (frame-of third)
        (declare (ignore th))
        (destructuring-bind (left width) (usable-width)
          (check "one given :top 0 :right 0 is in the top right corner of the screen"
                 (and (near ty 0) (near (+ tx tw) (+ left width)))
                 (frame-of third))))
      (bolt:close-window third)
      (check "and closes" (wait-for (lambda () (not (host-open-p third)))))
      (check "leaving the other two" (= 2 (length (open-hosts))))
      (bolt:close-window third)
      (check "closing it again does nothing" (= 2 (length (open-hosts)))))
    ;; Compiling code makes every window's app again.
    (let ((one (host-app first))
          (two (host-app *second*)))
      (eval '(defun almighty-bolt-examples/windows-smoke::compiled-in-the-editor () 1))
      (check "compiling a definition makes both apps again"
             (wait-for (lambda () (and (not (eq one (host-app first)))
                                       (not (eq two (host-app *second*)))))))
      (check "and each window still shows the count"
             (equal (princ-to-string *count*) (count-in *second*)) (count-in *second*)))
    ;; A window that belongs to another (as a tool's belongs to the app it looks at).
    (let ((tool (open-host (almighty-bolt/app:make-app-instance 'make-third) :title "Tool" :width 200
                                                                    :height 80
                                                                    :main nil :owner *second*)))
      (wait-for (lambda () (plusp (frames tool))))
      (bolt:close-window *second*)
      (check "a window that belongs to another closes with it"
             (wait-for (lambda () (not (or (host-open-p tool) (host-open-p *second*))))))
      (check "and the app's window stays" (and (host-open-p first) (eq first (running-host)))))
    (m:click (open-session first) :id :open)
    (wait-for (lambda () (= 2 (length (open-hosts)))))
    (quit)
    (check "Quit closes every window" (wait-for (lambda () (null (open-hosts)))))
    (check "and the loop stops, with the Lisp still here"
           (wait-for (lambda () (null almighty-bolt/platform/macos/host::*loop*))))
    ;; The loop starts again, for a window that is not the app's too.
    (let ((alone (bolt:open-window 'make-third :title "Alone" :width 200 :height 80)))
      (check "open-window with nothing open starts the loop"
             (wait-for (lambda () (plusp (frames alone)))))
      (check "and it is not the app's window" (null (running-host)))
      (let ((again (bolt:open-app 'make-first :title "First" :width 320 :height 200)))
        (check "open-app beside it opens the app's window"
               (and (wait-for (lambda () (plusp (frames again)))) (eq again (running-host))))
        (close-host)
        (check "close-app closes the app's window only"
               (and (not (host-open-p again)) (host-open-p alone))))
      (bolt:close-window alone)
      (check "and closing the last window stops the loop"
             (wait-for (lambda () (null almighty-bolt/platform/macos/host::*loop*)))))))

(let* ((output *standard-output*)
       (editor (sb-thread:make-thread
                (lambda ()
                  (let ((*standard-output* output))
                    (handler-case (checks)
                      (error (condition)
                        (check "the checks ran to the end" nil condition)))
                    (if (zerop *failures*)
                        (format t "~&The windows check passed.~%")
                        (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*))
                    (finish-output)
                    ;; From here: a check that failed may have left the loop running on the main
                    ;; thread, which then never comes back from waiting.
                    (sb-ext:exit :code (if (zerop *failures*) 0 1) :abort t)))
                :name "repl")))
  ;; The main thread has nothing to do but wait, like a REPL for its next form. The windows' loop is
  ;; run on it from the other thread.
  (sb-thread:join-thread editor :default nil))
