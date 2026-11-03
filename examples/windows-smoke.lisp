;;;; Check, in real windows, what an app with more than one window gets: a second window opened by a
;;;; button of the first, which shows what the first holds and changes it; another opened from the
;;;; REPL and closed; both made again when code is compiled; a window that belongs to another closing
;;;; with it; Quit closing them all and leaving the Lisp; and the loop starting again. From the
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
                #:+modifier-super+
                #:+modifier-alt+)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class)
  (:import-from #:almighty-bolt/platform/macos/host
                #:host-variant
                #:host-ring
                #:host-idle-p
                #:host-owner
                #:host-deliver-event
                #:wait-for-idle
                #:open-hosts
                #:host-open-p
                #:running-host
                #:open-variant
                #:close-variant))

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
        (incf *count* 10)))))

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
                  (almighty-bolt/internals:accessibility-tree (host-variant window)))
    (format nil "~{~a~^ | ~}" (nreverse names))))

(defun inspector-checks (first)
  "The inspector, in a window of its own beside the app's FIRST."
  (command-option-i first)
  (let ((inspector (and (wait-for (lambda () (the-inspector-of first))) (the-inspector-of first))))
    (check "Command-Option-I in the app's window opens the inspector's, which belongs to it"
           (and inspector (wait-for (lambda () (plusp (frames inspector))))))
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
                                       (almighty-bolt/ui/immediate:immediate-variant-ui
                                        (host-variant first)))))))))

(defun checks ()
  (let ((first (bolt:open-app 'make-first :title "First" :width 320 :height 200)))
    (check "the app's window opens" (wait-for (lambda () (plusp (frames first)))))
    (m:click (open-session first) :id :open)
    (check "a button opens a second window, after its frame"
           (wait-for (lambda () (and *second* (plusp (frames *second*))))))
    (check "two windows are open, and the first is still the app's"
           (and (= 2 (length (open-hosts))) (eq first (running-host))))
    (check "the second shows what the first holds" (equal "0" (count-in *second*)))
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
    ;; Another, from the REPL, closed again.
    (let ((third (bolt:open-window 'make-third :title "Third" :width 200 :height 80)))
      (check "a window opened from the REPL draws" (wait-for (lambda () (plusp (frames third)))))
      (bolt:close-window third)
      (check "and closes" (wait-for (lambda () (not (host-open-p third)))))
      (check "leaving the other two" (= 2 (length (open-hosts))))
      (bolt:close-window third)
      (check "closing it again does nothing" (= 2 (length (open-hosts)))))
    ;; Compiling code makes every window's app again.
    (let ((one (host-variant first))
          (two (host-variant *second*)))
      (eval '(defun almighty-bolt-examples/windows-smoke::compiled-in-the-editor () 1))
      (check "compiling a definition makes both apps again"
             (wait-for (lambda () (and (not (eq one (host-variant first)))
                                       (not (eq two (host-variant *second*)))))))
      (check "and each window still shows the count"
             (equal (princ-to-string *count*) (count-in *second*)) (count-in *second*)))
    ;; A window that belongs to another (as a tool's belongs to the app it looks at).
    (let ((tool (open-variant (almighty-bolt/app:make-app-instance 'make-third) :title "Tool" :width 200
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
        (close-variant)
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
