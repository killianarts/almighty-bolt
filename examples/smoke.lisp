;;;; Check, in a real window, what a program that uses only the public package gets from RUN-APP: the
;;;; counter example is run on the main thread, used, redefined while it runs (as compiling it from an
;;;; editor does), and closed, which ends the process. From the repository root:
;;;;   make examples-smoke
;;;; It says "The run-app check passed" when every check holds.

(asdf:load-system "almighty-bolt-examples")

(defpackage #:almighty-bolt-examples/smoke
  (:use #:cl)
  (:import-from #:almighty-bolt
                #:run-app
                #:app-value)
  (:import-from #:almighty-bolt/machine/host-session
                #:open-session)
  (:local-nicknames (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics))
  (:import-from #:almighty-bolt/internals
                #:lookup-target
                #:frame-ring-total
                #:make-pointer-event)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt/platform/macos/host
                #:host-variant
                #:host-ring
                #:host-deliver-event
                #:host-idle-p
                #:wait-for-idle
                #:close-host-window))

(in-package #:almighty-bolt-examples/smoke)

(defvar *failures* 0)

(defun check (what ok &optional detail)
  (unless ok (incf *failures*))
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ (~a)~]~%" ok what detail)
  (finish-output)
  ok)

(defun wait-for (predicate seconds)
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) end) (return nil))
      (sleep 0.01))))

(defun present-p (host id)
  (and (lookup-target (host-variant host) id) t))

(defun deliver (host event)
  ;; No pause after it: the events of a click arrive within a millisecond of each other, and the
  ;; ui takes one change of the button a frame, so the count is behind until the host is idle.
  (call-on-main (lambda () (host-deliver-event host event))))

(defun click (host id)
  (multiple-value-bind (x y w h) (lookup-target (host-variant host) id)
    (let ((px (float (+ x (/ w 2)) 1d0)) (py (float (+ y (/ h 2)) 1d0)))
      (deliver host (make-pointer-event :timestamp 0 :kind :move :x px :y py :button nil
                                        :buttons 0 :click-count 0))
      (deliver host (make-pointer-event :timestamp 0 :kind :down :x px :y py :button 0
                                        :buttons 1 :click-count 1))
      (deliver host (make-pointer-event :timestamp 0 :kind :up :x px :y py :button 0
                                        :buttons 0 :click-count 1)))))

(defun the-count (host)
  (declare (ignore host))
  (app-value 'almighty-bolt-examples/counter::count))

(defun edited-counter ()
  "What compiling the example again, with a label added, does."
  (eval '(defun almighty-bolt-examples/counter:make-counter ()
          (almighty-bolt:kept-let ((almighty-bolt-examples/counter::count 0))
            (lambda ()
              (almighty-bolt:padding (:all 24)
                (almighty-bolt:row (:gap 16 :cross-align :center)
                  (almighty-bolt:label (princ-to-string almighty-bolt-examples/counter::count)
                                       :id :count :size 32)
                  (almighty-bolt:label "probe" :id :probe)
                  (when (almighty-bolt:button "Count" :id :count-button)
                    (incf almighty-bolt-examples/counter::count)))))))))

(defun png-size (octets)
  "The width and height in the header of a PNG, or NIL if OCTETS is not one."
  (and (> (length octets) 24)
       (equalp (subseq octets 0 8) #(137 80 78 71 13 10 26 10))
       (flet ((u32 (at) (reduce (lambda (a b) (+ (* a 256) b)) (subseq octets at (+ at 4)))))
         (list (u32 16) (u32 20)))))

(defun through-the-machine-interface ()
  "The same window worked as an agent works it: seen as a tree, pressed by what things are called,
waited for, and photographed."
  (let* ((session (open-session))
         (before (s:node-name (m:find-node session :role :static-text :id :count))))
    (check "the tree has the button, called what it says"
           (m:find-node session :role :button :name "Count"))
    (check "and the count, as text" (stringp before) before)
    (m:click session :role :button :name "Count")
    (let ((after (s:node-name (m:find-node session :role :static-text :id :count))))
      (check "pressing the button by its name counts, and waiting makes the tree true at once"
             (equal after (princ-to-string (1+ (parse-integer before)))) (list before after)))
    (let* ((png (m:screenshot session))
           (size (png-size png)))
      (check "a screenshot is a PNG of the window's size, twice over (the scale)"
             (and size (= (first size) (* 2 360)) (= (second size) (* 2 160))) size)
      (let ((path (uiop:getenv "SMOKE_SCREENSHOT")))
        (when (and path (plusp (length path)))
          (with-open-file (out path :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
            (write-sequence png out)))))))

(defun drive (host)
  (unwind-protect
       (progn
         (wait-for (lambda () (plusp (frame-ring-total (host-ring host)))) 20)
         (sleep 1.0)
         (check "the window shows the example's button" (present-p host :count-button))
         (check "the count starts at 0" (eql 0 (the-count host)) (the-count host))
         (click host :count-button)
         (click host :count-button)
         ;; The ui takes one change of the button a frame and the host asks for the next frame, so
         ;; a quick double click is four frames from being counted: the host says when it is.
         (let* ((start (get-internal-real-time))
                (idle (wait-for-idle host))
                (took (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)))
           (check "the window answers and rests" idle)
           (check "and it is told in well under a second, not by the wait running out" (< took 1)
                  (format nil "~,3f s" took)))
         (check "two clicks count two" (eql 2 (the-count host)) (the-count host))
         (check "and a window at rest is idle" (host-idle-p host))
         (let ((before (host-variant host)))
           (edited-counter)
           (check "compiling the function again changes the window"
                  (wait-for (lambda () (present-p host :probe)) 20))
           (check "the app was made again" (not (eq before (host-variant host))))
           (check "and the count is still 2" (eql 2 (the-count host)) (the-count host))
           (click host :count-button)
           (wait-for-idle host)
           (check "the new app counts on from there" (eql 3 (the-count host)) (the-count host))
           ;; Again and again: a click, and the count is right the moment the host says it is idle,
           ;; whenever during the frames that take the click in the waiting is begun. A host that
           ;; says it is idle while a frame is still wanted gives a stale count in some of them.
           (let ((wrong 0))
             (dotimes (i 30)
               (click host :count-button)
               (sleep (* 0.001 (random 45)))
               (wait-for-idle host)
               (unless (eql (+ 4 i) (the-count host))
                 (incf wrong)))
             (check "thirty clicks, each counted when the host says it is idle" (zerop wrong)
                    (format nil "~d wrong" wrong)))
           (through-the-machine-interface)))
    (if (zerop *failures*)
        (format t "~&The run-app check passed.~%")
        (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*))
    (finish-output)
    (close-host-window host)))

(run-app 'almighty-bolt-examples/counter:make-counter
         :title "Counter" :width 360 :height 160 :hot t :driver #'drive)
