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
                #:kept-value)
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
                #:host-kept-state
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
  (call-on-main (lambda () (host-deliver-event host event)))
  (sleep 0.05))

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
  (kept-value (host-kept-state host) 'almighty-bolt-examples/counter::count))

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
         ;; a quick double click is four frames from being counted.
         (wait-for (lambda () (eql 2 (the-count host))) 5)
         (check "two clicks count two" (eql 2 (the-count host)) (the-count host))
         (let ((before (host-variant host)))
           (edited-counter)
           (check "compiling the function again changes the window"
                  (wait-for (lambda () (present-p host :probe)) 20))
           (check "the app was made again" (not (eq before (host-variant host))))
           (check "and the count is still 2" (eql 2 (the-count host)) (the-count host))
           (click host :count-button)
           (wait-for (lambda () (eql 3 (the-count host))) 5)
           (check "the new app counts on from there" (eql 3 (the-count host)) (the-count host))))
    (if (zerop *failures*)
        (format t "~&The run-app check passed.~%")
        (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*))
    (finish-output)
    (close-host-window host)))

(run-app 'almighty-bolt-examples/counter:make-counter
         :title "Counter" :width 360 :height 160 :hot t :driver #'drive)
