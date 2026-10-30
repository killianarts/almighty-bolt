;;;; Check, in a real window, what saving a file does: make 7guis ... HOT=1 on the real CRUD file of
;;;; this tree. From the repository root:
;;;;   make 7guis-hot-smoke
;;;; The app is used (a person added, a filter typed, the keyboard left in the name field); the file
;;;; is saved with a label given an id; the app must be made again, and its entries, the field that
;;;; has the keyboard and the caret must be where they were, and typing must go on at the caret. Then
;;;; the file is put back, and the app must change back. Then two saves that do not work (a file that
;;;; does not read, and code that fails when the app runs) must leave the old app in the window.
;;;; It says "The hot reload check passed" when every check holds.
;;;; It writes apps/7guis/crud/immediate.lisp and writes back what it read, whatever happens (a
;;;; watchdog does too). If the Lisp is killed in the middle, git checkout puts the file back.

(asdf:load-system "almighty-bolt-7guis/launcher")
(asdf:load-system "almighty-bolt-7guis/immediate")
(asdf:load-system "almighty-bolt-stress/immediate")

(defpackage #:almighty-bolt-7guis/hot-smoke
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:lookup-target
                #:frame-ring-total
                #:make-pointer-event
                #:make-key-event
                #:kept-value
                #:text-state-caret
                #:text-state-string)
  (:import-from #:almighty-bolt-core/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt-core/platform/macos/host
                #:running-host
                #:host-variant
                #:host-ring
                #:host-deliver-event
                #:host-kept-state
                #:close-host-window))

(in-package #:almighty-bolt-7guis/hot-smoke)

(defparameter *file* (merge-pathnames "apps/7guis/crud/immediate.lisp" (uiop:getcwd)))
(defparameter *label* "(label \"Filter prefix:\")")
(defparameter *labelled* "(label \"Filter prefix:\" :id :probe)")
(defparameter *original* (uiop:read-file-string *file*))

(defvar *failures* 0)

(defun write-file (text)
  (with-open-file (out *file* :direction :output :if-exists :supersede)
    (write-string text out)))

(defun check (what ok &optional detail)
  (unless ok (incf *failures*))
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ (~a)~]~%" ok what detail)
  (finish-output)
  ok)

(defun wait-for (predicate seconds)
  "The seconds it took for PREDICATE to hold, or NIL if it did not within SECONDS."
  (let ((start (get-internal-real-time)))
    (loop
      (when (funcall predicate)
        (return (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)))
      (when (> (- (get-internal-real-time) start) (* seconds internal-time-units-per-second))
        (return nil))
      (sleep 0.01))))

(defun present-p (host id)
  (and (lookup-target (host-variant host) id) t))

(defun symbol-in (package name)
  (or (find-symbol name package) (error "No ~a in ~a." name package)))

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

(defun type-text (host string)
  (loop :for character :across string
        :do (deliver host (make-key-event :timestamp 0 :kind :down :logical-key character
                                          :modifiers 0))))

(defun entries (host)
  "How the app's entries are shown, read from the state the host keeps for it."
  (let ((state (kept-value (host-kept-state host)
                           (symbol-in "ALMIGHTY-BOLT-7GUIS/CRUD/IMMEDIATE" "ENTRIES"))))
    (mapcar (symbol-in "ALMIGHTY-BOLT-7GUIS/CRUD/MODEL" "ENTRY-DISPLAY")
            (funcall (symbol-in "ALMIGHTY-BOLT-7GUIS/CRUD/MODEL" "ENTRIES-LIST") state))))

(defun caret (host)
  "What has the keyboard and where its caret is: the key of the focused widget, the caret's index
and the text of its field, as a list, or (NIL) if no field has the keyboard."
  (sleep 0.2)
  (let* ((ui (funcall (symbol-in "ALMIGHTY-BOLT-VARIANT-IMMEDIATE/IMMEDIATE" "IMMEDIATE-VARIANT-UI")
                      (host-variant host)))
         (key (funcall (symbol-in "ALMIGHTY-BOLT-VARIANT-IMMEDIATE/UI" "UI-FOCUS") ui))
         (field (gethash key (funcall (symbol-in "ALMIGHTY-BOLT-VARIANT-IMMEDIATE/UI" "UI-STATES")
                                      ui))))
    (if (and field (typep field (symbol-in "ALMIGHTY-BOLT-VARIANT-IMMEDIATE/WIDGETS" "FIELD-STATE")))
        (let ((state (funcall (symbol-in "ALMIGHTY-BOLT-VARIANT-IMMEDIATE/WIDGETS"
                                         "FIELD-STATE-STATE")
                              field)))
          (list key (text-state-caret state) (text-state-string state)))
        (list nil))))

(defun save-with (replacement)
  "Save the CRUD file with the filter's label written as REPLACEMENT."
  (write-file (let ((at (search *label* *original*)))
                (concatenate 'string (subseq *original* 0 at) replacement
                             (subseq *original* (+ at (length *label*)))))))

(defun use-the-app (host)
  (click host :name) (type-text host "Zed")
  (click host :surname) (type-text host "Q")
  (click host :create)
  (click host :filter) (type-text host "Q")
  (click host :name) (type-text host "!"))

(defun what-saving-does (host)
  (let ((before-entries (entries host))
        (before-caret (caret host))
        (before-variant (host-variant host)))
    (check "the person was added" (= 4 (length before-entries)) (format nil "~s" before-entries))
    (check "the keyboard is in the name field, after Zed!"
           (and (first before-caret) (equal "Zed!" (third before-caret)))
           (format nil "~s" before-caret))
    (save-with *labelled*)
    (let ((took (wait-for (lambda () (present-p host :probe)) 90)))
      (check "the saved file changed the app" took (and took (format nil "~,2f s" took))))
    (check "the app was made again" (not (eq before-variant (host-variant host))))
    (sleep 1)
    (let ((after-entries (entries host))
          (after-caret (caret host)))
      (check "the entries are the same" (equal before-entries after-entries)
             (format nil "~s" after-entries))
      (check "the keyboard and the caret are where they were" (equal before-caret after-caret)
             (format nil "before ~s, after ~s" before-caret after-caret))
      (type-text host "?")
      (let ((moved (caret host)))
        (check "typing goes on in the same field, at the caret"
               (and (first moved)
                    (equal (first moved) (first before-caret))
                    (equal (third moved) "Zed!?")
                    (= (second moved) (1+ (second after-caret))))
               (format nil "~s" moved))))
    (write-file *original*)
    (let ((took (wait-for (lambda () (not (present-p host :probe))) 90)))
      (check "the file put back changed it back" took (and took (format nil "~,2f s" took))))
    (check "and the entries are still there" (equal before-entries (entries host)))))

(defun what-a-bad-save-does (host)
  (let ((variant (host-variant host))
        (entries (entries host)))
    ;; A file that does not read: the parenthesis is not closed.
    (save-with "(label \"Filter prefix:\"")
    (sleep 3)
    (check "a file that does not read changes nothing"
           (and (eq variant (host-variant host)) (present-p host :name)))
    ;; Code that compiles and fails when the app runs: the new app is made, fails in its first
    ;; frame, and the old one is put back.
    (save-with "(label (error \"broken on purpose\"))")
    (sleep 4)
    (check "an edit that fails when the app runs leaves the old app"
           (and (eq variant (host-variant host)) (present-p host :name)))
    (check "and the entries are still there" (equal entries (entries host)))
    ;; The file put back: the app is made again, from code that works.
    (write-file *original*)
    (let ((took (wait-for (lambda () (not (eq variant (host-variant host)))) 90)))
      (check "the file put back is made into the app again" took
             (and took (format nil "~,2f s" took))))
    (sleep 1)
    (check "and the entries are still there" (equal entries (entries host)))))

(defun drive ()
  (let ((host (loop :for host = (running-host) :until host :do (sleep 0.05)
                    :finally (return host))))
    (unwind-protect
         (progn
           (wait-for (lambda () (plusp (frame-ring-total (host-ring host)))) 20)
           (sleep 1.5)
           (use-the-app host)
           (what-saving-does host)
           (what-a-bad-save-does host))
      (write-file *original*)
      (if (zerop *failures*)
          (format t "~&The hot reload check passed.~%")
          (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*))
      (finish-output)
      (close-host-window host))))

(when (search *labelled* *original*)
  ;; A run that was killed in the middle left the file as it saves it. Put it back.
  (setf *original* (let ((at (search *labelled* *original*)))
                     (concatenate 'string (subseq *original* 0 at) *label*
                                  (subseq *original* (+ at (length *labelled*))))))
  (write-file *original*)
  (format t "~&The CRUD file still had the probe's label from a run that was killed; put back.~%"))

(sb-thread:make-thread #'drive :name "hot smoke driver")
(sb-thread:make-thread (lambda ()
                         (sleep 240)
                         (write-file *original*)
                         (format t "~&FAIL: the check took more than 240 s; the file is put back.~%")
                         (finish-output)
                         (sb-ext:exit :code 2 :abort t))
                       :name "hot smoke watchdog")
;; The event loop runs on this, the main, thread, and returns when the window is closed.
(funcall (intern "RUN" "ALMIGHTY-BOLT-7GUIS/LAUNCHER") :immediate :crud :hot t)
