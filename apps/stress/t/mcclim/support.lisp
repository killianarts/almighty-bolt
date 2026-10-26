(defpackage #:almighty-bolt-stress/t/mcclim/support
  (:use #:cl)
  (:import-from #:bordeaux-threads
                #:make-thread
                #:join-thread)
  (:import-from #:clim
                #:find-port
                #:find-frame-manager
                #:frame-state
                #:run-frame-top-level
                #:queue-event
                #:frame-top-level-sheet
                #:window-manager-delete-event
                #:stream-output-history
                #:map-over-output-records
                #:text-displayed-output-record
                #:text-displayed-output-record-string)
  (:export #:call-with-running-frame
           #:with-running-frame
           #:await
           #:pane-strings
           #:count-records))

(in-package #:almighty-bolt-stress/t/mcclim/support)

;;; What the tests of the McCLIM apps need: a frame that is running on McCLIM's null port (nothing
;;; is drawn anywhere, but the panes, the gadgets and the output records are made and the events
;;; are handled), and the strings and the number of records that a pane has made.

(defun call-with-running-frame (maker function)
  "Make a frame with MAKER, which is called with the frame manager as :FRAME-MANAGER, run it in a
thread, call FUNCTION with it once it is enabled, and take it down."
  (let* ((manager (find-frame-manager :port (find-port :server-path '(:null))))
         (frame (funcall maker :frame-manager manager))
         (thread (make-thread (lambda () (run-frame-top-level frame)) :name "frame under test")))
    (unwind-protect
         (progn
           (loop :until (eq (frame-state frame) :enabled) :do (sleep 0.01))
           (funcall function frame))
      (let ((sheet (frame-top-level-sheet frame)))
        (queue-event sheet (make-instance 'window-manager-delete-event :sheet sheet)))
      (join-thread thread))))

(defun await (test &optional (seconds 5))
  "True once TEST is, which is waited for for SECONDS at most: a frame is enabled before the thread
that runs it has displayed its panes."
  (loop :repeat (* 100 seconds)
        :do (when (funcall test) (return t))
            (sleep 0.01)))

(defmacro with-running-frame ((frame maker) &body body)
  `(call-with-running-frame ,maker (lambda (,frame) ,@body)))

(defun map-over-records (function record)
  (map-over-output-records (lambda (child)
                             (funcall function child)
                             (map-over-records function child))
                           record))

(defun pane-strings (pane)
  "The strings that PANE has written, one for each piece of text."
  (let ((strings '()))
    (map-over-records (lambda (record)
                        (when (typep record 'text-displayed-output-record)
                          (push (text-displayed-output-record-string record) strings)))
                      (stream-output-history pane))
    (nreverse strings)))

(defun count-records (pane)
  "How many output records PANE has, of every kind."
  (let ((count 0))
    (map-over-records (lambda (record) (declare (ignore record)) (incf count))
                      (stream-output-history pane))
    count))
