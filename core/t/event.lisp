(defpackage #:almighty-bolt-core/t/event
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:event #:almighty-bolt-core/event)))

(in-package #:almighty-bolt-core/t/event)

(defun round-trip (event)
  "Write EVENT with PRIN1 and read it back, as a replay script would."
  (let ((*package* (find-package '#:cl-user)))
    (read-from-string (prin1-to-string event))))

(defun sample-events ()
  (list (event:make-pointer-event :timestamp 100 :kind :down :x 12.5d0 :y 40d0
                                  :button 0 :buttons 1 :modifiers event:+modifier-shift+
                                  :click-count 2)
        (event:make-pointer-event :timestamp 101 :kind :move :x 13d0 :y 41d0 :buttons 1)
        (event:make-wheel-event :timestamp 102 :x 5d0 :y 6d0 :dx 0d0 :dy -3.5d0
                                :precise-p t)
        (event:make-key-event :timestamp 103 :kind :down :physical-key 36
                              :logical-key #\a :modifiers event:+modifier-super+)
        (event:make-key-event :timestamp 104 :kind :up :physical-key 123
                              :repeat-p nil)
        (event:make-text-input-event :timestamp 105 :text "héllo")
        (event:make-composition-event :timestamp 106 :kind :changed :text "かな"
                                      :selection-start 1 :selection-length 1)
        (event:make-resize-event :timestamp 107 :width 800d0 :height 600d0)
        (event:make-scale-event :timestamp 108 :scale 2d0)
        (event:make-focus-event :timestamp 109 :focused-p t)))

;;; --- construction ---

(define-test events-carry-their-fields ()
  (let ((pointer (event:make-pointer-event :timestamp 7 :kind :up :x 1.5d0 :y 2.5d0
                                           :button 1 :buttons 0 :click-count 1)))
    (assert-eql 7 (event:event-timestamp pointer))
    (assert-eql :up (event:pointer-event-kind pointer))
    (assert-eql 1.5d0 (event:pointer-event-x pointer))
    (assert-eql 2.5d0 (event:pointer-event-y pointer))
    (assert-eql 1 (event:pointer-event-button pointer))
    (assert-eql 0 (event:pointer-event-buttons pointer))
    (assert-eql 0 (event:pointer-event-modifiers pointer))
    (assert-eql 1 (event:pointer-event-click-count pointer))))

(define-test every-event-is-an-event ()
  (dolist (e (sample-events))
    (assert-true (typep e 'event:event))
    (assert-true (integerp (event:event-timestamp e)))))

;;; The next two build events with bad arguments on purpose. The constructor is
;;; called out of line and the bad kind is read at run time, so that the
;;; compiler neither inlines the call nor rejects it before the test runs.

(defvar *bad-kind* :hover
  "Not one of the pointer event kinds.")

(declaim (notinline event:make-focus-event))

(define-test the-timestamp-is-required ()
  (assert-error 'error (event:make-focus-event :focused-p t)))

(define-test kinds-are-checked ()
  (assert-error 'type-error
                (event:make-pointer-event :timestamp 1 :kind *bad-kind*)))

(define-test modifiers-are-a-bit-set ()
  (let ((mods (logior event:+modifier-shift+ event:+modifier-alt+)))
    (assert-true (event:modifier-down-p mods event:+modifier-shift+))
    (assert-true (event:modifier-down-p mods event:+modifier-alt+))
    (assert-false (event:modifier-down-p mods event:+modifier-control+))
    (assert-false (event:modifier-down-p mods event:+modifier-super+))))

;;; --- serializable ---

(define-test events-survive-being-written-and-read-back ()
  (dolist (e (sample-events))
    (assert-equalp e (round-trip e))))

;;; --- allocation ---

(define-test an-event-is-a-few-words ()
  ;; Events are allocated when the operating system delivers them, outside
  ;; the frame. This pins down what that costs: a pointer event is under 128
  ;; bytes, so a 1 kHz mouse allocates well under 128 KB a second.
  (let* ((n 20000)
         (kept (make-array n))
         (before (sb-ext:get-bytes-consed)))
    (dotimes (i n)
      (setf (aref kept i)
            (event:make-pointer-event :timestamp i :kind :move :x 1d0 :y 2d0)))
    (let ((per-event (/ (- (sb-ext:get-bytes-consed) before) n)))
      (assert-true (< per-event 128))
      (assert-true (> per-event 32)))))

(define-test a-wheels-deltas-are-lines-and-a-trackpads-are-points-made-into-lines ()
  (flet ((lines (&rest keys)
           (multiple-value-list
            (event:wheel-event-lines (apply #'event:make-wheel-event :timestamp 0 keys)))))
    (assert-equal '(0d0 -3d0) (lines :dy -3d0))
    (assert-equal '(0d0 -3d0) (lines :dy -72d0 :precise-p t))
    (assert-equal '(2d0 0.5d0) (lines :dx 48d0 :dy 12d0 :precise-p t))
    (assert-equal 24d0 event:+points-per-line+)))
