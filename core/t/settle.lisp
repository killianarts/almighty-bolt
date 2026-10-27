(defpackage #:almighty-bolt-core/t/settle
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:variant #:almighty-bolt-core/variant)
                    (#:settle #:almighty-bolt-core/settle)
                    (#:dl #:almighty-bolt-core/display-list)))

(in-package #:almighty-bolt-core/t/settle)

;;; A variant that answers each build from a script: true (state changed) or false
;;; (settled). When the script runs out it repeats its last answer. Each build
;;; appends one rectangle whose x is the build number.

(defclass scripted-variant (variant:variant)
  ((script :initarg :script :accessor script)
   (builds :initform 0 :accessor builds)
   (build-numbers :initform '() :accessor build-numbers)))

(defmethod variant:frame ((variant scripted-variant) context list)
  (incf (builds variant))
  (push (variant:context-build-number context) (build-numbers variant))
  (dl:push-rect list (variant:context-build-number context) 0 10 10 #xFF0000FF)
  (let ((script (script variant)))
    (if (rest script)
        (pop (script variant))
        (first script))))

(defun run-settle (script &key (frame-number 7))
  "Settle one frame of a variant with SCRIPT. Returns the variant, the display list,
the number of builds, whether the cap was reached, and the warnings signalled."
  (let ((variant (make-instance 'scripted-variant :script script))
        (context (variant:make-context))
        (list (dl:make-display-list :capacity 16))
        (warnings '()))
    (setf (variant:context-frame-number context) frame-number)
    (multiple-value-bind (builds capped)
        (handler-bind ((warning (lambda (condition)
                                  (push condition warnings)
                                  (muffle-warning condition))))
          (settle:settle-frame variant context list))
      (values variant list builds capped (reverse warnings)))))

;;; --- settling ---

(define-test a-frame-that-changes-nothing-settles-in-one-build ()
  (multiple-value-bind (variant list builds capped warnings) (run-settle '(nil))
    (assert-eql 1 builds)
    (assert-false capped)
    (assert-eql 1 (builds variant))
    (assert-eql 1 (dl:display-list-count list))
    (assert-equal '() warnings)))

(define-test a-frame-that-changed-state-is-built-again ()
  (multiple-value-bind (variant list builds capped) (run-settle '(t nil))
    (assert-eql 2 builds)
    (assert-false capped)
    (assert-eql 2 (builds variant))
    (assert-eql 1 (dl:display-list-count list))))

(define-test the-third-build-may-be-the-one-that-settles ()
  (multiple-value-bind (variant list builds capped warnings) (run-settle '(t t nil))
    (assert-eql 3 builds)
    (assert-false capped)
    (assert-eql 3 (builds variant))
    (assert-eql 1 (dl:display-list-count list))
    (assert-equal '() warnings)))

(define-test the-cap-is-three-builds ()
  (assert-eql 3 settle:+max-builds+))

(define-test a-frame-that-never-settles-stops-at-the-cap-and-warns ()
  (multiple-value-bind (variant list builds capped warnings) (run-settle '(t) :frame-number 42)
    (assert-eql 3 builds)
    (assert-true capped)
    (assert-eql 3 (builds variant))
    (assert-eql 1 (length warnings))
    (assert-true (typep (first warnings) 'settle:settle-cap-reached))
    (assert-eql 42 (settle:settle-cap-reached-frame-number (first warnings)))
    (assert-true (search "42" (princ-to-string (first warnings))))
    ;; The last build is what is left to present.
    (assert-eql 1 (dl:display-list-count list))
    (assert-eql 3f0 (dl:display-list-x list 0))))

;;; --- what each build sees ---

(define-test the-list-is-cleared-before-every-build ()
  (multiple-value-bind (variant list) (run-settle '(t t nil))
    (declare (ignore variant))
    ;; Three builds each added a rectangle, but only the last one is there.
    (assert-eql 1 (dl:display-list-count list))
    (assert-eql 3f0 (dl:display-list-x list 0))))

(define-test each-build-is-numbered-from-one ()
  (let ((variant (run-settle '(t t nil))))
    (assert-equal '(1 2 3) (reverse (build-numbers variant)))))

(define-test an-error-in-a-build-reaches-the-caller ()
  (let ((variant (make-instance 'variant:variant))
        (context (variant:make-context))
        (list (dl:make-display-list :capacity 4)))
    ;; A bare variant has no FRAME of its own.
    (assert-error 'error (settle:settle-frame variant context list))))

;;; --- the caret of text input ---

;;; A variant with a canvas that says its caret in its own coordinates the first time it is
;;; built only (a retained canvas that is not changed is not painted again), and that can be
;;; moved between frames.

(defclass moving-variant (variant:variant)
  ((place :initarg :place :accessor place)
   (builds :initform 0 :accessor builds)))

(defmethod variant:frame ((variant moving-variant) context list)
  (declare (ignore list))
  (when (= 1 (incf (builds variant)))
    (variant:with-painting (context :editor)
      (variant:set-text-input-caret context 10f0 20f0 2f0 14f0)))
  nil)

(defmethod variant:lookup-target ((variant moving-variant) id)
  (when (eq id :editor)
    (values (first (place variant)) (second (place variant)) 100f0 50f0)))

(define-test a-settled-frame-has-the-caret-where-the-canvas-is-in-the-window ()
  (let ((variant (make-instance 'moving-variant :place '(30f0 40f0)))
        (context (variant:make-context))
        (list (dl:make-display-list :capacity 16)))
    (settle:settle-frame variant context list)
    (assert-equal '(40f0 60f0 2f0 14f0) (coerce (variant:context-text-input-rect context) 'list))
    ;; The canvas moves and is not painted again: the caret goes with it.
    (setf (place variant) '(130f0 240f0))
    (settle:settle-frame variant context list)
    (assert-eql 2 (builds variant))
    (assert-equal '(140f0 260f0 2f0 14f0)
                  (coerce (variant:context-text-input-rect context) 'list))))

;;; --- no allocation ---

(defclass busy-variant (variant:variant)
  ((n :initform 0 :type fixnum :accessor n)))

(defmethod variant:frame ((variant busy-variant) context list)
  (declare (ignore context))
  (dotimes (i 20)
    (dl:push-rect list i 0 10 10 #xFF0000FF))
  ;; True on every other call, so every frame takes two builds.
  (oddp (incf (n variant))))

(defun settle-many (variant context list count)
  "Compiled with this file, because lisp-unit2 evaluates test bodies under a
debugging policy that allocates."
  (declare (type fixnum count))
  (let ((builds 0))
    (declare (type fixnum builds))
    (dotimes (i count builds)
      (incf (variant:context-frame-number context))
      (incf builds (settle:settle-frame variant context list)))))

(define-test settling-frames-allocates-nothing-after-warm-up ()
  (let ((variant (make-instance 'busy-variant))
        (context (variant:make-context))
        (list (dl:make-display-list :capacity 64)))
    (settle-many variant context list 1000)         ; warm up the dispatch
    (let ((before (sb-ext:get-bytes-consed)))
      (assert-eql 200000 (settle-many variant context list 100000))
      ;; 200,000 builds. Sixteen bytes a build would be 3.2 MB.
      (let ((delta (- (sb-ext:get-bytes-consed) before)))
        (assert-true (< delta 500000) delta)))))

;;; --- timing the builds ---

(defclass sleeping-variant (variant:variant)
  ((sleeps :initarg :sleeps :accessor sleeps)))

(defmethod variant:frame ((variant sleeping-variant) context list)
  (declare (ignore list))
  (sleep (pop (sleeps variant)))
  (< (variant:context-build-number context) 2))

(define-test with-the-instrumentation-on-the-first-build-and-all-of-them-are-timed ()
  (let ((variant (make-instance 'sleeping-variant :sleeps '(0.01 0.03)))
        (context (variant:make-context))
        (list (dl:make-display-list :capacity 4)))
    (setf (variant:context-phases-p context) t)
    (assert-eql 2 (settle:settle-frame variant context list))
    (let ((slots (variant:context-phase-ns context)))
      (assert-true (<= 8000000 (aref slots variant:+phase-first-build+) 25000000))
      (assert-true (>= (aref slots variant:+phase-all-builds+) 35000000))
      (assert-true (> (aref slots variant:+phase-all-builds+)
                      (* 2 (aref slots variant:+phase-first-build+)))))))

(define-test with-the-instrumentation-off-the-builds-are-not-timed ()
  (let ((variant (make-instance 'sleeping-variant :sleeps '(0.01 0.01)))
        (context (variant:make-context))
        (list (dl:make-display-list :capacity 4)))
    (settle:settle-frame variant context list)
    (assert-eql 0 (aref (variant:context-phase-ns context) variant:+phase-first-build+))
    (assert-eql 0 (aref (variant:context-phase-ns context) variant:+phase-all-builds+))))

