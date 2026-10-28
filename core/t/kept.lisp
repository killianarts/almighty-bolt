(defpackage #:almighty-bolt-core/t/kept
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:kept #:almighty-bolt-core/kept)))

(in-package #:almighty-bolt-core/t/kept)

;;; The apps below are written as the real ones are: a function that makes closures over state.
;;; Each returns a getter and a setter of what is kept.

(defun make-counter-app ()
  (kept:kept-let ((count 0) (label "n"))
    (list (lambda () (list count label))
          (lambda (new) (setf count new)))))

(defun make-counter-app-edited ()
  ;; The same app after its initial value was edited.
  (kept:kept-let ((count 100) (label "n"))
    (list (lambda () (list count label))
          (lambda (new) (setf count new)))))

(defun make-app-without-count ()
  (kept:kept-let ((label "n"))
    (list (lambda () label))))

(defun make-app-with-two-of-a-name ()
  (kept:kept-let ((x 1))
    (kept:kept-let ((x 2))
      (list (lambda () x)
            (lambda (new) (setf x new))))))

(defun get-in (app) (funcall (first app)))
(defun set-in (app value) (funcall (second app) value))

(define-test without-a-store-it-is-let ()
  (let ((a (make-counter-app))
        (b (make-counter-app)))
    (set-in a 5)
    (assert-equal '(5 "n") (get-in a))
    ;; Nothing is kept, and nothing is shared.
    (assert-equal '(0 "n") (get-in b))))

(define-test the-initial-form-is-run-the-first-time ()
  (let ((state (kept:make-kept-state)))
    (assert-equal '(0 "n") (get-in (kept:with-kept-state (state) (make-counter-app))))))

(define-test an-app-made-again-has-the-state-it-had ()
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app))))
    (set-in first 7)
    (let ((second (kept:with-kept-state (state) (make-counter-app))))
      (assert-equal '(7 "n") (get-in second)))))

(define-test the-closures-of-both-apps-share-the-place ()
  ;; The cell is what is kept, so the old app and the new one see each other's writes.
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app)))
         (second (kept:with-kept-state (state) (make-counter-app))))
    (set-in second 9)
    (assert-equal '(9 "n") (get-in first))))

(define-test an-edited-initial-form-is-run-again ()
  ;; What was not edited is kept, and what was is made again, so an edit to an initial value shows.
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app))))
    (set-in first 7)
    (let ((edited (kept:with-kept-state (state) (make-counter-app-edited))))
      (assert-equal '(100 "n") (get-in edited)))))

(define-test an-unedited-name-is-kept-beside-an-edited-one ()
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app))))
    (set-in first 7)
    (kept:with-kept-state (state)
      ;; Makes count again, from the edited form.
      (make-counter-app-edited))
    ;; And then a pass of the first form, in which label was kept all along: still "n", count
    ;; is the edited one's cell, whose form is not this one's, so it is made again.
    (assert-equal '(0 "n") (get-in (kept:with-kept-state (state) (make-counter-app))))))

(define-test the-same-name-twice-in-an-app-is-two-cells ()
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-app-with-two-of-a-name))))
    (assert-eql 2 (get-in first))
    (set-in first 20)
    (assert-eql 20 (get-in (kept:with-kept-state (state) (make-app-with-two-of-a-name))))))

(define-test a-pass-that-fails-keeps-nothing-it-made ()
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app))))
    (set-in first 7)
    ;; An edited initial form makes a cell, and then the pass fails.
    (ignore-errors
     (kept:with-kept-state (state)
       (make-counter-app-edited)
       (error "the constructor failed")))
    ;; The next pass finds the cell that the app that still runs has: the edited one was not kept.
    (assert-equal '(7 "n") (get-in (kept:with-kept-state (state) (make-counter-app))))))

(define-test a-name-that-is-dropped-keeps-its-cell ()
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app))))
    (set-in first 7)
    (kept:with-kept-state (state) (make-app-without-count))
    (assert-equal '(7 "n") (get-in (kept:with-kept-state (state) (make-counter-app))))))

(define-test resetting-forgets-the-cells ()
  (let* ((state (kept:make-kept-state))
         (first (kept:with-kept-state (state) (make-counter-app))))
    (set-in first 7)
    (kept:reset-kept-state state)
    (assert-equal '(0 "n") (get-in (kept:with-kept-state (state) (make-counter-app))))))

(define-test the-state-knows-the-names-it-keeps ()
  (let ((state (kept:make-kept-state)))
    (kept:with-kept-state (state) (make-counter-app))
    (assert-equal '(count label) (kept:kept-state-names state))))

(define-test a-nil-state-is-no-store ()
  (assert-equal '(0 "n") (get-in (kept:with-kept-state (nil) (make-counter-app)))))

(define-test the-initial-forms-do-not-see-each-other ()
  ;; As in LET: a name that is bound beside it is not seen by an initial form.
  (let ((x 10))
    (assert-eql 11 (kept:kept-let ((x 1) (y (1+ x)))
                     y))))

(define-test an-app-is-known-to-keep-state-once-it-has-run-a-kept-let ()
  (let ((state (kept:make-kept-state)))
    (kept:with-kept-state (state)
      (assert-false (kept:kept-in-use-p))
      (kept:kept-let ((x 1))
        (assert-true (kept:kept-in-use-p))))
    ;; And it is a question about the app that is being made.
    (kept:with-kept-state (state)
      (assert-false (kept:kept-in-use-p)))))

(define-test without-a-store-no-state-is-in-use ()
  (kept:kept-let ((x 1))
    (assert-false (kept:kept-in-use-p))))

(define-test what-an-app-keeps-can-be-looked-at ()
  (let ((state (kept:make-kept-state)))
    (set-in (kept:with-kept-state (state) (make-counter-app)) 7)
    (assert-equal '(7 t) (multiple-value-list (kept:kept-value state 'count)))
    (assert-equal '("n" t) (multiple-value-list (kept:kept-value state 'label)))
    (assert-equal '(nil nil) (multiple-value-list (kept:kept-value state 'missing)))
    (kept:with-kept-state (state) (make-app-with-two-of-a-name))
    (assert-equal '(2 t) (multiple-value-list (kept:kept-value state 'x 2)))))
