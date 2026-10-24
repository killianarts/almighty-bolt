(defpackage #:almighty-bolt-core/t/display-list
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:dl #:almighty-bolt-core/display-list)))

(in-package #:almighty-bolt-core/t/display-list)

(defun red () (dl:pack-color 255 0 0))
(defun blue () (dl:pack-color 0 0 255))

;;; --- building ---

(define-test pack-color-puts-red-in-the-high-byte ()
  (assert-eql #xFF0000FF (dl:pack-color 255 0 0))
  (assert-eql #x00FF0080 (dl:pack-color 0 255 0 128))
  (assert-eql #x000000FF (dl:pack-color 0 0 0)))

(define-test a-new-list-is-empty ()
  (let ((list (dl:make-display-list :capacity 8)))
    (assert-eql 8 (dl:display-list-capacity list))
    (assert-eql 0 (dl:display-list-count list))))

(define-test commands-are-appended-in-order ()
  (let ((list (dl:make-display-list :capacity 8)))
    (assert-eql 0 (dl:push-rect list 1 2 30 40 (red)))
    (assert-eql 1 (dl:push-rounded-rect list 5f0 6f0 70f0 80f0 4f0 (blue)))
    (assert-eql 2 (dl:display-list-count list))
    (assert-eql dl:+command-rect+ (dl:display-list-kind list 0))
    (assert-eql dl:+command-rounded-rect+ (dl:display-list-kind list 1))
    (assert-eql 1f0 (dl:display-list-x list 0))
    (assert-eql 2f0 (dl:display-list-y list 0))
    (assert-eql 30f0 (dl:display-list-width list 0))
    (assert-eql 40f0 (dl:display-list-height list 0))
    (assert-eql 0f0 (dl:display-list-radius list 0))
    (assert-eql (red) (dl:display-list-color list 0))
    (assert-eql 4f0 (dl:display-list-radius list 1))
    (assert-eql (blue) (dl:display-list-color list 1))))

(define-test a-full-list-signals-an-error ()
  (let ((list (dl:make-display-list :capacity 2)))
    (dl:push-rect list 0 0 1 1 (red))
    (dl:push-rect list 0 0 1 1 (red))
    (assert-error 'dl:display-list-full (dl:push-rect list 0 0 1 1 (red)))
    (assert-eql 2 (dl:display-list-count list))))

(define-test clearing-makes-room-again ()
  (let ((list (dl:make-display-list :capacity 2)))
    (dl:push-rect list 0 0 1 1 (red))
    (dl:push-rect list 0 0 1 1 (red))
    (dl:clear-display-list list)
    (assert-eql 0 (dl:display-list-count list))
    (dl:push-rect list 9 9 1 1 (blue))
    (assert-eql 9f0 (dl:display-list-x list 0))))

(define-test reading-a-command-that-is-not-there-is-an-error ()
  (let ((list (dl:make-display-list :capacity 4)))
    (dl:push-rect list 0 0 1 1 (red))
    (assert-error 'error (dl:display-list-x list 1))
    (assert-error 'error (dl:display-list-x list -1))))

;;; --- hashing ---

(defun build (&key (x 10) (color (red)) (extra nil))
  (let ((list (dl:make-display-list :capacity 8)))
    (dl:push-rect list x 20 100 50 color)
    (dl:push-rounded-rect list 5 5 10 10 3 (blue))
    (when extra (dl:push-rect list 0 0 1 1 (red)))
    list))

(define-test equal-lists-hash-alike ()
  (assert-eql (dl:display-list-hash (build)) (dl:display-list-hash (build))))

(define-test the-hash-ignores-unused-capacity ()
  (let ((small (dl:make-display-list :capacity 4))
        (large (dl:make-display-list :capacity 400)))
    (dl:push-rect small 1 2 3 4 (red))
    (dl:push-rect large 1 2 3 4 (red))
    (assert-eql (dl:display-list-hash small) (dl:display-list-hash large))))

(define-test any-difference-changes-the-hash ()
  (let ((base (dl:display-list-hash (build))))
    (assert-false (eql base (dl:display-list-hash (build :x 11))))
    (assert-false (eql base (dl:display-list-hash (build :color (blue)))))
    (assert-false (eql base (dl:display-list-hash (build :extra t))))))

(define-test the-kind-of-command-changes-the-hash ()
  (let ((rect (dl:make-display-list :capacity 2))
        (rounded (dl:make-display-list :capacity 2)))
    (dl:push-rect rect 1 2 3 4 (red))
    (dl:push-rounded-rect rounded 1 2 3 4 0 (red))
    (assert-false (eql (dl:display-list-hash rect) (dl:display-list-hash rounded)))))

(define-test a-cleared-list-hashes-like-a-new-one ()
  (let ((used (build))
        (fresh (dl:make-display-list :capacity 8)))
    (dl:clear-display-list used)
    (assert-eql (dl:display-list-hash fresh) (dl:display-list-hash used))))

(define-test the-hash-is-a-fixnum ()
  ;; So that returning it does not allocate.
  (assert-true (typep (dl:display-list-hash (build)) '(unsigned-byte 62)))
  (assert-true (typep (dl:display-list-hash (build)) 'fixnum)))

;;; --- no allocation ---

(define-test building-and-hashing-a-frame-does-not-allocate ()
  (let ((list (dl:make-display-list :capacity 128))
        (sum 0)
        (before (sb-ext:get-bytes-consed)))
    (declare (type (unsigned-byte 62) sum))
    (dotimes (frame 20000)
      (dl:clear-display-list list)
      (dotimes (i 50)
        (dl:push-rect list i 0 10 10 #xFF0000FF)
        (dl:push-rounded-rect list i 1 10 10 2 #x00FF00FF))
      (setf sum (ldb (byte 62 0) (+ sum (dl:display-list-hash list)))))
    (assert-true (plusp sum))
    ;; Two million commands. One byte per command would already be 2 MB.
    (let ((delta (- (sb-ext:get-bytes-consed) before)))
      (assert-true (< delta 1000000) delta))))
