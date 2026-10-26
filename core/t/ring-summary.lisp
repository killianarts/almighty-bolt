(defpackage #:almighty-bolt-core/t/ring-summary
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:ring #:almighty-bolt-core/ring)
                    (#:summary #:almighty-bolt-core/ring-summary)))

(in-package #:almighty-bolt-core/t/ring-summary)

;;; What a ring says: the frame time and its parts, builds, the cost of settling.

(defun frame (ring start duration builds capped first-build build wait encode
              &key (layout 0) (paint 0) (gc 0) (bytes 0))
  (ring:record-frame ring start (+ start duration) 0 bytes build wait encode builds capped)
  (ring:record-frame-details ring :first-build-ns first-build :layout-ns layout :paint-ns paint
                                  :gc-ns gc))

(defun a-ring ()
  (let ((ring (ring:make-frame-ring :capacity 16)))
    ;; The first frame is cold and is skipped in most tests.
    (frame ring 0 5000 2 nil 1000 3000 500 500 :gc 7)
    (frame ring 10000 1000 1 nil 400 400 300 300 :layout 100 :paint 200 :bytes 1000)
    (frame ring 20000 2000 2 nil 500 1000 600 400 :layout 300 :paint 400 :bytes 2000)
    (frame ring 30000 3000 3 t 500 1500 1000 500 :layout 600 :paint 700 :bytes 3000 :gc 5)
    ring))

(define-test the-frames-after-the-first-are-counted ()
  (assert-eql 3 (getf (summary:summarize-ring (a-ring) :skip 1) :frames))
  (assert-eql 4 (getf (summary:summarize-ring (a-ring)) :frames))
  (assert-false (summary:summarize-ring (ring:make-frame-ring :capacity 4))))

(define-test the-frame-time-and-its-parts-are-summarized ()
  (let ((result (summary:summarize-ring (a-ring) :skip 1)))
    (assert-equal 2000 (getf (getf result :duration) :median))
    (assert-equal 1000 (getf (getf result :duration) :min))
    (assert-equal 3000 (getf (getf result :duration) :max))
    (assert-equal 1000 (getf (getf result :build) :median))
    (assert-equal 300 (getf (getf result :layout) :median))
    (assert-equal 400 (getf (getf result :paint) :median))
    (assert-equal 400 (getf (getf result :encode) :median))))

(define-test builds-are-counted-with-their-histogram-and-the-caps ()
  (let ((builds (getf (summary:summarize-ring (a-ring) :skip 1) :builds)))
    (assert-equal 2.0 (getf builds :mean))
    (assert-eql 3 (getf builds :max))
    (assert-equal '((1 . 1) (2 . 1) (3 . 1)) (getf builds :histogram))
    (assert-eql 1 (getf builds :cap-hits))))

(define-test the-cost-of-settling-is-the-build-beyond-the-first ()
  (let ((settle (getf (summary:summarize-ring (a-ring) :skip 1) :settle)))
    ;; Extra builds: 0, 500 and 1000 ns over frames of 400, 1000 and 1500 ns of building.
    (assert-eql 2 (getf settle :frames-with-extra-builds))
    (assert-equal 500 (getf (getf settle :extra-build-ns) :median))
    (assert-equal 1000 (getf (getf settle :extra-build-ns) :max))
    (assert-true (< (abs (- (getf settle :share-of-build) (/ 1500 2900.0))) 1e-4))))

(define-test garbage-collection-and-bytes-are-summed-and-averaged ()
  (let ((result (summary:summarize-ring (a-ring) :skip 1)))
    (assert-eql 5 (getf result :gc-ns))
    (assert-equal 2000.0 (getf result :bytes-per-frame))))

(define-test a-frame-whose-first-build-was-not-timed-has-no-extra-cost ()
  (let ((ring (ring:make-frame-ring :capacity 4)))
    (frame ring 0 1000 2 nil 0 700 100 100)
    (let ((settle (getf (summary:summarize-ring ring) :settle)))
      (assert-eql 0 (getf settle :frames-with-extra-builds)))))
