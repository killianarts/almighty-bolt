(defpackage #:almighty-bolt-variant-retained/t/cache
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:counter #:almighty-bolt-variant-retained/counter)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-retained/t/cache)

;;; What a node keeps of what it painted, and when it paints again. (Moving commands
;;; to where the node is is the core's APPEND-DISPLAY-LIST, which has its own tests.) They count how many
;;; times a label and a button are painted, which happens only when the node is not
;;; replaying its cache, and compare what the cache gives with painting from nothing.
;;; The context's text is 8 points a character, 10 above the baseline and 4 below.

(defvar *label-paints* 0)
(defvar *button-paints* 0)

(defmethod n:paint-node :before ((label w:label) context list x y)
  (declare (ignore context list x y))
  (incf *label-paints*))

(defmethod n:paint-node :before ((button w:button) context list x y)
  (declare (ignore context list x y))
  (incf *button-paints*))

(defvar *context* nil)

(defun make-test-counter (&key (caching t))
  "The Counter, with a fresh context. Returns the variant, the number and the button."
  (setf *context* (fake:make-fake-context))
  (multiple-value-bind (variant number button) (counter:make-counter)
    (unless caching
      (setf variant (retained:make-retained-variant
                     (let ((root (retained:retained-variant-root variant)))
                       (setf (n:node-tree root) nil)
                       root)
                     :caching nil)))
    (values variant number button)))

(defun build (variant)
  (let ((list (core:make-display-list :capacity 256)))
    (core:frame variant *context* list)
    list))

(defun paints ()
  (list *label-paints* *button-paints*))

(defun paints-since (before)
  (mapcar #'- (paints) before))

(defun pointer (variant kind x y &key (button nil) (buttons 0))
  (core:handle-event variant
                     (core:make-pointer-event :timestamp 0 :kind kind
                                              :x (float x 1d0) :y (float y 1d0)
                                              :button button :buttons buttons)))

;;; --- when a node paints again ---

(define-test the-first-frame-paints-every-cacheable-node-once-and-a-clean-frame-none ()
  (multiple-value-bind (variant) (make-test-counter)
    (let ((before (paints)))
      (build variant)
      ;; The number and the button, whose label is painted into the button's cache.
      (assert-equal '(2 1) (paints-since before))
      (setf before (paints))
      (build variant)
      (build variant)
      (assert-equal '(0 0) (paints-since before)))))

(define-test changing-a-colour-paints-only-that-node-again ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (let ((before (paints)))
      (setf (w:label-color number) (core:pack-color 255 0 0))
      (let ((list (build variant)))
        (assert-equal '(1 0) (paints-since before))
        (assert-eql (core:pack-color 255 0 0) (core:display-list-color list 1))))))

(define-test hovering-a-button-paints-it-and-nothing-else ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    (let ((before (paints)))
      (assert-true (pointer variant :move 82 38))
      (build variant)
      ;; The button, and its label inside it; not the number.
      (assert-equal '(1 1) (paints-since before)))))

(define-test a-node-that-only-moves-is-replayed-in-its-new-place ()
  (multiple-value-bind (variant number) (make-test-counter)
    (let ((list (build variant)))
      (assert-eql 48f0 (core:display-list-x list 2)))
    (let ((before (paints)))
      ;; The number gets wider, so the button moves right.
      (setf (w:label-text number) "10")
      (let ((list (build variant)))
        ;; The number is painted again, since its text and size changed; the button
        ;; is not, and its commands are the old ones, moved by 8.
        (assert-equal '(1 0) (paints-since before))
        (assert-eql core:+command-rounded-rect+ (core:display-list-kind list 3))
        (assert-eql 56f0 (core:display-list-x list 3))
        (assert-eql 70f0 (core:display-list-x list 4))))))

(define-test a-window-that-changes-size-but-not-the-layout-paints-nothing-again ()
  (multiple-value-bind (variant) (make-test-counter)
    (build variant)
    (let ((before (paints)))
      (setf (core:context-width *context*) 500f0)
      (build variant)
      (assert-equal '(0 0) (paints-since before)))))

(define-test a-cache-that-is-too-small-grows ()
  (setf *context* (fake:make-fake-context))
  (let* ((label (w:label (make-string 100 :initial-element #\a)))
         (variant (retained:make-retained-variant label))
         (before (paints)))
    (let ((first (build variant))
          (second (build variant)))
      ;; A background and a glyph for each character, in each frame. The label was
      ;; painted twice in the first, since the first try overflowed the cache of 64
      ;; commands, and not at all in the second.
      (assert-eql 101 (core:display-list-count first))
      (assert-eql 101 (core:display-list-count second))
      (assert-equal '(2 0) (paints-since before))
      (assert-eql (core:display-list-hash first) (core:display-list-hash second)))))

;;; --- what the cache gives is what painting gives ---

(defun painted (root context caching)
  "What painting ROOT from the top left gives, with or without the caches."
  (let ((list (core:make-display-list :capacity 256))
        (n:*paint-caching* caching))
    (n:paint-node root context list 0f0 0f0)
    list))

(defun same-as-painting-from-nothing-p (variant)
  (let ((root (retained:retained-variant-root variant)))
    (= (core:display-list-hash (painted root *context* t))
       (core:display-list-hash (painted root *context* nil)))))

(define-test the-cache-gives-what-painting-from-nothing-gives ()
  (multiple-value-bind (variant number) (make-test-counter)
    (build variant)
    (assert-true (same-as-painting-from-nothing-p variant))
    (pointer variant :move 82 38)
    (build variant)
    (assert-true (same-as-painting-from-nothing-p variant))
    (pointer variant :down 82 38 :button 0 :buttons 1)
    (build variant)
    (assert-true (same-as-painting-from-nothing-p variant))
    (pointer variant :up 82 38 :button 0)
    (build variant)
    (assert-true (same-as-painting-from-nothing-p variant))
    (setf (w:label-color number) (core:pack-color 1 2 3))
    (build variant)
    (assert-true (same-as-painting-from-nothing-p variant))
    (dotimes (i 12)
      (pointer variant :down 82 38 :button 0 :buttons 1)
      (pointer variant :up 82 38 :button 0)
      (build variant))
    ;; The click on the way up counted too.
    (assert-equal "13" (w:label-text number))
    (assert-true (same-as-painting-from-nothing-p variant))
    (core:handle-event variant (core:make-key-event :timestamp 0 :kind :down :logical-key #\Tab))
    (build variant)
    (assert-true (same-as-painting-from-nothing-p variant))))

(define-test a-cacheable-node-is-painted-on-a-whole-pixel-with-or-without-its-cache ()
  ;; Text is put on whole device pixels from its position in the window, so a cache
  ;; that is moved must be moved by whole pixels. At two pixels a point, 10.3 is 10.5.
  (setf *context* (fake:make-fake-context :scale 2f0))
  (let* ((label (w:label "ab"))
         (root (w:padding label :left 10.3 :top 0.2)))
    (n:layout-node root *context* 0f0 400f0 0f0 300f0)
    (dolist (caching '(t nil))
      (let ((list (core:make-display-list :capacity 16))
            (n:*paint-caching* caching))
        (n:paint-node root *context* list 0f0 0f0)
        (assert-eql 10.5f0 (core:display-list-x list 0) caching)
        (assert-eql 18.5f0 (core:display-list-x list 1) caching)
        (assert-eql 0f0 (core:display-list-y list 0) caching)))))

(define-test a-variant-without-caching-paints-everything-every-frame ()
  (multiple-value-bind (variant) (make-test-counter :caching nil)
    (assert-false (retained:retained-variant-caching variant))
    (let ((before (paints)))
      (build variant)
      (build variant)
      (assert-equal '(4 2) (paints-since before)))))
