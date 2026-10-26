(defpackage #:almighty-bolt-variant-cursor/t/cursor
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:c #:almighty-bolt-variant-cursor/cursor)
                    (#:w #:almighty-bolt-variant-cursor/widgets)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-cursor/t/cursor)

;;; The cursor front end: what it places, and what each mode does about a size it did not
;;; have. The context's text is 8 points a character, 10 above the baseline and 4 below, so a
;;; line is 14 high.

(defvar *context* nil)
(defvar *list* nil)

(defun show (function &key (mode :rebuild) (width 400f0) (height 300f0))
  "A variant of MODE for FUNCTION, a fresh context and list, and its first build."
  (setf *context* (fake:make-fake-context :width width :height height)
        *list* (core:make-display-list :capacity 4096))
  (let ((variant (c:make-cursor-variant function :mode mode)))
    (core:frame variant *context* *list*)
    variant))

(defun build-again (variant)
  "One more build, as the core makes it. Returns what FRAME returns."
  (core:clear-display-list *list*)
  (core:frame variant *context* *list*))

(defun rect (variant id)
  (multiple-value-list (core:lookup-target variant id)))

(defun commands (kind)
  "The x, y, width and height of the commands of KIND in the list, in order."
  (loop :for i :below (core:display-list-count *list*)
        :when (= (core:display-list-kind *list* i) kind)
          :collect (list (core:display-list-x *list* i) (core:display-list-y *list* i)
                         (core:display-list-width *list* i) (core:display-list-height *list* i))))

;;; --- placing ---

(define-test labels-go-one-under-another-from-the-corner ()
  (let ((variant (show (lambda () (w:label "ab" :id :a) (w:label "abc" :id :b)))))
    (assert-equal '(0f0 0f0 16f0 14f0) (rect variant :a))
    (assert-equal '(0f0 14f0 24f0 14f0) (rect variant :b))))

(define-test the-gap-of-a-column-is-between-its-children ()
  (let ((variant (show (lambda ()
                         (w:with-column (:gap 6)
                           (w:label "ab" :id :a)
                           (w:label "ab" :id :b))))))
    (assert-equal '(0f0 20f0 16f0 14f0) (rect variant :b))))

(define-test a-row-goes-across ()
  (let ((variant (show (lambda ()
                         (w:with-row (:gap 4)
                           (w:label "ab" :id :a)
                           (w:label "abc" :id :b))))))
    (assert-equal '(20f0 0f0 24f0 14f0) (rect variant :b))))

(define-test a-paragraph-is-broken-to-the-room-across ()
  (let ((variant (show (lambda ()
                         (w:with-column (:width 80)
                           (w:paragraph "abcdefghijkl" :id :p))))))
    ;; Ten characters to a line in 80 points, so two lines.
    (assert-equal '(0f0 0f0 80f0 28f0) (rect variant :p))))

(define-test the-inset-narrows-the-room-all-round ()
  (let ((variant (show (lambda ()
                         (w:with-column (:inset 12)
                           (w:label "ab" :id :a)
                           (assert-eql 376f0 (w:avail-width))
                           (assert-eql 262f0 (w:avail-height)))))))
    (assert-equal '(12f0 12f0 16f0 14f0) (rect variant :a))))

(define-test the-sizes-of-a-split-add-up-with-the-gaps ()
  (let ((sizes (w:split-sizes 100 10 1 2)))
    (assert-true (< (abs (- 30f0 (first sizes))) 1e-4))
    (assert-true (< (abs (- 60f0 (second sizes))) 1e-4))))

(define-test a-panel-with-a-size-is-drawn-first-and-holds-its-children-in-by-the-padding ()
  (let ((variant (show (lambda ()
                         (w:with-panel (:id :p :pad 8 :width 100 :height 50)
                           (w:label "ab" :id :l))))))
    (assert-equal '(0f0 0f0 100f0 50f0) (rect variant :p))
    (assert-equal '(8f0 8f0 16f0 14f0) (rect variant :l))
    ;; The window's background first, then the panel's, then the label's text.
    (assert-eql core:+command-rect+ (core:display-list-kind *list* 0))
    (assert-eql core:+command-rounded-rect+ (core:display-list-kind *list* 1))
    (assert-equal '((0f0 0f0 100f0 50f0)) (commands core:+command-rounded-rect+))))

;;; --- a size that is not known yet ---

(defun card ()
  (w:centered (:id :card)
    (w:with-panel (:id :card :pad 8)
      (w:label "ab" :id :text))))

(define-test the-first-lagging-build-draws-a-card-it-knows-nothing-of-and-says-so ()
  (let ((variant (show #'card :mode :lag)))
    ;; Nothing remembered: a background of no size, centred as if the card had none.
    (assert-equal '((200f0 150f0 0f0 0f0)) (commands core:+command-rounded-rect+))
    (assert-true (c:cursor-variant-stale variant))
    ;; What it found, for the next build.
    (assert-equal '(200f0 150f0 32f0 30f0) (rect variant :card))))

(define-test the-next-lagging-build-has-the-size-and-is-not-stale ()
  (let ((variant (show #'card :mode :lag)))
    (assert-false (build-again variant))
    (assert-false (c:cursor-variant-stale variant))
    (assert-equal '((184f0 135f0 32f0 30f0)) (commands core:+command-rounded-rect+))
    (assert-equal '(184f0 135f0 32f0 30f0) (rect variant :card))))

(define-test a-lagging-build-never-asks-for-another ()
  (let ((variant (show #'card :mode :lag)))
    (assert-true (c:cursor-variant-stale variant))
    (assert-false (core:take-frame-request *context*))))

(define-test a-requesting-build-asks-for-a-frame-while-it-is-stale-and-then-stops ()
  (let ((variant (show #'card :mode :request)))
    (assert-true (core:take-frame-request *context*))
    (build-again variant)
    (assert-false (core:take-frame-request *context*))
    (assert-equal '((184f0 135f0 32f0 30f0)) (commands core:+command-rounded-rect+))))

(define-test a-rebuilding-build-says-it-changed-state-until-it-has-the-size ()
  (let ((variant (c:make-cursor-variant #'card :mode :rebuild))
        (context (fake:make-fake-context :width 400f0 :height 300f0))
        (list (core:make-display-list :capacity 4096)))
    (assert-eql 2 (core:settle-frame variant context list))
    (assert-equal '(184f0 135f0 32f0 30f0) (multiple-value-list (core:lookup-target variant :card)))))

(defun nested-card ()
  "A card that holds a panel, and moves over by the width that panel had in the build before:
its size depends on a size that depends on nothing, and where it goes on its own."
  (w:centered (:id :outer)
    (w:with-panel (:id :outer :pad 8)
      (skip-by-remembered :inner)
      (w:with-panel (:id :inner :pad 8)
        (w:label "ab" :id :text)))))

(defun skip-by-remembered (id)
  (multiple-value-bind (width height) (almighty-bolt-variant-cursor/ui:remembered-size
                                       almighty-bolt-variant-cursor/ui:*ui* id)
    (declare (ignore height))
    (w:skip width 0)))

(define-test a-card-in-a-card-takes-three-builds-to-settle ()
  (let ((variant (c:make-cursor-variant #'nested-card :mode :rebuild))
        (context (fake:make-fake-context :width 400f0 :height 300f0))
        (list (core:make-display-list :capacity 4096)))
    (assert-eql 3 (core:settle-frame variant context list))
    ;; Inner 32 wide, moved over by its own width, in an outer of 8 + 32 + 32 + 8.
    (assert-equal '(80f0) (list (third (multiple-value-list (core:lookup-target variant :outer)))))))

;;; --- the fragment mode ---

(define-test the-fragment-mode-puts-the-card-in-the-middle-on-the-first-build ()
  (let ((variant (show #'card :mode :fragment)))
    (assert-false (c:cursor-variant-stale variant))
    (assert-equal '((184f0 135f0 32f0 30f0)) (commands core:+command-rounded-rect+))
    (assert-equal '(184f0 135f0 32f0 30f0) (rect variant :card))
    ;; Its text is where it is in a lagging build that has the size.
    (assert-equal '(192f0 143f0 16f0 14f0) (rect variant :text))))

(define-test the-fragment-mode-draws-what-a-lagging-build-with-the-size-draws ()
  (let ((fragment (show #'card :mode :fragment))
        (fragment-hash (core:display-list-hash *list*)))
    (declare (ignore fragment))
    (let ((lag (show #'card :mode :lag)))
      (build-again lag)
      (assert-eql fragment-hash (core:display-list-hash *list*)))))

(define-test fragments-nest ()
  (let ((variant (show (lambda ()
                         (w:centered ()
                           (w:with-panel (:id :a :pad 8)
                             (w:with-panel (:id :b :pad 4)
                               (w:label "ab" :id :text)))))
                       :mode :fragment)))
    ;; The label 16 by 14, in a panel 24 by 22, in a panel 40 by 38, in the middle of 400 by 300.
    (assert-equal '(180f0 131f0 40f0 38f0) (rect variant :a))
    (assert-equal '(188f0 139f0 24f0 22f0) (rect variant :b))
    (assert-equal '(192f0 143f0 16f0 14f0) (rect variant :text))))

;;; --- the measure mode ---

(define-test the-measure-mode-has-the-size-on-the-first-build-and-is-not-stale ()
  (let ((variant (show #'card :mode :measure)))
    (assert-false (c:cursor-variant-stale variant))
    (assert-equal '((184f0 135f0 32f0 30f0)) (commands core:+command-rounded-rect+))
    (assert-equal '(184f0 135f0 32f0 30f0) (rect variant :card))
    (assert-eql 1 (core:settle-frame variant *context* *list*))))

(define-test the-measure-mode-draws-what-a-lagging-build-with-the-size-draws ()
  (show #'card :mode :measure)
  (let ((measured-hash (core:display-list-hash *list*))
        (lag (show #'card :mode :lag)))
    (build-again lag)
    (assert-eql measured-hash (core:display-list-hash *list*))))

(define-test the-measure-mode-describes-the-card-twice-and-the-other-modes-once ()
  (flet ((runs (mode)
           (let ((runs 0))
             (show (lambda ()
                     (w:centered (:id :card)
                       (w:with-panel (:id :card :pad 8)
                         (incf runs)
                         (w:label "ab" :id :text))))
                   :mode mode)
             runs)))
    (assert-eql 2 (runs :measure))
    (assert-eql 1 (runs :lag))
    (assert-eql 1 (runs :fragment))))

(define-test the-measure-mode-does-not-describe-a-card-in-a-card-four-times ()
  (let ((runs 0))
    (show (lambda ()
            (w:centered (:id :outer)
              (w:with-panel (:id :outer :pad 8)
                (w:with-panel (:id :inner :pad 8)
                  (incf runs)
                  (w:label "ab" :id :text)))))
          :mode :measure)
    (assert-eql 2 runs)))

;;; --- what the fragment mode does to pixels ---

(defun spread-card ()
  (w:centered (:id :card)
    (w:with-panel (:id :card :pad 8)
      (w:with-row (:gap 0.3)
        (w:label "ab" :id :one)
        (w:label "abc" :id :two)))))

(define-test the-fragment-mode-snaps-a-fragment-and-the-text-in-it-separately ()
  ;; At two pixels to a point, a card centred in a window 400 wide has a label at 16.3 from its
  ;; corner. The other modes snap that label where it is in the window; the fragment mode snaps
  ;; the label in the fragment and the fragment in the window, and the text lands on the next
  ;; pixel: half a point.
  (flet ((draw (mode)
           (setf *context* (fake:make-fake-context :width 400f0 :height 300f0 :scale 2f0)
                 *list* (core:make-display-list :capacity 4096))
           (core:settle-frame (c:make-cursor-variant #'spread-card :mode mode) *context* *list*)
           (loop :for i :below (core:display-list-count *list*)
                 :when (= (core:display-list-kind *list* i) core:+command-glyph+)
                   :collect (core:display-list-x *list* i))))
    (let ((measured (draw :measure))
          (fragment (draw :fragment)))
      (assert-equal (length measured) (length fragment))
      (assert-false (equal measured fragment))
      (assert-true (<= (reduce #'max (mapcar (lambda (a b) (abs (- a b))) measured fragment))
                       0.5f0))
      (assert-true (> (reduce #'max (mapcar (lambda (a b) (abs (- a b))) measured fragment))
                      0f0)))))

;;; --- room and the settle cap ---

(define-test a-widget-wider-than-the-room-across-takes-the-room ()
  (let ((variant (show (lambda ()
                         (w:with-column (:width 80)
                           (w:label "abcdefghijklmnop" :id :wide))))))
    (assert-equal '(0f0 0f0 80f0 14f0) (rect variant :wide))))

(defun chain (levels)
  "LEVELS cards, each inside the one before, each moved over by the width its inner card had in
the build before: a size that depends on a size that depends on a size."
  (labels ((level (n)
             (if (zerop n)
                 (w:label "ab" :id :text)
                 (let ((id (intern (format nil "LEVEL-~d" n) :keyword)))
                   (w:with-panel (:id id :pad 8)
                     (when (> n 1)
                       (skip-by-remembered (intern (format nil "LEVEL-~d" (1- n)) :keyword)))
                     (level (1- n)))))))
    (lambda () (w:centered (:id (intern (format nil "LEVEL-~d" levels) :keyword))
                 (level levels)))))

(define-test one-more-level-of-nesting-takes-one-more-build-and-the-third-is-the-cap ()
  (flet ((builds (levels)
           (let ((variant (c:make-cursor-variant (chain levels) :mode :rebuild))
                 (context (fake:make-fake-context :width 400f0 :height 300f0))
                 (list (core:make-display-list :capacity 4096))
                 (capped nil))
             (values (handler-bind ((core:settle-cap-reached
                                      (lambda (condition)
                                        (setf capped t)
                                        (muffle-warning condition))))
                       (core:settle-frame variant context list))
                     capped))))
    (assert-equal '(2 nil) (multiple-value-list (builds 1)))
    (assert-equal '(3 nil) (multiple-value-list (builds 2)))
    (assert-equal '(3 t) (multiple-value-list (builds 3)))))
