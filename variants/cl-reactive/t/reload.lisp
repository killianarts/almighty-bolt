(defpackage #:almighty-bolt-variant-reactive/t/reload
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:v #:almighty-bolt-variant-reactive/views)
                    (#:b #:almighty-bolt-variant-reactive/builder)
                    (#:r #:almighty-bolt-variant-reactive/reactive)
                    (#:s #:almighty-bolt-variant-reactive/signals)
                    (#:i #:almighty-bolt-variant-retained/inputs)
                    (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-reactive/t/reload)

;;; An app that is made again after its code changed (decision record 0031): its signals are kept
;;; with KEPT-LET, and the nodes carry only what a signal does not hold.

(defvar *context* nil)
(defvar *name* nil
  "The signal of the field's text, of the app that was made last.")

(defun build (variant)
  (core:frame variant *context* (core:make-display-list :capacity 2048)))

(defun show (variant)
  (setf *context* (fake:make-fake-context))
  (build variant)
  variant)

(defun node (variant id)
  (n:find-node (retained:retained-variant-root (r:reactive-variant-inner variant)) id))

(defun pointer (variant kind x y &key button (buttons 0))
  (core:handle-event variant (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                                      :y (float y 1d0) :button button
                                                      :buttons buttons)))

(defun click-at (variant id dx)
  "Press and release DX points from the left of the control ID, in its middle from the top."
  (multiple-value-bind (x y width height) (core:lookup-target variant id)
    (declare (ignore width))
    (let ((px (+ x dx)) (py (+ y (/ height 2))))
      (pointer variant :move px py)
      (pointer variant :down px py :button 0 :buttons 1)
      (pointer variant :up px py :button 0)
      (build variant))))

(defun click (variant id)
  (multiple-value-bind (x y width height) (core:lookup-target variant id)
    (declare (ignore x y height))
    (click-at variant id (/ width 2))))

(defun type-text (variant string)
  (loop :for character :across string
        :do (core:handle-event variant (core:make-key-event :timestamp 0 :kind :down
                                                            :logical-key character :modifiers 0)))
  (build variant))

(defun make-roster (&key (title "Name:") (entry-format "~a"))
  "An app written as the real ones are: the signals in KEPT-LET, what is worked out from them made
again, and the screen described from both."
  (core:kept-let ((entries (s:make-signal (list "ann" "bob")))
                  (name (s:make-signal "")))
    (setf *name* name)
    (let ((shown (s:computed (mapcar (lambda (entry) (format nil entry-format entry))
                                     (s:value entries)))))
      (r:make-reactive-variant
       (lambda ()
         (b:column ()
           (v:label title :id :heading)
           (v:text-field :id :name :width 120 :value (s:value name)
                         :on-change (lambda (text) (setf (s:value name) text)))
           (v:list-box :id :box :rows 3 :items (s:value shown))
           (v:button "Add" :id :add :enabled (plusp (length (s:value name)))
                           :on-click (lambda ()
                                       (setf (s:value entries)
                                             (append (s:value entries) (list (s:value name)))
                                             (s:value name) "")))))))))

(defun fresh-roster (&rest options)
  (let ((state (core:make-kept-state)))
    (values (show (core:with-kept-state (state) (apply #'make-roster options)))
            state)))

(defun remake (old make-app state &key (inherit t))
  (let ((new (core:with-kept-state (state) (funcall make-app))))
    (when inherit
      (core:inherit-state new old))
    (build new)
    new))

(define-test the-signals-the-text-the-caret-and-the-keyboard-survive ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-at old :name 40)
    (type-text old "xy")
    ;; The caret is between the two characters.
    (click-at old :name (+ 8 8 1))
    (let ((new (remake old #'make-roster state)))
      (assert-equal "xy" (i:text-field-text (node new :name)))
      (assert-true (n:node-enabled (node new :add)))
      (assert-eq (node new :name) (retained:retained-variant-focus (r:reactive-variant-inner new)))
      (type-text new "z")
      (assert-equal "xzy" (s:peek *name*)))))

(define-test without-inheriting-the-text-is-kept-and-the-keyboard-is-not ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-at old :name 40)
    (type-text old "xy")
    (let ((new (remake old #'make-roster state :inherit nil)))
      (assert-equal "xy" (i:text-field-text (node new :name)))
      (type-text new "z")
      (assert-equal "xy" (s:peek *name*)))))

(define-test the-entries-survive ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-at old :name 40)
    (type-text old "cy")
    (click old :add)
    (assert-equalp #("ann" "bob" "cy") (i:list-box-items (node old :box)))
    (let ((new (remake old #'make-roster state)))
      (assert-equalp #("ann" "bob" "cy") (i:list-box-items (node new :box)))
      (assert-equal "" (i:text-field-text (node new :name))))))

(define-test an-edited-label-and-an-edited-way-to-show-the-entries-both-show ()
  ;; The list was described again after an entry was added, so its items were set by a signal.
  ;; That is not a state to carry: the new code shows them its way.
  (multiple-value-bind (old state) (fresh-roster)
    (click-at old :name 40)
    (type-text old "cy")
    (click old :add)
    (assert-equalp #("ann" "bob" "cy") (i:list-box-items (node old :box)))
    (let ((new (remake old (lambda () (make-roster :title "First name:" :entry-format "- ~a"))
                       state)))
      (assert-equal "First name:" (w:label-text (node new :heading)))
      (assert-equalp #("- ann" "- bob" "- cy") (i:list-box-items (node new :box))))))

(defun observers (signal)
  (length (s::source-observers signal)))

(define-test the-variant-that-is-retired-is-told-nothing-more ()
  (multiple-value-bind (old state) (fresh-roster)
    (let ((one (observers *name*)))
      (assert-true (plusp one))
      (let ((new (remake old #'make-roster state)))
        ;; Both are subscribed to the signal that they share.
        (assert-true (> (observers *name*) one))
        (core:retire-variant old)
        (assert-eql one (observers *name*))
        ;; And the new one goes on being told.
        (setf (s:value *name*) "q")
        (build new)
        (assert-equal "q" (i:text-field-text (node new :name)))))))

(define-test a-new-variant-that-is-retired-leaves-the-old-one-told ()
  ;; What the host does with a replacement whose first frame fails: the old one stays in use.
  (multiple-value-bind (old state) (fresh-roster)
    (let* ((one (observers *name*))
           (new (remake old #'make-roster state)))
      (core:retire-variant new)
      (assert-eql one (observers *name*))
      (setf (s:value *name*) "q")
      (build old)
      (assert-equal "q" (i:text-field-text (node old :name))))))
