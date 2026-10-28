(defpackage #:almighty-bolt-variant-retained/t/reload
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:w #:almighty-bolt-variant-retained/widgets)
                    (#:i #:almighty-bolt-variant-retained/inputs)
                    (#:n #:almighty-bolt-variant-retained/node)
                    (#:b #:almighty-bolt-variant-retained/builder)
                    (#:retained #:almighty-bolt-variant-retained/retained)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-variant-retained/t/reload)

;;; An app that is made again after its code changed takes the state of the one it replaces
;;; (decision record 0031): the app's own state through KEPT-LET, and the widgets' by id. The
;;; context's text is 8 points a character.

(defvar *context* nil)

(defun build (variant)
  (core:frame variant *context* (core:make-display-list :capacity 2048)))

(defun show (variant)
  "VARIANT with its first frame built, in a fresh context."
  (setf *context* (fake:make-fake-context))
  (build variant)
  variant)

(defun node (variant id)
  (n:find-node (retained:retained-variant-root variant) id))

(defun pointer (variant kind x y &key button (buttons 0))
  (core:handle-event variant (core:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0)
                                                      :y (float y 1d0) :button button
                                                      :buttons buttons)))

(defun click-node (variant id &key (dx 0))
  "Move to the middle of the node ID (and DX points right of it), press and release."
  (let ((node (node variant id)))
    (multiple-value-bind (x y) (n:node-origin node)
      (let ((px (+ x dx (if (zerop dx) (/ (n:node-w node) 2) 0)))
            (py (+ y (/ (n:node-h node) 2))))
        (pointer variant :move px py)
        (pointer variant :down px py :button 0 :buttons 1)
        (pointer variant :up px py :button 0)
        (build variant)))))

(defun type-text (variant string)
  (loop :for character :across string
        :do (core:handle-event variant (core:make-key-event :timestamp 0 :kind :down
                                                            :logical-key character :modifiers 0)))
  (build variant))

(defun remake (old make-app state &key (inherit t))
  "What the host does when the app is made again: the app made with STATE, which takes what OLD
kept if INHERIT, with its first frame built."
  (let ((new (core:with-kept-state (state) (funcall make-app))))
    (when inherit
      (core:inherit-state new old))
    (build new)
    new))

(defun aware-variant (root &rest options)
  "A retained variant for the app of ROOT, as the app of one that keeps state with KEPT-LET: the
variant only carries the state of the nodes into an app that does."
  (core:kept-let ((aware t))
    (apply #'retained:make-retained-variant root options)))

(defvar *reloads* 0
  "How many times the app's own function for what it works out was run after a reload.")

(defun make-roster (&key (title "Name:") (initial "") (entry-format "~a") (hook t))
  "An app written as the real ones are: the model in KEPT-LET, the widgets built once and named,
and one function that brings the list and the button in line with the model and the field."
  (core:kept-let ((entries (list "ann" "bob")))
    (let (heading name box add)
      (labels ((refresh ()
                 (setf (i:list-box-items box) (mapcar (lambda (entry) (format nil entry-format entry))
                                                      entries)
                       (n:node-enabled add) (plusp (length (i:text-field-text name)))))
               (add-entry ()
                 (setf entries (append entries (list (i:text-field-text name)))
                       (i:text-field-text name) "")
                 (refresh)))
        (setf heading (w:label title :id :heading)
              name (i:text-field :id :name :text initial :width 120
                                 :on-change (lambda (text) (declare (ignore text)) (refresh)))
              box (i:list-box :id :box :rows 3 :width 120)
              add (w:button "Add" :id :add :on-click #'add-entry))
        (refresh)
        (retained:make-retained-variant
         (b:column (:gap 4) heading name box add)
         :after-reload (and hook (lambda () (incf *reloads*) (refresh))))))))

(defun fresh-roster (&rest options)
  "A roster made with a store of its own, shown, and the store."
  (let ((state (core:make-kept-state)))
    (values (show (core:with-kept-state (state) (apply #'make-roster options)))
            state)))

(define-test typed-text-and-the-enabled-button-survive-and-the-keyboard-stays ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-node old :name)
    (type-text old "xy")
    (assert-true (n:node-enabled (node old :add)))
    (let ((new (remake old #'make-roster state)))
      (assert-equal "xy" (i:text-field-text (node new :name)))
      (assert-true (n:node-enabled (node new :add)))
      ;; The keyboard is in the new field, at the end of the text.
      (assert-true (eq (node new :name) (retained:retained-variant-focus new)))
      (type-text new "z")
      (assert-equal "xyz" (i:text-field-text (node new :name))))))

(define-test the-model-and-what-was-worked-out-from-it-survive ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-node old :name)
    (type-text old "cy")
    (click-node old :add)
    (assert-equalp #("ann" "bob" "cy") (i:list-box-items (node old :box)))
    (let ((new (remake old #'make-roster state)))
      (assert-equalp #("ann" "bob" "cy") (i:list-box-items (node new :box)))
      (assert-equal "" (i:text-field-text (node new :name)))
      (assert-false (n:node-enabled (node new :add))))))

(define-test the-selection-in-a-list-survives ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-node old :box)
    (let ((selected (i:list-box-selected (node old :box))))
      (assert-true selected)
      (assert-eql selected (i:list-box-selected (node (remake old #'make-roster state) :box))))))

(define-test an-edited-label-shows ()
  (multiple-value-bind (old state) (fresh-roster)
    (let ((new (remake old (lambda () (make-roster :title "First name:")) state)))
      (assert-equal "First name:" (w:label-text (node new :heading))))))

(define-test a-label-that-the-app-changed-keeps-what-it-was-changed-to ()
  (multiple-value-bind (old state) (fresh-roster)
    (setf (w:label-text (node old :heading)) "Added!")
    (let ((new (remake old (lambda () (make-roster :title "First name:")) state)))
      (assert-equal "Added!" (w:label-text (node new :heading))))))

(define-test an-initial-value-that-nobody-changed-takes-the-new-code ()
  (multiple-value-bind (old state) (fresh-roster :initial "hello")
    (let ((new (remake old (lambda () (make-roster :initial "bye")) state)))
      (assert-equal "bye" (i:text-field-text (node new :name))))))

(define-test an-initial-value-that-was-typed-in-keeps-what-it-became ()
  (multiple-value-bind (old state) (fresh-roster :initial "hello")
    (click-node old :name)
    (type-text old "!")
    (let ((new (remake old (lambda () (make-roster :initial "bye")) state)))
      (assert-equal "hello!" (i:text-field-text (node new :name))))))

(define-test what-the-app-works-out-follows-the-new-code-when-it-says-how ()
  ;; A key typed makes the app work out the list, and what it worked out is what was changed
  ;; in the node. The hook, the app's own function for that, brings it in line with new code.
  (multiple-value-bind (old state) (fresh-roster)
    (click-node old :name)
    (type-text old "x")
    (let* ((*reloads* 0)
           (new (remake old (lambda () (make-roster :entry-format "- ~a")) state)))
      (assert-equalp #("- ann" "- bob") (i:list-box-items (node new :box)))
      (assert-eql 1 *reloads*))))

(define-test without-that-the-list-is-what-it-was ()
  ;; The cost of an app that does not say how: what it worked out and then changed stays.
  (multiple-value-bind (old state) (fresh-roster)
    (click-node old :name)
    (type-text old "x")
    (let ((new (remake old (lambda () (make-roster :entry-format "- ~a" :hook nil)) state)))
      (assert-equalp #("ann" "bob") (i:list-box-items (node new :box))))))

(define-test an-app-that-never-worked-the-list-out-again-shows-the-new-code ()
  ;; No key was typed, so the list is what the new code made.
  (multiple-value-bind (old state) (fresh-roster)
    (let ((new (remake old (lambda () (make-roster :entry-format "- ~a" :hook nil)) state)))
      (assert-equalp #("- ann" "- bob") (i:list-box-items (node new :box))))))

(defun two-anonymous-fields ()
  (aware-variant
   (b:column (:gap 4) (i:text-field :width 100) (i:text-field :width 100))))

(define-test nodes-without-an-id-are-the-same-by-where-they-are ()
  (let* ((old (show (two-anonymous-fields)))
         (second (aref (n:node-children (retained:retained-variant-root old)) 1)))
    (click-node-at old second)
    (type-text old "hi")
    (let* ((new (remake old #'two-anonymous-fields (core:make-kept-state)))
           (children (n:node-children (retained:retained-variant-root new))))
      (assert-equal "" (i:text-field-text (aref children 0)))
      (assert-equal "hi" (i:text-field-text (aref children 1)))
      (assert-true (eq (aref children 1) (retained:retained-variant-focus new))))))

(defun click-node-at (variant node)
  (multiple-value-bind (x y) (n:node-origin node)
    (let ((px (+ x (/ (n:node-w node) 2))) (py (+ y (/ (n:node-h node) 2))))
      (pointer variant :move px py)
      (pointer variant :down px py :button 0 :buttons 1)
      (pointer variant :up px py :button 0)
      (build variant))))

(define-test a-node-of-another-type-with-the-same-id-gets-nothing ()
  (let ((old (show (aware-variant (b:column () (i:text-field :id :x :text "abc"))))))
    (click-node old :x)
    (type-text old "z")
    (assert-equal "abcz" (i:text-field-text (node old :x)))
    ;; The field was changed, and the node that replaces it is a label: nothing of the field's
    ;; state goes into it, and nothing fails.
    (let ((new (remake old (lambda ()
                             (aware-variant
                              (b:column () (w:label "now a label" :id :x))))
                       (core:make-kept-state))))
      (assert-equal "now a label" (w:label-text (node new :x))))))

(define-test the-keyboard-goes-nowhere-if-the-node-that-had-it-is-gone ()
  (multiple-value-bind (old state) (fresh-roster)
    (click-node old :name)
    (assert-true (retained:retained-variant-focus old))
    (let ((new (remake old (lambda ()
                             (aware-variant (b:column () (w:label "alone" :id :x))))
                       state)))
      (assert-false (retained:retained-variant-focus new)))))

(define-test the-state-of-sliders-checkboxes-and-choices-survives ()
  (flet ((make-controls ()
           (aware-variant
            (b:column ()
              (i:slider :id :volume :maximum 10 :value 2)
              (i:checkbox "on" :id :on)
              (i:choice '("a" "b" "c") :id :pick :selected 0)
              (w:progress :id :bar :fraction 0.1)))))
    (let ((old (show (make-controls))))
      (setf (i:slider-value (node old :volume)) 7
            (i:checkbox-checked (node old :on)) t
            (i:choice-selected (node old :pick)) 2
            (w:progress-fraction (node old :bar)) 0.5)
      (let ((new (remake old #'make-controls (core:make-kept-state))))
        (assert-eql 7d0 (i:slider-value (node new :volume)))
        (assert-true (i:checkbox-checked (node new :on)))
        (assert-eql 2 (i:choice-selected (node new :pick)))
        (assert-eql 0.5d0 (w:progress-fraction (node new :bar)))))))

(define-test a-node-that-was-never-touched-is-left-as-the-new-code-made-it ()
  (flet ((make-controls (value)
           (lambda ()
             (aware-variant
              (b:column () (i:slider :id :volume :maximum 10 :value value))))))
    (let* ((old (show (funcall (make-controls 2))))
           (new (remake old (make-controls 9) (core:make-kept-state))))
      (assert-eql 9d0 (i:slider-value (node new :volume))))))

(define-test a-node-added-after-the-tree-was-made-carries-nothing ()
  (let* ((old (show (two-anonymous-fields)))
         (added (i:text-field :id :late :text "late"))
         (root (retained:retained-variant-root old)))
    (n:adopt root (append (coerce (n:node-children root) 'list) (list added)))
    (assert-true (eq :none (n:node-initial-state added)))
    ;; Nothing is carried from it, and nothing fails.
    (n:carry-mutated (i:text-field :id :late) added)))

(define-test a-disabled-and-a-hidden-node-stay-so ()
  (multiple-value-bind (old state) (fresh-roster)
    (setf (n:node-visible (node old :heading)) nil)
    (let ((new (remake old #'make-roster state)))
      (assert-false (n:node-visible (node new :heading))))))

;;; --- only what the person did (a tree whose values are somewhere else) ---

(defun make-fields (&key (text "abcdef"))
  (aware-variant
   (b:column ()
     (i:text-field :id :f :text text :width 200)
     (i:list-box :id :l :items (loop :for k :below 10 :collect (princ-to-string k)) :rows 3))))

(define-test the-caret-of-the-same-text-is-carried-without-the-text-of-the-slots ()
  (let ((old (show (make-fields))))
    (click-node old :f :dx 28)                  ; after the third character
    (setf (i:text-field-state (node old :f)) (core:make-text-state "abcdef" :caret 3 :anchor 1))
    (let* ((new (show (make-fields))))
      (retained:inherit-retained-state new old)
      (let ((state (i:text-field-state (node new :f))))
        (assert-eql 3 (core:text-state-caret state))
        (assert-eql 1 (core:text-state-anchor state))))))

(define-test the-caret-of-other-text-is-not-carried ()
  (let ((old (show (make-fields))))
    (setf (i:text-field-state (node old :f)) (core:make-text-state "abcdef" :caret 3))
    (let ((new (show (make-fields :text "other"))))
      (retained:inherit-retained-state new old)
      (assert-equal "other" (i:text-field-text (node new :f)))
      (assert-eql 5 (core:text-state-caret (i:text-field-state (node new :f)))))))

(define-test the-scroll-of-a-list-is-carried-and-its-selection-is-not ()
  (let ((old (show (make-fields))))
    (setf (i:list-box-selected (node old :l)) 8)
    (assert-eql 6 (i:list-box-top (node old :l)))
    (let ((new (show (make-fields))))
      (retained:inherit-retained-state new old)
      (assert-eql 6 (i:list-box-top (node new :l)))
      (assert-false (i:list-box-selected (node new :l))))))

;;; --- who is the same widget, and the keyboard ---

(define-test state-follows-the-id-when-the-widgets-are-in-another-order ()
  (flet ((make-a-then-b ()
           (aware-variant
            (b:column () (i:text-field :id :a :width 100) (i:text-field :id :b :width 100))))
         (make-b-then-a ()
           (aware-variant
            (b:column () (i:text-field :id :b :width 100) (i:text-field :id :a :width 100)))))
    (let ((old (show (make-a-then-b))))
      (click-node old :b)
      (type-text old "bee")
      (let ((new (remake old #'make-b-then-a (core:make-kept-state))))
        (assert-equal "bee" (i:text-field-text (node new :b)))
        (assert-equal "" (i:text-field-text (node new :a)))
        (assert-true (eq (node new :b) (retained:retained-variant-focus new)))))))

(defun make-field-and-autofocus-canvas ()
  (aware-variant
   (b:column ()
     (i:text-field :id :field :width 100)
     (i:canvas :id :board :width 50 :height 50 :focusable t :autofocus t))))

(define-test an-autofocus-canvas-does-not-take-the-keyboard-again ()
  (let ((old (show (make-field-and-autofocus-canvas))))
    ;; The canvas had the keyboard when the window opened, and then the person took it away.
    (assert-true (eq (node old :board) (retained:retained-variant-focus old)))
    (n:focus-node (retained:retained-variant-session old) nil)
    (let ((new (remake old #'make-field-and-autofocus-canvas (core:make-kept-state))))
      ;; It asks for the keyboard when a window opens, and this one has been open.
      (assert-false (retained:retained-variant-focus new)))))

(define-test the-keyboard-is-in-the-field-it-was-moved-to-and-not-in-an-autofocus-canvas ()
  (let ((old (show (make-field-and-autofocus-canvas))))
    (click-node old :field)
    (let ((new (remake old #'make-field-and-autofocus-canvas (core:make-kept-state))))
      (assert-true (eq (node new :field) (retained:retained-variant-focus new))))))

(define-test a-selection-beyond-the-items-of-the-new-code-is-dropped ()
  (flet ((make-items (count)
           (lambda ()
             (aware-variant
              (b:column () (i:list-box :id :l :rows 3
                                       :items (loop :for k :below count
                                                    :collect (princ-to-string k))))))))
    (let ((old (show (funcall (make-items 10)))))
      (setf (i:list-box-selected (node old :l)) 8)
      (let ((new (remake old (make-items 5) (core:make-kept-state))))
        (assert-equalp #("0" "1" "2" "3" "4") (i:list-box-items (node new :l)))
        (assert-false (i:list-box-selected (node new :l)))))))

;;; --- an app that keeps nothing ---

(defun make-plain-fields ()
  "An app with no KEPT-LET: its variables, if it had any, start again when it is made."
  (retained:make-retained-variant
   (b:column () (i:text-field :id :f :width 100))
   :after-reload (lambda () (incf *reloads*))))

(define-test an-app-that-keeps-nothing-is-started-again-with-nothing-of-the-nodes ()
  ;; The nodes hold what the app reads back, and the app's variables are not carried, so carrying
  ;; what the nodes hold would show what the variables do not have.
  (let ((old (show (make-plain-fields))))
    (click-node old :f)
    (type-text old "typed")
    (let* ((*reloads* 0)
           (new (remake old #'make-plain-fields (core:make-kept-state))))
      (assert-equal "" (i:text-field-text (node new :f)))
      (assert-false (retained:retained-variant-focus new))
      (assert-eql 0 *reloads*))))
