(defpackage #:almighty-bolt-7guis/t/acceptance/machine
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)))

(in-package #:almighty-bolt-7guis/t/acceptance/machine)

;;; The machine interface as an agent uses it: it sees a tree of things with roles, names and values,
;;; works them by what they are, and waits for the app to answer. Nothing here knows an id of the
;;; app's, except where a test says so. The events go through the same input path as a person's.

(defun session-for (task)
  (d:driver-session (d:start :immediate task)))

(defun items (session)
  (getf (s:node-state (m:find-node session :role :list)) :items))

(defun field-value (session name)
  (s:node-value (m:find-node session :role :text-field :name name)))

(define-test an-agent-adds-a-person-named-ada-lovelace-in-the-crud ()
  (let ((session (session-for :crud)))
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (items session))
    (m:set-value session "Ada" :role :text-field :name "Name:")
    (m:set-value session "Lovelace" :role :text-field :name "Surname:")
    (assert-equal "Ada" (field-value session "Name:"))
    (m:click session :role :button :name "Create")
    ;; The list is in the order the people were added.
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman" "Lovelace, Ada") (items session))))

(define-test an-agent-picks-someone-from-the-list-and-deletes-them ()
  (let ((session (session-for :crud)))
    (m:set-value session "Mustermann, Max" :role :list)
    (assert-equal "Max" (field-value session "Name:"))
    (assert-equal "Mustermann" (field-value session "Surname:"))
    (m:click session :role :button :name "Delete")
    (assert-equal '("Emil, Hans" "Tisch, Roman") (items session))))

(define-test an-agent-edits-a-field-by-selecting-what-is-there-and-typing-over-it ()
  (let ((session (session-for :crud)))
    (m:set-value session "Anna" :role :text-field :name "Name:")
    (m:set-value session "Zeus" :role :text-field :name "Name:")
    (assert-equal "Zeus" (field-value session "Name:"))
    (m:set-value session "" :role :text-field :name "Name:")
    (assert-equal "" (field-value session "Name:"))))

(define-test an-agent-types-with-the-keyboard-and-presses-keys-with-modifiers ()
  (let ((session (session-for :temperature)))
    (m:focus session :role :text-field :name "Celsius =")
    (m:type-text session "100")
    (assert-equal "212" (field-value session "Fahrenheit"))
    (m:press-key session :backspace)
    (assert-equal "10" (field-value session "Celsius ="))
    (m:press-key session "a" :modifiers '(:command))
    (m:type-text session "-40")
    (assert-equal "-40" (field-value session "Fahrenheit"))))

(define-test an-agent-sets-a-slider-a-choice-and-a-field-to-what-it-wants ()
  (let ((session (session-for :timer)))
    (m:set-value session 20 :role :slider :name "Duration:")
    (let ((slider (m:find-node session :role :slider :name "Duration:")))
      (assert-true (< 19.5d0 (s:node-value slider) 20.5d0) (s:node-value slider))))
  (let ((session (session-for :flight)))
    (m:set-value session "return flight" :role :radio-group)
    (assert-equal "return flight" (s:node-value (m:find-node session :role :radio-group)))
    (m:set-value session 0 :role :radio-group)
    (assert-equal "one-way flight" (s:node-value (m:find-node session :role :radio-group)))))

(define-test what-cannot-be-done-is-said-in-words-for-whoever-asked ()
  (let ((session (session-for :crud)))
    (flet ((message (condition-type thunk)
             (handler-case (progn (funcall thunk) nil)
               (m:machine-error (condition)
                 (and (typep condition condition-type) (m:machine-error-message condition))))))
      ;; Nothing like it.
      (let ((message (message 'm:no-such-node
                              (lambda () (m:click session :role :button :name "Fly")))))
        (assert-true (and message (search "nothing on the screen" message)) message))
      ;; More than one: the label and the field it names.
      (let ((message (message 'm:ambiguous-node (lambda () (m:click session :name "Name:")))))
        (assert-true (and message (search "2 things match" message) (search "text-field" message))
                     message))
      ;; Not possible now.
      (let ((message (message 'm:cannot
                              (lambda () (m:click session :role :button :name "Delete")))))
        (assert-true (and message (search "disabled" message)) message))
      (let ((message (message 'm:cannot
                              (lambda () (m:set-value session 3 :role :text-field :name "Name:")))))
        (assert-true (and message (search "string" message)) message))
      ;; Without a way to take a screenshot, the session says so.
      (let ((message (message 'm:cannot (lambda () (m:screenshot session)))))
        (assert-true (and message (search "screenshot" message)) message)))))

(define-test the-keys-have-names ()
  (assert-equal #\Return (m:key-character "Enter"))
  (assert-equal #\Backspace (m:key-character :backspace))
  (assert-equal #\Escape (m:key-character "esc"))
  (assert-equal almighty-bolt/internals:+key-left+ (m:key-character "ArrowLeft"))
  (assert-equal almighty-bolt/internals:+key-page-down+ (m:key-character :page-down))
  (assert-equal #\a (m:key-character "A"))
  (assert-error 'm:cannot (m:key-character "hyper")))
