(defpackage #:almighty-bolt/machine
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:accessibility-tree
                #:now-ns
                #:make-pointer-event
                #:make-key-event
                #:make-wheel-event
                #:+modifier-shift+
                #:+modifier-control+
                #:+modifier-alt+
                #:+modifier-super+
                #:+key-up+
                #:+key-down+
                #:+key-left+
                #:+key-right+
                #:+key-delete-forward+
                #:+key-home+
                #:+key-end+
                #:+key-page-up+
                #:+key-page-down+)
  (:import-from #:almighty-bolt/ui/semantics
                #:node-id
                #:node-role
                #:node-name
                #:node-value
                #:node-state
                #:node-bounds
                #:node-children
                #:node-property
                #:find-nodes)
  (:export #:session
           #:make-session
           #:session-variant
           #:session-deliver
           #:session-settle
           #:session-on-main
           #:session-screenshot
           #:machine-error
           #:machine-error-message
           #:no-such-node
           #:ambiguous-node
           #:cannot
           #:see
           #:find-node
           #:describe-node
           #:click
           #:double-click
           #:focus
           #:type-text
           #:press-key
           #:set-value
           #:select-item
           #:scroll
           #:wait
           #:screenshot
           #:key-character))

(in-package #:almighty-bolt/machine)

;;; What a program that is not a person needs to use an app, and the same for a test that has
;;; only the tree: to SEE the screen as a tree of things with roles, names, values and states
;;; (the semantic tree); to ACT on a thing by what it is (its id, or its role and name), with the
;;; events a person's hands would make, through the same input path as the real ones, so that the
;;; app cannot tell; and to WAIT until the app has answered, so that nothing polls or sleeps. A
;;; SESSION is what these need of a place where an app runs: a window (HOST-SESSION), or a
;;; test's stand-in for one.

(defstruct (session (:constructor make-session (&key variant deliver settle on-main screenshot)))
  "VARIANT is a function of no arguments that returns what is running. DELIVER takes an event and
offers it to the app as the window would. SETTLE returns when the app has answered everything
delivered so far. ON-MAIN takes a function and calls it on the thread that owns the ui, between
frames, and returns its values. SCREENSHOT is a function of no arguments that returns the screen as
a PNG in a vector of bytes, or NIL if it cannot."
  variant deliver settle on-main screenshot)

(define-condition machine-error (error)
  ((message :initarg :message :reader machine-error-message))
  (:report (lambda (condition stream)
             (write-string (machine-error-message condition) stream)))
  (:documentation "A request that cannot be done, with a message that says why, for whoever asked."))

(define-condition no-such-node (machine-error) ())
(define-condition ambiguous-node (machine-error) ())
(define-condition cannot (machine-error) ())

(defun refuse (condition format-control &rest arguments)
  (error condition :message (apply #'format nil format-control arguments)))

;;; --- seeing ---

(defun see (session)
  "The semantic tree of the screen as it is now (see ALMIGHTY-BOLT/UI/SEMANTICS)."
  (funcall (session-on-main session)
           (lambda () (accessibility-tree (funcall (session-variant session))))))

(defun describe-node (node)
  (format nil "~(~a~)~@[ ~s~]~@[ with the id ~s~]" (node-role node) (node-name node) (node-id node)))

(defun find-node (session &key id role name)
  "The one node of the screen with the given ID, ROLE and NAME (each only if given). An error that
lists what there is if there is none or more than one."
  (unless (or id role name)
    (refuse 'no-such-node "Say which thing: an id, or a role and a name."))
  (let ((found (find-nodes (see session) :id id :role role :name name)))
    (case (length found)
      (1 (first found))
      (0 (refuse 'no-such-node "There is nothing on the screen~@[ with the id ~s~]~@[ with the role ~(~a~)~]~@[ called ~s~]."
                 id role name))
      (t (refuse 'ambiguous-node "~d things match~@[ the id ~s~]~@[ the role ~(~a~)~]~@[ the name ~s~]: ~{~a~^; ~}. Say more."
                 (length found) id role name (mapcar #'describe-node found))))))

;;; --- the events of hands ---

(defun deliver (session event)
  (funcall (session-deliver session) event))

(defun settle (session)
  (funcall (session-settle session)))

(defun pointer (session kind x y &key button (buttons 0) (count 0))
  (deliver session (make-pointer-event :timestamp (now-ns) :kind kind
                                       :x (float x 1d0) :y (float y 1d0)
                                       :button button :buttons buttons :click-count count)))

(defun bounds-point (bounds &optional (fx 0.5) (fy 0.5))
  "The point at the fraction FX, FY across and down BOUNDS, a list of x, y, width and height."
  (destructuring-bind (x y width height) bounds
    (values (+ x (* fx width)) (+ y (* fy height)))))

(defun middle (node)
  (bounds-point (node-bounds node)))

(defun press-at (session x y &key (count 1))
  "A press and release of the primary button at X, Y, the pointer having moved there first."
  (pointer session :move x y)
  (pointer session :down x y :button 0 :buttons 1 :count count)
  (pointer session :up x y :button 0 :buttons 0 :count count))

(defun modifier-bits (modifiers)
  (let ((bits 0))
    (dolist (modifier modifiers bits)
      (setf bits (logior bits
                         (ecase (intern (string-upcase (string modifier)) :keyword)
                           (:shift +modifier-shift+)
                           (:control +modifier-control+)
                           ((:alt :option) +modifier-alt+)
                           ((:super :command :cmd :meta) +modifier-super+)))))))

(defun key-character (key)
  "The character that KEY is: a character, a string of one, or the name of a key, such as
:return, \"Backspace\", \"ArrowLeft\", \"Escape\" or :page-down, in any case."
  (etypecase key
    (character key)
    ((or string symbol)
     (let ((name (remove #\- (remove #\_ (remove #\Space (string-downcase (string key)))))))
       (cond ((= (length name) 1) (char name 0))
             (t (let ((name (if (eql 0 (search "arrow" name)) (subseq name 5) name)))
                  (cond ((member name '("return" "enter") :test #'string=) #\Return)
                        ((string= name "tab") #\Tab)
                        ((member name '("escape" "esc") :test #'string=) #\Escape)
                        ((string= name "backspace") #\Backspace)
                        ((member name '("delete" "forwarddelete") :test #'string=)
                         +key-delete-forward+)
                        ((string= name "space") #\Space)
                        ((string= name "up") +key-up+)
                        ((string= name "down") +key-down+)
                        ((string= name "left") +key-left+)
                        ((string= name "right") +key-right+)
                        ((string= name "home") +key-home+)
                        ((string= name "end") +key-end+)
                        ((string= name "pageup") +key-page-up+)
                        ((string= name "pagedown") +key-page-down+)
                        (t (refuse 'cannot "There is no key called ~s." key))))))))))

(defun press-key (session key &key modifiers)
  "Press KEY (see KEY-CHARACTER) with MODIFIERS (a list of :shift, :control, :alt, :super) down."
  (deliver session (make-key-event :timestamp (now-ns) :kind :down
                                   :logical-key (key-character key)
                                   :modifiers (modifier-bits modifiers)))
  (settle session)
  nil)

(defun type-text (session text &key id role name)
  "Type TEXT, a key for each character, into the thing that has the keyboard, or, if ID, ROLE or
NAME says which, into that thing, after pressing it to give it the keyboard."
  (when (or id role name)
    (focus session :id id :role role :name name))
  (loop :for character :across text
        :do (deliver session (make-key-event :timestamp (now-ns) :kind :down
                                             :logical-key character :modifiers 0)))
  (settle session)
  nil)

;;; --- acting on things ---

(defun click (session &key id role name)
  "Press and release the primary button in the middle of the thing."
  (let ((node (find-node session :id id :role role :name name)))
    (when (node-property node :disabled)
      (refuse 'cannot "The ~a is disabled." (describe-node node)))
    (multiple-value-bind (x y) (middle node)
      (press-at session x y))
    (settle session)
    nil))

(defun double-click (session &key id role name)
  (let ((node (find-node session :id id :role role :name name)))
    (multiple-value-bind (x y) (middle node)
      (press-at session x y :count 1)
      (press-at session x y :count 2))
    (settle session)
    nil))

(defun focus (session &key id role name)
  "Give the thing the keyboard, by pressing it."
  (click session :id id :role role :name name))

(defun scroll (session &key id role name (dx 0) (dy 0))
  "Turn the wheel over the thing: DX and DY in lines, a negative DY showing what is further down."
  (let ((node (find-node session :id id :role role :name name)))
    (multiple-value-bind (x y) (middle node)
      (deliver session (make-wheel-event :timestamp (now-ns) :x (float x 1d0) :y (float y 1d0)
                                         :dx (float dx 1d0) :dy (float dy 1d0)
                                         :precise-p nil :modifiers 0)))
    (settle session)
    nil))

(defun select-item (session index &key id role name)
  "Pick the item INDEX of a list, scrolling it into view first if it is not showing."
  (let ((target (find-node session :id id :role (or role :list) :name name)))
    (unless (eq (node-role target) :list)
      (refuse 'cannot "The ~a is not a list." (describe-node target)))
    (let ((count (length (node-property target :items))))
      (unless (and (integerp index) (< -1 index count))
        (refuse 'cannot "The list has ~d item~:p, from 0; there is no item ~s." count index)))
    (loop :repeat 200
          :do (let* ((now (find-node session :id (node-id target) :role :list
                                             :name (node-name target)))
                     (first (node-property now :first-visible))
                     (showing (length (node-property now :item-bounds)))
                     (last (+ first showing -1)))
                (cond ((< index first)
                       (scroll session :id (node-id target) :role :list
                                       :dy (min 5 (- first index))))
                      ((> index last)
                       (scroll session :id (node-id target) :role :list
                                       :dy (- (min 5 (- index last)))))
                      (t (multiple-value-bind (x y)
                             (bounds-point (nth (- index first) (node-property now :item-bounds)))
                           (press-at session x y)
                           (settle session)
                           (return-from select-item nil))))))
    (refuse 'cannot "Could not scroll to item ~d." index)))

(defun choose-option (session node value)
  (let* ((options (node-property node :options))
         (index (cond ((integerp value) value)
                      (t (position value options :test #'equal)))))
    (unless (and index (< -1 index (length options)))
      (refuse 'cannot "The ~a has the options ~{~s~^, ~}; there is none like ~s."
              (describe-node node) options value))
    (multiple-value-bind (x y) (bounds-point (nth index (node-property node :option-bounds)))
      (press-at session x y))))

(defun set-value (session value &key id role name)
  "Make the thing have VALUE, by what a person would do to it: type it into a text field (after
selecting what was there), press a check box if it is not as wanted, press a slider where the
value is, press the option of a choice, pick the item of a list."
  (let ((node (find-node session :id id :role role :name name)))
    (when (node-property node :disabled)
      (refuse 'cannot "The ~a is disabled." (describe-node node)))
    (ecase (node-role node)
      (:text-field
       (unless (stringp value)
         (refuse 'cannot "A text field takes a string, not ~s." value))
       (multiple-value-bind (x y) (middle node)
         (press-at session x y))
       (deliver session (make-key-event :timestamp (now-ns) :kind :down :logical-key #\a
                                        :modifiers +modifier-super+))
       (if (zerop (length value))
           (deliver session (make-key-event :timestamp (now-ns) :kind :down
                                            :logical-key #\Backspace :modifiers 0))
           (loop :for character :across value
                 :do (deliver session (make-key-event :timestamp (now-ns) :kind :down
                                                      :logical-key character :modifiers 0)))))
      (:checkbox
       (unless (eq (and value t) (and (node-value node) t))
         (multiple-value-bind (x y) (middle node)
           (press-at session x y))))
      (:slider
       (let ((minimum (node-property node :minimum))
             (maximum (node-property node :maximum)))
         (unless (and (realp value) (<= minimum value maximum))
           (refuse 'cannot "The ~a goes from ~a to ~a; ~s is outside that." (describe-node node)
                   minimum maximum value))
         (destructuring-bind (x y width height) (node-bounds node)
           ;; The thumb is 16 points wide and its middle goes from 8 points in to 8 points from the end.
           (let ((travel (max 1 (- width 16)))
                 (fraction (if (= maximum minimum) 0 (/ (- value minimum) (- maximum minimum)))))
             (press-at session (+ x 8 (* fraction travel)) (+ y (/ height 2)))))))
      (:radio-group (choose-option session node value))
      (:list (if (integerp value)
                 (select-item session value :id (node-id node) :role :list :name (node-name node))
                 (select-item session
                              (or (position value (node-property node :items) :test #'equal)
                                  (refuse 'cannot "The list has no item like ~s." value))
                              :id (node-id node) :role :list :name (node-name node))))
      ((:button :static-text :paragraph :progress-bar :canvas :overlay :group)
       (refuse 'cannot "The ~a has no value to set." (describe-node node))))
    (settle session)
    nil))

;;; --- waiting and looking ---

(defun wait (session)
  "Return when the app has answered everything it has been given."
  (settle session)
  nil)

(defun screenshot (session)
  "The screen as a PNG, in a vector of bytes."
  (or (and (session-screenshot session) (funcall (session-screenshot session)))
      (refuse 'cannot "This session cannot take a screenshot.")))
