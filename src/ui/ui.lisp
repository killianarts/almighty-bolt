(defpackage #:almighty-bolt/ui/ui
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/hash
                #:mix
                #:hash-of
                #:+seed+)
  (:import-from #:almighty-bolt/ui/box
                #:make-box
                #:reset-box
                #:box-key
                #:box-id
                #:box-kind
                #:box-labelled-by
                #:box-parent
                #:box-children)
  (:import-from #:almighty-bolt/internals
                #:pointer-event
                #:pointer-event-kind
                #:pointer-event-x
                #:pointer-event-y
                #:pointer-event-button
                #:pointer-event-click-count
                #:wheel-event
                #:wheel-event-x
                #:wheel-event-y
                #:make-axis-lock
                #:lock-wheel
                #:key-event
                #:key-event-kind
                #:key-event-logical-key
                #:key-event-modifiers
                #:text-input-event
                #:composition-event
                #:modifier-down-p
                #:+modifier-shift+
                #:make-display-list)
  (:export #:ui
           #:make-ui
           #:*ui*
           #:ui-context
           #:ui-root
           #:ui-parent
           #:ui-count
           #:ui-boxes
           #:ui-rect-pool
           #:ui-rect-count
           #:ui-hot
           #:ui-active
           #:ui-focus
           #:ui-started
           #:ui-pointer-x
           #:ui-pointer-y
           #:ui-down
           #:ui-pressed
           #:ui-click-count
           #:ui-released
           #:ui-secondary-pressed
           #:ui-moved
           #:ui-keys
           #:ui-inputs
           #:consume-input
           #:ui-wheel-dx
           #:ui-wheel-dy
           #:ui-queue
           #:ui-consumed
           #:ui-states
           #:ui-paint-cache
           #:ui-rects
           #:ui-id-rects
           #:ui-hits
           #:ui-hit-count
           #:ui-frame-layout-hash
           #:ui-frame-paint-hash
           #:ui-last-layout-hash
           #:ui-last-width
           #:ui-last-height
           #:ui-last-scale
           #:ui-last-paint-hash
           #:ui-last-list
           #:ui-saved-geometry
           #:ui-tab-pending
           #:rect
           #:make-rect
           #:rect-x
           #:rect-y
           #:rect-w
           #:rect-h
           #:hit
           #:make-hit
           #:hit-key
           #:hit-id
           #:hit-x
           #:hit-y
           #:hit-w
           #:hit-h
           #:hit-focusable
           #:hit-moves
           #:duplicate-id
           #:duplicate-id-id
           #:duplicate-id-kinds
           #:open-box
           #:close-box
           #:begin-build
           #:enqueue-event
           #:begin-frame-input
           #:end-frame-input
           #:compute-hot
           #:consume-press
           #:focus-by-click
           #:consume-release
           #:consume-secondary-press
           #:consume-key
           #:consume-wheel
           #:rect-of
           #:state-of
           #:needs-frame-p))

(in-package #:almighty-bolt/ui/ui)

;;; What the interface is made of while a frame is described, and what it knows of the pointer
;;; and the keyboard. The interface is a function that is called for every build of every
;;; frame; the functions it calls make boxes here (WIDGETS), and those that can be clicked or
;;; typed into look at the INPUT of the frame, take the parts of it that are for them (which
;;; removes those parts, so that a second build of the same frame does not act on them
;;; again), and say what they did by returning it. Where things were in the last frame that
;;; was presented is what decides which of them the pointer is over, as 0009 says.

(defvar *ui* nil
  "The UI whose frame is being described.")

(defstruct (rect (:constructor make-rect ()))
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (w 0f0 :type single-float)
  (h 0f0 :type single-float))

(defstruct (hit (:constructor make-hit ()))
  "A box of the last presented frame that takes the pointer, in the order they were drawn."
  (key 0 :type fixnum)
  (id nil)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (w 0f0 :type single-float)
  (h 0f0 :type single-float)
  (focusable nil)
  (moves nil))

(defstruct (ui (:constructor make-ui ()))
  (boxes (make-array 128 :initial-element nil) :type simple-vector)
  (count 0 :type fixnum)
  (parent nil)
  (root nil)
  (context nil)
  ;; The pointer and keys, for this frame.
  (pointer-x -1000f0 :type single-float)
  (pointer-y -1000f0 :type single-float)
  (down nil)
  (pressed nil)
  ;; How many clicks in a row the press is, as the platform counts them.
  (click-count 1 :type fixnum)
  (released nil)
  (secondary-pressed nil)
  (moved nil)
  (keys '() :type list)
  ;; Everything typed this frame, in the order it came: the key events (which are also in KEYS, for
  ;; the widgets that take keys) and the text and composition events of the input method. A canvas
  ;; takes them from here, so that a key and the text after it stay in order.
  (inputs '() :type list)
  (wheel-dx 0f0 :type single-float)
  (wheel-dy 0f0 :type single-float)
  ;; Which way the trackpad gesture in progress is going, so that a swipe goes one way.
  (wheel-lock (make-axis-lock))
  ;; Events that have arrived and not been looked at yet, oldest first.
  (queue '() :type list)
  ;; The widget the pointer is over, the one a press began on, the one with the keyboard.
  (hot 0 :type fixnum)
  (active 0 :type fixnum)
  (focus 0 :type fixnum)
  ;; True once a frame has been laid out and drawn: the window has opened, and a canvas that asks
  ;; for the keyboard (AUTOFOCUS) can no longer be given it for that.
  (started nil)
  (tab-pending nil)
  ;; True if the keyboard is where it is because the pointer pressed there (a field), so that
  ;; a press anywhere else takes it away; a button the keyboard went to by Tab keeps it.
  (click-focused nil)
  ;; True if this build took something from the frame's input, which may have changed what
  ;; the application holds, so that the frame is built again.
  (consumed nil)
  ;; Per widget, by key: what must outlive a frame (a field's caret), where it was, what was
  ;; painted for it.
  (states (make-hash-table))
  (rects (make-hash-table))
  ;; The keys of the boxes of this build, to see that no two are the same (CLAIM-KEY): an open
  ;; addressing table whose entries count only if their stamp is this build's, so that it is not
  ;; cleared between builds.
  (seen-keys (make-array 256 :element-type 'fixnum :initial-element 0)
   :type (simple-array fixnum (*)))
  (seen-stamps (make-array 256 :element-type 'fixnum :initial-element -1)
   :type (simple-array fixnum (*)))
  (build-stamp 0 :type fixnum)
  ;; The records that RECTS and ID-RECTS refer to: the first RECT-COUNT of the pool, kept and
  ;; reused from layout to layout.
  (rect-pool (make-array 32 :adjustable t :fill-pointer 0))
  (rect-count 0 :type fixnum)
  (id-rects (make-hash-table :test 'equal))
  (paint-cache (make-hash-table))
  ;; The boxes of the last presented frame that take the pointer, in the order drawn: the
  ;; first HIT-COUNT of the HITS, which are kept and reused from frame to frame.
  (hits (make-array 32 :adjustable t :fill-pointer 0))
  (hit-count 0 :type fixnum)
  ;; What the frame described, and what the last presented one did, to be compared.
  (frame-layout-hash 0 :type (unsigned-byte 62))
  (frame-paint-hash 0 :type (unsigned-byte 62))
  (last-layout-hash -1 :type fixnum)
  ;; The size and scale of the window of the last presented frame: the same description in
  ;; another window is not the same frame.
  (last-width -1f0 :type single-float)
  (last-height -1f0 :type single-float)
  (last-scale -1f0 :type single-float)
  (last-paint-hash -1 :type fixnum)
  (last-list (make-display-list :capacity 1024))
  ;; The places of the boxes of the last presented frame, in order: x, y, w, h each.
  (saved-geometry (make-array 0 :element-type 'single-float)
   :type (simple-array single-float (*))))

;;; --- boxes ---

(defun next-box (ui)
  (let ((boxes (ui-boxes ui))
        (count (ui-count ui)))
    (when (>= count (length boxes))
      (let ((bigger (make-array (* 2 (length boxes)) :initial-element nil)))
        (replace bigger boxes)
        (setf (ui-boxes ui) bigger
              boxes bigger)))
    (incf (ui-count ui))
    (or (svref boxes count)
        (setf (svref boxes count) (make-box)))))

(define-condition duplicate-id (error)
  ((id :initarg :id :reader duplicate-id-id)
   (kinds :initarg :kinds :reader duplicate-id-kinds)
   (container :initarg :container :reader duplicate-id-container)
   ;; True if one of the two has no id and is known by its place.
   (by-place :initarg :by-place :reader duplicate-id-by-place))
  (:report (lambda (condition stream)
             (destructuring-bind (first second) (duplicate-id-kinds condition)
               (cond ((null (duplicate-id-container condition))
                      (format stream "A screen is described by one thing, and this is a ~(~a~) and a ~(~a~) ~
                                      at the top. Put them in a container, such as (column () ...)."
                              first second))
                     ((not (duplicate-id-by-place condition))
                      (format stream "Two things inside ~a have the id ~s: a ~(~a~) and a ~(~a~). An id names ~
                                      one thing from frame to frame (its caret, its scroll and the keyboard ~
                                      stay with it), so each needs its own. In a loop over data, use something ~
                                      that belongs to each item, such as (task-id task)."
                              (duplicate-id-container condition) (duplicate-id-id condition) first second))
                     (t
                      (format stream "Two things inside ~a are the same to the ui: a ~(~a~) and a ~(~a~). ~
                                      A number used as an id (here ~s) can be the same as the place of a thing ~
                                      with no id among its container's children; give each a name that is not ~
                                      a number."
                              (duplicate-id-container condition) first second (duplicate-id-id condition)))))))
  (:documentation "Two boxes of one build that the ui could not tell apart: the same id inside the
same container (or an id that is a number and the place of a box that has none)."))

(declaim (inline claim-key))
(defun claim-key (ui key)
  "Record KEY as the key of a box of this build. True if an earlier box of this build has it."
  (declare (type fixnum key) (optimize speed (safety 0)))
  (let* ((stamp (ui-build-stamp ui))
         (keys (ui-seen-keys ui))
         (stamps (ui-seen-stamps ui))
         (mask (1- (length keys)))
         (index (logand key mask)))
    (declare (type fixnum stamp mask index))
    (loop
      (cond ((/= (aref stamps index) stamp)
             (setf (aref keys index) key
                   (aref stamps index) stamp)
             (return nil))
            ((= (aref keys index) key)
             (return t)))
      (setf index (logand (1+ index) mask)))))

(defun grow-seen-keys (ui)
  "Make the table of keys seen this build bigger, and put in it the keys of the boxes so far (all
but the last, which is being opened)."
  (let ((size 256)
        (count (ui-count ui)))
    (loop :while (< size (* 4 count)) :do (setf size (* 2 size)))
    (setf (ui-seen-keys ui) (make-array size :element-type 'fixnum :initial-element 0)
          (ui-seen-stamps ui) (make-array size :element-type 'fixnum :initial-element -1))
    (let ((boxes (ui-boxes ui)))
      (dotimes (index (1- count))
        (claim-key ui (box-key (svref boxes index)))))))

(defun refuse-duplicate (ui kind id parent key)
  "Signal that the box being opened, of KIND with ID inside PARENT, has the key of an earlier box."
  (let* ((boxes (ui-boxes ui))
         (earlier (loop :for index :below (1- (ui-count ui))
                        :for box = (svref boxes index)
                        :when (= (box-key box) key) :return box)))
    (error 'duplicate-id
           :id (or id (and earlier (box-id earlier)))
           :by-place (or (null id) (null earlier) (null (box-id earlier)))
           :kinds (list (if earlier (box-kind earlier) :box) kind)
           :container (and parent
                           (format nil "the ~(~a~)~@[ ~s~]" (box-kind parent) (box-id parent))))))

(defun open-box (ui kind &key id labelled-by)
  "Start a box of KIND inside the one that is open. Its key is made from its parent's and
its ID (or, with none, its place among its parent's children), so it is the same from one
frame to the next as long as it is described in the same place. Two boxes of one build with
the same key are an error (DUPLICATE-ID): the ui could not tell them apart."
  (let* ((box (reset-box (next-box ui) kind))
         (parent (ui-parent ui)))
    (setf (box-id box) id
          (box-labelled-by box) labelled-by
          (box-parent box) parent)
    (let ((key (mix (if parent (box-key parent) +seed+)
                    (if id (hash-of id) (if parent (fill-pointer (box-children parent)) 0)))))
      (setf (box-key box) (if (zerop key) 1 key)))
    (when (> (* 2 (ui-count ui)) (length (ui-seen-keys ui)))
      (grow-seen-keys ui))
    (when (claim-key ui (box-key box))
      (refuse-duplicate ui kind id parent (box-key box)))
    (if parent
        (vector-push-extend box (box-children parent))
        (setf (ui-root ui) box))
    (setf (ui-parent ui) box)
    box))

(defun close-box (ui)
  (setf (ui-parent ui) (box-parent (ui-parent ui))))

(defun begin-build (ui context)
  "Get UI ready for the interface to be described: no boxes yet."
  (setf (ui-context ui) context
        (ui-count ui) 0
        (ui-build-stamp ui) (1+ (ui-build-stamp ui))
        (ui-parent ui) nil
        (ui-root ui) nil
        (ui-consumed ui) nil
        (ui-frame-layout-hash ui) +seed+
        (ui-frame-paint-hash ui) +seed+))

;;; --- what arrives ---

(defun enqueue-event (ui event)
  "Note that EVENT arrived. Returns true if a frame is wanted for it: the pointer moving
over nothing in particular is not, and everything that a widget could use is."
  (typecase event
    (pointer-event
     (let ((x (float (pointer-event-x event) 1f0))
           (y (float (pointer-event-y event) 1f0)))
       (ecase (pointer-event-kind event)
         ((:move :enter)
          (let ((hot-before (compute-hot-at ui x y)))
            ;; The position is a level and not an event, so it is kept as it is, but a move
            ;; that changes what is hot, or is over a box that wants moves or while
            ;; something is held, is wanted.
            (enqueue ui event)
            (or (/= hot-before (ui-hot ui))
                (/= (ui-active ui) 0)
                (moves-at-p ui x y))))
         (:leave
          (enqueue ui event)
          (/= (ui-hot ui) 0))
         ((:down :up)
          (enqueue ui event)
          t))))
    (wheel-event
     (enqueue ui event)
     (/= (compute-hot-at ui (float (wheel-event-x event) 1f0)
                         (float (wheel-event-y event) 1f0))
         0))
    (key-event
     (when (eq (key-event-kind event) :down)
       (enqueue ui event)
       t))
    ((or text-input-event composition-event)
     (enqueue ui event)
     t)
    (t nil)))

(defun enqueue (ui event)
  (setf (ui-queue ui) (nconc (ui-queue ui) (list event))))

(defun inside-p (hit x y)
  (and (<= (hit-x hit) x) (< x (+ (hit-x hit) (hit-w hit)))
       (<= (hit-y hit) y) (< y (+ (hit-y hit) (hit-h hit)))))

(defun compute-hot-at (ui x y)
  "The key of the topmost box of the last presented frame that takes the pointer at X, Y, or
0. Does not change anything."
  (let ((hits (ui-hits ui)))
    (loop :for i :from (1- (ui-hit-count ui)) :downto 0
          :do (let ((hit (aref hits i)))
                (when (inside-p hit x y)
                  (return-from compute-hot-at (hit-key hit)))))
    0))

(defun moves-at-p (ui x y)
  (let ((hits (ui-hits ui)))
    (loop :for i :from (1- (ui-hit-count ui)) :downto 0
          :do (let ((hit (aref hits i)))
                (when (inside-p hit x y)
                  (return-from moves-at-p (hit-moves hit)))))
    nil))

(defun compute-hot (ui)
  (setf (ui-hot ui) (compute-hot-at ui (ui-pointer-x ui) (ui-pointer-y ui))))

(defun begin-frame-input (ui)
  "Turn the events that have arrived into the input of this frame: where the pointer is, an
edge for a button that went down or up, the keys, the wheel. At most one change of the
primary button is taken a frame, and a Tab only when no key was taken before it, so that
what happened first is seen first; the rest wait for the next frame. Returns true if some
were left."
  (let ((changed-button nil)
        (queue (ui-queue ui)))
    (setf (ui-pressed ui) nil
          (ui-released ui) nil
          (ui-secondary-pressed ui) nil
          (ui-moved ui) nil)
    (loop
      (when (null queue) (return))
      (let ((event (first queue)))
        (typecase event
          (pointer-event
           (let ((x (float (pointer-event-x event) 1f0))
                 (y (float (pointer-event-y event) 1f0)))
             (ecase (pointer-event-kind event)
               ((:move :enter)
                (setf (ui-pointer-x ui) x (ui-pointer-y ui) y (ui-moved ui) t))
               (:leave
                (setf (ui-pointer-x ui) -1000f0 (ui-pointer-y ui) -1000f0 (ui-moved ui) t))
               (:down
                (cond ((not (eql (pointer-event-button event) 0))
                       (setf (ui-pointer-x ui) x (ui-pointer-y ui) y
                             (ui-secondary-pressed ui) t))
                      (changed-button (return))
                      (t (setf (ui-pointer-x ui) x (ui-pointer-y ui) y
                               (ui-down ui) t (ui-pressed ui) t changed-button t
                               (ui-click-count ui) (max 1 (pointer-event-click-count event))))))
               (:up
                (cond ((not (eql (pointer-event-button event) 0)))
                      (changed-button (return))
                      (t (setf (ui-pointer-x ui) x (ui-pointer-y ui) y
                               (ui-down ui) nil (ui-released ui) t changed-button t)))))))
          (wheel-event
           ;; A wheel acts where the pointer is, which it says.
           (setf (ui-pointer-x ui) (float (wheel-event-x event) 1f0)
                 (ui-pointer-y ui) (float (wheel-event-y event) 1f0))
           (multiple-value-bind (dx dy) (lock-wheel (ui-wheel-lock ui) event)
             (incf (ui-wheel-dx ui) (float dx 1f0))
             (incf (ui-wheel-dy ui) (float dy 1f0))))
          (key-event
           (when (and (eql (key-event-logical-key event) #\Tab)
                      (ui-keys ui))
             (return))
           (if (eql (key-event-logical-key event) #\Tab)
               (setf (ui-tab-pending ui) (if (modifier-down-p (key-event-modifiers event)
                                                              +modifier-shift+)
                                             :backward
                                             :forward))
               (setf (ui-keys ui) (nconc (ui-keys ui) (list event))
                     (ui-inputs ui) (nconc (ui-inputs ui) (list event)))))
          ((or text-input-event composition-event)
           (setf (ui-inputs ui) (nconc (ui-inputs ui) (list event))))))
      (pop queue))
    (setf (ui-queue ui) queue)
    (compute-hot ui)
    ;; A press takes the keyboard from what a press gave it to; a widget that wants it gives
    ;; it back as it takes the press.
    (when (and (ui-pressed ui) (ui-click-focused ui))
      (setf (ui-focus ui) 0
            (ui-click-focused ui) nil))
    (and queue t)))

(defun end-frame-input (ui)
  "Drop what the frame's input held that no widget took."
  (setf (ui-pressed ui) nil
        (ui-released ui) nil
        (ui-secondary-pressed ui) nil
        (ui-moved ui) nil
        (ui-keys ui) '()
        (ui-inputs ui) '()
        (ui-wheel-dx ui) 0f0
        (ui-wheel-dy ui) 0f0))

(defun needs-frame-p (ui)
  (and (ui-queue ui) t))

;;; --- taking what is for a widget ---

(defun focus-by-click (ui key)
  "Give the keyboard to the box KEY, which the pointer pressed."
  (setf (ui-focus ui) key
        (ui-click-focused ui) t))

(defun consume-press (ui)
  (when (ui-pressed ui)
    (setf (ui-pressed ui) nil
          (ui-consumed ui) t)
    t))

(defun consume-release (ui)
  (when (ui-released ui)
    (setf (ui-released ui) nil
          (ui-consumed ui) t)
    t))

(defun consume-secondary-press (ui)
  (when (ui-secondary-pressed ui)
    (setf (ui-secondary-pressed ui) nil
          (ui-consumed ui) t)
    t))

(defun consume-key (ui event)
  "Take the key EVENT from the frame's keys."
  (setf (ui-keys ui) (delete event (ui-keys ui) :test #'eq :count 1)
        (ui-consumed ui) t)
  event)

(defun consume-input (ui event)
  "Take EVENT, a text or composition event, from what was typed this frame."
  (setf (ui-inputs ui) (delete event (ui-inputs ui) :test #'eq :count 1)
        (ui-consumed ui) t)
  event)

(defun consume-wheel (ui)
  "Take the wheel movement of the frame: its two parts, as values."
  (let ((dx (ui-wheel-dx ui))
        (dy (ui-wheel-dy ui)))
    (when (or (/= dx 0f0) (/= dy 0f0))
      (setf (ui-wheel-dx ui) 0f0
            (ui-wheel-dy ui) 0f0
            (ui-consumed ui) t))
    (values dx dy)))

;;; --- remembered things ---

(defun rect-of (ui key)
  "Where the box with KEY was in the last presented frame, or NIL."
  (gethash key (ui-rects ui)))

(defun state-of (ui key make)
  "The state kept for the box KEY, made with the function MAKE the first time."
  (or (gethash key (ui-states ui))
      (setf (gethash key (ui-states ui)) (funcall make))))
