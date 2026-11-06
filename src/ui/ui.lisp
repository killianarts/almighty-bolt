(defpackage #:almighty-bolt/ui/ui
  (:use #:cl)
  (:local-nicknames (#:hash #:almighty-bolt/ui/hash)
                    (#:box #:almighty-bolt/ui/box)
                    (#:core #:almighty-bolt/internals))
  (:export #:*drawing-view*
           #:ui-click-focused
           #:ui
           #:make-ui
           #:*ui*
           #:ui-context
           #:ui-root
           #:ui-floats
           #:open-float
           #:close-float
           #:float-spec
           #:make-float-spec
           #:float-spec-anchor
           #:float-spec-placement
           #:float-spec-x
           #:float-spec-y
           #:float-spec-min-width
           #:float-spec-modal
           #:float-spec-tool
           #:ui-inspection
           #:ui-inert
           #:ui-menus
           #:ui-drag
           #:drag
           #:make-drag
           #:drag-origin
           #:drag-payload
           #:drag-label
           #:drag-x
           #:drag-y
           #:drag-moving
           #:drag-dropping
           #:ui-menu-chosen
           #:inspection
           #:make-inspection
           #:inspection-picking
           #:inspection-picked
           #:inspection-theme
           #:inspection-hover
           #:inspection-shown
           #:ui-parent
           #:ui-count
           #:ui-boxes
           #:ui-rect-pool
           #:ui-rect-count
           #:ui-hot
           #:ui-hot-since
           #:ui-toasts
           #:ui-tooltip-dismissed
           #:ui-active
           #:ui-focus
           #:ui-started
           #:ui-made-again
           #:ui-pointer-x
           #:ui-pointer-y
           #:ui-down
           #:ui-pressed
           #:ui-click-count
           #:ui-press-modifiers
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
           #:hit-layer
           #:ui-modal-layer
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
           #:ui-cursor
           #:want-cursor
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

(defstruct (drag (:constructor make-drag (origin payload label x y)))
  "Something being dragged: what is carried (PAYLOAD), what the ghost under the pointer says
(LABEL), the key of the box it came from (ORIGIN), and where the press was. MOVING once the
pointer has gone far enough from there; DROPPING in the frame it is let go of, while it is moving."
  origin payload label (x 0f0) (y 0f0) (moving nil) (dropping nil))

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
  (moves nil)
  ;; The key of the float it is in, or 0 for the screen.
  (layer 0 :type fixnum))

(defstruct (ui (:constructor make-ui ()))
  (boxes (make-array 128 :initial-element nil) :type simple-vector)
  (count 0 :type fixnum)
  (parent nil)
  (root nil)
  ;; The boxes of this build that float above the root (a menu, a tooltip, a dialog), in the order
  ;; they were opened, which is the order they are laid out, drawn and found by the pointer in.
  (floats (make-array 4 :adjustable t :fill-pointer 0))
  (context nil)
  ;; The pointer and keys, for this frame.
  (pointer-x -1000f0 :type single-float)
  (pointer-y -1000f0 :type single-float)
  (down nil)
  (pressed nil)
  ;; How many clicks in a row the press is, as the platform counts them.
  (click-count 1 :type fixnum)
  ;; The modifier keys down at the press (the event's bits).
  (press-modifiers 0 :type fixnum)
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
  (wheel-lock (core:make-axis-lock))
  ;; Events that have arrived and not been looked at yet, oldest first.
  (queue '() :type list)
  ;; The widget the pointer is over, the one a press began on, the one with the keyboard.
  (hot 0 :type fixnum)
  ;; When the pointer came onto what it is over (the time of the frame), and whether a press since
  ;; has put the tooltip of it away: a tooltip shows when the pointer has rested on its widget.
  (hot-since 0 :type (unsigned-byte 62))
  ;; The toasts showing, oldest first: (words . until), UNTIL a time in nanoseconds.
  (toasts '() :type list)
  (tooltip-dismissed nil)
  ;; What the inspector, in a window of its own, has asked of this window (see inspection.lisp), or
  ;; NIL while no inspector looks at it.
  (inspection nil)
  ;; The app's own menus this build described (APP-MENU), each title then its items, kept and
  ;; reused from build to build; and the menu and item chosen this frame, or NIL.
  (menus (make-array 4 :adjustable t :fill-pointer 0))
  (menu-chosen nil)
  ;; What is being dragged (a DRAG), or NIL.
  (drag nil)
  ;; How many design surfaces the build is inside (see design-surface.lisp): what is described there
  ;; is drawn and takes no input, and a float opened there is inert too.
  (inert 0 :type fixnum)
  (active 0 :type fixnum)
  (focus 0 :type fixnum)
  ;; True once a frame has been laid out and drawn: the window has opened, and a canvas that asks
  ;; for the keyboard (AUTOFOCUS) can no longer be given it for that.
  (started nil)
  ;; How many times the app has been made again with this ui (INHERIT-STATE), so that an app in a
  ;; pane of it is made again too.
  (made-again 0 :type fixnum)
  (tab-pending nil)
  ;; True if the keyboard is where it is because the pointer pressed there (a field), so that
  ;; a press anywhere else takes it away; a button the keyboard went to by Tab keeps it.
  (click-focused nil)
  ;; The shape the pointer has, from the widget it is over (or that a press began on) in this build.
  (cursor :arrow)
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
  ;; The key of the last modal float of the last presented frame (a dialog), or 0: the keyboard stays
  ;; in it.
  (modal-layer 0 :type fixnum)
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
  (last-list (core:make-display-list :capacity 1024))
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
        (setf (svref boxes count) (box:make-box)))))

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
        (claim-key ui (box:box-key (svref boxes index)))))))

(defun refuse-duplicate (ui kind id parent key)
  "Signal that the box being opened, of KIND with ID inside PARENT, has the key of an earlier box."
  (let* ((boxes (ui-boxes ui))
         (earlier (loop :for index :below (1- (ui-count ui))
                        :for box = (svref boxes index)
                        :when (= (box:box-key box) key) :return box)))
    (error 'duplicate-id
           :id (or id (and earlier (box:box-id earlier)))
           :by-place (or (null id) (null earlier) (null (box:box-id earlier)))
           :kinds (list (if earlier (box:box-kind earlier) :box) kind)
           :container (and parent
                           (format nil "the ~(~a~)~@[ ~s~]" (box:box-kind parent) (box:box-id parent))))))

(defvar *drawing-view* nil
  "The name of the view being drawn (DEFINE-VIEW), which each box opened notes, or NIL.")

(defun open-box (ui kind &key id labelled-by tooltip rooted-at)
  "Start a box of KIND inside the one that is open. Its key is made from its parent's and
its ID (or, with none, its place among its parent's children), so it is the same from one
frame to the next as long as it is described in the same place. With ROOTED-AT, a key, it is made
from that key in place of its parent's: the box, and what is in it, keep their keys (and so their
state) wherever in that key's box they are described, as a pane of a dock moved to another group. Two boxes of one build with
the same key are an error (DUPLICATE-ID): the ui could not tell them apart."
  (let* ((box (box:reset-box (next-box ui) kind))
         (parent (ui-parent ui)))
    (setf (box:box-id box) id
          (box:box-labelled-by box) labelled-by
          (box:box-tooltip box) tooltip
          (box:box-view box) *drawing-view*
          (box:box-parent box) parent)
    (let ((key (hash:mix (cond (rooted-at (hash:mix rooted-at (hash:hash-of :rooted)))
                               (parent (box:box-key parent))
                               (t hash:+seed+))
                         (if id (hash:hash-of id) (if parent (fill-pointer (box:box-children parent)) 0)))))
      (setf (box:box-key box) (if (zerop key) 1 key)))
    (when (> (* 2 (ui-count ui)) (length (ui-seen-keys ui)))
      (grow-seen-keys ui))
    (when (claim-key ui (box:box-key box))
      (refuse-duplicate ui kind id parent (box:box-key box)))
    (cond (parent (vector-push-extend box (box:box-children parent)))
          ;; A second thing at the top would take the first one's place, and the first would not
          ;; be seen, whatever their ids.
          ((ui-root ui)
           (error 'duplicate-id :id id :kinds (list (box:box-kind (ui-root ui)) kind) :container nil
                                :by-place nil))
          (t (setf (ui-root ui) box)))
    (setf (ui-parent ui) box)
    box))

(defun close-box (ui)
  (setf (ui-parent ui) (box:box-parent (ui-parent ui))))

;;; --- boxes that float above the screen ---
;;;
;;; A menu, a tooltip or a dialog is described by the widget that has it, wherever that widget is,
;;; and shows above everything else. Its box is the root of a tree of its own (a float): it has no
;;; parent and is in no container, it is laid out after the screen against the window and put by
;;; the thing it belongs to (its anchor), and it is drawn and found by the pointer after the screen,
;;; and so on top. Its key comes from its owner's (the widget's) key, so that a dropdown in every row
;;; of a list has a menu of its own.

(defstruct (float-spec (:constructor make-float-spec
                           (&key anchor (placement :below) (x 0f0) (y 0f0) (min-width 0f0) modal
                                 tool)))
  "Where a float goes. PLACEMENT is :below (or above, if there is no room below) or :right of the box
ANCHOR, :over it (where it is, and its size), :at the point X, Y of the window, :center of the window,
or :fill the window. MIN-WIDTH is the
least it is as wide. A MODAL float is one the keyboard stays in. A TOOL float is the library's (the
inspector's outlines), not the app's: it is not in the semantic tree."
  (anchor nil)
  (placement :below :type keyword)
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (min-width 0f0 :type single-float)
  (modal nil)
  (tool nil))

(defun open-float (ui kind owner id spec)
  "Start a float of KIND belonging to the box with the key OWNER, called ID among that box's floats
and placed as SPEC says. What is described until CLOSE-FLOAT is inside it. Returns the box and the
box that was open, which CLOSE-FLOAT is given back."
  (let ((box (box:reset-box (next-box ui) kind))
        (open (ui-parent ui)))
    (setf (box:box-id box) id
          (box:box-data box) spec
          (box:box-inert box) (plusp (ui-inert ui))
          (box:box-key box) (let ((key (hash:mix (hash:mix owner (hash:hash-of :float)) (hash:hash-of id))))
                              (if (zerop key) 1 key)))
    (when (> (* 2 (ui-count ui)) (length (ui-seen-keys ui)))
      (grow-seen-keys ui))
    (when (claim-key ui (box:box-key box))
      (refuse-duplicate ui kind id nil (box:box-key box)))
    (vector-push-extend box (ui-floats ui))
    (setf (ui-parent ui) box)
    (values box open)))

(defun close-float (ui open)
  "End the float that OPEN-FLOAT started; OPEN is the box that was open then."
  (setf (ui-parent ui) open))

(defun begin-build (ui context)
  "Get UI ready for the interface to be described: no boxes yet. Nothing takes text input until
something described says so (an editable text with the keyboard, a canvas that paints a caret)."
  (when context
    (setf (core:context-text-input-p context) nil))
  (setf (ui-context ui) context
        (ui-count ui) 0
        (ui-build-stamp ui) (1+ (ui-build-stamp ui))
        (ui-parent ui) nil
        (ui-root ui) nil
        (fill-pointer (ui-floats ui)) 0
        (ui-consumed ui) nil
        ;; A surface whose body failed has put its input back (it unwinds), and is closed.
        (ui-inert ui) 0
        (fill-pointer (ui-menus ui)) 0
        (ui-cursor ui) :arrow
        (ui-frame-layout-hash ui) hash:+seed+
        (ui-frame-paint-hash ui) hash:+seed+))

;;; --- what the inspector asks of the window ---
;;;
;;; The inspector, in a window of its own, asks the app's window to pick a widget and to outline it
;;; (see inspection.lisp). It is here because the pointer's events must know that it picks: the
;;; outline follows the pointer from one thing to the next, which a window draws only when asked.

(defstruct (inspection (:constructor make-inspection (theme)))
  ;; True while the next press on the app picks.
  (picking nil)
  ;; The key of the box picked, or 0.
  (picked 0 :type fixnum)
  ;; The theme the outlines are drawn in (the inspector's).
  (theme nil)
  ;; Where the box under the pointer is while picking, and where the picked one is, in the window,
  ;; as lists of x, y, width and height, or NIL.
  (hover nil)
  (shown nil))

;;; --- what arrives ---

(defun picking-p (ui)
  "True while the inspector picks in UI's window: then the outline under the pointer moves with it,
and every move of the pointer wants a frame."
  (let ((inspection (ui-inspection ui)))
    (and inspection (inspection-picking inspection))))

(defun enqueue-event (ui event)
  "Note that EVENT arrived. Returns true if a frame is wanted for it: the pointer moving
over nothing in particular is not, and everything that a widget could use is."
  (typecase event
    (core:pointer-event
     (let ((x (float (core:pointer-event-x event) 1f0))
           (y (float (core:pointer-event-y event) 1f0)))
       (ecase (core:pointer-event-kind event)
         ((:move :enter)
          (let ((hot-before (compute-hot-at ui x y)))
            ;; The position is a level and not an event, so it is kept as it is, but a move
            ;; that changes what is hot, or is over a box that wants moves or while
            ;; something is held, is wanted.
            (enqueue ui event)
            (or (/= hot-before (ui-hot ui))
                (/= (ui-active ui) 0)
                (moves-at-p ui x y)
                (picking-p ui))))
         (:leave
          (enqueue ui event)
          (or (/= (ui-hot ui) 0)
              (picking-p ui)))
         ((:down :up)
          (enqueue ui event)
          t))))
    (core:wheel-event
     (enqueue ui event)
     (/= (compute-hot-at ui (float (core:wheel-event-x event) 1f0)
                         (float (core:wheel-event-y event) 1f0))
         0))
    (core:key-event
     (when (eq (core:key-event-kind event) :down)
       (enqueue ui event)
       t))
    ((or core:text-input-event core:composition-event core:menu-event)
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
          (core:pointer-event
           (let ((x (float (core:pointer-event-x event) 1f0))
                 (y (float (core:pointer-event-y event) 1f0)))
             (ecase (core:pointer-event-kind event)
               ((:move :enter)
                (setf (ui-pointer-x ui) x (ui-pointer-y ui) y (ui-moved ui) t))
               (:leave
                (setf (ui-pointer-x ui) -1000f0 (ui-pointer-y ui) -1000f0 (ui-moved ui) t))
               (:down
                ;; A press of the primary button with Control down is a secondary press, as a Mac
                ;; takes it. (Its release, with no press of the primary before it, does nothing.)
                (cond ((or (not (eql (core:pointer-event-button event) 0))
                           (core:modifier-down-p (core:pointer-event-modifiers event) core:+modifier-control+))
                       (setf (ui-pointer-x ui) x (ui-pointer-y ui) y
                             (ui-secondary-pressed ui) t))
                      (changed-button (return))
                      (t (setf (ui-pointer-x ui) x (ui-pointer-y ui) y
                               (ui-down ui) t (ui-pressed ui) t changed-button t
                               (ui-click-count ui) (max 1 (core:pointer-event-click-count event))
                               (ui-press-modifiers ui) (core:pointer-event-modifiers event)))))
               (:up
                (cond ((not (eql (core:pointer-event-button event) 0)))
                      (changed-button (return))
                      (t (setf (ui-pointer-x ui) x (ui-pointer-y ui) y
                               (ui-down ui) nil (ui-released ui) t changed-button t)))))))
          (core:wheel-event
           ;; A wheel acts where the pointer is, which it says.
           (setf (ui-pointer-x ui) (float (core:wheel-event-x event) 1f0)
                 (ui-pointer-y ui) (float (core:wheel-event-y event) 1f0))
           (multiple-value-bind (dx dy) (core:lock-wheel (ui-wheel-lock ui) event)
             (incf (ui-wheel-dx ui) (float dx 1f0))
             (incf (ui-wheel-dy ui) (float dy 1f0))))
          (core:key-event
           (when (and (eql (core:key-event-logical-key event) #\Tab)
                      (ui-keys ui))
             (return))
           (if (eql (core:key-event-logical-key event) #\Tab)
               (setf (ui-tab-pending ui) (if (core:modifier-down-p (core:key-event-modifiers event)
                                                                   core:+modifier-shift+)
                                             :backward
                                             :forward))
               (setf (ui-keys ui) (nconc (ui-keys ui) (list event))
                     (ui-inputs ui) (nconc (ui-inputs ui) (list event)))))
          ((or core:text-input-event core:composition-event)
           (setf (ui-inputs ui) (nconc (ui-inputs ui) (list event))))
          (core:menu-event
           (setf (ui-menu-chosen ui) (cons (core:menu-event-menu event) (core:menu-event-item event))))))
      (pop queue))
    (setf (ui-queue ui) queue)
    (let ((before (ui-hot ui)))
      (compute-hot ui)
      (unless (= before (ui-hot ui))
        (setf (ui-hot-since ui) (core:now-ns)
              (ui-tooltip-dismissed ui) nil)))
    (when (or (ui-pressed ui) (ui-secondary-pressed ui) (ui-keys ui))
      (setf (ui-tooltip-dismissed ui) t))
    ;; A press takes the keyboard from what a press gave it to; a widget that wants it gives
    ;; it back as it takes the press.
    ;; A drag that is moving is let go of by the release, which is for the drop and not for a widget:
    ;; whatever is under the pointer, described before or after the source, takes it this frame.
    (let ((drag (ui-drag ui)))
      (when (and drag (drag-moving drag) (ui-released ui))
        (setf (drag-dropping drag) t
              (ui-released ui) nil
              (ui-active ui) 0)))
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
        (ui-menu-chosen ui) nil
        ;; A drop that nothing took is over.
        (ui-drag ui) (let ((drag (ui-drag ui))) (and drag (not (drag-dropping drag)) drag))
        (ui-wheel-dx ui) 0f0
        (ui-wheel-dy ui) 0f0))

(defun needs-frame-p (ui)
  (and (ui-queue ui) t))

;;; --- taking what is for a widget ---

(defun want-cursor (ui key shape)
  "Give the pointer SHAPE (one of +CURSORS+) while it is over the box KEY, or while a press that began
on it is held."
  (when (or (= (ui-hot ui) key) (= (ui-active ui) key))
    (setf (ui-cursor ui) shape)))

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
