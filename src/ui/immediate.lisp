(defpackage #:almighty-bolt/ui/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:context-appearance
                #:variant
                #:frame
                #:handle-event
                #:hit-target
                #:lookup-target
                #:inherit-state
                #:forget-painting
                #:request-frame
                #:context-width
                #:context-scale
                #:context-height
                #:context-now-ns
                #:with-phase
                #:+phase-update+
                #:+phase-layout+
                #:+phase-paint+
                #:push-rect
                #:make-display-list
                #:display-list-capacity
                #:display-list-count
                #:replace-display-list
                #:+max-builds+)
  (:import-from #:almighty-bolt/ui/theme
                #:*theme*
                #:find-theme
                #:theme-background)
  (:import-from #:almighty-bolt/ui/hash
                #:mix)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-theme
                #:box-id
                #:box-kind
                #:box-tooltip
                #:box-parent
                #:box-data
                #:box-children
                #:box-interactive
                #:box-focusable
                #:box-moves
                #:box-x
                #:box-y
                #:box-w
                #:box-h
                #:box-layout-hash
                #:box-paint-hash)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:make-ui
                #:ui-context
                #:ui-root
                #:open-box
                #:close-box
                #:ui-floats
                #:float-spec-anchor
                #:float-spec-placement
                #:float-spec-x
                #:float-spec-y
                #:float-spec-min-width
                #:float-spec-modal
                #:ui-count
                #:ui-boxes
                #:ui-hot
                #:ui-focus
                #:ui-started
                #:ui-consumed
                #:ui-rects
                #:ui-id-rects
                #:ui-rect-pool
                #:ui-rect-count
                #:ui-hits
                #:ui-hit-count
                #:ui-tab-pending
                #:ui-click-focused
                #:ui-queue
                #:ui-frame-layout-hash
                #:ui-frame-paint-hash
                #:ui-last-layout-hash
                #:ui-last-width
                #:ui-last-height
                #:ui-last-scale
                #:ui-last-paint-hash
                #:ui-last-list
                #:ui-saved-geometry
                #:ui-paint-cache
                #:make-rect
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h
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
                #:begin-build
                #:enqueue-event
                #:begin-frame-input
                #:end-frame-input)
  (:import-from #:almighty-bolt/ui/widgets
                #:own-hashes
                #:describe-tooltip
                #:describe-toasts)
  (:import-from #:almighty-bolt/ui/layout
                #:layout-box)
  (:import-from #:almighty-bolt/ui/paint
                #:paint-box)
  (:export #:immediate-variant
           #:make-immediate-variant
           #:immediate-variant-ui
           #:now
           #:current-context
           #:ask-for-frame))

(in-package #:almighty-bolt/ui/immediate)

;;; The immediate variant. Every build of every frame calls the application's function, which
;;; describes the whole interface again with the widgets, taking from the frame's input what
;;; is for them. Then the boxes it made are given hashes, and if the hash of what decides the
;;; layout is what it was in the last frame presented, the places of the boxes are taken from
;;; that frame; if the hash of what decides the look is too, the commands are copied from it;
;;; and otherwise each box draws itself unless it is the same as it was (paint.lisp). A build
;;; that took anything from the input is followed by another, because it may have changed
;;; what the application holds, and only the last build is laid out and drawn.

(defclass immediate-variant (variant)
  ((ui :initform (make-ui) :accessor immediate-variant-ui)
   (app :initarg :app :reader app)
   ;; True if events were left in the queue by the last frame.
   (more-input :initform nil :accessor more-input))
  (:documentation "A variant whose interface is a function that is called every frame."))

(defun make-immediate-variant (app)
  "A variant that shows what APP, a function of no arguments that calls the widgets, describes."
  (make-instance 'immediate-variant :app app))

(defun now ()
  "The time in nanoseconds, for an interface that changes as time passes."
  (context-now-ns (ui-context *ui*)))

(defun current-context ()
  "The context of the frame being built, which measures text: for the code that handles what a
canvas returned, to turn a point into a place in text."
  (ui-context *ui*))

(defun ask-for-frame ()
  "Ask for another frame, which an interface that is animating does from its function."
  (request-frame (ui-context *ui*)))

;;; --- hashes ---

(defun compute-hashes (ui)
  "Give every box of the frame a hash of what decides its size and of what decides its look,
each folded with those of its children, which come later in the order the boxes are in."
  (let ((boxes (ui-boxes ui)))
    (loop :for i :from (1- (ui-count ui)) :downto 0
          :do (let ((box (aref boxes i)))
                (multiple-value-bind (layout paint) (own-hashes box)
                  (loop :for child :across (box-children box)
                        :do (setf layout (mix layout (box-layout-hash child))
                                  paint (mix paint (box-paint-hash child))))
                  (setf (box-layout-hash box) layout
                        (box-paint-hash box) paint))))
    (let ((root (ui-root ui)))
      (when root
        (let ((layout (box-layout-hash root))
              (paint (box-paint-hash root)))
          ;; What floats above the screen is part of the frame too.
          (loop :for float :across (ui-floats ui)
                :do (setf layout (mix layout (box-layout-hash float))
                          paint (mix paint (box-paint-hash float))))
          (setf (ui-frame-layout-hash ui) layout
                (ui-frame-paint-hash ui) paint))))))

;;; --- remembering where things are ---

(defun next-rect (ui)
  "A record for a place, from the pool."
  (let ((pool (ui-rect-pool ui))
        (index (ui-rect-count ui)))
    (when (>= index (length pool))
      (vector-push-extend (make-rect) pool))
    (incf (ui-rect-count ui))
    (aref pool index)))

(defun remember-geometry (ui)
  "Note where each box that has an id or takes the pointer is in the window, and which of
them take it, in the order they are drawn, for the next frame; and save every box's place. A box
inside a scroll container shows only where the container's window is: the pointer finds it only
there, and a box wholly out of the window has no place by its id (LOOKUP-TARGET); its place by its
key (RECT-OF, which a widget measures from) is the whole of it, wherever it is."
  (clrhash (ui-rects ui))
  (clrhash (ui-id-rects ui))
  (setf (ui-hit-count ui) 0
        (ui-rect-count ui) 0)
  (let ((boxes (ui-boxes ui))
        (count (ui-count ui)))
    (setf (ui-modal-layer ui) 0)
    (labels ((walk (box x y left top right bottom layer)
               (declare (type single-float x y left top right bottom)
                        (type fixnum layer))
               (let* ((x (+ x (box-x box)))
                      (y (+ y (box-y box)))
                      ;; The part of the box that shows.
                      (shown-left (max x left))
                      (shown-top (max y top))
                      (shown-w (max 0f0 (- (min (+ x (box-w box)) right) shown-left)))
                      (shown-h (max 0f0 (- (min (+ y (box-h box)) bottom) shown-top))))
                 (when (or (box-id box) (box-interactive box) (box-tooltip box))
                   (let ((rect (next-rect ui)))
                     (setf (rect-x rect) x (rect-y rect) y
                           (rect-w rect) (box-w box) (rect-h rect) (box-h box))
                     (setf (gethash (box-key box) (ui-rects ui)) rect)
                     ;; Out of the window is a box with an area of which none shows; one with no
                     ;; area (an empty label) is where it is.
                     (when (and (box-id box)
                                (or (zerop (box-w box)) (zerop (box-h box))
                                    (and (plusp shown-w) (plusp shown-h))))
                       (setf (gethash (box-id box) (ui-id-rects ui)) rect))))
                 ;; A box with a tooltip takes the pointer too, so that the pointer can rest on it.
                 (when (or (box-interactive box) (box-tooltip box))
                   (let ((index (ui-hit-count ui))
                         (hits (ui-hits ui)))
                     (when (>= index (length hits))
                       (vector-push-extend (make-hit) hits))
                     (let ((hit (aref hits index)))
                       (setf (hit-key hit) (box-key box) (hit-id hit) (box-id box)
                             (hit-x hit) shown-left (hit-y hit) shown-top (hit-w hit) shown-w
                             (hit-h hit) shown-h (hit-focusable hit) (box-focusable box)
                             (hit-moves hit) (box-moves box)
                             (hit-layer hit) layer))
                     (incf (ui-hit-count ui))))
                 (if (member (box-kind box) '(:scroll :lazy-list))
                     (loop :for child :across (box-children box)
                           :do (walk child x y shown-left shown-top (+ shown-left shown-w)
                                     (+ shown-top shown-h) layer))
                     (loop :for child :across (box-children box)
                           :do (walk child x y left top right bottom layer))))))
      (when (ui-root ui)
        (walk (ui-root ui) 0f0 0f0 -1f30 -1f30 1f30 1f30 0))
      ;; The floats after the screen, so that the pointer finds them first; each is at its place in
      ;; the window (its X and Y), with nothing over it.
      (loop :for float :across (ui-floats ui)
            :do (walk float 0f0 0f0 -1f30 -1f30 1f30 1f30 (box-key float))
                (when (float-spec-modal (box-data float))
                  (setf (ui-modal-layer ui) (box-key float)))))
    (let ((saved (ui-saved-geometry ui)))
      (unless (= (length saved) (* 4 count))
        (setf saved (make-array (* 4 count) :element-type 'single-float)
              (ui-saved-geometry ui) saved))
      (dotimes (i count)
        (let ((box (aref boxes i)))
          (setf (aref saved (* 4 i)) (box-x box)
                (aref saved (+ (* 4 i) 1)) (box-y box)
                (aref saved (+ (* 4 i) 2)) (box-w box)
                (aref saved (+ (* 4 i) 3)) (box-h box)))))))

(defun restore-geometry (ui)
  "Put back the places the last presented frame had, for a frame that is laid out the same."
  (let ((saved (ui-saved-geometry ui))
        (boxes (ui-boxes ui)))
    (dotimes (i (ui-count ui))
      (let ((box (aref boxes i)))
        (setf (box-x box) (aref saved (* 4 i))
              (box-y box) (aref saved (+ (* 4 i) 1))
              (box-w box) (aref saved (+ (* 4 i) 2))
              (box-h box) (aref saved (+ (* 4 i) 3)))))))

;;; --- the keyboard focus by Tab ---

(defun move-focus (ui)
  "Move the keyboard to the next (or, with Shift, the previous) box that can have it in the last
frame presented, round. While a modal float (a dialog) shows, only what is in it can have the
keyboard: it goes there when the dialog comes, and Tab goes round inside it."
  (let ((direction (ui-tab-pending ui))
        (modal (ui-modal-layer ui))
        (keys '()))
    ;; Only when there is something to do: a frame with no Tab and no dialog allocates nothing here.
    (when (or direction (/= modal 0))
      (loop :for i :below (ui-hit-count ui)
            :do (let ((hit (aref (ui-hits ui) i)))
                  (when (and (hit-focusable hit) (or (zerop modal) (= (hit-layer hit) modal)))
                    (push (hit-key hit) keys))))
      (setf keys (nreverse keys)))
    ;; Text that can be selected has the keyboard from a press only (it is not in the order); in the
    ;; dialog, it keeps it.
    (when (and (/= modal 0) keys (not (member (ui-focus ui) keys))
               (not (and (ui-click-focused ui)
                         (loop :for i :below (ui-hit-count ui)
                               :thereis (let ((hit (aref (ui-hits ui) i)))
                                          (and (= (hit-key hit) (ui-focus ui))
                                               (= (hit-layer hit) modal)))))))
      (setf (ui-focus ui) (first keys)
            (ui-click-focused ui) nil))
    (when direction
      (setf (ui-tab-pending ui) nil
            (ui-click-focused ui) nil)
      (when keys
        (let ((position (position (ui-focus ui) keys)))
          (setf (ui-focus ui)
                (if (eq direction :forward)
                    (if position (nth (mod (1+ position) (length keys)) keys) (first keys))
                    (if position (nth (mod (1- position) (length keys)) keys) (car (last keys))))))))))

;;; --- what floats above the screen ---

(defun window-position (box)
  "Where BOX is in the window, as laid out this frame: its place in its parent, and its parent's, up
to a root (the screen, or a float, whose place is in the window)."
  (let ((x 0f0) (y 0f0))
    (loop :for b = box :then (box-parent b)
          :while b
          :do (incf x (box-x b))
              (incf y (box-y b)))
    (values x y)))

(defun lay-out-floats (ui context width height)
  "Lay out each float of the frame against the window, in the order they were opened, and put it
where its spec says: against its anchor where that is in this frame (so a submenu can be put by a
menu), turned to the other side or moved in where it would go out of the window."
  (loop :for float :across (ui-floats ui)
        :do (let ((spec (box-data float)))
              (multiple-value-bind (w h)
                  (if (eq (float-spec-placement spec) :fill)
                      (layout-box float context width width height height)
                      (layout-box float context (min width (float-spec-min-width spec)) width
                                  0f0 height))
                (multiple-value-bind (x y)
                    (ecase (float-spec-placement spec)
                      (:fill (values 0f0 0f0))
                      ;; In the middle, a little above the bottom.
                      (:bottom (values (/ (- width w) 2) (- height h 24f0)))
                      (:center (values (/ (- width w) 2) (/ (- height h) 2)))
                      (:at (values (float-spec-x spec) (float-spec-y spec)))
                      ((:below :right)
                       (let ((anchor (float-spec-anchor spec)))
                         (multiple-value-bind (ax ay) (window-position anchor)
                           (if (eq (float-spec-placement spec) :below)
                               (let ((below (+ ay (box-h anchor) 2f0)))
                                 (values ax (if (and (> (+ below h) height) (>= (- ay h 2f0) 0f0))
                                                (- ay h 2f0)
                                                below)))
                               (let ((right (+ ax (box-w anchor))))
                                 (values (if (and (> (+ right w) width) (>= (- ax w) 0f0))
                                             (- ax w)
                                             right)
                                         ay)))))))
                  ;; Inside the window, as far as it fits.
                  (setf (box-x float) (max 0f0 (min x (- width w)))
                        (box-y float) (max 0f0 (min y (- height h)))))))))

;;; --- a frame ---

(defun lay-out-and-paint (variant context list)
  (let* ((ui (immediate-variant-ui variant))
         (root (ui-root ui))
         (width (context-width context))
         (height (context-height context))
         (scale (context-scale context)))
    (cond
      ((null root)
       (push-rect list 0 0 width height (theme-background (window-theme context))))
      (t
       (let ((same-layout (and (= (ui-last-width ui) width)
                               (= (ui-last-height ui) height)
                               (= (ui-last-scale ui) scale)
                               (= (ui-last-layout-hash ui) (ui-frame-layout-hash ui))
                               (= (length (ui-saved-geometry ui)) (* 4 (ui-count ui))))))
         (with-phase (context +phase-layout+)
           (if same-layout
               (restore-geometry ui)
               (progn (layout-box root context 0f0 width 0f0 height)
                      (setf (box-x root) 0f0 (box-y root) 0f0)
                      (lay-out-floats ui context width height)
                      (remember-geometry ui))))
         (with-phase (context +phase-paint+)
           (cond
             ((and same-layout (= (ui-last-paint-hash ui) (ui-frame-paint-hash ui)))
              (replace-display-list list (ui-last-list ui)))
             (t
              (push-rect list 0 0 width height (theme-background (box-theme root)))
              (paint-box ui root context list 0f0 0f0)
              (loop :for float :across (ui-floats ui)
                    :do (paint-box ui float context list 0f0 0f0))
              (when (< (display-list-capacity (ui-last-list ui)) (display-list-capacity list))
                (setf (ui-last-list ui) (make-display-list :capacity (display-list-capacity list))))
              (replace-display-list (ui-last-list ui) list))))
         (setf (ui-last-layout-hash ui) (ui-frame-layout-hash ui)
               (ui-last-paint-hash ui) (ui-frame-paint-hash ui)
               (ui-last-width ui) width
               (ui-last-height ui) height
               (ui-last-scale ui) scale))))))

(defun window-theme (context)
  "The theme of a window whose app gives none: dark or light, as the system is."
  (find-theme (context-appearance context)))

(defmethod frame ((variant immediate-variant) context list)
  (let ((ui (immediate-variant-ui variant)))
    (when (<= (almighty-bolt/internals:context-build-number context) 1)
      (setf (more-input variant) (begin-frame-input ui))
      (move-focus ui))
    (with-phase (context +phase-update+)
      (begin-build ui context)
      (let ((*ui* ui)
            (*theme* (window-theme context)))
        (funcall (app variant))
        ;; An app that describes only what floats (a dialog, and nothing under it) has an empty
        ;; screen under it, so that the floats are laid out, painted and in the tree.
        (unless (ui-root ui)
          (open-box ui :column)
          (close-box ui))
        (describe-toasts ui)
        (describe-tooltip ui))
      (compute-hashes ui))
    (let ((consumed (ui-consumed ui))
          (last-build (>= (almighty-bolt/internals:context-build-number context) +max-builds+)))
      (cond
        ((and consumed (not last-build))
         ;; Something took input, so what the application holds may have changed: describe
         ;; it again before laying anything out.
         t)
        (t
         (lay-out-and-paint variant context list)
         (setf (ui-started ui) t)
         (end-frame-input ui)
         (when (more-input variant)
           (request-frame context))
         consumed)))))

;;; --- an app made again ---

(defmethod inherit-state ((new immediate-variant) (old immediate-variant))
  "The new variant takes the old one's UI whole: the keyboard focus, the state of each widget by its key
(a field's caret and selection and where it is scrolled to, a list's scroll), and the input not yet
looked at. The widgets are given what they show by the app each frame, so none of it is the app's.
What was laid out and painted is forgotten, so that the first frame does both again, whatever the
hashes say: the code that changed may be the library's, which the hashes do not describe."
  (let ((ui (immediate-variant-ui old)))
    (setf (ui-last-layout-hash ui) -1)
    (forget-painting old)
    (setf (immediate-variant-ui new) ui)))

(defmethod forget-painting ((variant immediate-variant))
  "Every leaf is painted afresh in the next frame, and nothing is copied from the frame before."
  (let ((ui (immediate-variant-ui variant)))
    (setf (ui-last-paint-hash ui) -1)
    (clrhash (ui-paint-cache ui))))

;;; --- asking where things are ---

(defmethod hit-target ((variant immediate-variant) x y)
  (let ((ui (immediate-variant-ui variant)))
    (loop :for i :from (1- (ui-hit-count ui)) :downto 0
          :do (let ((hit (aref (ui-hits ui) i)))
                (when (and (hit-id hit)
                           (<= (hit-x hit) x) (< x (+ (hit-x hit) (hit-w hit)))
                           (<= (hit-y hit) y) (< y (+ (hit-y hit) (hit-h hit))))
                  (return (hit-id hit)))))))

(defmethod lookup-target ((variant immediate-variant) id)
  (let ((rect (gethash id (ui-id-rects (immediate-variant-ui variant)))))
    (when rect
      (values (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))))

(defmethod handle-event ((variant immediate-variant) event)
  (enqueue-event (immediate-variant-ui variant) event))
