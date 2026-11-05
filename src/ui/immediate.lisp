(defpackage #:almighty-bolt/ui/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:context-appearance
                #:context-cursor
                #:context-window
                #:app-instance
                #:frame
                #:handle-event
                #:hit-target
                #:app-menus
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
                #:+max-builds+
                #:*show-frame-errors*
                #:pack-color
                #:context-font
                #:context-measure
                #:context-push-text)
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
                #:box-inert
                #:box-focusable
                #:box-moves
                #:box-x
                #:box-y
                #:box-w
                #:box-h
                #:box-pref-w
                #:box-pref-h
                #:box-layout-hash
                #:box-paint-hash)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:ui-made-again
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
                #:ui-cursor
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
                #:ui-menus
                #:begin-build
                #:enqueue-event
                #:begin-frame-input
                #:end-frame-input)
  (:import-from #:almighty-bolt/ui/widgets
                #:own-hashes
                #:describe-tooltip
                #:describe-toasts)
  (:import-from #:almighty-bolt/ui/inspection
                #:inspection-take-input
                #:describe-outlines)
  (:import-from #:almighty-bolt/ui/layout
                #:layout-box)
  (:import-from #:almighty-bolt/ui/paint
                #:paint-box)
  (:export #:immediate-app
           #:make-immediate-app
           #:immediate-app-ui
           #:immediate-app-tools
           #:immediate-app-failure
           #:immediate-app-tree
           #:immediate-app-tree-frame
           #:now
           #:current-context
           #:this-window
           #:ask-for-frame))

(in-package #:almighty-bolt/ui/immediate)

;;; The immediate app. Every build of every frame calls the application's function, which
;;; describes the whole interface again with the widgets, taking from the frame's input what
;;; is for them. Then the boxes it made are given hashes, and if the hash of what decides the
;;; layout is what it was in the last frame presented, the places of the boxes are taken from
;;; that frame; if the hash of what decides the look is too, the commands are copied from it;
;;; and otherwise each box draws itself unless it is the same as it was (paint.lisp). A build
;;; that took anything from the input is followed by another, because it may have changed
;;; what the application holds, and only the last build is laid out and drawn.

(defclass immediate-app (app-instance)
  ((ui :initform (make-ui) :accessor immediate-app-ui)
   (app :initarg :app :reader app)
   ;; True if events were left in the queue by the last frame.
   (more-input :initform nil :accessor more-input)
   ;; True if the tools for developing the app (the inspector) can be opened from its window.
   (tools :initform nil :accessor immediate-app-tools)
   ;; What the last frame's error said, while frames fail, so that it is reported once.
   (failure :initform nil :accessor immediate-app-failure)
   ;; The semantic tree last asked for in a frame that did not fail, and what that frame was (its
   ;; hashes and size), for while frames fail: the tree of the frame that is shown.
   (tree :initform nil :accessor immediate-app-tree)
   (tree-frame :initform nil :accessor immediate-app-tree-frame))
  (:documentation "An app whose interface is a function that is called every frame."))

(defun make-immediate-app (app)
  "An app that shows what APP, a function of no arguments that calls the widgets, describes."
  (make-instance 'immediate-app :app app))

(defun now ()
  "The time in nanoseconds, for an interface that changes as time passes."
  (context-now-ns (ui-context *ui*)))

(defun current-context ()
  "The context of the frame being built, which measures text: for the code that handles what a
canvas returned, to turn a point into a place in text."
  (ui-context *ui*))

(defun this-window ()
  "The window whose frame is being built, as OPEN-WINDOW returns it: for CLOSE-WINDOW, or for
OPEN-WINDOW's :BESIDE, from the app's own function. NIL outside a frame, and in a frame with no
window (a test's)."
  (and *ui* (context-window (ui-context *ui*))))

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
    ;; INERT is true inside a design surface: what is there is drawn, and has a place, but the
    ;; pointer finds only the surface.
    (labels ((walk (box x y left top right bottom layer &optional inert)
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
                 (when (and (not inert) (or (box-interactive box) (box-tooltip box)))
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
                 (let ((inert (or inert (eq (box-kind box) :surface))))
                   (if (member (box-kind box) '(:scroll :lazy-list :pane))
                       (loop :for child :across (box-children box)
                             :do (walk child x y shown-left shown-top (+ shown-left shown-w)
                                       (+ shown-top shown-h) layer inert))
                       (loop :for child :across (box-children box)
                             :do (walk child x y left top right bottom layer inert)))))))
      (when (ui-root ui)
        (walk (ui-root ui) 0f0 0f0 -1f30 -1f30 1f30 1f30 0))
      ;; The floats after the screen, so that the pointer finds them first; each is at its place in
      ;; the window (its X and Y), with nothing over it.
      (loop :for float :across (ui-floats ui)
            :do (walk float 0f0 0f0 -1f30 -1f30 1f30 1f30 (box-key float) (box-inert float))
                ;; A dialog opened in a design surface does not keep the keyboard.
                (when (and (float-spec-modal (box-data float)) (not (box-inert float)))
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
              ;; Over its anchor, it is the anchor's size (an outline round it).
              (when (eq (float-spec-placement spec) :over)
                (let ((anchor (float-spec-anchor spec)))
                  (setf (box-pref-w float) (box-w anchor)
                        (box-pref-h float) (box-h anchor))))
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
                      (:over (window-position (float-spec-anchor spec)))
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

(defun lay-out-and-paint (instance context list)
  (let* ((ui (immediate-app-ui instance))
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

(defun one-line (condition)
  "What CONDITION says, on one line."
  (let ((words (with-output-to-string (out)
                 (let ((space nil))
                   (loop :for character :across (princ-to-string condition)
                         :do (if (member character '(#\Space #\Tab #\Newline #\Return))
                                 (setf space t)
                                 (progn (when space (write-char #\Space out) (setf space nil))
                                        (write-char character out))))))))
    (string-trim " " words)))

(defun show-failure (instance context list condition)
  "Put in LIST the last frame that did not fail, with a band along the bottom of the window that
says CONDITION, and report CONDITION if it is not the one the frame before failed with. The places
of things stay those of that frame, so the pointer finds what is shown."
  (let* ((ui (immediate-app-ui instance))
         (said (one-line condition))
         (width (context-width context))
         (height (context-height context))
         (font (context-font context "Helvetica" 13))
         ;; The error first: in a narrow window it is all there is room for.
         (text (format nil "Frame failed: ~a (the last good frame is shown)" said)))
    (unless (equal said (immediate-app-failure instance))
      (format *error-output* "~&The app failed in a frame (the last good one is shown): ~a~%" said)
      (finish-output *error-output*))
    (setf (immediate-app-failure instance) said)
    (end-frame-input ui)
    ;; What has come and not been looked at (the release after a press) has a frame of its own.
    (when (more-input instance)
      (request-frame context))
    (replace-display-list list (ui-last-list ui))
    ;; Before any frame has been drawn there is nothing to show but the window's background.
    (when (zerop (display-list-count list))
      (push-rect list 0 0 width height (theme-background (window-theme context))))
    ;; As much of the words as fits, on a dark red band.
    (loop :while (and (> (length text) 1)
                      (> (context-measure context font text) (- width 24)))
          :do (setf text (subseq text 0 (- (length text) 2))))
    (push-rect list 0 (- height 36) width 36 (pack-color 140 24 32))
    (context-push-text context list font text 12 (- height 13) (pack-color 255 255 255))))

(defmethod frame :around ((instance immediate-app) context list)
  "While *SHOW-FRAME-ERRORS*, a frame that signals an error shows the last one that did not, with the
error over it (SHOW-FAILURE), and the window goes on: the next frame is made as any other."
  (if (not *show-frame-errors*)
      (call-next-method)
      (handler-case (call-next-method)
        (error (condition)
          (show-failure instance context list condition)
          nil))))

(defmethod frame ((instance immediate-app) context list)
  (let ((ui (immediate-app-ui instance))
        (inspected nil))
    (when (<= (almighty-bolt/internals:context-build-number context) 1)
      (setf (more-input instance) (begin-frame-input ui))
      (move-focus ui)
      (setf inspected (inspection-take-input ui (and (immediate-app-tools instance) instance))))
    (with-phase (context +phase-update+)
      (begin-build ui context)
      ;; What the inspector took before the build is input this frame took (BEGIN-BUILD forgets it).
      (when inspected
        (setf (ui-consumed ui) t))
      (let ((*ui* ui)
            (*theme* (window-theme context)))
        (funcall (app instance))
        ;; An app that describes only what floats (a dialog, and nothing under it) has an empty
        ;; screen under it, so that the floats are laid out, painted and in the tree.
        (unless (ui-root ui)
          (open-box ui :column)
          (close-box ui))
        (describe-toasts ui)
        (describe-tooltip ui)
        (describe-outlines ui)
        (setf (context-cursor context) (ui-cursor ui)))
      (compute-hashes ui))
    (let ((consumed (ui-consumed ui))
          (last-build (>= (almighty-bolt/internals:context-build-number context) +max-builds+)))
      (cond
        ((and consumed (not last-build))
         ;; Something took input, so what the application holds may have changed: describe
         ;; it again before laying anything out.
         t)
        (t
         (lay-out-and-paint instance context list)
         ;; A frame laid out and painted: what failed before is mended. (A build that took input
         ;; and builds again has not got this far.)
         (setf (immediate-app-failure instance) nil)
         (setf (ui-started ui) t)
         (end-frame-input ui)
         (when (more-input instance)
           (request-frame context))
         consumed)))))

;;; --- an app made again ---

(defmethod inherit-state ((new immediate-app) (old immediate-app))
  "The new app takes the old one's UI whole: the keyboard focus, the state of each widget by its key
(a field's caret and selection and where it is scrolled to, a list's scroll), and the input not yet
looked at. The widgets are given what they show by the app each frame, so none of it is the app's.
What was laid out and painted is forgotten, so that the first frame does both again, whatever the
hashes say: the code that changed may be the library's, which the hashes do not describe."
  (let ((ui (immediate-app-ui old)))
    (setf (ui-last-layout-hash ui) -1)
    (incf (ui-made-again ui))
    (forget-painting old)
    (setf (immediate-app-ui new) ui)))

(defmethod forget-painting ((instance immediate-app))
  "Every leaf is painted afresh in the next frame, and nothing is copied from the frame before."
  (let ((ui (immediate-app-ui instance)))
    (setf (ui-last-paint-hash ui) -1)
    (clrhash (ui-paint-cache ui))))

;;; --- asking where things are ---

(defmethod app-menus ((instance immediate-app))
  (ui-menus (immediate-app-ui instance)))

(defmethod hit-target ((instance immediate-app) x y)
  (let ((ui (immediate-app-ui instance)))
    (loop :for i :from (1- (ui-hit-count ui)) :downto 0
          :do (let ((hit (aref (ui-hits ui) i)))
                (when (and (hit-id hit)
                           (<= (hit-x hit) x) (< x (+ (hit-x hit) (hit-w hit)))
                           (<= (hit-y hit) y) (< y (+ (hit-y hit) (hit-h hit))))
                  (return (hit-id hit)))))))

(defmethod lookup-target ((instance immediate-app) id)
  (let ((rect (gethash id (ui-id-rects (immediate-app-ui instance)))))
    (when rect
      (values (rect-x rect) (rect-y rect) (rect-w rect) (rect-h rect)))))

(defmethod handle-event ((instance immediate-app) event)
  (enqueue-event (immediate-app-ui instance) event))
