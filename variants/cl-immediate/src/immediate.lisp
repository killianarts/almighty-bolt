(defpackage #:almighty-bolt-variant-immediate/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:handle-event
                #:hit-target
                #:lookup-target
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
  (:import-from #:almighty-bolt-variant-retained/theme
                #:+background+)
  (:import-from #:almighty-bolt-variant-immediate/hash
                #:mix)
  (:import-from #:almighty-bolt-variant-immediate/box
                #:box-key
                #:box-id
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
  (:import-from #:almighty-bolt-variant-immediate/ui
                #:*ui*
                #:make-ui
                #:ui-context
                #:ui-root
                #:ui-count
                #:ui-boxes
                #:ui-hot
                #:ui-focus
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
                #:begin-build
                #:enqueue-event
                #:begin-frame-input
                #:end-frame-input)
  (:import-from #:almighty-bolt-variant-immediate/widgets
                #:own-hashes)
  (:import-from #:almighty-bolt-variant-immediate/layout
                #:layout-box)
  (:import-from #:almighty-bolt-variant-immediate/paint
                #:paint-box)
  (:export #:immediate-variant
           #:make-immediate-variant
           #:immediate-variant-ui
           #:now
           #:current-context
           #:ask-for-frame))

(in-package #:almighty-bolt-variant-immediate/immediate)

;;; The immediate variant. Every build of every frame calls the application's function, which
;;; describes the whole interface again with the widgets, taking from the frame's input what
;;; is for them. Then the boxes it made are given hashes, and if the hash of what decides the
;;; layout is what it was in the last frame presented, the places of the boxes are taken from
;;; that frame; if the hash of what decides the look is too, the commands are copied from it;
;;; and otherwise each box draws itself unless it is the same as it was (paint.lisp). A build
;;; that took anything from the input is followed by another, because it may have changed
;;; what the application holds, and only the last build is laid out and drawn.

(defclass immediate-variant (variant)
  ((ui :initform (make-ui) :reader immediate-variant-ui)
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
        (setf (ui-frame-layout-hash ui) (box-layout-hash root)
              (ui-frame-paint-hash ui) (box-paint-hash root))))))

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
them take it, in the order they are drawn, for the next frame; and save every box's place."
  (clrhash (ui-rects ui))
  (clrhash (ui-id-rects ui))
  (setf (ui-hit-count ui) 0
        (ui-rect-count ui) 0)
  (let ((boxes (ui-boxes ui))
        (count (ui-count ui)))
    (labels ((walk (box x y)
               (let ((x (+ x (box-x box)))
                     (y (+ y (box-y box))))
                 (when (or (box-id box) (box-interactive box))
                   (let ((rect (next-rect ui)))
                     (setf (rect-x rect) x (rect-y rect) y
                           (rect-w rect) (box-w box) (rect-h rect) (box-h box))
                     (setf (gethash (box-key box) (ui-rects ui)) rect)
                     (when (box-id box)
                       (setf (gethash (box-id box) (ui-id-rects ui)) rect))))
                 (when (box-interactive box)
                   (let ((index (ui-hit-count ui))
                         (hits (ui-hits ui)))
                     (when (>= index (length hits))
                       (vector-push-extend (make-hit) hits))
                     (let ((hit (aref hits index)))
                       (setf (hit-key hit) (box-key box) (hit-id hit) (box-id box)
                             (hit-x hit) x (hit-y hit) y (hit-w hit) (box-w box)
                             (hit-h hit) (box-h box) (hit-focusable hit) (box-focusable box)
                             (hit-moves hit) (box-moves box)))
                     (incf (ui-hit-count ui))))
                 (loop :for child :across (box-children box)
                       :do (walk child x y)))))
      (when (ui-root ui)
        (walk (ui-root ui) 0f0 0f0)))
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
frame presented, round."
  (let ((direction (ui-tab-pending ui))
        (keys '()))
    (when direction
      (setf (ui-tab-pending ui) nil
            (ui-click-focused ui) nil)
      (loop :for i :below (ui-hit-count ui)
            :do (let ((hit (aref (ui-hits ui) i)))
                  (when (hit-focusable hit)
                    (push (hit-key hit) keys))))
      (setf keys (nreverse keys))
      (when keys
        (let ((position (position (ui-focus ui) keys)))
          (setf (ui-focus ui)
                (if (eq direction :forward)
                    (if position (nth (mod (1+ position) (length keys)) keys) (first keys))
                    (if position (nth (mod (1- position) (length keys)) keys) (car (last keys))))))))))

;;; --- a frame ---

(defun lay-out-and-paint (variant context list)
  (let* ((ui (immediate-variant-ui variant))
         (root (ui-root ui))
         (width (context-width context))
         (height (context-height context))
         (scale (context-scale context)))
    (cond
      ((null root)
       (push-rect list 0 0 width height +background+))
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
                      (remember-geometry ui))))
         (with-phase (context +phase-paint+)
           (cond
             ((and same-layout (= (ui-last-paint-hash ui) (ui-frame-paint-hash ui)))
              (replace-display-list list (ui-last-list ui)))
             (t
              (push-rect list 0 0 width height +background+)
              (paint-box ui root context list 0f0 0f0)
              (when (< (display-list-capacity (ui-last-list ui)) (display-list-capacity list))
                (setf (ui-last-list ui) (make-display-list :capacity (display-list-capacity list))))
              (replace-display-list (ui-last-list ui) list))))
         (setf (ui-last-layout-hash ui) (ui-frame-layout-hash ui)
               (ui-last-paint-hash ui) (ui-frame-paint-hash ui)
               (ui-last-width ui) width
               (ui-last-height ui) height
               (ui-last-scale ui) scale))))))

(defmethod frame ((variant immediate-variant) context list)
  (let ((ui (immediate-variant-ui variant)))
    (when (<= (almighty-bolt-core:context-build-number context) 1)
      (setf (more-input variant) (begin-frame-input ui))
      (move-focus ui))
    (with-phase (context +phase-update+)
      (begin-build ui context)
      (let ((*ui* ui))
        (funcall (app variant)))
      (compute-hashes ui))
    (let ((consumed (ui-consumed ui))
          (last-build (>= (almighty-bolt-core:context-build-number context) +max-builds+)))
      (cond
        ((and consumed (not last-build))
         ;; Something took input, so what the application holds may have changed: describe
         ;; it again before laying anything out.
         t)
        (t
         (lay-out-and-paint variant context list)
         (end-frame-input ui)
         (when (more-input variant)
           (request-frame context))
         consumed)))))

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
