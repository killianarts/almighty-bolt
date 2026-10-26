(defpackage #:almighty-bolt-variant-cursor/ui
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-width
                #:context-height
                #:context-scale
                #:make-display-list
                #:clear-display-list
                #:append-display-list)
  (:export #:*ui*
           #:make-ui
           #:ui-mode
           #:ui-context
           #:ui-list
           #:ui-leaf
           #:ui-region
           #:ui-rects
           #:ui-stale
           #:region
           #:region-x
           #:region-y
           #:region-width
           #:region-height
           #:region-cursor-x
           #:region-cursor-y
           #:region-pending-x
           #:region-pending-y
           #:region-used-w
           #:region-used-h
           #:region-gap
           #:region-direction
           #:make-region
           #:begin-build
           #:end-build
           #:peek-position
           #:place
           #:open-region
           #:close-region
           #:note-rect
           #:remembered-size
           #:ui-measure-state
           #:measure-pass
           #:note-measured
           #:fragment-mark
           #:begin-fragment
           #:end-fragment
           #:place-fragment
           #:snap-to-pixel))

(in-package #:almighty-bolt-variant-cursor/ui)

;;; The state of a front end that places what it is told where the cursor is. A region is a
;;; row or a column being filled: it has an origin in the window, room for its children, and a
;;; cursor that moves along as each child is placed. Nothing is kept of the children but the
;;; commands they drew and, by their id, the rectangle each took, which the next build can ask
;;; for (REMEMBERED-SIZE): that is the only way a widget placed now can know the size of
;;; something that has not been described yet.
;;;
;;; The mode says what a frame does about a size it needed and did not have right:
;;;   :lag       nothing; the frame is drawn with the sizes of the build before it
;;;   :request   a frame is asked for, so that the next one has the sizes
;;;   :rebuild   the build says that it changed state, and the core builds again (0009)
;;;   :measure   a container that needs its size is run twice in the build, the first time to
;;;              find the sizes in it, which the second remembers; the frame is right and has
;;;              one build, and what is in the container is described twice
;;;   :fragment  nothing is remembered; a child that is to be placed by its size is drawn at
;;;              the origin into a fragment of its own, which returns its size, and is then
;;;              appended where the parent puts it

(defvar *ui*)

(defstruct (region (:constructor make-region (x y width height direction gap)))
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (width 0f0 :type single-float)
  (height 0f0 :type single-float)
  (direction :column :type (member :column :row))
  (gap 0f0 :type single-float)
  ;; Where the next child goes, from the region's origin, and an offset for that child alone.
  (cursor-x 0f0 :type single-float)
  (cursor-y 0f0 :type single-float)
  (pending-x 0f0 :type single-float)
  (pending-y 0f0 :type single-float)
  ;; The corner of the children's bounding box, from the origin.
  (used-w 0f0 :type single-float)
  (used-h 0f0 :type single-float)
  (count 0 :type fixnum)
  (parent nil))

(defstruct (ui (:constructor make-ui (mode)))
  (mode :rebuild :type (member :lag :request :rebuild :measure :fragment))
  (context nil)
  (list nil)
  ;; The text of one label or paragraph is drawn at the origin and then appended where the
  ;; widget is, as the other variants do, so that it is moved by whole pixels.
  (leaf (make-display-list :capacity 8192))
  (fragments (make-array 4 :initial-element nil))
  (depth 0 :type fixnum)
  (region nil)
  (rects (make-hash-table :test 'eql))
  (last (make-hash-table :test 'eql))
  (noted '())
  (used '())
  (stale nil)
  ;; The measure mode: NIL, :measuring while a container is run to find its sizes, and :valid
  ;; while it is run again for real, when MEASURED holds the sizes the first run found.
  (measure-state nil)
  (measured (make-hash-table :test 'eql)))

(declaim (inline snap-to-pixel))
(defun snap-to-pixel (position scale)
  (declare (type single-float position scale))
  (* (/ 1f0 scale)
     (round (the (single-float -1f6 1f6) (* scale position)))))

(defun begin-build (ui context list)
  "Start a build into LIST: what the last build noted becomes what can be remembered."
  (rotatef (ui-rects ui) (ui-last ui))
  (clrhash (ui-rects ui))
  (clrhash (ui-measured ui))
  (setf (ui-context ui) context
        (ui-list ui) list
        (ui-noted ui) '()
        (ui-used ui) '()
        (ui-stale ui) nil
        (ui-measure-state ui) nil
        (ui-depth ui) 0
        (ui-region ui) (make-region 0f0 0f0 (context-width context) (context-height context)
                                    :column 0f0))
  ui)

(defun end-build (ui)
  "Finish a build. Returns true if a size it asked for was not the size the build found: the
build was made with sizes that were wrong."
  (setf (ui-stale ui)
        (loop :for id :in (ui-used ui)
              :thereis (let ((old (gethash id (ui-last ui)))
                             (new (gethash id (ui-rects ui))))
                         (and new
                              (or (null old)
                                  (/= (svref old 2) (svref new 2))
                                  (/= (svref old 3) (svref new 3)))))))
  (ui-stale ui))

;;; --- places ---

(defun peek-position (region)
  "Where the next child of REGION would go in the window, as two values."
  (values (+ (region-x region) (region-cursor-x region) (region-pending-x region))
          (+ (region-y region) (region-cursor-y region) (region-pending-y region))))

(defun place (ui width height)
  "Take WIDTH by HEIGHT at the cursor of the current region, move the cursor on, and return
where it is in the window and the width and height it was given, which are less than asked
for if the room across is."
  (declare (type single-float width height))
  (let* ((region (ui-region ui))
         (gap (region-gap region))
         (rx (+ (region-cursor-x region) (region-pending-x region)))
         (ry (+ (region-cursor-y region) (region-pending-y region))))
    (declare (type single-float rx ry gap))
    ;; Nothing is taller or wider than the room it is given across the flow, as in a layout
    ;; that passes constraints down: a line a little wider than its room (a measured line of
    ;; text can be) takes the room, and its parent does not grow past it.
    (ecase (region-direction region)
      (:column (setf width (min width (region-width region))))
      (:row (setf height (min height (region-height region)))))
    (setf (region-pending-x region) 0f0
          (region-pending-y region) 0f0)
    (ecase (region-direction region)
      (:column (setf (region-cursor-y region) (+ ry (+ height gap))))
      (:row (setf (region-cursor-x region) (+ rx (+ width gap)))))
    (setf (region-used-w region) (max (region-used-w region) (+ rx width))
          (region-used-h region) (max (region-used-h region) (+ ry height)))
    (incf (region-count region))
    (values (+ (region-x region) rx) (+ (region-y region) ry) width height)))

(defun open-region (ui x y width height direction gap)
  "Make a region at X, Y in the window the current one."
  (setf (ui-region ui) (let ((region (make-region x y width height direction gap)))
                         (setf (region-parent region) (ui-region ui))
                         region)))

(defun close-region (ui)
  "Make the region that contained the current one current again, and return the one closed."
  (let ((region (ui-region ui)))
    (setf (ui-region ui) (region-parent region))
    region))

;;; --- what the build before knew ---

(defun note-rect (ui id x y width height)
  "Say that the thing called ID took this rectangle in the window."
  (when id
    (let ((entry (vector x y width height)))
      (setf (gethash id (ui-rects ui)) entry)
      (push (cons id entry) (ui-noted ui))))
  nil)

(defun remembered-size (ui id)
  "The width and height ID had in the build before, or 0 and 0 if it had none. The build
is stale if what it finds for ID is not what it was given. While a container is run for
real after being run to find its sizes, they are what it remembers, and the build is not
stale for them."
  (let ((measured (and (eq (ui-measure-state ui) :valid) (gethash id (ui-measured ui)))))
    (if measured
        (values (svref measured 2) (svref measured 3))
        (let ((entry (gethash id (ui-last ui))))
          (pushnew id (ui-used ui))
          (if entry
              (values (svref entry 2) (svref entry 3))
              (values 0f0 0f0))))))

;;; --- fragments ---

(defun fragment-mark (ui)
  (ui-noted ui))

(defun begin-fragment (ui width height &optional (gap 0f0))
  "Draw what follows into a scratch list, in a column from the origin with WIDTH by HEIGHT
of room and GAP between children. Returns the list."
  (let* ((depth (ui-depth ui))
         (fragment (or (svref (ui-fragments ui) depth)
                       (setf (svref (ui-fragments ui) depth)
                             (make-display-list :capacity 16384)))))
    (clear-display-list fragment)
    (setf (ui-list ui) fragment)
    (incf (ui-depth ui))
    (open-region ui 0f0 0f0 width height :column gap)
    fragment))

(defun end-fragment (ui outer)
  "Stop drawing into the fragment and draw into OUTER again. Returns the width and height
the fragment's children took."
  (let ((region (close-region ui)))
    (decf (ui-depth ui))
    (setf (ui-list ui) outer)
    (values (region-used-w region) (region-used-h region))))

(defun place-fragment (ui fragment x y mark)
  "Append FRAGMENT with its origin at X, Y, which is moved to a whole pixel, and move the
rectangles noted since MARK to X, Y itself."
  (declare (type single-float x y))
  (let ((scale (context-scale (ui-context ui))))
    (append-display-list (ui-list ui) fragment (snap-to-pixel x scale) (snap-to-pixel y scale)))
  (loop :for cell :on (ui-noted ui)
        :until (eq cell mark)
        :do (let ((entry (cdr (car cell))))
              (incf (svref entry 0) x)
              (incf (svref entry 1) y)))
  nil)

;;; --- the measure mode ---

(defun measure-pass (ui width height gap thunk)
  "Run THUNK, drawing nothing, to find the sizes in it. The sizes of everything in it that
had an id are what a run of it for real remembers (the state is :valid), until the caller puts
the state back to NIL. Returns the width and height the children of THUNK took."
  (let ((saved-used (ui-used ui))
        (outer (ui-list ui))
        (mark (ui-noted ui)))
    (setf (ui-measure-state ui) :measuring)
    (begin-fragment ui width height gap)
    (multiple-value-bind (used-w used-h)
        (unwind-protect
             (progn (funcall thunk)
                    (end-fragment ui outer))
          (setf (ui-measure-state ui) nil))
      (loop :for cell :on (ui-noted ui)
            :until (eq cell mark)
            :do (setf (gethash (car (car cell)) (ui-measured ui)) (cdr (car cell))))
      (setf (ui-used ui) saved-used
            (ui-measure-state ui) :valid)
      (values used-w used-h))))

(defun note-measured (ui id width height)
  "Say that ID was found to be WIDTH by HEIGHT by a measuring run."
  (setf (gethash id (ui-measured ui)) (vector 0f0 0f0 width height))
  nil)
