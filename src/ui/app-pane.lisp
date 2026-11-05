(defpackage #:almighty-bolt/ui/app-pane
  (:use #:cl)
  (:import-from #:almighty-bolt/event
                #:key-event
                #:text-input-event
                #:composition-event
                #:composition-event-kind
                #:make-composition-event
                #:make-pointer-event
                #:make-wheel-event)
  (:import-from #:almighty-bolt/display-list
                #:make-display-list
                #:display-list-count
                #:display-list-capacity
                #:display-list-full
                #:display-list-hash
                #:zoom-display-list
                #:unzoom-glyphs)
  (:import-from #:almighty-bolt/app-instance
                #:context
                #:context-scale
                #:context-frame-requested
                #:context-appearance
                #:context-window
                #:context-cursor
                #:context-text-input-p
                #:context-text-input-rect
                #:set-text-input-caret
                #:handle-event
                #:inherit-state
                #:retire-app
                #:request-frame
                #:context-font
                #:context-measure
                #:context-break-lines
                #:context-caret-x
                #:context-index-at-x
                #:context-caret-rect
                #:context-push-text
                #:context-forget-glyphs
                #:context-clipboard-text
                #:context-set-clipboard-text
                #:context-choose-file)
  (:import-from #:almighty-bolt/kept
                #:make-kept-state
                #:with-kept-state)
  (:import-from #:almighty-bolt/settle
                #:settle-frame)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-interactive
                #:box-focusable
                #:box-moves
                #:box-pref-w
                #:box-pref-h
                #:box-flex
                #:box-data
                #:box-n3
                #:box-look)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:open-box
                #:close-box
                #:state-of
                #:ui-states
                #:rect-of
                #:rect-x
                #:rect-y
                #:rect-w
                #:rect-h
                #:ui-context
                #:ui-made-again
                #:ui-hot
                #:ui-active
                #:ui-focus
                #:ui-pressed
                #:ui-released
                #:ui-down
                #:ui-moved
                #:ui-click-count
                #:ui-pointer-x
                #:ui-pointer-y
                #:ui-keys
                #:ui-inputs
                #:ui-consumed
                #:consume-press
                #:consume-secondary-press
                #:consume-release
                #:consume-key
                #:consume-input
                #:consume-wheel
                #:focus-by-click
                #:want-cursor)
  (:import-from #:almighty-bolt/ui/widgets
                #:required-id
                #:set-size-limits
                #:look-of)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-app
                #:immediate-app)
  (:import-from #:almighty-bolt/ui/views
                #:make-weak-table
                #:views-version)
  (:export #:app-pane
           #:settle-app-pane
           #:app-pane-inside))

(in-package #:almighty-bolt/ui/app-pane)

;;; An app inside another, in a pane of it: a designer showing the app being made, working, at a
;;; zoom. The inner app is made once from its constructor and kept by the pane's id; it has a ui of
;;; its own and a context of its own, as large as the pane (divided by the zoom), that measures and
;;; draws text through the window's. The pointer over the pane, a press on it, the wheel and, while
;;; the pane has the keyboard, the keys are given to it as the window gives them, in its own points.
;;; It is laid out and drawn in its own points, and what it draws is made ZOOM times larger to show:
;;; its text drawn with fonts ZOOM times the size, so that it is sharp. While the pane has the
;;; keyboard and something in the inner app takes text, the window does: the input method's text
;;; and what it composes go to the inner app, and its candidates open by the inner app's caret.
;;; When the window's app is made again because its code changed, so is the inner app, from its
;;; constructor, keeping what its STATE-LETs hold and what was done to its widgets, as a window's.
;;; What the inner app's screen means (its semantic tree) is inside the pane's, where the pane
;;; shows it, for a screen reader and the machine interface (see APP-PANE-INSIDE).

;;; --- the inner app's context ---

(defstruct (pane-context (:include context))
  "The context of an app in a pane: as large as the pane, its text measured and drawn by OUTER's,
at ZOOM."
  (outer nil)
  (zoom 1f0 :type single-float))

(defstruct (zoomed-font (:constructor zoomed-font (base zoomed)))
  "A font of a pane's context: BASE, the outer context's at the size asked for, which measures, and
ZOOMED, the same at the pane's zoom, which draws."
  base zoomed)

(defmethod context-font ((context pane-context) name size &optional (weight :regular))
  (let* ((outer (pane-context-outer context))
         (zoom (pane-context-zoom context))
         (base (context-font outer name size weight)))
    (zoomed-font base (if (= zoom 1f0) base (context-font outer name (* size zoom) weight)))))

(defmethod context-measure ((context pane-context) font string)
  (context-measure (pane-context-outer context) (zoomed-font-base font) string))

(defmethod context-break-lines ((context pane-context) font string width)
  (context-break-lines (pane-context-outer context) (zoomed-font-base font) string width))

(defmethod context-caret-x ((context pane-context) font string index)
  (context-caret-x (pane-context-outer context) (zoomed-font-base font) string index))

(defmethod context-index-at-x ((context pane-context) font string x)
  (context-index-at-x (pane-context-outer context) (zoomed-font-base font) string x))

(defmethod context-caret-rect ((context pane-context) font string index)
  (context-caret-rect (pane-context-outer context) (zoomed-font-base font) string index))

(defmethod context-push-text ((context pane-context) list font string x baseline color)
  (let ((start (display-list-count list))
        (zoom (pane-context-zoom context)))
    (multiple-value-prog1
        (context-push-text (pane-context-outer context) list (zoomed-font-zoomed font) string x
                           baseline color)
      (unless (= zoom 1f0)
        (unzoom-glyphs list start (float x 1f0) (float baseline 1f0) zoom)))))

(defmethod context-forget-glyphs ((context pane-context))
  (context-forget-glyphs (pane-context-outer context)))

(defmethod context-clipboard-text ((context pane-context))
  (context-clipboard-text (pane-context-outer context)))

(defmethod context-set-clipboard-text ((context pane-context) string)
  (context-set-clipboard-text (pane-context-outer context) string))

(defmethod context-choose-file ((context pane-context) kind types name callback)
  (context-choose-file (pane-context-outer context) kind types name callback))

;;; --- the pane ---

(defstruct (app-pane-state (:constructor make-app-pane-state ()))
  (constructor nil)
  (instance nil)
  ;; What the inner app's STATE-LETs keep, from one time it is made to the next.
  (kept (make-kept-state))
  ;; How many times the window's app had been made again when the inner app was last made.
  (made-again 0 :type fixnum)
  ;; How many times views had been defined when the inner app last drew: one defined since (in a
  ;; designer, as it is edited) is drawn again.
  (views-version 0 :type fixnum)
  (context nil)
  ;; What the inner app drew, in its points, and that made ZOOM times larger, which is shown.
  (list (make-display-list :capacity 4096))
  (shown (make-display-list :capacity 4096))
  (zoom 0f0 :type single-float)
  (was-hot nil)
  ;; Whether the input method is composing in the inner app: it began and has not been committed.
  (composing nil))

(defvar *pane-states* (make-weak-table)
  "The state of the pane each app in a pane is in, by the app.")

(defun settle-pane (state)
  "Settle the frame of STATE's app, and make what it drew ZOOM times larger to show."
  (let ((inner (app-pane-state-context state)))
    (setf (context-frame-requested inner) nil)
    (loop
      (handler-case
          (progn (settle-frame (app-pane-state-instance state) inner (app-pane-state-list state))
                 (zoom-display-list (app-pane-state-shown state) (app-pane-state-list state)
                                    (pane-context-zoom inner))
                 (return))
        (display-list-full ()
          (let ((capacity (* 2 (display-list-capacity (app-pane-state-list state)))))
            (setf (app-pane-state-list state) (make-display-list :capacity capacity)
                  (app-pane-state-shown state) (make-display-list :capacity capacity))))))))

(defun settle-app-pane (instance)
  "Settle the frame of INSTANCE, an app in an app pane, now: for a tool that works it between
frames (the inspector's actions) and must see what each step did. The window shows it at its next
frame."
  (let ((state (gethash instance *pane-states*)))
    (when state
      (settle-pane state)
      (request-frame (pane-context-outer (app-pane-state-context state))))))

(defun app-pane-inside (ui key)
  "If the box KEY of UI's last frame is an app pane, the app in it, its zoom, and whether the pane
has the keyboard; otherwise NIL."
  (let ((state (gethash key (ui-states ui))))
    (when (and (app-pane-state-p state) (app-pane-state-instance state))
      (values (app-pane-state-instance state)
              (pane-context-zoom (app-pane-state-context state))
              (= (ui-focus ui) key)))))

(defun make-inner (state)
  "The app that STATE's constructor makes, with STATE's store for what it keeps."
  (with-kept-state ((app-pane-state-kept state))
    (make-immediate-app (funcall (app-pane-state-constructor state)))))

(defun make-inner-again (state id)
  "Make STATE's app again from its constructor, in place of the one it has, which hands on what it
kept (INHERIT-STATE). If that fails, the one it has stays, and what went wrong is said."
  (let ((old (app-pane-state-instance state)))
    (handler-case
        (let ((new (make-inner state)))
          (handler-case (inherit-state new old)
            (error (condition)
              (retire-app new)
              (error condition)))
          (remhash old *pane-states*)
          (setf (app-pane-state-instance state) new
                (gethash new *pane-states*) state)
          (retire-app old)
          t)
      (error (condition)
        (format *error-output* "~&Hot reload: the app in the pane ~s could not be made again, so ~
                                the old one stays: ~a~%" id condition)
        (finish-output *error-output*)
        nil))))

(defun deliver (state event)
  "Give EVENT to the inner app."
  (handle-event (app-pane-state-instance state) event)
  t)

(defun pointer-event (kind x y &key button (buttons 0) (clicks 0))
  (make-pointer-event :timestamp 0 :kind kind :x (float x 1d0) :y (float y 1d0) :button button
                      :buttons buttons :click-count clicks))

(defun app-pane (constructor &key (id (required-id 'app-pane)) (zoom 1) width height (flex 0)
                               min-width min-height max-width max-height)
  "The app that CONSTRUCTOR (a function, or the name of one, as OPEN-APP takes) makes, working, in
an area of this window, at ZOOM (1 as it is; 2 twice as large). It is made the first time and kept
by the pane's id, and made again if CONSTRUCTOR is another. The pointer and the wheel over the area,
and the keys while it has the keyboard (a press gives it), are the inner app's, in its own points.
WIDTH and HEIGHT fix the area's size, as a container's; with neither, give it a FLEX. Returns the
inner app (for a tool that looks at it, as the inspector does). The :id is required."
  (let* ((ui *ui*)
         (box (open-box ui :canvas :id id))
         (key (box-key box))
         (state (state-of ui key #'make-app-pane-state))
         (rect (rect-of ui key))
         (hot (= (ui-hot ui) key))
         (zoom (float zoom 1f0))
         (outer (ui-context ui))
         (moved nil))
    (set-size-limits box width height min-width min-height max-width max-height)
    (setf (box-interactive box) t
          (box-focusable box) t
          (box-moves box) t
          (box-pref-w box) (float (or width 0) 1f0)
          (box-pref-h box) (float (or height 0) 1f0)
          (box-flex box) (float flex 1f0))
    ;; The inner app, made the first time and when it is another, with a store of its own; and made
    ;; again, keeping what it kept, when the window's app has been made again.
    (unless (and (app-pane-state-instance state)
                 (eq constructor (app-pane-state-constructor state)))
      (setf (app-pane-state-constructor state) constructor
            (app-pane-state-kept state) (make-kept-state)
            (app-pane-state-made-again state) (ui-made-again ui)
            (app-pane-state-instance state) (make-inner state)
            (gethash (app-pane-state-instance state) *pane-states*) state
            (app-pane-state-context state)
            (make-pane-context :outer outer
                               :requester (lambda (context)
                                            (setf (context-frame-requested context) t)
                                            (request-frame (pane-context-outer context))))
            moved t))
    (when (/= (app-pane-state-views-version state) (views-version))
      (setf (app-pane-state-views-version state) (views-version)
            moved t))
    (when (/= (app-pane-state-made-again state) (ui-made-again ui))
      (setf (app-pane-state-made-again state) (ui-made-again ui))
      (when (make-inner-again state id)
        (setf moved t)))
    (let ((inner (app-pane-state-context state)))
      ;; The window's, which the inner app follows, and its size, the pane's in its points.
      (setf (pane-context-outer inner) outer
            (pane-context-zoom inner) zoom
            (context-scale inner) (context-scale outer)
            (context-appearance inner) (context-appearance outer)
            (context-window inner) (context-window outer))
      (when rect
        (let ((w (/ (rect-w rect) zoom)) (h (/ (rect-h rect) zoom)))
          (unless (and (= w (almighty-bolt/app-instance:context-width inner))
                       (= h (almighty-bolt/app-instance:context-height inner))
                       (= zoom (app-pane-state-zoom state)))
            (setf (almighty-bolt/app-instance:context-width inner) w
                  (almighty-bolt/app-instance:context-height inner) h
                  (app-pane-state-zoom state) zoom
                  moved t)))
        ;; The pointer, in the inner app's points.
        (let ((x (/ (- (ui-pointer-x ui) (rect-x rect)) zoom))
              (y (/ (- (ui-pointer-y ui) (rect-y rect)) zoom)))
          (when (and hot (ui-pressed ui))
            (consume-press ui)
            (setf (ui-active ui) key)
            (focus-by-click ui key)
            (setf moved (deliver state (pointer-event :down x y :button 0 :buttons 1
                                                                :clicks (ui-click-count ui)))))
          (when (and hot (consume-secondary-press ui))
            (deliver state (pointer-event :down x y :button 1 :buttons 2 :clicks 1))
            (setf moved (deliver state (pointer-event :up x y :button 1))))
          (when (and (ui-moved ui) (or hot (= (ui-active ui) key)))
            (setf (ui-moved ui) nil
                  moved (deliver state (pointer-event :move x y
                                                      :buttons (if (and (= (ui-active ui) key)
                                                                        (ui-down ui))
                                                                   1 0)))))
          (when (and (= (ui-active ui) key) (ui-released ui))
            (consume-release ui)
            (setf (ui-active ui) 0
                  moved (deliver state (pointer-event :up x y :button 0))))
          (when hot
            (multiple-value-bind (dx dy) (consume-wheel ui)
              (when (or (/= dx 0f0) (/= dy 0f0))
                (setf moved (deliver state (make-wheel-event :timestamp 0 :x (float x 1d0)
                                                             :y (float y 1d0) :dx (float dx 1d0)
                                                             :dy (float dy 1d0)))))))))
      (when (and (app-pane-state-was-hot state) (not hot))
        (setf moved (deliver state (pointer-event :leave -1 -1))))
      (setf (app-pane-state-was-hot state) hot)
      ;; The keys, while the pane has the keyboard: those no widget of this window took.
      (when (= (ui-focus ui) key)
        (dolist (event (copy-list (ui-inputs ui)))
          (typecase event
            (key-event
             (when (member event (ui-keys ui) :test #'eq)
               (consume-key ui event)
               (consume-input ui event)
               (setf moved (deliver state event))))
            ((or text-input-event composition-event)
             (consume-input ui event)
             (when (typep event 'composition-event)
               (setf (app-pane-state-composing state)
                     (not (eq (composition-event-kind event) :committed))))
             (setf moved (deliver state event))))))
      ;; The keyboard went elsewhere while the inner app was composing: the window's input method
      ;; drops what it composed, and so does the inner app.
      (when (and (app-pane-state-composing state) (/= (ui-focus ui) key))
        (setf (app-pane-state-composing state) nil
              moved (deliver state (make-composition-event :timestamp 0 :kind :committed
                                                           :text ""))))
      ;; Its frame, when something happened to it or it asked for one; then made ZOOM times larger.
      ;; Not before the pane has been laid out, when its size is not known (its first frame), but in
      ;; the window's next, which it asks for.
      (if rect
          (when (or moved (context-frame-requested inner))
            (settle-pane state)
            (setf (ui-consumed ui) t))
          (request-frame outer))
      (when hot
        (want-cursor ui key (context-cursor inner)))
      ;; What takes text in the inner app takes it in the window, with its caret where the pane
      ;; shows it.
      (when (and rect (= (ui-focus ui) key) (context-text-input-p inner))
        (let ((caret (context-text-input-rect inner)))
          (set-text-input-caret outer
                                (+ (rect-x rect) (* zoom (aref caret 0)))
                                (+ (rect-y rect) (* zoom (aref caret 1)))
                                (* zoom (aref caret 2))
                                (* zoom (aref caret 3))))))
    (let ((shown (app-pane-state-shown state)))
      (setf (box-data box) shown
            (box-n3 box) (display-list-hash shown)
            (box-look box) (look-of ui key t)))
    (close-box ui)
    (app-pane-state-instance state)))
