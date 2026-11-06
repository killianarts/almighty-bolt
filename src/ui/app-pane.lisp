(defpackage #:almighty-bolt/ui/app-pane
  (:use #:cl)
  (:local-nicknames (#:event #:almighty-bolt/event)
                    (#:dl #:almighty-bolt/display-list)
                    (#:app-instance #:almighty-bolt/app-instance)
                    (#:kept #:almighty-bolt/kept)
                    (#:settle #:almighty-bolt/settle)
                    (#:hash #:almighty-bolt/ui/hash)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:widgets #:almighty-bolt/ui/widgets)
                    (#:inspection #:almighty-bolt/ui/inspection)
                    (#:immediate #:almighty-bolt/ui/immediate)
                    (#:views #:almighty-bolt/ui/views))
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
;;; The pane is kept by its id wherever in the window it is described (a pane of one dock, then of
;;; another), so the inner app is one app however the window is arranged. In design mode the inner
;;; app is shown and given nothing: a press on it picks the widget of a view it is on, and the pane
;;; outlines that widget and the one the pointer is over, as a design surface does.
;;; What the inner app's screen means (its semantic tree) is inside the pane's, where the pane
;;; shows it, for a screen reader and the machine interface (see APP-PANE-INSIDE).

;;; --- the inner app's context ---

(defstruct (pane-context (:include app-instance:context))
  "The context of an app in a pane: as large as the pane, its text measured and drawn by OUTER's,
at ZOOM."
  (outer nil)
  (zoom 1f0 :type single-float))

(defstruct (zoomed-font (:constructor zoomed-font (base zoomed)))
  "A font of a pane's context: BASE, the outer context's at the size asked for, which measures, and
ZOOMED, the same at the pane's zoom, which draws."
  base zoomed)

(defmethod app-instance:context-font ((context pane-context) name size &optional weight tracking)
  (let* ((outer (pane-context-outer context))
         (zoom (pane-context-zoom context))
         (tracking (or tracking 0))
         (base (app-instance:context-font outer name size weight tracking)))
    ;; Drawn ZOOM times larger, the space after each character too.
    (zoomed-font base (if (= zoom 1f0)
                          base
                          (app-instance:context-font outer name (* size zoom) weight (* tracking zoom))))))

(defmethod app-instance:context-measure ((context pane-context) font string)
  (app-instance:context-measure (pane-context-outer context) (zoomed-font-base font) string))

(defmethod app-instance:context-break-lines ((context pane-context) font string width)
  (app-instance:context-break-lines (pane-context-outer context) (zoomed-font-base font) string width))

(defmethod app-instance:context-caret-x ((context pane-context) font string index)
  (app-instance:context-caret-x (pane-context-outer context) (zoomed-font-base font) string index))

(defmethod app-instance:context-index-at-x ((context pane-context) font string x)
  (app-instance:context-index-at-x (pane-context-outer context) (zoomed-font-base font) string x))

(defmethod app-instance:context-caret-rect ((context pane-context) font string index)
  (app-instance:context-caret-rect (pane-context-outer context) (zoomed-font-base font) string index))

(defmethod app-instance:context-push-text ((context pane-context) list font string x baseline color
                                           &optional turn)
  ;; Turned or not, the glyphs are drawn larger about the point the text starts at, X, BASELINE.
  (let ((start (dl:display-list-count list))
        (zoom (pane-context-zoom context)))
    (multiple-value-prog1
        (app-instance:context-push-text (pane-context-outer context) list (zoomed-font-zoomed font) string x
                                        baseline color turn)
      (unless (= zoom 1f0)
        (dl:unzoom-glyphs list start (float x 1f0) (float baseline 1f0) zoom)))))

(defmethod app-instance:context-push-image ((context pane-context) list image x y width height &key tint (opacity 1))
  ;; The image is drawn where the app says, in its points; the pane makes the whole list larger.
  (app-instance:context-push-image (pane-context-outer context) list image x y width height
                                   :tint tint :opacity opacity))

(defmethod app-instance:context-forget-glyphs ((context pane-context))
  (app-instance:context-forget-glyphs (pane-context-outer context)))

(defmethod app-instance:context-clipboard-text ((context pane-context))
  (app-instance:context-clipboard-text (pane-context-outer context)))

(defmethod app-instance:context-set-clipboard-text ((context pane-context) string)
  (app-instance:context-set-clipboard-text (pane-context-outer context) string))

(defmethod app-instance:context-choose-file ((context pane-context) kind types name callback)
  (app-instance:context-choose-file (pane-context-outer context) kind types name callback))

;;; --- the pane ---

(defstruct (app-pane-state (:constructor make-app-pane-state ()))
  (constructor nil)
  (instance nil)
  ;; What the inner app's STATE-LETs keep, from one time it is made to the next.
  (kept (kept:make-kept-state))
  ;; How many times the window's app had been made again when the inner app was last made.
  (made-again 0 :type fixnum)
  ;; How many times views had been defined when the inner app last drew: one defined since (in a
  ;; designer, as it is edited) is drawn again.
  (views-version 0 :type fixnum)
  (context nil)
  ;; What the inner app drew, in its points, and that made ZOOM times larger, which is shown.
  (list (dl:make-display-list :capacity 4096))
  (shown (dl:make-display-list :capacity 4096))
  (zoom 0f0 :type single-float)
  (was-hot nil)
  ;; Whether the input method is composing in the inner app: it began and has not been committed.
  (composing nil))

(defvar *pane-states* (views:make-weak-table)
  "The state of the pane each app in a pane is in, by the app.")

(defun settle-pane (state)
  "Settle the frame of STATE's app, and make what it drew ZOOM times larger to show."
  (let ((inner (app-pane-state-context state)))
    (setf (app-instance:context-frame-requested inner) nil)
    (loop
      (handler-case
          (progn (settle:settle-frame (app-pane-state-instance state) inner (app-pane-state-list state))
                 (dl:zoom-display-list (app-pane-state-shown state) (app-pane-state-list state)
                                       (pane-context-zoom inner))
                 (return))
        (dl:display-list-full ()
          (let ((capacity (* 2 (dl:display-list-capacity (app-pane-state-list state)))))
            (setf (app-pane-state-list state) (dl:make-display-list :capacity capacity)
                  (app-pane-state-shown state) (dl:make-display-list :capacity capacity))))))))

(defun settle-app-pane (instance)
  "Settle the frame of INSTANCE, an app in an app pane, now: for a tool that works it between
frames (the inspector's actions) and must see what each step did. The window shows it at its next
frame."
  (let ((state (gethash instance *pane-states*)))
    (when state
      (settle-pane state)
      (app-instance:request-frame (pane-context-outer (app-pane-state-context state))))))

(defun app-pane-inside (ui key)
  "If the box KEY of UI's last frame is an app pane, the app in it, its zoom, and whether the pane
has the keyboard; otherwise NIL."
  (let ((state (gethash key (ui:ui-states ui))))
    (when (and (app-pane-state-p state) (app-pane-state-instance state))
      (values (app-pane-state-instance state)
              (pane-context-zoom (app-pane-state-context state))
              (= (ui:ui-focus ui) key)))))

(defun make-inner (state)
  "The app that STATE's constructor makes, with STATE's store for what it keeps."
  (kept:with-kept-state ((app-pane-state-kept state))
    (immediate:make-immediate-app (funcall (app-pane-state-constructor state)))))

(defun make-inner-again (state id)
  "Make STATE's app again from its constructor, in place of the one it has, which hands on what it
kept (INHERIT-STATE). If that fails, the one it has stays, and what went wrong is said."
  (let ((old (app-pane-state-instance state)))
    (handler-case
        (let ((new (make-inner state)))
          (handler-case (app-instance:inherit-state new old)
            (error (condition)
              (app-instance:retire-app new)
              (error condition)))
          (remhash old *pane-states*)
          (setf (app-pane-state-instance state) new
                (gethash new *pane-states*) state)
          (app-instance:retire-app old)
          t)
      (error (condition)
        (format *error-output* "~&Hot reload: the app in the pane ~s could not be made again, so ~
                                the old one stays: ~a~%" id condition)
        (finish-output *error-output*)
        nil))))

(defun deliver (state event)
  "Give EVENT to the inner app."
  (app-instance:handle-event (app-pane-state-instance state) event)
  t)

(defun pointer-event (kind x y &key button (buttons 0) (clicks 0))
  (event:make-pointer-event :timestamp 0 :kind kind :x (float x 1d0) :y (float y 1d0) :button button
                            :buttons buttons :click-count clicks))

;;; --- designing in it ---

(defun inner-ui (state)
  "The ui of STATE's app, if it is an immediate app whose last frame did not fail, or NIL."
  (let ((instance (app-pane-state-instance state)))
    (and (typep instance 'immediate:immediate-app)
         (null (immediate:immediate-app-failure instance))
         (immediate:immediate-app-ui instance))))

(defun widget-box (box name id)
  "The box that is the widget ID of the view NAME, as VIEW-WIDGET found it from BOX: BOX or one it
is in, the view's outermost if none has the id (the code gave it another)."
  (let ((outermost box))
    (loop :for at = box :then (box:box-parent at)
          :while (and at (eq (box:box-view at) name))
          :do (when (eq (box:box-id at) id)
                (return-from widget-box at))
              (setf outermost at))
    outermost))

(defun view-widget-at (ui x y)
  "The widget of a view under X, Y (in the inner app's points) in UI's last frame: the view's name,
the widget's id and where its box is (x, y, width and height), or NIL."
  (let ((box (inspection:box-at ui x y)))
    (when box
      (multiple-value-bind (name id) (views:view-widget box)
        (when name
          (let ((widget (widget-box box name id)))
            (multiple-value-bind (found wx wy) (inspection:box-by-key ui (box:box-key widget))
              (when found
                (values name id (list wx wy (box:box-w widget) (box:box-h widget)))))))))))

(defun view-widget-place (ui name id)
  "Where the widget ID of the view NAME is in UI's last frame (x, y, width and height), or NIL."
  (inspection:walk-frame ui (lambda (box x y left top right bottom)
                              (declare (ignore left top right bottom))
                              (when (and (eq (box:box-view box) name) (eql (box:box-id box) id))
                                (return-from view-widget-place
                                  (list x y (box:box-w box) (box:box-h box))))))
  nil)

(defun outline-widget (ui owner id place rect zoom color)
  "A frame of COLOR round PLACE (in the inner app's points), where the pane at RECT shows it: one of
OWNER's floats, called ID."
  (when place
    (destructuring-bind (x y w h) place
      (multiple-value-bind (float open)
          (ui:open-float ui :outline owner id
                         (ui:make-float-spec :placement :at :x (+ (ui:rect-x rect) (* x zoom))
                                             :y (+ (ui:rect-y rect) (* y zoom)) :tool t))
        (setf (box:box-pref-w float) (float (* w zoom) 1f0)
              (box:box-pref-h float) (float (* h zoom) 1f0)
              (box:box-color float) color)
        (ui:close-float ui open)))))

(defparameter +pane-root+ (hash:hash-of :app-pane)
  "What an app pane's key is made from in place of its parent's, so that it is kept by its id
wherever in the window it is described.")

(defun app-pane (constructor &key (id (widgets:required-id 'app-pane)) (zoom 1) width height (flex 0)
                               min-width min-height max-width max-height (mode :run) selected)
  "The app that CONSTRUCTOR (a function, or the name of one, as OPEN-APP takes) makes, working, in
an area of this window, at ZOOM (1 as it is; 2 twice as large). It is made the first time and kept
by the pane's id, wherever in the window the pane is (two app panes of a window have two ids), and
made again if CONSTRUCTOR is another. The pointer and the wheel over the area, and the keys while it
has the keyboard (a press gives it), are the inner app's, in its own points. WIDTH and HEIGHT fix
the area's size, as a container's; with neither, give it a FLEX.

With MODE :design the inner app is given nothing: it is shown as it is, and a press on it picks the
widget of a view (DEFINE-VIEW) it is on. The pane outlines that widget and the one the pointer is
over; SELECTED, a list of a view's name and a widget's id, is outlined as chosen.

Returns the inner app (for a tool that looks at it, as the inspector does), and, on the frame of a
press in design mode on a widget of a view, the view's name and the widget's id. The :id is
required."
  (let* ((ui ui:*ui*)
         (box (ui:open-box ui :canvas :id id :rooted-at +pane-root+))
         (key (box:box-key box))
         (state (ui:state-of ui key #'make-app-pane-state))
         (rect (ui:rect-of ui key))
         (hot (= (ui:ui-hot ui) key))
         (design (eq mode :design))
         (zoom (float zoom 1f0))
         (outer (ui:ui-context ui))
         (moved nil)
         (picked-view nil)
         (picked-id nil)
         (hover nil))
    (widgets:set-size-limits box width height min-width min-height max-width max-height)
    (setf (box:box-interactive box) t
          (box:box-focusable box) (not design)
          (box:box-moves box) t
          (box:box-pref-w box) (float (or width 0) 1f0)
          (box:box-pref-h box) (float (or height 0) 1f0)
          (box:box-flex box) (float flex 1f0))
    ;; The inner app, made the first time and when it is another, with a store of its own; and made
    ;; again, keeping what it kept, when the window's app has been made again.
    (unless (and (app-pane-state-instance state)
                 (eq constructor (app-pane-state-constructor state)))
      (setf (app-pane-state-constructor state) constructor
            (app-pane-state-kept state) (kept:make-kept-state)
            (app-pane-state-made-again state) (ui:ui-made-again ui)
            (app-pane-state-instance state) (make-inner state)
            (gethash (app-pane-state-instance state) *pane-states*) state
            (app-pane-state-context state)
            (make-pane-context :outer outer
                               :requester (lambda (context)
                                            (setf (app-instance:context-frame-requested context) t)
                                            (app-instance:request-frame (pane-context-outer context))))
            moved t))
    (when (/= (app-pane-state-views-version state) (views:views-version))
      (setf (app-pane-state-views-version state) (views:views-version)
            moved t))
    (when (/= (app-pane-state-made-again state) (ui:ui-made-again ui))
      (setf (app-pane-state-made-again state) (ui:ui-made-again ui))
      (when (make-inner-again state id)
        (setf moved t)))
    (let ((inner (app-pane-state-context state)))
      ;; The system gone dark or light: the inner app is drawn again, in its new theme.
      (unless (eq (app-instance:context-appearance inner) (app-instance:context-appearance outer))
        (setf moved t))
      ;; The window's, which the inner app follows, and its size, the pane's in its points.
      (setf (pane-context-outer inner) outer
            (pane-context-zoom inner) zoom
            (app-instance:context-scale inner) (app-instance:context-scale outer)
            (app-instance:context-appearance inner) (app-instance:context-appearance outer)
            (app-instance:context-window inner) (app-instance:context-window outer))
      (when rect
        (let ((w (/ (ui:rect-w rect) zoom)) (h (/ (ui:rect-h rect) zoom)))
          (unless (and (= w (app-instance:context-width inner))
                       (= h (app-instance:context-height inner))
                       (= zoom (app-pane-state-zoom state)))
            (setf (app-instance:context-width inner) w
                  (app-instance:context-height inner) h
                  (app-pane-state-zoom state) zoom
                  moved t)))
        ;; The pointer, in the inner app's points.
        (let ((x (/ (- (ui:ui-pointer-x ui) (ui:rect-x rect)) zoom))
              (y (/ (- (ui:ui-pointer-y ui) (ui:rect-y rect)) zoom)))
          ;; Designing: what is under the pointer is found in what the inner app last drew, and a
          ;; press picks it; the inner app has none of it.
          (when (and design hot)
            (let ((inner-ui (inner-ui state)))
              (when inner-ui
                (multiple-value-bind (name widget place) (view-widget-at inner-ui x y)
                  (setf hover (and name
                                   (not (and (eq name (first selected))
                                             (eql widget (second selected))))
                                   place))
                  (when (ui:ui-pressed ui)
                    (ui:consume-press ui)
                    (setf picked-view name
                          picked-id widget))))))
          (when (and (not design) hot (ui:ui-pressed ui))
            (ui:consume-press ui)
            (setf (ui:ui-active ui) key)
            (ui:focus-by-click ui key)
            (setf moved (deliver state (pointer-event :down x y :button 0 :buttons 1
                                                                :clicks (ui:ui-click-count ui)))))
          (when (and (not design) hot (ui:consume-secondary-press ui))
            (deliver state (pointer-event :down x y :button 1 :buttons 2 :clicks 1))
            (setf moved (deliver state (pointer-event :up x y :button 1))))
          (when (and (not design) (ui:ui-moved ui) (or hot (= (ui:ui-active ui) key)))
            (setf (ui:ui-moved ui) nil
                  moved (deliver state (pointer-event :move x y
                                                      :buttons (if (and (= (ui:ui-active ui) key)
                                                                        (ui:ui-down ui))
                                                                   1 0)))))
          (when (and (= (ui:ui-active ui) key) (ui:ui-released ui))
            (ui:consume-release ui)
            (setf (ui:ui-active ui) 0
                  moved (deliver state (pointer-event :up x y :button 0))))
          (when (and (not design) hot)
            (multiple-value-bind (dx dy) (ui:consume-wheel ui)
              (when (or (/= dx 0f0) (/= dy 0f0))
                (setf moved (deliver state (event:make-wheel-event :timestamp 0 :x (float x 1d0)
                                                                   :y (float y 1d0) :dx (float dx 1d0)
                                                                   :dy (float dy 1d0)))))))))
      ;; The pointer gone, or the inner app no longer given it.
      (when (and (app-pane-state-was-hot state) (not (and hot (not design))))
        (setf moved (deliver state (pointer-event :leave -1 -1))))
      (setf (app-pane-state-was-hot state) (and hot (not design)))
      ;; The keys, while the pane has the keyboard: those no widget of this window took.
      (when (and (not design) (= (ui:ui-focus ui) key))
        (dolist (event (copy-list (ui:ui-inputs ui)))
          (typecase event
            (event:key-event
             (when (member event (ui:ui-keys ui) :test #'eq)
               (ui:consume-key ui event)
               (ui:consume-input ui event)
               (setf moved (deliver state event))))
            ((or event:text-input-event event:composition-event)
             (ui:consume-input ui event)
             (when (typep event 'event:composition-event)
               (setf (app-pane-state-composing state)
                     (not (eq (event:composition-event-kind event) :committed))))
             (setf moved (deliver state event))))))
      ;; The keyboard went elsewhere while the inner app was composing: the window's input method
      ;; drops what it composed, and so does the inner app.
      (when (and (app-pane-state-composing state) (/= (ui:ui-focus ui) key))
        (setf (app-pane-state-composing state) nil
              moved (deliver state (event:make-composition-event :timestamp 0 :kind :committed
                                                                 :text ""))))
      ;; Its frame, when something happened to it or it asked for one; then made ZOOM times larger.
      ;; Not before the pane has been laid out, when its size is not known (its first frame), but in
      ;; the window's next, which it asks for.
      (if rect
          (when (or moved (app-instance:context-frame-requested inner))
            (settle-pane state)
            (setf (ui:ui-consumed ui) t))
          (app-instance:request-frame outer))
      (when (and hot (not design))
        (ui:want-cursor ui key (app-instance:context-cursor inner)))
      ;; What takes text in the inner app takes it in the window, with its caret where the pane
      ;; shows it.
      (when (and rect (not design) (= (ui:ui-focus ui) key)
                 (app-instance:context-text-input-p inner))
        (let ((caret (app-instance:context-text-input-rect inner)))
          (app-instance:set-text-input-caret outer
                                             (+ (ui:rect-x rect) (* zoom (aref caret 0)))
                                             (+ (ui:rect-y rect) (* zoom (aref caret 1)))
                                             (* zoom (aref caret 2))
                                             (* zoom (aref caret 3))))))
    (let ((shown (app-pane-state-shown state)))
      (setf (box:box-data box) shown
            (box:box-n3 box) (dl:display-list-hash shown)
            (box:box-look box) (widgets:look-of ui key t)))
    (ui:close-box ui)
    ;; Designing: what the pointer is over, and what is chosen, where the inner app drew it now.
    (when (and design rect)
      (outline-widget ui key :hover hover rect zoom (theme:theme-dim-text theme:*theme*))
      (let ((inner-ui (and selected (inner-ui state))))
        (when inner-ui
          (outline-widget ui key :selected
                          (view-widget-place inner-ui (first selected) (second selected))
                          rect zoom (theme:theme-ring theme:*theme*)))))
    (values (app-pane-state-instance state) picked-view picked-id)))
