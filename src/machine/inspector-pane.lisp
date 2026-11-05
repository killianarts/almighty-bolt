(defpackage #:almighty-bolt/machine/inspector-pane
  (:use #:cl)
  (:import-from #:almighty-bolt/app-instance
                #:handle-event
                #:request-frame)
  (:import-from #:almighty-bolt/ui/ui
                #:*ui*
                #:open-box
                #:close-box
                #:state-of
                #:ui-context)
  (:import-from #:almighty-bolt/ui/box
                #:box-key
                #:box-cross-align
                #:box-fill
                #:box-flex)
  (:import-from #:almighty-bolt/ui/widgets
                #:required-id
                #:label)
  (:import-from #:almighty-bolt/ui/inspector
                #:make-inspector)
  (:import-from #:almighty-bolt/ui/app-pane
                #:settle-app-pane)
  (:import-from #:almighty-bolt/machine
                #:make-session
                #:perform-actions)
  (:export #:inspector-pane))

(in-package #:almighty-bolt/machine/inspector-pane)

;;; The inspector, in a pane of the window, looking at an app in an app pane of the same window: a
;;; designer's studio has the app being made and the inspector of it side by side. It is the
;;; inspector of a window of its own (see inspector.lisp), given the app in the pane, and doing what
;;; it asks there, by the machine interface, at the start of the next frame: an action waits for the
;;; app between its steps (a dropdown's menu opens before its row is pressed), and an app in a pane
;;; is settled for it then, in this window's thread.

(defstruct (inspector-pane-state (:constructor make-inspector-pane-state ()))
  ;; The app looked at, the function that describes the inspector, and what it asked, in order:
  ;; each an action and the function that is told what went wrong.
  (target nil)
  (describe nil)
  (asked '())
  ;; What Edit in the View, and Edit Source and Edit Wiring, call, as the frame gives them.
  (edit-view nil)
  (edit-source nil))

(defun do-asked (state)
  "Do what the inspector asked, in order, to the app in the pane."
  (let ((target (inspector-pane-state-target state)))
    (loop :while (inspector-pane-state-asked state)
          :do (destructuring-bind (action . report) (pop (inspector-pane-state-asked state))
                (when target
                  (let* ((session (make-session :app (lambda () target)
                                                :deliver (lambda (event)
                                                           (handle-event target event))
                                                :settle (lambda () (settle-app-pane target))
                                                :on-main #'funcall))
                         (message (handler-case (progn (perform-actions session (list action)) nil)
                                    (error (condition) (princ-to-string condition)))))
                    (when message
                      (funcall report message))))))))

(defun inspector-pane (target &key (id (required-id 'inspector-pane)) (flex 1) edit-view
                                  edit-source)
  "The inspector, here, of TARGET: an app in an app pane of this window, as APP-PANE returns it, or
NIL (it then says there is nothing to inspect). Pick, then a press on the app in the pane, says what
the widget there is, and the inspector changes its value and its theme as it does in a window of its
own. For a widget a view drew it says which view and widget, and what the code gives it; EDIT-VIEW,
if given, is called with the view's name and the widget's id when Edit in the View is pressed;
EDIT-SOURCE, if given, with what to show (:view, :wiring or :code) and the widget's facts (a property
list: :view, :view-widget, :id, :kind ...) when Edit Source or Edit Wiring is, and returns NIL or what
went wrong. The :id is required: what it picked is kept by it."
  (let* ((ui *ui*)
         (box (open-box ui :column :id id))
         (state (state-of ui (box-key box) #'make-inspector-pane-state)))
    (setf (box-cross-align box) :stretch
          (box-fill box) t
          (box-flex box) (float flex 1f0)
          (inspector-pane-state-target state) target
          (inspector-pane-state-edit-view state) edit-view
          (inspector-pane-state-edit-source state) edit-source)
    (unless (inspector-pane-state-describe state)
      (setf (inspector-pane-state-describe state)
            (make-inspector (lambda () (inspector-pane-state-target state))
                            (lambda (action report)
                              (setf (inspector-pane-state-asked state)
                                    (append (inspector-pane-state-asked state)
                                            (list (cons action report))))
                              (request-frame (ui-context ui)))
                            :edit-view (lambda (name widget)
                                         (let ((edit (inspector-pane-state-edit-view state)))
                                           (when edit (funcall edit name widget))))
                            :edit-source (lambda (what facts)
                                           (let ((edit (inspector-pane-state-edit-source state)))
                                             (and edit (funcall edit what facts)))))))
    (do-asked state)
    (if target
        (funcall (inspector-pane-state-describe state))
        (label "Nothing to inspect: no app is running in a pane." :id :nothing))
    (close-box ui)
    nil))
