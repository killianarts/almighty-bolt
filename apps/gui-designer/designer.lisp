(defpackage #:almighty-bolt-gui-designer
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:import-from #:almighty-bolt-gui-designer/view
                #:node-kind
                #:node-id
                #:form-node
                #:find-by-id)
  (:import-from #:almighty-bolt-gui-designer/catalogue
                #:+catalogue+
                #:entry-kind
                #:entry-title
                #:entry-options
                #:option-name
                #:find-entry
                #:node-option
                #:draw-node)
  (:export #:make-designer
           #:open-designer))

(in-package #:almighty-bolt-gui-designer)

;;; A GUI designer: the widgets it knows on the left, the view being designed in the middle, drawn
;;; with the real widgets but not working (a press there picks what it was on), and what is picked on
;;; the right. For now the view is one written here; placing widgets, changing their options, and
;;; saving the view come next. It uses the public package only.
;;;
;;;   (almighty-bolt-gui-designer:open-designer)

(defparameter +sample+
  '(:column :sign-in-form (:gap 12)
    (:label :heading (:text "Sign in" :size 22 :weight :bold))
    (:label :name-label (:text "Name"))
    (:text-field :name ())
    (:label :password-label (:text "Password"))
    (:text-field :password ())
    (:row :actions (:gap 8)
     (:button :sign-in (:text "Sign in"))
     (:button :forgot (:text "Forgot your password?"))))
  "The view the designer opens with, as a form (see FORM-NODE).")

(defun id (&rest parts)
  "A keyword made of PARTS."
  (intern (string-upcase (format nil "~{~a~^-~}" parts)) :keyword))

(defun show-value (value)
  (let ((*print-case* :downcase))
    (if (stringp value) (format nil "~s" value) (prin1-to-string value))))

(defun palette ()
  (bolt:panel (:id :palette :all 14 :radius 0)
    (bolt:column (:gap 8 :min-width 140)
      (bolt:label "Widgets" :id :palette-heading :size 17)
      (dolist (entry +catalogue+)
        (bolt:label (entry-title entry) :id (id "palette" (entry-kind entry)))))))

(defun selection (view selected)
  "What the node called SELECTED in VIEW is, and its options."
  (bolt:panel (:id :selection :all 14 :radius 0)
    (bolt:column (:gap 8 :min-width 220)
      (bolt:label "Selected" :id :selection-heading :size 17)
      (let ((node (and selected (find-by-id view selected))))
        (if (null node)
            (bolt:paragraph "Nothing. Press a widget in the view to select it."
                            :id :selected-nothing)
            (let ((entry (find-entry (node-kind node))))
              (bolt:label (format nil "~a ~(~s~)" (entry-title entry) (node-id node))
                          :id :selected-what :weight :bold)
              (dolist (option (entry-options entry))
                (let ((name (option-name option)))
                  (bolt:label (format nil "~(~a~): ~a" name (show-value (node-option node name)))
                              :id (id "selected" name))))))))))

(defun make-designer ()
  (bolt:state-let ((view (form-node +sample+))
                   (selected nil))
    (lambda ()
      (bolt:row (:id :designer :fill t :cross-align :stretch)
        (palette)
        (bolt:panel (:id :canvas-back :color :background :radius 0 :flex 1 :all 32)
          (let ((pressed (bolt:design-surface (:id :canvas :selected selected)
                           (draw-node view))))
            (when pressed
              (setf selected pressed))))
        (selection view selected)))))

(defun open-designer ()
  "Open the GUI designer in a window, from the REPL."
  (bolt:open-app 'make-designer :title "GUI designer" :width 1100 :height 640))
