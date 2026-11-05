(defpackage #:almighty-bolt-studio/title-bar
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:studio-theme #:almighty-bolt-studio/theme)
                    (#:chrome #:almighty-bolt-studio/chrome))
  (:export #:title-bar
           #:+modes+
           #:breadcrumb))

(in-package #:almighty-bolt-studio/title-bar)

;;; The studio's title bar, as the prototype draws it: the studio's name; the modes, each a dock of
;;; its own; where the person is (the view, the app and the file); the image the studio runs in;
;;; and Save and Run Window. Its words are in the theme's label font, in capitals, as the
;;; prototype's are.

(defparameter +modes+
  '((:design "Design") (:wire "Wire") (:debug "Debug") (:theme "Theme") (:agent "Agent"))
  "The studio's modes, in the order of their tabs, each with its tab's words.")

(defun short-package (name)
  "The last part of the package's NAME, after its last slash: a package named after its file
(my-app/views/login) by the file's name."
  (subseq name (1+ (or (position #\/ name :from-end t) -1))))

(defun breadcrumb (about app-name)
  "The parts of where the person is: the view, the app (the package of the app being run, by its
name, if there is one, or else the view's), and the view's file, as ABOUT (the GUI designer's)
says. A view not saved has no file."
  (let ((file (getf about :file)))
    (list "View"
          (getf about :name)
          (short-package (if (plusp (length app-name))
                             (subseq app-name 0 (or (position #\: app-name) (length app-name)))
                             (getf about :package)))
          (if file (file-namestring file) "not saved"))))

(defun title-bar (mode about app-name runnable)
  "Draw the title bar in MODE (one of +MODES+), for what ABOUT (the GUI designer's) says is being
designed and the app run by APP-NAME. Run Window is enabled if RUNNABLE. Returns the mode now, and
whether Save and whether Run Window was pressed."
  (let ((chosen mode) (save nil) (run nil))
    (bolt:column (:id :studio-title :cross-align :stretch)
      (bolt:panel (:id :title-bar :color :background :radius 0 :left 16 :right 12 :height 44)
        (bolt:row (:id :title-row :gap 12 :cross-align :center :fill t)
          (bolt:row (:id :brand :gap 8 :cross-align :center)
            (bolt:label "ALMIGHTY-BOLT STUDIO" :id :studio-name :font '(:label :size 10 :weight :bold))
            (bolt:label "スタジオ" :id :studio-kana :font chrome:+small+ :color :dim-text))
          (multiple-value-bind (index changed)
              (bolt:tabs (mapcar (lambda (m) (chrome:caps (second m))) +modes+)
                         (position mode +modes+ :key #'first)
                         :id :modes :font chrome:+small+)
            (when changed
              (setf chosen (first (nth index +modes+)))))
          (bolt:spacer)
          (chrome:words :where (breadcrumb about app-name))
          (bolt:spacer)
          ;; The Lisp the studio runs in; the dot is green while a view is live in it (an edit
          ;; changes the running app as it is made), which its help says in words.
          (bolt:row (:id :image-state :gap 6 :cross-align :center)
            (bolt:panel (:id :live-dot :radius 4 :width 8 :height 8
                         :color (if (getf about :live) (studio-theme:tool-color :ok) (studio-theme:tool-color :line-strong))))
            (chrome:words :lisp (list (format nil "~a ~a" (lisp-implementation-type)
                                              (lisp-implementation-version)))
                          :color :text
                          :tooltip (if (getf about :live)
                                       (format nil "The view ~(~a~) is live in this image: an edit changes ~
                                             the running app as it is made."
                                               (getf about :live))
                                       "No view is live in this image.")))
          (when (bolt:pressable (:id :title-save :all 8 :radius 4 :border-color :field-border
                                 :tooltip "Save the view")
                  (bolt:label "SAVE" :id :title-save-words :font :button))
            (setf save t))
          (when (bolt:button "RUN WINDOW" :id :run-window :enabled runnable)
            (setf run t))))
      ;; The line under it.
      (bolt:panel (:id :title-rule :color (studio-theme:tool-color :line) :radius 0 :height 1)))
    (values chosen save run)))
