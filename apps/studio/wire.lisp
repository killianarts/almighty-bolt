(defpackage #:almighty-bolt-studio/wire
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:chrome #:almighty-bolt-studio/chrome))
  (:export #:views-list
           #:state-list))

(in-package #:almighty-bolt-studio/wire)

;;; Wire's left pane, as the prototype draws it: the views the image has, each with how many of its
;;; widgets the code wires, the one open in the designer chosen; and under them the state of the app
;;; on the canvas, each variable its STATE-LET keeps, live, to edit and to pin.

(defun views-list (views open)
  "VIEWS (the GUI designer's :views: each a name and how many of its widgets are wired), each a row
to press, OPEN (the name of the view open, or NIL) chosen. Returns the name of the one pressed on
the frame it is, or NIL."
  (let ((pressed nil))
    (bolt:column (:id :views-list :gap 2 :fill t :cross-align :stretch)
      (chrome:section-header :views-heading "Views" (length views))
      (loop :for (name count) :in views
            :do (when (bolt:pressable (:id (list :view-row name) :selected (eq name open)
                                       :radius 0 :left 8 :right 8 :top 4 :bottom 4
                                       :name (format nil "View ~(~a~), ~d wire~:p" name count)
                                       :tooltip (format nil "~(~s~): ~d of its widgets wired. ~
                                                             Press to open it."
                                                        name count))
                      (bolt:row (:id :view-row-parts :gap 8 :cross-align :center :fill t)
                        (bolt:label "VIEW" :id :view-kind :font chrome:+small+ :color :dim-text)
                        (bolt:label (string-downcase (symbol-name name)) :id :view-name :flex 1)
                        (bolt:label (princ-to-string count) :id :view-wires :font chrome:+small+
                                                                             :color :dim-text)))
                  (setf pressed name))))
    pressed))


(defun read-text (text package)
  "The value TEXT says, read in PACKAGE with no evaluation, and true; or NIL and NIL if it is not
one whole form."
  (handler-case
      (let ((*package* package) (*read-eval* nil))
        (multiple-value-bind (value end) (read-from-string text)
          (if (every (lambda (c) (member c '(#\Space #\Tab #\Newline))) (subseq text end))
              (values value t)
              (values nil nil))))
    (error () (values nil nil))))

(defun readable-text (value package)
  "VALUE printed so that it reads back, in PACKAGE, with no evaluation, or NIL if it does not print
so: the text is read again to see (SBCL prints a hash table readably as #.(...), which only reads
by evaluating it)."
  (let ((text (handler-case (let ((*package* package) (*print-readably* t))
                              (prin1-to-string value))
                (error () nil))))
    (and text (nth-value 1 (read-text text package)) text)))

(defun state-list (app drafts)
  "The variables APP (the app running on the canvas, or NIL) keeps with STATE-LET, each its name, its
value, live, in a field, and a pin. A value typed there and entered with Return is the variable's:
the app shows it. DRAFTS, a table the caller keeps, holds what is typed until then. A pinned
variable keeps its value when the app is made again even if its first value was edited."
  (let ((variables (bolt:app-pane-variables app)))
    (bolt:column (:id :state-list :gap 6 :fill t :cross-align :stretch)
      (chrome:section-header :state-heading "State" (and app (length variables)))
      (cond ((null app)
             (bolt:paragraph "No app is running on the canvas: name one in its bar, and pick Live, to see its state here."
                             :id :state-note :color :dim-text))
            ((null variables)
             (bolt:paragraph "The app on the canvas keeps no state with STATE-LET."
                             :id :state-note :color :dim-text))
            (t
             (dolist (variable variables)
               (destructuring-bind (&key name occurrence value pinned &allow-other-keys) variable
                 (let* ((package (or (symbol-package name) *package*))
                        (key (list name occurrence))
                        (printed (readable-text value package))
                        (draft (gethash key drafts)))
                   (bolt:row (:id (list :state-row name occurrence) :gap 8 :cross-align :center
                              :fill t)
                     (bolt:label (format nil "~a~@[ ~d~]" (symbol-name name)
                                         (and (> occurrence 1) occurrence))
                                 :id :state-name :font chrome:+small+ :color :dim-text)
                     (if printed
                         (multiple-value-bind (text edited entered)
                             (bolt:text-field (or draft printed) :id :state-value :width 80 :flex 1
                                                                 :labelled-by :state-name
                                                                 :font :code
                                                                 :invalid (and draft
                                                                               (not (nth-value
                                                                                     1 (read-text draft package)))))
                           (when edited
                             (if (string= text printed)
                                 (remhash key drafts)
                                 (setf (gethash key drafts) text)))
                           (when entered
                             (multiple-value-bind (new ok) (read-text text package)
                               (when ok
                                 (remhash key drafts)
                                 (bolt:set-app-pane-variable app name new :occurrence occurrence)))))
                         ;; What does not print to be read back is shown, not edited.
                         (bolt:label (let ((*package* package) (*print-length* 8) (*print-level* 3))
                                       (prin1-to-string value))
                                     :id :state-value :flex 1 :font :code))
                     (when (bolt:pressable (:id :state-pin :selected pinned :radius 4 :all 4
                                            :name (format nil "Pin ~(~a~)" name)
                                            :tooltip "Pinned, it keeps its value when the app is made again, even if its first value was edited.")
                             (bolt:label "PIN" :id :state-pin-words :font chrome:+small+))
                       (bolt:pin-app-pane-variable app name (not pinned) :occurrence occurrence))))))
             (bolt:paragraph "Live, from the running app. Type a value and press Return, and the app shows it; pin one to keep it when its first value is edited."
                             :id :state-note :color :dim-text))))))
