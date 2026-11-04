;;;; Check, in a real window, what a screen reader gets: the to-do example, with one of each of the
;;;; other widgets beside it, is run on the main thread and read through the system's accessibility API
;;;; (AXUIElement), the one that VoiceOver and the Accessibility Inspector use, from another thread of
;;;; this process. From the repository root:
;;;;   make accessibility-smoke
;;;; It says "The accessibility check passed" when every check holds. The process has to be allowed to
;;;; use the API (System Settings, Privacy & Security, Accessibility: the terminal it was started from);
;;;; if it is not, it says that the check was skipped and why.

(asdf:load-system "almighty-bolt-examples")

(sb-alien:load-shared-object
 "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices")

(defpackage #:almighty-bolt-examples/accessibility-smoke
  (:use #:cl)
  (:import-from #:almighty-bolt
                #:run-app
                #:state-let
                #:row
                #:column
                #:stack
                #:label
                #:slider
                #:progress
                #:choice
                #:list-box
                #:canvas
                #:scrim
                #:split
                #:tabs)
  (:import-from #:almighty-bolt/machine/host-session
                #:open-session)
  (:local-nicknames (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:objc #:almighty-bolt/platform/macos/objc)
                    (#:a11y #:almighty-bolt/platform/macos/accessibility)
                    (#:view #:almighty-bolt/platform/macos/view))
  (:import-from #:almighty-bolt/internals
                #:frame-ring-total)
  (:import-from #:almighty-bolt/platform/macos/host
                #:host-ring
                #:host-view
                #:wait-for-idle
                #:close-host-window))

(in-package #:almighty-bolt-examples/accessibility-smoke)

;;; --- the app: the to-do list, and one of each of the widgets it does not have ---

(defun make-gallery ()
  (state-let ((volume 30d0) (flavour 1) (picked 2))
    (lambda ()
      (column (:gap 4)
        (label "Volume" :id :volume-label)
        (multiple-value-bind (value changed)
            (slider volume 0 100 :id :volume :labelled-by :volume-label)
          (when changed (setf volume value)))
        (progress 0.25d0 :id :progress)
        (label "Flavour" :id :flavour-label)
        (multiple-value-bind (index changed)
            (choice '("Plain" "Salted" "Sweet") flavour :id :flavour :labelled-by :flavour-label)
          (when changed (setf flavour index)))
        (multiple-value-bind (index changed)
            (list-box '("One" "Two" "Three" "Four" "Five" "Six" "Seven") picked
                      :id :numbers :rows 3)
          (when changed (setf picked index)))
        (canvas 40 20 (lambda (list context width height)
                        (declare (ignore list context width height))
                        nil)
                :id :drawing)
        (row (:gap 8)
          (split (:id :sides :size 50 :min 40 :width 160 :height 20)
            (label "Left" :id :left-words)
            (label "Right" :id :right-words))
          (tabs '("Design" "Theme") 1 :id :pages))
        (stack ()
          (label "Under" :id :under)
          (scrim :id :veil))))))

(defconstant +content-height+ 340 "The height of the window's content, which is the app's.")

(defun make-board ()
  (let ((todo (almighty-bolt-examples/todo:make-todo))
        (gallery (make-gallery)))
    (lambda ()
      (row (:gap 20)
        (funcall todo)
        (funcall gallery)))))

(defvar *failures* 0)

(defun check (what ok &optional detail)
  (unless ok (incf *failures*))
  ;; What was found, when it is not what was wanted.
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ (~a)~]~%" ok what (and (not ok) detail))
  (finish-output)
  ok)

(defun wait-for (predicate seconds)
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) end) (return nil))
      (sleep 0.01))))

;;; --- the accessibility API, as a client of it uses it ---

(defun str (object)
  (cond ((zerop object) nil)
        ((objc:msg-send :bool object "isKindOfClass:" (:id (objc:objc-class "NSString")))
         (objc:lisp-string object))
        ((objc:msg-send :bool object "isKindOfClass:" (:id (objc:objc-class "NSNumber")))
         (objc:lisp-string (objc:msg-send :id object "stringValue")))
        (t (objc:lisp-string (objc:msg-send :id object "description")))))

(defun array-list (array)
  (if (zerop array)
      '()
      (loop :for index :below (objc:msg-send :ulong array "count")
            :collect (objc:msg-send :id array "objectAtIndex:" (:ulong index)))))

(defun ax-attribute (element name)
  "The value of the attribute NAME of ELEMENT, or zero."
  (sb-alien:with-alien ((out sb-alien:unsigned-long 0))
    (let ((error (sb-alien:alien-funcall
                  (sb-alien:extern-alien "AXUIElementCopyAttributeValue"
                                         (function sb-alien:int sb-alien:unsigned-long
                                                   sb-alien:unsigned-long
                                                   (* sb-alien:unsigned-long)))
                  element (objc:ns-string name) (sb-alien:addr out))))
      (if (zerop error) out 0))))

(defun ax-set (element name value)
  (zerop (sb-alien:alien-funcall
          (sb-alien:extern-alien "AXUIElementSetAttributeValue"
                                 (function sb-alien:int sb-alien:unsigned-long
                                           sb-alien:unsigned-long sb-alien:unsigned-long))
          element (objc:ns-string name) value)))

(defun ax-actions (element)
  (sb-alien:with-alien ((out sb-alien:unsigned-long 0))
    (let ((error (sb-alien:alien-funcall
                  (sb-alien:extern-alien "AXUIElementCopyActionNames"
                                         (function sb-alien:int sb-alien:unsigned-long
                                                   (* sb-alien:unsigned-long)))
                  element (sb-alien:addr out))))
      (if (zerop error) (mapcar #'str (array-list out)) '()))))

(defun ax-action-names (element)
  (ax-actions element))

(defun ax-perform (element action)
  (zerop (sb-alien:alien-funcall
          (sb-alien:extern-alien "AXUIElementPerformAction"
                                 (function sb-alien:int sb-alien:unsigned-long
                                           sb-alien:unsigned-long))
          element (objc:ns-string action))))

(defun ax-frame (element)
  "The x, y, width and height of ELEMENT on the screen, from the top left of the main display."
  (let ((value (ax-attribute element "AXFrame")))
    (unless (zerop value)
      (sb-alien:with-alien ((rect (array sb-alien:double 4)))
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "AXValueGetValue"
                                (function (sb-alien:unsigned 8) sb-alien:unsigned-long
                                          sb-alien:int (* t)))
         value 3 (sb-alien:alien-sap rect))
        (loop :for index :below 4 :collect (sb-alien:deref rect index))))))

(defun ax-element-at (application x y)
  "The element that the system finds at the point X, Y on the screen, or zero."
  (sb-alien:with-alien ((out sb-alien:unsigned-long 0))
    (let ((error (sb-alien:alien-funcall
                  (sb-alien:extern-alien "AXUIElementCopyElementAtPosition"
                                         (function sb-alien:int sb-alien:unsigned-long
                                                   sb-alien:single-float sb-alien:single-float
                                                   (* sb-alien:unsigned-long)))
                  application (float x 1f0) (float y 1f0) (sb-alien:addr out))))
      (if (zerop error) out 0))))

(defun trusted-p ()
  (/= 0 (sb-alien:alien-funcall
         (sb-alien:extern-alien "AXIsProcessTrusted" (function (sb-alien:unsigned 8))))))

(defun application ()
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "AXUIElementCreateApplication"
                          (function sb-alien:unsigned-long sb-alien:int))
   (sb-alien:alien-funcall (sb-alien:extern-alien "getpid" (function sb-alien:int)))))

;;; --- the checks ---

(defun window-elements (application)
  "The window of the application and what is in it, as the system lists them."
  (let ((window (first (array-list (ax-attribute application "AXWindows")))))
    (values window (and window (array-list (ax-attribute window "AXChildren"))))))

(defun named (elements identifier)
  (find identifier elements :key (lambda (element) (str (ax-attribute element "AXIdentifier")))
                            :test #'equal))

(defun said (element what)
  (str (ax-attribute element what)))

(defun fresh (application identifier)
  "The element with the identifier IDENTIFIER, asked for again: a screen reader that was told of a
change asks again."
  (multiple-value-bind (window elements) (window-elements application)
    (declare (ignore window))
    (named elements identifier)))

(defun inside-p (frame window-frame)
  (destructuring-bind (x y width height) frame
    (destructuring-bind (wx wy ww wh) window-frame
      (and (<= wx x) (<= wy y) (<= (+ x width) (+ wx ww)) (<= (+ y height) (+ wy wh))))))

(defun number-of (element what)
  "The number that the attribute WHAT of ELEMENT holds, or NIL."
  (let ((text (said element what)))
    (and text (let ((*read-default-float-format* 'double-float))
                (ignore-errors (let ((number (read-from-string text)))
                                 (and (realp number) number)))))))

(defun about (want got)
  (and got (< (abs (- want got)) 0.01)))

(defun parts (element)
  (array-list (ax-attribute element "AXChildren")))

(defun the-other-widgets (application host)
  "The slider, the progress bar, the choice, the list, the drawing and the layer, as the system lists
them, and what pressing and adjusting them does."
  (multiple-value-bind (window elements) (window-elements application)
    (declare (ignore window))
    (let ((volume (named elements "volume"))
          (progress (named elements "progress"))
          (flavour (named elements "flavour"))
          (numbers (named elements "numbers"))
          (drawing (named elements "drawing"))
          (veil (named elements "veil")))
      (check "the system finds the slider, the bar, the choice, the list, the drawing and the layer"
             (and volume progress flavour numbers drawing veil))
      (check "a slider is a slider, called by its label, between its ends"
             (and (equal "AXSlider" (said volume "AXRole"))
                  (equal "Volume" (said volume "AXDescription"))
                  (about 30 (number-of volume "AXValue"))
                  (about 0 (number-of volume "AXMinValue"))
                  (about 100 (number-of volume "AXMaxValue"))))
      (check "a slider is adjusted up and down, and not pressed"
             (let ((actions (ax-action-names volume)))
               (and (member "AXIncrement" actions :test #'equal)
                    (member "AXDecrement" actions :test #'equal)
                    (not (member "AXPress" actions :test #'equal)))))
      (check "the system makes the slider go up a step"
             (and (ax-perform volume "AXIncrement")
                  (progn (wait-for-idle host) t)
                  (about 35 (number-of (fresh application "volume") "AXValue"))))
      (let ((divider (named elements "divider")))
        (check "a split's divider is a splitter that says how wide the pane before it is"
               (and divider
                    (equal "AXSplitter" (said divider "AXRole"))
                    (about 50 (number-of divider "AXValue")))
               (and divider (list (said divider "AXRole") (number-of divider "AXValue")))))
      (let* ((pages (named elements "pages"))
             (tabs (and pages (parts pages))))
        (check "a tab bar is a tab group of its tabs, as radio buttons, the one shown chosen"
               (and pages
                    (equal "AXTabGroup" (said pages "AXRole"))
                    (= 2 (length tabs))
                    (every (lambda (tab) (equal "AXRadioButton" (said tab "AXRole"))) tabs)
                    (equal '("Design" "Theme") (mapcar (lambda (tab) (said tab "AXDescription")) tabs))
                    (equal '("0" "1") (mapcar (lambda (tab) (said tab "AXValue")) tabs)))
               (and pages (list (said pages "AXRole")
                                (mapcar (lambda (tab) (said tab "AXDescription")) tabs)))))
      (check "a progress bar is a progress bar, between 0 and 1"
             (and (equal "AXProgressIndicator" (said progress "AXRole"))
                  (about 0.25 (number-of progress "AXValue"))
                  (about 1 (number-of progress "AXMaxValue"))))
      (let ((options (parts flavour)))
        (check "a choice is a group of radio buttons called by its options, one of them chosen"
               (and (equal "AXRadioGroup" (said flavour "AXRole"))
                    (equal "Flavour" (said flavour "AXDescription"))
                    (equal '("Plain" "Salted" "Sweet") (mapcar (lambda (e) (said e "AXDescription")) options))
                    (equal '("0" "1" "0") (mapcar (lambda (e) (said e "AXValue")) options))))
        (check "the system chooses an option by pressing it"
               (and (ax-perform (third options) "AXPress")
                    (progn (wait-for-idle host) t)
                    (equal '("0" "0" "1")
                           (mapcar (lambda (e) (said e "AXValue"))
                                   (parts (fresh application "flavour")))))))
      (let ((rows (parts numbers)))
        (check "a list is a list of the rows that show, called by what is in them"
               (and (equal "AXList" (said numbers "AXRole"))
                    (equal '("One" "Two" "Three") (mapcar (lambda (e) (said e "AXDescription")) rows))
                    (every (lambda (e) (equal "AXRow" (said e "AXRole"))) rows)))
        (check "and a row is where the app drew it, inside the list"
               (let ((row (ax-frame (second rows)))
                     (list (ax-frame numbers)))
                 (and row list (inside-p row list))))
        (check "the system picks a row by pressing it, and the list says which is picked"
               (and (ax-perform (second rows) "AXPress")
                    (progn (wait-for-idle host) t)
                    (let ((picked (array-list (ax-attribute (fresh application "numbers")
                                                            "AXSelectedChildren"))))
                      (and (= 1 (length picked))
                           (equal "Two" (said (first picked) "AXDescription")))))))
      (check "a drawing and a layer are groups"
             (and (equal "AXGroup" (said drawing "AXRole")) (equal "AXGroup" (said veil "AXRole")))))))

(defun checks (host)
  (let ((application (application))
        (session (open-session)))
    (multiple-value-bind (window elements) (window-elements application)
      (check "the system lists one window of the application with the title" window)
      (check "and the window's title is the app's" (equal "Board" (said window "AXTitle")))
      (let ((field (named elements "draft"))
            (add (named elements "add"))
            (left (named elements "left"))
            (first-box (named elements "1"))
            (second-box (named elements "2")))
        (check "the system finds the text field, the button, the count and the check boxes"
               (and field add left first-box second-box))
        (check "a text field is a text field" (equal "AXTextField" (said field "AXRole")))
        (check "a button is a button, called by its words" (and (equal "AXButton" (said add "AXRole"))
                                                                (equal "Add" (said add "AXDescription"))))
        (check "a check box is a check box, called by what is to be done, and is checked or not"
               (and (equal "AXCheckBox" (said first-box "AXRole"))
                    (equal "Read the README" (said first-box "AXDescription"))
                    (equal "1" (said first-box "AXValue"))
                    (equal "0" (said second-box "AXValue"))))
        (check "the count is text that says what is left" (equal "2 left" (said left "AXValue")))
        (check "a button can be pressed" (member "AXPress" (ax-actions add) :test #'equal))
        (check "a check box can be pressed" (member "AXPress" (ax-actions second-box) :test #'equal))
        (check "a text and a field are not offered to be pressed"
               (and (not (member "AXPress" (ax-actions left) :test #'equal))
                    (not (member "AXPress" (ax-actions field) :test #'equal))))
        (let ((window-frame (ax-frame window))
              (add-frame (ax-frame add)))
          (check "what the system lists is inside the window, where it is drawn"
                 (every (lambda (element) (inside-p (ax-frame element) window-frame))
                        (list field add left first-box second-box))
                 (list add-frame window-frame))
          ;; Where the app drew the button, from the app, and where the system says it is: the window's
          ;; own frame, from the top left of the display, with the title bar above the content.
          (destructuring-bind (nx ny nw nh) (s:node-bounds (m:find-node session :id :add))
            (destructuring-bind (ax ay aw ah) add-frame
              (destructuring-bind (wx wy ww wh) window-frame
                (declare (ignore ww))
                (check "and the button is where the app drew it, in the window"
                       (every (lambda (want got) (< (abs (- want got)) 1))
                              (list (+ wx nx) (+ wy (- wh +content-height+) ny) nw nh)
                              (list ax ay aw ah))
                       (list add-frame window-frame (list nx ny nw nh))))))
          (destructuring-bind (x y width height) add-frame
            (check "the system finds the button under the middle of it"
                   (equal "add" (str (ax-attribute (ax-element-at application (+ x (/ width 2))
                                                                  (+ y (/ height 2)))
                                                   "AXIdentifier")))))))
      ;; The keyboard: a screen reader that asks for the field's focus gives the field the keyboard.
      (check "no field has the keyboard to begin with"
             (not (s:node-property (m:find-node session :id :draft) :focused)))
      (let ((true (objc:msg-send :id (objc:objc-class "NSNumber") "numberWithBool:" (:bool t))))
        (check "the system can ask for the focus of the field" (ax-set (named elements "draft")
                                                                       "AXFocused" true)))
      (wait-for-idle host)
      (check "and the field has the keyboard"
             (s:node-property (m:find-node session :id :draft) :focused))
      ;; From now on a screen reader is on: what changes is told to the system, for real.
      (setf (a11y:bridge-assistive (view:metal-view-bridge (host-view host))) (constantly t))
      (m:press-key session #\H)
      (m:press-key session #\i)
      (check "what is typed is what the system reads in the field"
             (equal "Hi" (said (fresh application "draft") "AXValue"))
             (said (fresh application "draft") "AXValue"))
      (check "the system presses Add" (ax-perform (fresh application "add") "AXPress"))
      (wait-for-idle host)
      (check "and the count says one more is left" (equal "3 left" (said (fresh application "left") "AXValue")))
      (check "and there is a check box for it, called what was typed"
             (let ((box (fresh application "4")))
               (and box (equal "Hi" (said box "AXDescription")))))
      (check "the system presses a check box" (ax-perform (fresh application "2") "AXPress"))
      (wait-for-idle host)
      (check "and it is checked, and one less is left"
             (and (equal "1" (said (fresh application "2") "AXValue"))
                  (equal "2 left" (said (fresh application "left") "AXValue"))))
      (the-other-widgets application host)
      (check "the window is still at rest" (wait-for-idle host)))))

(defvar *skipped* nil
  "Why the checks could not be made, or NIL.")

(defun readable-p (host)
  "Whether the system gives this process's window when asked, which it does not for a process that
may not use the API, or when the screen is locked (it then answers with the application and its menu
bar and nothing else). Sets *SKIPPED* to say why if not."
  (wait-for (lambda () (plusp (frame-ring-total (host-ring host)))) 20)
  (sleep 1.0)
  (cond ((not (trusted-p))
         (setf *skipped* "this process may not use the system's accessibility API, which it must to be read as VoiceOver reads it. Allow the terminal in System Settings, Privacy & Security, Accessibility.")
         nil)
        ((not (wait-for (lambda ()
                          (let ((window (window-elements (application))))
                            (and window (equal "AXWindow" (said window "AXRole")))))
                        5))
         (setf *skipped* "the system does not list the window (the screen is locked, or nobody is logged in at it).")
         nil)
        (t t)))

(defun drive (host)
  (unwind-protect
       (if (readable-p host)
           (handler-case (checks host)
             (error (condition)
               (check "the checks ran to the end" nil condition)))
           (format t "~&The accessibility check was skipped: ~a~%" *skipped*))
    (unless *skipped*
      (if (zerop *failures*)
          (format t "~&The accessibility check passed.~%")
          (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*)))
    (finish-output)
    (close-host-window host)))

(run-app 'make-board :title "Board" :width 720 :height +content-height+ :driver #'drive)
