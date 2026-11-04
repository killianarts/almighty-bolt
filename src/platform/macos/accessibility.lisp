(defpackage #:almighty-bolt/platform/macos/accessibility
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:foreign-constant
                #:release
                #:define-callback
                #:define-runtime-class
                #:define-startup
                #:ivar-offset
                #:instance-slot
                #:register-object
                #:registered-object
                #:unregister-object)
  (:export #:make-bridge
           #:bridge-tree
           #:bridge-watched
           #:bridge-assistive
           #:bridge-poster
           #:bridge-announcer
           #:release-bridge
           #:frame-presented
           #:bridge-children
           #:bridge-hit-test
           #:bridge-focused-element))

(in-package #:almighty-bolt/platform/macos/accessibility)

;;; What VoiceOver reads. A window that draws itself has one view and nothing in it that the system
;;; can see; this makes the semantic tree (src/ui/semantics.lisp) visible to NSAccessibility, as
;;; elements that the view hands out as its children.
;;;
;;; A node of the tree becomes an ITEM, the Lisp side of an element, when it has a role that this
;;; knows; the layout containers, which say nothing to a person who cannot see them, are left out
;;; and what is in them is taken up to the container's parent. The elements are the objects
;;; of a class made at run time, which read what they say from their item when they are asked, so
;;; that nothing is worked out when no one asks. An element keeps its identity across frames (it
;;; is found again by its PATH, the ids from the top, or the place among its brothers for a
;;; container that has no id), because a screen reader follows an element, not what it says.
;;;
;;; Everything here runs on the main thread, as the frames do: AppKit asks there, and so do the
;;; frames that change what the items say.

(defstruct (bridge (:constructor make-bridge (&key view tree press convert)))
  ;; The NSView whose children the elements are, or 0.
  (view 0 :type (unsigned-byte 64))
  ;; Returns the node (a property list) of the whole window, or NIL if there is none. It is asked
  ;; each time, so that a new app (the app made again) is read from then on.
  (tree (lambda () nil) :type function)
  ;; Called with the x and y, in points from the top left of the view, of where a person's hand would
  ;; press, to press there. It does what a click does, through the events the window gets.
  (press (lambda (x y) (declare (ignore x y)) nil) :type function)
  ;; Called with x, y, width and height in the view's own points, returns the same on the screen;
  ;; NIL leaves them as they are.
  (convert nil :type (or null function))
  ;; The items by their path, and the ones that are not inside another.
  (items (make-hash-table :test 'equal) :read-only t)
  (roots '())
  ;; True when a frame was presented since the items were made.
  (stale t)
  ;; True once the window is closed: there is nothing to read then.
  (closed nil)
  ;; True once something has asked what the window shows (VoiceOver, an inspector): from then on it is
  ;; told what changes. NSWorkspace's flag for VoiceOver was found to say no inside an app while VoiceOver
  ;; was running, so it is not what this goes by alone.
  (watched nil)
  ;; The item that was last said to have the focus, so that a change is told once.
  (announced-focus nil)
  ;; Whether the system says that a screen reader is running (ASSISTIVE-CLIENT-P), and how a
  ;; notification is told to the system (POST), which is given an element and :LAYOUT-CHANGED, :VALUE-CHANGED,
  ;; :SELECTION-CHANGED, :FOCUSED-CHANGED, :MOVED or :RESIZED, and for a layout change the elements that
  ;; moved, if it is about them. A test puts others here.
  (assistive #'assistive-client-p :type function)
  (poster #'post :type function)
  ;; How a text is read out by a screen reader wherever its cursor is (SPEAK), given the view and the
  ;; text. A test puts another here.
  (announcer #'speak :type function))

(defstruct (item (:constructor make-item (bridge path role)))
  bridge
  path
  (role :static-text)
  (element 0 :type (unsigned-byte 64))
  (index 0 :type fixnum)
  ;; What the element says: a string or a number or NIL.
  (label nil)
  (value nil)
  ;; The ends of what a slider or a progress bar goes between, and whether a row or an option is the
  ;; chosen one.
  (minimum nil)
  (maximum nil)
  (selected nil)
  (enabled t)
  (focused nil)
  ;; True for a text that is read out whenever it changes.
  (live nil)
  (identifier nil)
  ;; A widget's tooltip, which a screen reader gives as its help.
  (help nil)
  ;; Where it is, in points from the top left of the view.
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (width 0d0 :type double-float)
  (height 0d0 :type double-float)
  ;; Whether the last time it was filled it had moved, and whether its size had changed.
  (moved nil)
  (resized nil)
  (parent nil)
  (children '()))

;;; --- the elements ---

(defvar *element-slot-offset* 0
  "Offset of the instance variable that holds an element's index.")

(defun item-of (object)
  "The item that the element OBJECT says things for, or NIL if it was taken away."
  (registered-object (instance-slot object *element-slot-offset*)))

(defmacro define-item-callback (name result-type default &body body)
  "A callback of an element: BODY runs with ITEM bound to its item, and DEFAULT is what a taken-away
element (that a client still holds) answers."
  `(define-callback ,name ,result-type
       ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
     (let ((item (item-of self)))
       (if item
           (progn ,@body)
           ,default))))

(defun autoreleased-string (string)
  (msg-send :id (objc-class "NSString") "stringWithUTF8String:" (:c-string string)))

(defun role-constant (role)
  (ecase role
    (:static-text (foreign-constant "NSAccessibilityStaticTextRole"))
    (:button (foreign-constant "NSAccessibilityButtonRole"))
    (:text-field (foreign-constant "NSAccessibilityTextFieldRole"))
    (:combo-box (foreign-constant "NSAccessibilityComboBoxRole"))
    (:text-area (foreign-constant "NSAccessibilityTextAreaRole"))
    (:checkbox (foreign-constant "NSAccessibilityCheckBoxRole"))
    (:busy-indicator (foreign-constant "NSAccessibilityBusyIndicatorRole"))
    (:pop-up-button (foreign-constant "NSAccessibilityPopUpButtonRole"))
    (:menu-button (foreign-constant "NSAccessibilityMenuButtonRole"))
    (:menu-item (foreign-constant "NSAccessibilityMenuItemRole"))
    (:progress-bar (foreign-constant "NSAccessibilityProgressIndicatorRole"))
    (:slider (foreign-constant "NSAccessibilitySliderRole"))
    (:radio-group (foreign-constant "NSAccessibilityRadioGroupRole"))
    (:tab-group (foreign-constant "NSAccessibilityTabGroupRole"))
    (:radio-button (foreign-constant "NSAccessibilityRadioButtonRole"))
    (:list (foreign-constant "NSAccessibilityListRole"))
    (:row (foreign-constant "NSAccessibilityRowRole"))
    (:splitter (foreign-constant "NSAccessibilitySplitterRole"))
    (:group (foreign-constant "NSAccessibilityGroupRole"))))

(defun object-for (thing)
  "An Objective-C object for THING, a string or a number, or zero for NIL."
  (typecase thing
    (string (autoreleased-string thing))
    (real (msg-send :id (objc-class "NSNumber") "numberWithDouble:"
                    (:double (float thing 1d0))))
    (t 0)))

(define-item-callback element-role sb-alien:unsigned-long 0
  (role-constant (item-role item)))

(define-item-callback element-label sb-alien:unsigned-long 0
  (object-for (item-label item)))

(define-item-callback element-help sb-alien:unsigned-long 0
  (object-for (item-help item)))

(define-item-callback element-value sb-alien:unsigned-long 0
  (object-for (item-value item)))

(define-item-callback element-identifier sb-alien:unsigned-long 0
  (object-for (item-identifier item)))

(define-item-callback element-enabled sb-alien:unsigned-char 0
  (if (item-enabled item) 1 0))

(define-item-callback element-focused sb-alien:unsigned-char 0
  (if (item-focused item) 1 0))

(define-item-callback element-parent sb-alien:unsigned-long 0
  (let ((parent (item-parent item)))
    (if parent
        (item-element parent)
        (bridge-view (item-bridge item)))))

(defvar *frame-result* nil
  "Where the method that returns an NSRect puts it: a callback returns a structure by value, and
this is a place for it that is not allocated again for each call.")

(define-startup accessibility-frame-result
  (setf *frame-result* (sb-alien:make-alien (sb-alien:struct almighty-bolt/platform/macos/objc::cg-rect))))

(defun frame-result (x y width height)
  (let ((origin (sb-alien:slot *frame-result* 'almighty-bolt/platform/macos/objc::origin))
        (size (sb-alien:slot *frame-result* 'almighty-bolt/platform/macos/objc::size)))
    (setf (sb-alien:slot origin 'almighty-bolt/platform/macos/objc::x) x
          (sb-alien:slot origin 'almighty-bolt/platform/macos/objc::y) y
          (sb-alien:slot size 'almighty-bolt/platform/macos/objc::width) width
          (sb-alien:slot size 'almighty-bolt/platform/macos/objc::height) height))
  (sb-alien:deref *frame-result*))

(defun screen-rect (item)
  "The rectangle of ITEM on the screen, as four values, worked out now so that a window that was moved
is where it is."
  (let ((convert (bridge-convert (item-bridge item))))
    (if convert
        (funcall convert (item-x item) (item-y item) (item-width item) (item-height item))
        (values (item-x item) (item-y item) (item-width item) (item-height item)))))

(sb-alien:define-alien-callable element-frame
    (sb-alien:struct almighty-bolt/platform/macos/objc::cg-rect)
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long))
  cmd
  (handler-case
      (let ((item (item-of self)))
        (if item
            (multiple-value-bind (x y width height) (screen-rect item)
              (frame-result x y width height))
            (frame-result 0d0 0d0 0d0 0d0)))
    (error (condition)
      (format *error-output* "~&Error in the callback element-frame: ~a~%" condition)
      (frame-result 0d0 0d0 0d0 0d0))))

(defun press-item (item)
  "Press where ITEM is, as a person's hand does. True if it was done: a thing that is not enabled, or
that was taken away, is not pressed."
  (let ((bridge (item-bridge item)))
    ;; What the item says may be a frame old, and the thing may have moved or gone.
    (ensure-fresh bridge)
    (when (and (eq item (gethash (item-path item) (bridge-items bridge)))
               (item-enabled item))
      (funcall (bridge-press bridge)
               (+ (item-x item) (/ (item-width item) 2))
               (+ (item-y item) (/ (item-height item) 2)))
      t)))

(define-item-callback element-press sb-alien:unsigned-char 0
  (if (press-item item) 1 0))

(defconstant +thumb-width+ 16
  "The width of a slider's thumb, whose middle goes from half of it in to half of it from the end
(the machine interface presses a slider by the same measure).")

(defun nudge-item (item direction)
  "Move the slider ITEM a twentieth of its range up (DIRECTION 1) or down (-1), the step of an arrow key,
by pressing where the value then is, as a hand would. True if that is a change."
  (let ((bridge (item-bridge item))
        (minimum (item-minimum item))
        (maximum (item-maximum item))
        (value (item-value item)))
    (ensure-fresh bridge)
    (when (and (eq item (gethash (item-path item) (bridge-items bridge)))
               (item-enabled item)
               (realp value) (realp minimum) (realp maximum) (> maximum minimum))
      (let ((target (max minimum (min maximum (+ value (* direction (/ (- maximum minimum) 20)))))))
        (when (/= target value)
          (let ((travel (max 1d0 (- (item-width item) +thumb-width+)))
                (fraction (/ (- target minimum) (- maximum minimum))))
            (funcall (bridge-press bridge)
                     (+ (item-x item) (/ +thumb-width+ 2) (* fraction travel))
                     (+ (item-y item) (/ (item-height item) 2)))
            t))))))

(define-item-callback element-increment sb-alien:unsigned-char 0
  (if (nudge-item item 1) 1 0))

(define-item-callback element-decrement sb-alien:unsigned-char 0
  (if (nudge-item item -1) 1 0))

(define-item-callback element-children sb-alien:unsigned-long 0
  (if (item-children item)
      (elements-array (item-children item))
      0))

(define-item-callback element-selected-children sb-alien:unsigned-long 0
  (if (item-children item)
      (elements-array (remove-if-not #'item-selected (item-children item)))
      0))

(define-item-callback element-selected sb-alien:unsigned-char 0
  (if (item-selected item) 1 0))

(define-item-callback element-minimum sb-alien:unsigned-long 0
  (object-for (item-minimum item)))

(define-item-callback element-maximum sb-alien:unsigned-long 0
  (object-for (item-maximum item)))

(define-callback element-set-focused sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (flag sb-alien:unsigned-char))
  ;; A screen reader that is asked to follow the keyboard, or to type into a field, asks for the focus
  ;; of a field. A press on it is what gives a field the keyboard here. Other things do not need it.
  (let ((item (item-of self)))
    (when (and item (/= flag 0) (not (item-focused item))
               (member (item-role item) '(:text-field :combo-box :text-area)))
      (press-item item))))

(define-runtime-class *element-class* "AlmightyBoltAccessibilityElement" "NSAccessibilityElement"
  :ivars '(("lispSlot" 8 3 "Q"))
  :methods '(("accessibilityRole" "@@:" element-role)
             ("accessibilityLabel" "@@:" element-label)
             ("accessibilityHelp" "@@:" element-help)
             ("accessibilityValue" "@@:" element-value)
             ("accessibilityIdentifier" "@@:" element-identifier)
             ("isAccessibilityEnabled" "B@:" element-enabled)
             ("isAccessibilityFocused" "B@:" element-focused)
             ("setAccessibilityFocused:" "v@:B" element-set-focused)
             ("accessibilityParent" "@@:" element-parent)
             ("accessibilityChildren" "@@:" element-children)
             ("accessibilitySelectedChildren" "@@:" element-selected-children)
             ("isAccessibilitySelected" "B@:" element-selected)
             ("accessibilityMinValue" "@@:" element-minimum)
             ("accessibilityMaxValue" "@@:" element-maximum)
             ("accessibilityFrame" "{CGRect={CGPoint=dd}{CGSize=dd}}@:" element-frame)))

;;; AppKit offers an action to a client when the class has the method that does it, so a thing that
;;; can be pressed is of another class, and a label does not offer to be pressed.
(define-runtime-class *pressable-class* "AlmightyBoltAccessibilityPressable"
  "AlmightyBoltAccessibilityElement"
  :methods '(("accessibilityPerformPress" "B@:" element-press)))

;;; A slider is adjusted, up and down, and is not pressed.
(define-runtime-class *adjustable-class* "AlmightyBoltAccessibilityAdjustable"
  "AlmightyBoltAccessibilityElement"
  :methods '(("accessibilityPerformIncrement" "B@:" element-increment)
             ("accessibilityPerformDecrement" "B@:" element-decrement)))

(define-startup accessibility-slot-offset
  (setf *element-slot-offset* (ivar-offset *element-class* "lispSlot")))

(defun make-element (item)
  "Make the element for ITEM, and remember the item for it."
  (let* ((class (case (item-role item)
                  ((:button :checkbox :radio-button :row :pop-up-button :menu-button
                    :menu-item)
                   *pressable-class*)
                  (:slider *adjustable-class*)
                  (t *element-class*)))
         (element (msg-send :id (msg-send :id class "alloc") "init")))
    (setf (item-element item) element
          (item-index item) (register-object item)
          (instance-slot element *element-slot-offset*) (item-index item))
    element))

(defun drop-item (item)
  "Take ITEM away: its element, which a client may still hold, answers with nothing from now on."
  (setf (instance-slot (item-element item) *element-slot-offset*) 0)
  (unregister-object (item-index item))
  (release (item-element item)))

;;; --- making the items from the tree ---

(defun role-of (node)
  "The role of the item that NODE becomes, or NIL if it has none: it is a container, or a kind of
thing that is not told yet."
  (case (getf node :role)
    ((:static-text :paragraph :toast) :static-text)
    ((:button :toggle-button) :button)
    (:text-field :text-field)
    (:combo-box :combo-box)
    (:text-area :text-area)
    ;; A switch is a check box to a screen reader, as AppKit's own switch is.
    ((:checkbox :switch) :checkbox)
    (:busy-indicator :busy-indicator)
    (:pop-up-button :pop-up-button)
    (:menu-button :menu-button)
    (:menu-item :menu-item)
    (:progress-bar :progress-bar)
    (:slider :slider)
    (:splitter :splitter)
    ;; A tab bar is a tab group, whose tabs are radio buttons, as AppKit's own is.
    (:radio-group (if (eq (getf (getf node :state) :style) :tabs) :tab-group :radio-group))
    (:list :list)
    ((:canvas :overlay :grid) :group)))

(defun text-of (thing)
  "What a person is shown for THING, an item of a list or an option of a choice."
  (if (stringp thing) thing (princ-to-string thing)))

(defun place-item (item bounds)
  "Put ITEM where BOUNDS say. True if it moved, and true if its size changed."
  (destructuring-bind (x y width height) bounds
    (let ((x (float x 1d0)) (y (float y 1d0))
          (width (float width 1d0)) (height (float height 1d0)))
      (multiple-value-prog1
          (values (or (/= x (item-x item)) (/= y (item-y item)))
                  (or (/= width (item-width item)) (/= height (item-height item))))
        (setf (item-x item) x
              (item-y item) y
              (item-width item) width
              (item-height item) height)))))

(defun fill-item (item node)
  "Make ITEM say what NODE says. True if the value changed."
  (let* ((state (getf node :state))
         (name (getf node :name))
         (id (getf node :id))
         (old-value (item-value item))
         (old-label (item-label item)))
    (multiple-value-bind (moved resized) (place-item item (getf node :bounds))
      (setf (item-moved item) moved
            (item-resized item) resized))
    (setf (item-enabled item) (not (getf state :disabled))
          (item-focused item) (and (getf state :focused) t)
          (item-live item) (and (getf state :live) t)
          (item-identifier item) (and id (string-downcase (princ-to-string id)))
          (item-help item) (getf node :help))
    (ecase (item-role item)
      (:static-text (setf (item-label item) nil
                          (item-value item) name))
      (:button (setf (item-label item) name
                     (item-value item) nil))
      ((:text-field :combo-box :text-area) (setf (item-label item) name
                         (item-value item) (or (getf node :value) "")))
      (:checkbox (setf (item-label item) name
                       (item-value item) (if (getf node :value) 1 0)))
      ;; A divider says where it is: how wide the pane before it is.
      (:splitter (setf (item-label item) name
                       (item-value item) (getf node :value)))
      (:busy-indicator (setf (item-label item) name
                             (item-value item) nil))
      ;; A pop-up button says the option it shows; a row of a menu is called by its words.
      (:pop-up-button (setf (item-label item) name
                            (item-value item) (getf node :value)))
      ((:menu-item :menu-button) (setf (item-label item) name
                                       (item-value item) nil))
      (:progress-bar (setf (item-label item) name
                           (item-value item) (getf node :value)
                           (item-minimum item) 0d0
                           (item-maximum item) 1d0))
      (:slider (setf (item-label item) name
                     (item-value item) (getf node :value)
                     (item-minimum item) (getf state :minimum)
                     (item-maximum item) (getf state :maximum)))
      ;; What a choice or a list holds is in the things inside it; the group is called by its label.
      ((:radio-group :tab-group :list :group) (setf (item-label item) name
                                         (item-value item) nil)))
    (not (and (equal old-value (item-value item))
              (equal old-label (item-label item))))))

(defun fill-part (item parent label value selected bounds)
  "Make ITEM, an option of a choice or a row of a list inside PARENT, say what it says. Returns true if
its value changed, and true if it became chosen or stopped being."
  (let ((old-value (item-value item))
        (old-selected (item-selected item)))
    (multiple-value-bind (moved resized) (place-item item bounds)
      (setf (item-moved item) moved
            (item-resized item) resized))
    (setf (item-label item) label
          (item-value item) value
          (item-selected item) selected
          (item-enabled item) (item-enabled parent)
          (item-focused item) nil
          (item-identifier item) (and (item-identifier parent)
                                      (format nil "~a.~a" (item-identifier parent)
                                              (cdr (first (last (item-path item)))))))
    (values (not (equal old-value value))
            (not (eq (and old-selected t) (and selected t))))))

(defun option-specs (node)
  "What each option of the choice NODE is: (path-part label value chosen bounds)."
  (let* ((state (getf node :state))
         (chosen (getf state :selected)))
    (loop :for option :in (getf state :options)
          :for bounds :in (getf state :option-bounds)
          :for index :from 0
          :collect (list (cons :option index) (text-of option)
                         (if (eql index chosen) 1 0) (eql index chosen) bounds))))

(defun row-specs (node)
  "The same for each row of the list NODE that is showing."
  (let* ((state (getf node :state))
         (items (getf state :items))
         (chosen (getf state :selected)))
    (loop :for bounds :in (getf state :item-bounds)
          :for index :from (or (getf state :first-visible) 0)
          :while (< index (length items))
          :collect (list (cons :row index) (text-of (elt items index))
                         nil (eql index chosen) bounds))))

(defun refresh (bridge)
  "Make the items again from the tree of the frame last built, keeping the ones that are still there
(by their path, so that the elements are the same objects) and taking the others away. Returns the
items whose value changed, whether any came or went, the lists whose chosen row changed, and the items
that were already there and have moved and the ones that have a new size."
  (let* ((old (bridge-items bridge))
         (new (make-hash-table :test 'equal))
         (changed '())
         (chosen '())
         (moved '())
         (resized '())
         (arrived '())
         (structure nil)
         (tree (funcall (bridge-tree bridge))))
    (labels ((note-frame (item)
               (when (item-moved item) (push item moved))
               (when (item-resized item) (push item resized)))
             (claim (path role parent)
               ;; The item at PATH, the one there was if it has the same role and a new one if not.
               ;; True as the second value for a new one.
               (let ((item (gethash path old))
                     (fresh nil))
                 (unless (and item (eq (item-role item) role))
                   (when item
                     (drop-item item)
                     (remhash path old))
                   (setf item (make-item bridge path role)
                         fresh t
                         structure t)
                   (make-element item))
                 (setf (gethash path new) item
                       (item-parent item) parent)
                 (values item fresh)))
             (parts (item path role specs)
               (loop :for (key label value selected bounds) :in specs
                     :collect (multiple-value-bind (part fresh)
                                  (claim (append path (list key)) role item)
                                (multiple-value-bind (value-changed selection-changed)
                                    (fill-part part item label value selected bounds)
                                  (unless fresh
                                    (note-frame part)
                                    (when value-changed
                                      (push part changed))
                                    (when (and selection-changed (eq role :row))
                                      (pushnew item chosen))))
                                part)))
             (visit (node path place parent)
               (let ((path (append path (list (if (getf node :id)
                                                  (cons :id (getf node :id))
                                                  place))))
                     (role (role-of node)))
                 (if role
                     (multiple-value-bind (item fresh) (claim path role parent)
                       (let ((value-changed (fill-item item node)))
                         ;; A thing that is to be read out as it comes (a toast), and has come.
                         (when (and fresh (getf (getf node :state) :announce))
                           (push item arrived))
                         (unless fresh
                           (note-frame item)
                           (when value-changed
                             (push item changed))))
                       (setf (item-children item)
                             (case role
                               ((:radio-group :tab-group)
                                (parts item path :radio-button (option-specs node)))
                               (:list (parts item path :row (row-specs node)))
                               (t (visit-children node path item))))
                       (list item))
                     (visit-children node path parent))))
             (visit-children (node path parent)
               (loop :for child :in (getf node :children)
                     :for place :from 0
                     :append (visit child path place parent))))
      (let ((roots (and tree (visit tree '() 0 nil))))
        (maphash (lambda (path item)
                   (unless (eq item (gethash path new))
                     (drop-item item)
                     (setf structure t)))
                 old)
        (clrhash old)
        (maphash (lambda (path item) (setf (gethash path old) item)) new)
        (setf (bridge-roots bridge) roots
              (bridge-stale bridge) nil)
        (values (nreverse changed) structure (nreverse chosen)
                (nreverse moved) (nreverse resized) (nreverse arrived))))))

(defun ensure-fresh (bridge)
  (when (and (bridge-stale bridge) (not (bridge-closed bridge)))
    (refresh bridge)))

(defun all-items (bridge)
  (loop :for item :being :the :hash-values :of (bridge-items bridge) :collect item))

(defun focused-item (bridge)
  (find-if #'item-focused (all-items bridge)))

;;; --- what the view answers ---

(defun elements-array (items)
  (let ((array (msg-send :id (objc-class "NSMutableArray") "array")))
    (dolist (item items array)
      (msg-send :void array "addObject:" (:id (item-element item))))))

(defun bridge-children (bridge)
  "An NSArray of the elements that are not inside another: the view's accessibility children."
  (setf (bridge-watched bridge) t)
  (ensure-fresh bridge)
  (elements-array (bridge-roots bridge)))

(defun bridge-hit-test (bridge x y)
  "The element under the point X, Y on the screen (Cocoa's, from the bottom left), the one that is
deepest and drawn last, or zero if there is none."
  (setf (bridge-watched bridge) t)
  (ensure-fresh bridge)
  (let ((found 0))
    (labels ((walk (items)
               (dolist (item items)
                 (multiple-value-bind (left bottom width height) (screen-rect item)
                   (when (and (<= left x (+ left width)) (<= bottom y (+ bottom height)))
                     (setf found (item-element item))))
                 (walk (item-children item)))))
      (walk (bridge-roots bridge)))
    found))

(defun bridge-focused-element (bridge)
  "The element that has the keyboard, or zero if none does."
  (setf (bridge-watched bridge) t)
  (ensure-fresh bridge)
  (let ((item (focused-item bridge)))
    (if item (item-element item) 0)))

;;; --- telling the system what changed ---

(defun post (element notification &optional elements)
  "Tell the system that NOTIFICATION happened to ELEMENT (zero is no element, and is not told). With
ELEMENTS, a list of elements, a layout change is told of the window of ELEMENT, a view, and names them:
it is what makes VoiceOver draw its cursor again where the thing it is on has gone."
  (unless (zerop element)
    (if elements
        (relayout element elements)
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "NSAccessibilityPostNotification"
                                (function sb-alien:void sb-alien:unsigned-long sb-alien:unsigned-long))
         element
         (notification-constant notification)))))

(defun relayout (view elements)
  (let* ((window (msg-send :id view "window"))
         (target (if (zerop window) view window))
         (array (msg-send :id (objc-class "NSMutableArray") "array"))
         (information (msg-send :id (objc-class "NSMutableDictionary") "dictionary")))
    (dolist (element elements)
      (msg-send :void array "addObject:" (:id element)))
    (msg-send :void information "setObject:forKey:"
              (:id array) (:id (foreign-constant "NSAccessibilityUIElementsKey")))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "NSAccessibilityPostNotificationWithUserInfo"
                            (function sb-alien:void sb-alien:unsigned-long
                                      sb-alien:unsigned-long sb-alien:unsigned-long))
     target (notification-constant :layout-changed) information)))

(defun notification-constant (notification)
  (ecase notification
       (:layout-changed (foreign-constant "NSAccessibilityLayoutChangedNotification"))
       (:value-changed (foreign-constant "NSAccessibilityValueChangedNotification"))
       (:moved (foreign-constant "NSAccessibilityMovedNotification"))
       (:resized (foreign-constant "NSAccessibilityResizedNotification"))
       (:selection-changed
        (foreign-constant "NSAccessibilitySelectedChildrenChangedNotification"))
       (:focused-changed
        (foreign-constant "NSAccessibilityFocusedUIElementChangedNotification"))))

(defun speak (view text)
  "Have a screen reader read TEXT out now, though its cursor is on something else: an announcement,
told through the window of VIEW."
  (let* ((window (if (zerop view) 0 (msg-send :id view "window")))
         (target (if (zerop window) view window)))
    (unless (or (zerop target) (zerop (length text)))
      ;; Not DICTIONARYWITHOBJECTSANDKEYS:, which takes any number of arguments and so cannot be sent
      ;; as a fixed number of them.
      (let ((information (msg-send :id (objc-class "NSMutableDictionary") "dictionary")))
        (msg-send :void information "setObject:forKey:"
                  (:id (autoreleased-string text))
                  (:id (foreign-constant "NSAccessibilityAnnouncementKey")))
        (msg-send :void information "setObject:forKey:"
                  (:id (msg-send :id (objc-class "NSNumber") "numberWithInteger:" (:long 50)))
                  (:id (foreign-constant "NSAccessibilityPriorityKey")))
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "NSAccessibilityPostNotificationWithUserInfo"
                                (function sb-alien:void sb-alien:unsigned-long
                                          sb-alien:unsigned-long sb-alien:unsigned-long))
         target
         (foreign-constant "NSAccessibilityAnnouncementRequestedNotification")
         information)))))

(defun assistive-client-p ()
  "True if VoiceOver or Switch Control is running: the ones that follow changes as they happen and
need to be told."
  (let ((workspace (msg-send :id (objc-class "NSWorkspace") "sharedWorkspace")))
    (or (msg-send :bool workspace "isVoiceOverEnabled")
        (msg-send :bool workspace "isSwitchControlEnabled"))))

(defun announce (bridge)
  "Make the items again and tell the system what is new: a layout change if things came or went, a
move or a new size for the things that have one, and a layout change that names them (a screen
reader draws its cursor where it last drew it until told so), a change of value for the ones whose value changed, a change of the chosen row
for a list, the new focus, and the new words of a live text that changed. A screen reader says what
changed on the thing that its cursor is on, so only the last is read out wherever the cursor is."
  (multiple-value-bind (changed structure chosen moved resized arrived) (refresh bridge)
    (let ((focus (focused-item bridge))
          (post (bridge-poster bridge)))
      (when structure
        (funcall post (bridge-view bridge) :layout-changed))
      (dolist (item moved)
        (funcall post (item-element item) :moved))
      (dolist (item resized)
        (funcall post (item-element item) :resized))
      ;; VoiceOver keeps where a thing is when told it moved, but draws its cursor again only for a
      ;; layout change that names the things (seen on the screen: the box stayed behind the button).
      (when (or moved resized)
        (funcall post (bridge-view bridge) :layout-changed
                 (mapcar #'item-element (union moved resized))))
      (dolist (item changed)
        (funcall post (item-element item) :value-changed))
      (dolist (item chosen)
        (funcall post (item-element item) :selection-changed))
      (when (and focus (not (eq focus (bridge-announced-focus bridge))))
        (funcall post (item-element focus) :focused-changed))
      (setf (bridge-announced-focus bridge) focus)
      (dolist (item changed)
        (when (item-live item)
          (funcall (bridge-announcer bridge) (bridge-view bridge)
                   (text-of (item-value item)))))
      (dolist (item arrived)
        (funcall (bridge-announcer bridge) (bridge-view bridge)
                 (text-of (item-value item)))))))

(defun frame-presented (bridge)
  "A frame was presented, so what the items say may be out of date. Nothing is worked out for it
unless something has asked what the window shows or the system says that a screen reader is running;
the items are made again when someone asks."
  (setf (bridge-stale bridge) t)
  (when (and (not (bridge-closed bridge))
             (or (bridge-watched bridge) (funcall (bridge-assistive bridge))))
    ;; What a screen reader is told must never stop the window from being drawn.
    (handler-case (announce bridge)
      (error (condition)
        (format *error-output* "~&Error while telling a screen reader what changed: ~a~%"
                condition)))))

(defun release-bridge (bridge)
  "Take every item away, when the window is closed."
  (dolist (item (all-items bridge))
    (drop-item item))
  (clrhash (bridge-items bridge))
  (setf (bridge-roots bridge) '()
        (bridge-announced-focus bridge) nil
        (bridge-closed bridge) t))
