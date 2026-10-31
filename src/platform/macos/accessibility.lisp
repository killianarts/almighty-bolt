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
           #:bridge-assistive
           #:bridge-poster
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
  ;; each time, so that a new variant (the app made again) is read from then on.
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
  ;; The item that was last said to have the focus, so that a change is told once.
  (announced-focus nil)
  ;; Whether a screen reader is running (ASSISTIVE-CLIENT-P), and how a notification is told to the
  ;; system (POST), which is given an element and :LAYOUT-CHANGED, :VALUE-CHANGED or
  ;; :FOCUSED-CHANGED. A test puts others here.
  (assistive #'assistive-client-p :type function)
  (poster #'post :type function))

(defstruct (item (:constructor make-item (bridge path role)))
  bridge
  path
  (role :static-text)
  (element 0 :type (unsigned-byte 64))
  (index 0 :type fixnum)
  ;; What the element says: a string or a number or NIL.
  (label nil)
  (value nil)
  (enabled t)
  (focused nil)
  (identifier nil)
  ;; Where it is, in points from the top left of the view.
  (x 0d0 :type double-float)
  (y 0d0 :type double-float)
  (width 0d0 :type double-float)
  (height 0d0 :type double-float)
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
    (:checkbox (foreign-constant "NSAccessibilityCheckBoxRole"))))

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

(define-callback element-set-focused sb-alien:void
    ((self sb-alien:unsigned-long) (cmd sb-alien:unsigned-long) (flag sb-alien:unsigned-char))
  ;; A screen reader that is asked to follow the keyboard, or to type into a field, asks for the focus
  ;; of a field. A press on it is what gives a field the keyboard here. Other things do not need it.
  (let ((item (item-of self)))
    (when (and item (/= flag 0) (not (item-focused item)) (eq (item-role item) :text-field))
      (press-item item))))

(define-runtime-class *element-class* "AlmightyBoltAccessibilityElement" "NSAccessibilityElement"
  :ivars '(("lispSlot" 8 3 "Q"))
  :methods '(("accessibilityRole" "@@:" element-role)
             ("accessibilityLabel" "@@:" element-label)
             ("accessibilityValue" "@@:" element-value)
             ("accessibilityIdentifier" "@@:" element-identifier)
             ("isAccessibilityEnabled" "B@:" element-enabled)
             ("isAccessibilityFocused" "B@:" element-focused)
             ("setAccessibilityFocused:" "v@:B" element-set-focused)
             ("accessibilityParent" "@@:" element-parent)
             ("accessibilityFrame" "{CGRect={CGPoint=dd}{CGSize=dd}}@:" element-frame)))

;;; AppKit offers an action to a client when the class has the method that does it, so a thing that
;;; can be pressed is of another class, and a label does not offer to be pressed.
(define-runtime-class *pressable-class* "AlmightyBoltAccessibilityPressable"
  "AlmightyBoltAccessibilityElement"
  :methods '(("accessibilityPerformPress" "B@:" element-press)))

(define-startup accessibility-slot-offset
  (setf *element-slot-offset* (ivar-offset *element-class* "lispSlot")))

(defun make-element (item)
  "Make the element for ITEM, and remember the item for it."
  (let* ((class (if (member (item-role item) '(:button :checkbox))
                    *pressable-class*
                    *element-class*))
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
    ((:static-text :paragraph) :static-text)
    (:button :button)
    (:text-field :text-field)
    (:checkbox :checkbox)))

(defun fill-item (item node)
  "Make ITEM say what NODE says. True if the value changed."
  (let* ((state (getf node :state))
         (name (getf node :name))
         (id (getf node :id))
         (old-value (item-value item))
         (old-label (item-label item)))
    (destructuring-bind (x y width height) (getf node :bounds)
      (setf (item-x item) (float x 1d0)
            (item-y item) (float y 1d0)
            (item-width item) (float width 1d0)
            (item-height item) (float height 1d0)))
    (setf (item-enabled item) (not (getf state :disabled))
          (item-focused item) (and (getf state :focused) t)
          (item-identifier item) (and id (string-downcase (princ-to-string id))))
    (ecase (item-role item)
      (:static-text (setf (item-label item) nil
                          (item-value item) name))
      (:button (setf (item-label item) name
                     (item-value item) nil))
      (:text-field (setf (item-label item) name
                         (item-value item) (or (getf node :value) "")))
      (:checkbox (setf (item-label item) name
                       (item-value item) (if (getf node :value) 1 0))))
    (not (and (equal old-value (item-value item))
              (equal old-label (item-label item))))))

(defun refresh (bridge)
  "Make the items again from the tree of the frame last built, keeping the ones that are still there
(by their path, so that the elements are the same objects) and taking the others away. Returns the
items whose value changed, and whether any came or went."
  (let* ((old (bridge-items bridge))
         (new (make-hash-table :test 'equal))
         (changed '())
         (structure nil)
         (tree (funcall (bridge-tree bridge))))
    (labels ((visit (node path place parent)
               (let ((path (append path (list (if (getf node :id)
                                                  (cons :id (getf node :id))
                                                  place))))
                     (role (role-of node)))
                 (if role
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
                       (when (and (fill-item item node) (not fresh))
                         (push item changed))
                       (setf (gethash path new) item
                             (item-parent item) parent
                             (item-children item) (visit-children node path item))
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
        (values (nreverse changed) structure)))))

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
  (ensure-fresh bridge)
  (elements-array (bridge-roots bridge)))

(defun bridge-hit-test (bridge x y)
  "The element under the point X, Y on the screen (Cocoa's, from the bottom left), the one that is
deepest and drawn last, or zero if there is none."
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
  (ensure-fresh bridge)
  (let ((item (focused-item bridge)))
    (if item (item-element item) 0)))

;;; --- telling the system what changed ---

(defun post (element notification)
  "Tell the system that NOTIFICATION happened to ELEMENT (zero is no element, and is not told)."
  (unless (zerop element)
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "NSAccessibilityPostNotification"
                            (function sb-alien:void sb-alien:unsigned-long sb-alien:unsigned-long))
     element
     (ecase notification
       (:layout-changed (foreign-constant "NSAccessibilityLayoutChangedNotification"))
       (:value-changed (foreign-constant "NSAccessibilityValueChangedNotification"))
       (:focused-changed
        (foreign-constant "NSAccessibilityFocusedUIElementChangedNotification"))))))

(defun assistive-client-p ()
  "True if VoiceOver or Switch Control is running: the ones that follow changes as they happen and
need to be told."
  (let ((workspace (msg-send :id (objc-class "NSWorkspace") "sharedWorkspace")))
    (or (msg-send :bool workspace "isVoiceOverEnabled")
        (msg-send :bool workspace "isSwitchControlEnabled"))))

(defun announce (bridge)
  "Make the items again and tell the system what is new: a layout change if things came or went, a
change of value for the ones whose value changed, and the new focus."
  (multiple-value-bind (changed structure) (refresh bridge)
    (let ((focus (focused-item bridge))
          (post (bridge-poster bridge)))
      (when structure
        (funcall post (bridge-view bridge) :layout-changed))
      (dolist (item changed)
        (funcall post (item-element item) :value-changed))
      (when (and focus (not (eq focus (bridge-announced-focus bridge))))
        (funcall post (item-element focus) :focused-changed))
      (setf (bridge-announced-focus bridge) focus))))

(defun frame-presented (bridge)
  "A frame was presented, so what the items say may be out of date. Nothing is worked out for it
unless a screen reader is running; the items are made again when someone asks."
  (setf (bridge-stale bridge) t)
  (when (and (not (bridge-closed bridge)) (funcall (bridge-assistive bridge)))
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
