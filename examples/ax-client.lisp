;;;; A client of the system's accessibility API (AXUIElement), the one VoiceOver and the Accessibility
;;;; Inspector use, for the smoke checks that read a window of this process as a screen reader does
;;;; (examples/accessibility-smoke.lisp, apps/studio/smoke.lisp). Loaded by them, not a system.

(sb-alien:load-shared-object
 "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices")

(defpackage #:almighty-bolt-examples/ax-client
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc))
  (:export #:str
           #:array-list
           #:ax-attribute
           #:ax-set
           #:ax-actions
           #:ax-action-names
           #:ax-perform
           #:ax-frame
           #:ax-element-at
           #:trusted-p
           #:application
           #:window-elements
           #:named
           #:said
           #:everything-in
           #:anywhere))

(in-package #:almighty-bolt-examples/ax-client)

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

(defun window-elements (application)
  "The window of the application and what is in it, as the system lists them."
  (let ((window (first (array-list (ax-attribute application "AXWindows")))))
    (values window (and window (array-list (ax-attribute window "AXChildren"))))))

(defun named (elements identifier)
  (find identifier elements :key (lambda (element) (str (ax-attribute element "AXIdentifier")))
                            :test #'equal))

(defun said (element what)
  (str (ax-attribute element what)))

;;; What a screen reader finds at any depth, not only among the window's own children.

(defun everything-in (element)
  "ELEMENT's children, and theirs, depth first."
  (loop :for child :in (array-list (ax-attribute element "AXChildren"))
        :collect child
        :append (everything-in child)))

(defun anywhere (application identifier)
  "The element of the application's window, at any depth, with the identifier IDENTIFIER, asked for
now, or NIL."
  (let ((window (window-elements application)))
    (and window (named (everything-in window) identifier))))
