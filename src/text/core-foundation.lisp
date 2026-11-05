(defpackage #:almighty-bolt/text/core-foundation
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc))
  (:export #:ccall
           #:cf-constant
           #:cf-retain
           #:cf-release
           #:cf-string
           #:cf-array-count
           #:cf-array-ref
           #:cf-dictionary
           #:cf-double
           #:cf-number-double
           #:font-attributes))

(in-package #:almighty-bolt/text/core-foundation)

;;; Calling the C functions of Core Foundation, Core Graphics and Core Text. As with
;;; Objective-C messages, handles are plain integers, and a call has a fixed
;;; signature written out at the call site, so that nothing is allocated.

(dolist (framework '("CoreFoundation" "CoreGraphics" "CoreText"))
  (sb-alien:load-shared-object
   (format nil "/System/Library/Frameworks/~a.framework/~a" framework framework)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun alien-type-for (type)
    (ecase type
      ((:ulong :pointer) 'sb-alien:unsigned-long)
      (:long 'sb-alien:long)
      (:int 'sb-alien:int)
      (:uint 'sb-alien:unsigned-int)
      (:ushort '(sb-alien:unsigned 16))
      (:bool '(sb-alien:unsigned 8))
      (:double 'sb-alien:double)
      (:float 'sb-alien:single-float)
      (:void 'sb-alien:void))))

(defmacro ccall (name return-type &rest arguments)
  "Call the C function NAME and return its result as RETURN-TYPE. Each of ARGUMENTS
is (TYPE VALUE). The types are :ulong (also :pointer, for an address or a handle)
:long :int :uint :ushort :bool :double :float. A structure of two words, such as a
CFRange, is passed as its two words, one after the other; a structure of up to four
doubles, such as a CGPoint, as its doubles."
  (let ((bindings (loop :for (nil value) :in arguments
                        :collect (list (gensym "A") value))))
    `(let ,bindings
       ,(let ((call `(sb-alien:alien-funcall
                      (sb-alien:extern-alien ,name
                                             (function ,(alien-type-for return-type)
                                                       ,@(loop :for (type) :in arguments
                                                               :collect (alien-type-for type))))
                      ,@(loop :for (var) :in bindings
                              :for (type) :in arguments
                              :collect (if (eq type :bool) `(if ,var 1 0) var)))))
          (if (eq return-type :bool) `(/= 0 ,call) call)))))

(defmacro cf-constant (name)
  "The value of the Core Foundation constant NAME, a literal string (a global such as
kCTFontAttributeName that holds a reference), as an integer. It is looked up once."
  `(objc:foreign-constant ,name))

(declaim (inline cf-retain cf-release))
(defun cf-retain (object)
  (ccall "CFRetain" :ulong (:ulong object)))

(defun cf-release (object)
  (ccall "CFRelease" :void (:ulong object)))

(defun cf-string (string)
  "A new CFString with the contents of STRING (an NSString, which is the same
object). The caller owns it and must CF-RELEASE it."
  (objc:ns-string string))

(declaim (inline cf-array-count cf-array-ref))
(defun cf-array-count (array)
  (ccall "CFArrayGetCount" :long (:ulong array)))

(defun cf-array-ref (array index)
  (ccall "CFArrayGetValueAtIndex" :ulong (:ulong array) (:long index)))

(defun cf-dictionary (&rest keys-and-values)
  "A new CFDictionary of KEYS-AND-VALUES, each a Core Foundation object (an address), the key
first. The caller owns it."
  (let ((count (floor (length keys-and-values) 2)))
    (sb-alien:with-alien ((keys (array sb-alien:unsigned-long 8))
                          (values (array sb-alien:unsigned-long 8)))
      (assert (<= count 8))
      (loop :for (key value) :on keys-and-values :by #'cddr
            :for i :from 0
            :do (setf (sb-alien:deref keys i) key
                      (sb-alien:deref values i) value))
      (ccall "CFDictionaryCreate" :ulong
             (:ulong 0)
             (:pointer (sb-sys:sap-int (sb-alien:alien-sap keys)))
             (:pointer (sb-sys:sap-int (sb-alien:alien-sap values)))
             (:long count)
             (:pointer (objc:foreign-address "kCFTypeDictionaryKeyCallBacks"))
             (:pointer (objc:foreign-address "kCFTypeDictionaryValueCallBacks"))))))

(defconstant +cf-number-double+ 13 "kCFNumberDoubleType.")

(defun cf-double (number)
  "A new CFNumber of NUMBER, a double. The caller owns it."
  (sb-alien:with-alien ((value sb-alien:double (float number 1d0)))
    (ccall "CFNumberCreate" :ulong (:ulong 0) (:long +cf-number-double+)
           (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr value)))))))

(defun cf-number-double (number)
  "The value of the CFNumber NUMBER as a double, or NIL if NUMBER is 0 (none)."
  (unless (zerop number)
    (sb-alien:with-alien ((value sb-alien:double 0d0))
      (ccall "CFNumberGetValue" :bool (:ulong number) (:long +cf-number-double+)
             (:pointer (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr value)))))
      value)))

(defun font-attributes (ct-font &optional (tracking 0f0))
  "A new dictionary that gives CT-FONT as the font attribute, and TRACKING points of space after
each character if it is not zero (Core Text's kerning). The caller owns it."
  (if (zerop tracking)
      (cf-dictionary (cf-constant "kCTFontAttributeName") ct-font)
      (let* ((kern (cf-double tracking))
             (dictionary (cf-dictionary (cf-constant "kCTFontAttributeName") ct-font
                                        (cf-constant "kCTKernAttributeName") kern)))
        (cf-release kern)
        dictionary)))
