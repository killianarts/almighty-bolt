(defpackage #:almighty-bolt-core/text/core-foundation
  (:use #:cl)
  (:import-from #:almighty-bolt-core/platform/macos/objc
                #:ns-string)
  (:export #:ccall
           #:cf-constant
           #:cf-retain
           #:cf-release
           #:cf-string
           #:cf-array-count
           #:cf-array-ref
           #:font-attributes))

(in-package #:almighty-bolt-core/text/core-foundation)

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

(defun cf-constant (name)
  "The value of the Core Foundation constant NAME (a global such as
kCTFontAttributeName that holds a reference), as an integer."
  (let ((address (sb-sys:find-dynamic-foreign-symbol-address name)))
    (unless address
      (error "There is no Core Foundation constant ~s." name))
    (sb-sys:sap-ref-64 (sb-sys:int-sap address) 0)))

(declaim (inline cf-retain cf-release))
(defun cf-retain (object)
  (ccall "CFRetain" :ulong (:ulong object)))

(defun cf-release (object)
  (ccall "CFRelease" :void (:ulong object)))

(defun cf-string (string)
  "A new CFString with the contents of STRING (an NSString, which is the same
object). The caller owns it and must CF-RELEASE it."
  (ns-string string))

(declaim (inline cf-array-count cf-array-ref))
(defun cf-array-count (array)
  (ccall "CFArrayGetCount" :long (:ulong array)))

(defun cf-array-ref (array index)
  (ccall "CFArrayGetValueAtIndex" :ulong (:ulong array) (:long index)))

(defun font-attributes (ct-font)
  "A new dictionary that gives CT-FONT as the font attribute. The caller owns it."
  (sb-alien:with-alien ((keys (array sb-alien:unsigned-long 1))
                        (values (array sb-alien:unsigned-long 1)))
    (setf (sb-alien:deref keys 0) (cf-constant "kCTFontAttributeName")
          (sb-alien:deref values 0) ct-font)
    (ccall "CFDictionaryCreate" :ulong
           (:ulong 0)
           (:pointer (sb-sys:sap-int (sb-alien:alien-sap keys)))
           (:pointer (sb-sys:sap-int (sb-alien:alien-sap values)))
           (:long 1)
           (:pointer (sb-sys:find-dynamic-foreign-symbol-address "kCFTypeDictionaryKeyCallBacks"))
           (:pointer (sb-sys:find-dynamic-foreign-symbol-address "kCFTypeDictionaryValueCallBacks")))))
