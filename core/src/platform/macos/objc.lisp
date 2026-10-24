(defpackage #:almighty-bolt-core/platform/macos/objc
  (:use #:cl)
  (:export #:id
           #:find-objc-class
           #:register-selector
           #:objc-class
           #:selector
           #:msg-send
           #:with-autorelease-pool
           #:with-masked-float-traps
           #:retain
           #:release
           #:ns-string
           #:lisp-string
           #:callable-address
           #:define-callback
           #:ensure-class
           #:ivar-offset
           #:instance-slot
           #:register-object
           #:registered-object
           #:unregister-object))

(in-package #:almighty-bolt-core/platform/macos/objc)

;;; The Objective-C runtime, reached straight through sb-alien.
;;;
;;; Objects, classes and selectors are plain integers (addresses), not alien
;;; pointers, so passing them around never allocates. Classes and selectors
;;; written as literals are looked up once, when the code is loaded. The
;;; runtime's one message function, objc_msgSend, is called with a fixed
;;; argument list for each call site. It is never called through a variadic
;;; interface: Apple's arm64 convention passes variadic arguments on the stack,
;;; which objc_msgSend does not expect.

(deftype id () '(unsigned-byte 64))

;;; The frameworks whose classes we use.

(dolist (framework '("Foundation" "AppKit" "QuartzCore" "Metal" "MetalKit"))
  (sb-alien:load-shared-object
   (format nil "/System/Library/Frameworks/~a.framework/~a" framework framework)
   :dont-save t))

;;; Geometry. These are only used as the result types of messages such as
;;; -[NSView bounds]. Struct results come back through sb-alien. Struct
;;; arguments are passed flattened (see MSG-SEND).

(sb-alien:define-alien-type nil
  (sb-alien:struct cg-point
    (x sb-alien:double)
    (y sb-alien:double)))

(sb-alien:define-alien-type nil
  (sb-alien:struct cg-size
    (width sb-alien:double)
    (height sb-alien:double)))

(sb-alien:define-alien-type nil
  (sb-alien:struct cg-rect
    (origin (sb-alien:struct cg-point))
    (size (sb-alien:struct cg-size))))

;;; Classes and selectors

(defun find-objc-class (name &optional (errorp t))
  "The Objective-C class called NAME as an integer, or NIL (or an error)."
  (let ((class (sb-alien:alien-funcall
                (sb-alien:extern-alien "objc_getClass"
                                       (function sb-alien:unsigned-long sb-alien:c-string))
                name)))
    (cond ((/= class 0) class)
          (errorp (error "There is no Objective-C class ~s." name))
          (t nil))))

(defun register-selector (name)
  "The selector for the message NAME, as an integer."
  (sb-alien:alien-funcall
   (sb-alien:extern-alien "sel_registerName"
                          (function sb-alien:unsigned-long sb-alien:c-string))
   name))

(defmacro objc-class (name)
  "The class called NAME, looked up once when the code is loaded."
  `(load-time-value (find-objc-class ,name)))

(defmacro selector (name)
  "The selector NAME, looked up once when the code is loaded."
  `(load-time-value (register-selector ,name)))

;;; Sending messages

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun scalar-alien-type (type)
    (ecase type
      ((:id :pointer :ulong) 'sb-alien:unsigned-long)
      (:long 'sb-alien:long)
      (:int 'sb-alien:int)
      (:uint 'sb-alien:unsigned-int)
      (:ushort '(sb-alien:unsigned 16))
      (:bool '(sb-alien:unsigned 8))
      (:double 'sb-alien:double)
      (:float 'sb-alien:single-float)
      (:c-string 'sb-alien:c-string)))

  (defun struct-alien-type (type)
    (ecase type
      (:rect '(sb-alien:struct cg-rect))
      (:size '(sb-alien:struct cg-size))
      (:point '(sb-alien:struct cg-point))))

  (defun flattened-arguments (type)
    "The number of doubles that a struct argument of TYPE is passed as."
    (ecase type
      (:rect 4)
      (:size 2)
      (:point 2)
      (:color 4))))

(defmacro msg-send (return-type receiver selector-name &rest arguments)
  "Send the message SELECTOR-NAME to RECEIVER and return its result as RETURN-TYPE.

Each of ARGUMENTS is (TYPE VALUE). The types are :id :pointer :ulong :long :int
:uint :ushort :bool :double :float :c-string, and the structures :rect (x y
width height), :size (width height), :point (x y) and :color (red green blue
alpha), each written as its numbers: (:rect 0 0 480 320).

A structure argument is passed as its doubles, one by one. On arm64 a structure
of up to four doubles travels in the same registers as that many separate
double arguments, so the callee cannot tell the difference, and nothing has to
be allocated.

RETURN-TYPE is one of the scalar types, :void, or a structure (:rect, :size or
:point), for which the numbers come back as multiple values."
  (let ((receiver-var (gensym "RECEIVER"))
        (bindings '())
        (alien-types '())
        (call-arguments '()))
    (dolist (argument arguments)
      (destructuring-bind (type &rest values) argument
        (case type
          ((:rect :size :point :color)
           (assert (= (length values) (flattened-arguments type)) ()
                   "A ~s argument takes ~d numbers." type (flattened-arguments type))
           (dolist (value values)
             (let ((var (gensym "D")))
               (push `(,var (the double-float (float ,value 1d0))) bindings)
               (push 'sb-alien:double alien-types)
               (push var call-arguments))))
          (t
           (let ((var (gensym "A")))
             (push `(,var ,(if (eq type :bool) `(if ,(first values) 1 0) (first values)))
                   bindings)
             (push (scalar-alien-type type) alien-types)
             (push var call-arguments))))))
    (let* ((selector-form (if (stringp selector-name)
                              `(selector ,selector-name)
                              `(register-selector ,selector-name)))
           (struct-result (member return-type '(:rect :size :point)))
           (alien-return (cond ((eq return-type :void) 'sb-alien:void)
                               (struct-result (struct-alien-type return-type))
                               (t (scalar-alien-type return-type))))
           (call `(sb-alien:alien-funcall
                   (sb-alien:extern-alien "objc_msgSend"
                                          (function ,alien-return
                                                    sb-alien:unsigned-long
                                                    sb-alien:unsigned-long
                                                    ,@(reverse alien-types)))
                   ,receiver-var ,selector-form ,@(reverse call-arguments))))
      `(let* ((,receiver-var ,receiver)
              ,@(reverse bindings))
         ,(case return-type
            (:void `(progn ,call nil))
            (:bool `(/= 0 ,call))
            (:rect (let ((s (gensym "S")))
                     `(let ((,s ,call))
                        (values (sb-alien:slot (sb-alien:slot ,s 'origin) 'x)
                                (sb-alien:slot (sb-alien:slot ,s 'origin) 'y)
                                (sb-alien:slot (sb-alien:slot ,s 'size) 'width)
                                (sb-alien:slot (sb-alien:slot ,s 'size) 'height)))))
            (:size (let ((s (gensym "S")))
                     `(let ((,s ,call))
                        (values (sb-alien:slot ,s 'width) (sb-alien:slot ,s 'height)))))
            (:point (let ((s (gensym "S")))
                      `(let ((,s ,call))
                         (values (sb-alien:slot ,s 'x) (sb-alien:slot ,s 'y)))))
            (t call))))))

;;; Floating point. SBCL traps invalid operations, overflow and division by
;;; zero by default, and the system frameworks perform some of those as a matter
;;; of course (for example while AppKit starts up), which would signal a Lisp
;;; error in the middle of foreign code. Everything that enters AppKit or Metal,
;;; the event loop above all, runs with those traps masked. Callbacks into
;;; Lisp keep the mask of the code that called them.

(defmacro with-masked-float-traps (&body body)
  "Run BODY with the floating-point traps that the system frameworks expect off."
  `(sb-int:with-float-traps-masked (:invalid :overflow :divide-by-zero)
     ,@body))

;;; Memory

(defmacro with-autorelease-pool (&body body)
  "Run BODY inside its own autorelease pool, as AppKit does for each event."
  (let ((pool (gensym "POOL")))
    `(let ((,pool (sb-alien:alien-funcall
                   (sb-alien:extern-alien "objc_autoreleasePoolPush"
                                          (function sb-alien:unsigned-long)))))
       (unwind-protect (progn ,@body)
         (sb-alien:alien-funcall
          (sb-alien:extern-alien "objc_autoreleasePoolPop"
                                 (function sb-alien:void sb-alien:unsigned-long))
          ,pool)))))

(declaim (inline retain release))
(defun retain (object)
  (msg-send :id object "retain"))

(defun release (object)
  (msg-send :void object "release"))

;;; Strings

(defun ns-string (string)
  "A new NSString with the contents of the Lisp STRING. The caller owns it and
must RELEASE it."
  (msg-send :id (msg-send :id (objc-class "NSString") "alloc")
            "initWithUTF8String:" (:c-string string)))

(defun lisp-string (ns-string)
  "The contents of NS-STRING as a Lisp string."
  (msg-send :c-string ns-string "UTF8String"))

;;; Making classes

(defun report-callback-error (name condition)
  (format *error-output* "~&Error in the callback ~(~a~): ~a~%" name condition)
  (finish-output *error-output*))

(defmacro define-callback (name result-type typed-lambda-list &body body)
  "Like SB-ALIEN:DEFINE-ALIEN-CALLABLE, for a method of a class made with
ENSURE-CLASS. An error in BODY is reported and the callback returns zero rather
than unwinding through the system framework's frames, which Lisp cannot do
safely (zero, or nil for void). Every argument counts as used, so a method need not declare the receiver
and selector ignored."
  (let ((declarations (loop while (and (consp (first body))
                                       (eq (first (first body)) 'declare))
                            collect (pop body))))
    `(sb-alien:define-alien-callable ,name ,result-type ,typed-lambda-list
       ,@declarations
       ,@(mapcar #'first typed-lambda-list)
       (handler-case (progn ,@body)
         (error (condition)
           (report-callback-error ',name condition)
           ,(case result-type
              (sb-alien:void nil)
              (sb-alien:double 0d0)
              (sb-alien:single-float 0f0)
              (t 0)))))))

(defun callable-address (name)
  "The address of the alien callable NAME, as a method implementation."
  (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:alien-callable-function name))))

(defun add-ivar (class name size alignment encoding)
  (unless (/= 0 (sb-alien:alien-funcall
                 (sb-alien:extern-alien "class_addIvar"
                                        (function (sb-alien:unsigned 8)
                                                  sb-alien:unsigned-long sb-alien:c-string
                                                  sb-alien:unsigned-long (sb-alien:unsigned 8)
                                                  sb-alien:c-string))
                 class name size alignment encoding))
    (error "Could not add the instance variable ~s." name)))

(defun add-class-method (class selector-name encoding callable-name)
  (unless (/= 0 (sb-alien:alien-funcall
                 (sb-alien:extern-alien "class_addMethod"
                                        (function (sb-alien:unsigned 8)
                                                  sb-alien:unsigned-long sb-alien:unsigned-long
                                                  sb-alien:unsigned-long sb-alien:c-string))
                 class (register-selector selector-name)
                 (callable-address callable-name) encoding))
    (error "Could not add the method ~s." selector-name)))

(defun ensure-class (name superclass-name &key ivars methods)
  "The Objective-C class NAME, made as a subclass of SUPERCLASS-NAME if it does
not exist yet. IVARS are (name size log2-alignment encoding); METHODS are
(selector type-encoding callable-name), where the callable is an alien callable
whose first two arguments are the receiver and the selector.

If the class exists already (the file was loaded twice) it is returned as it
is. Its methods call the callables by name, so redefining a callable takes
effect."
  (or (find-objc-class name nil)
      (let ((class (sb-alien:alien-funcall
                    (sb-alien:extern-alien "objc_allocateClassPair"
                                           (function sb-alien:unsigned-long
                                                     sb-alien:unsigned-long sb-alien:c-string
                                                     sb-alien:unsigned-long))
                    (find-objc-class superclass-name) name 0)))
        (when (zerop class)
          (error "Could not make the class ~s." name))
        (loop for (ivar-name size alignment encoding) in ivars
              do (add-ivar class ivar-name size alignment encoding))
        (loop for (selector-name encoding callable-name) in methods
              do (add-class-method class selector-name encoding callable-name))
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "objc_registerClassPair"
                                (function sb-alien:void sb-alien:unsigned-long))
         class)
        class)))

(defun ivar-offset (class name)
  "The byte offset of the instance variable NAME within an instance of CLASS."
  (let ((ivar (sb-alien:alien-funcall
               (sb-alien:extern-alien "class_getInstanceVariable"
                                      (function sb-alien:unsigned-long
                                                sb-alien:unsigned-long sb-alien:c-string))
               class name)))
    (when (zerop ivar)
      (error "The class has no instance variable ~s." name))
    (sb-alien:alien-funcall
     (sb-alien:extern-alien "ivar_getOffset"
                            (function sb-alien:long sb-alien:unsigned-long))
     ivar)))

(defmacro instance-slot (object offset)
  "The 64-bit word at OFFSET (from IVAR-OFFSET) in the Objective-C OBJECT. Can
be set with SETF. Reading it costs no call into the runtime."
  `(sb-sys:sap-ref-64 (sb-sys:int-sap ,object) ,offset))

;;; Lisp objects behind Objective-C objects. A class made at run time keeps an
;;; integer in an instance variable; that integer indexes this table, so a
;;; method that receives only the Objective-C object can find its Lisp side.

(defvar *objects* (make-array 64 :initial-element nil))
(defvar *objects-lock* (sb-thread:make-mutex :name "objc objects"))

(defun register-object (object)
  "Remember the Lisp OBJECT and return its index (never zero)."
  (sb-thread:with-mutex (*objects-lock*)
    (let ((index (or (position nil *objects* :start 1)
                     (let ((old *objects*))
                       (setf *objects* (make-array (* 2 (length old)) :initial-element nil))
                       (replace *objects* old)
                       (length old)))))
      (setf (svref *objects* index) object)
      index)))

(declaim (inline registered-object))
(defun registered-object (index)
  (svref *objects* index))

(defun unregister-object (index)
  (sb-thread:with-mutex (*objects-lock*)
    (setf (svref *objects* index) nil)))
