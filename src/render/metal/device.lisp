(defpackage #:almighty-bolt/render/metal/device
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:ns-string
                #:lisp-string
                #:release)
  (:export #:default-device
           #:make-command-queue
           #:compile-library
           #:make-function
           #:make-pipeline-state
           #:ns-error-description))

(in-package #:almighty-bolt/render/metal/device)

;;; The Metal device, its command queue, and the objects built from them that
;;; are made once: shader libraries and pipeline states. None of this runs per
;;; frame, so it does not try to avoid allocating.

(defun default-device ()
  "The system's default Metal device, as an integer."
  (let ((device (sb-alien:alien-funcall
                 (sb-alien:extern-alien "MTLCreateSystemDefaultDevice"
                                        (function sb-alien:unsigned-long)))))
    (when (zerop device)
      (error "This machine has no Metal device."))
    device))

(defun make-command-queue (device)
  "A new command queue on DEVICE. The caller owns it."
  (msg-send :id device "newCommandQueue"))

(defun ns-error-description (ns-error)
  "The message of NS-ERROR, or a placeholder if there is none."
  (if (zerop ns-error)
      "no error was reported"
      (lisp-string (msg-send :id ns-error "localizedDescription"))))

(defmacro with-error-slot ((address) &body body)
  "Run BODY with ADDRESS bound to the address of a word that Metal can store an
NSError into, and return BODY's value and the error stored there (zero if none)."
  `(sb-alien:with-alien ((slot sb-alien:unsigned-long 0))
     (let* ((,address (sb-sys:sap-int (sb-alien:alien-sap (sb-alien:addr slot))))
            (result (progn ,@body)))
       (values result slot))))

(defun compile-library (device source)
  "Compile the Metal Shading Language SOURCE into a library, at run time. Metal's
compiler is part of the operating system, so this needs no Xcode. The caller owns
the result. Signals an error with the compiler's message if the source is bad."
  (let ((string (ns-string source)))
    (multiple-value-bind (library error)
        (with-error-slot (error-address)
          (msg-send :id device "newLibraryWithSource:options:error:"
                    (:id string) (:id 0) (:pointer error-address)))
      (release string)
      (when (zerop library)
        (error "Metal could not compile the shaders: ~a" (ns-error-description error)))
      library)))

(defun make-function (library name)
  "The function NAME of LIBRARY. The caller owns it."
  (let* ((string (ns-string name))
         (function (msg-send :id library "newFunctionWithName:" (:id string))))
    (release string)
    (when (zerop function)
      (error "The shader library has no function ~s." name))
    function))

(defun make-pipeline-state (device descriptor)
  "A render pipeline state for DESCRIPTOR. The caller owns it."
  (multiple-value-bind (state error)
      (with-error-slot (error-address)
        (msg-send :id device "newRenderPipelineStateWithDescriptor:error:"
                  (:id descriptor) (:pointer error-address)))
    (when (zerop state)
      (error "Metal could not make the pipeline: ~a" (ns-error-description error)))
    state))
