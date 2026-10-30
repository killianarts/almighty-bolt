(defpackage #:almighty-bolt/ui/hash
  (:use #:cl)
  (:export #:mix
           #:hash-of
           #:+seed+))

(in-package #:almighty-bolt/ui/hash)

;;; The hashes the immediate variant compares to tell whether what it described this frame is
;;; what it described last frame: each box has one for what decides its size and one for what
;;; decides how it looks, folded together with those of its children. A hash is 62 bits and
;;; made with arithmetic that does not allocate.

(defconstant +seed+ #x2545F4914F6CDD1D)

(declaim (inline mix))
(defun mix (hash value)
  "HASH, a 62-bit number, with VALUE (any fixnum) folded into it."
  (declare (type (unsigned-byte 62) hash) (type fixnum value)
           (optimize speed (safety 0)))
  (let ((h (ldb (byte 64 0) (logxor hash (ldb (byte 64 0) value)))))
    (declare (type (unsigned-byte 64) h))
    (setf h (ldb (byte 64 0) (* h 1099511628211)))
    (ldb (byte 62 0) (logxor h (ash h -29)))))

(declaim (inline hash-of))
(defun hash-of (object)
  "A fixnum for OBJECT: numbers and characters by value, strings by their characters, T and
NIL as themselves, and anything else by SXHASH. Inline, so that a call on a slot of a known
type is the hash of that type and no dispatch."
  (typecase object
    (null 0)
    ((eql t) 1)
    (fixnum object)
    (single-float #+sbcl (sb-kernel:single-float-bits object)
                  #-sbcl (sxhash object))
    (t (sxhash object))))
