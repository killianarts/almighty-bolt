(defpackage #:almighty-bolt/machine/png
  (:use #:cl)
  (:export #:png-octets
           #:write-png))

(in-package #:almighty-bolt/machine/png)

;;; A PNG writer, for looking at what was drawn: a screenshot goes to a person or to an agent, over a
;;; socket, and a window's pixels uncompressed are megabytes. The deflating is the system's zlib, which
;;; is part of macOS, so that no Lisp library is needed.

(cffi:define-foreign-library libz
  (:darwin "libz.dylib")
  (:unix (:or "libz.so.1" "libz.so")))

(cffi:use-foreign-library libz)

(defvar *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c)
                      (logxor #xEDB88320 (ash c -1))
                      (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (bytes &optional (crc #xFFFFFFFF))
  "The running CRC-32 of BYTES, not yet inverted."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes)
           (type (unsigned-byte 32) crc))
  (loop :for byte :across bytes
        :do (setf crc (logxor (aref *crc-table* (logand (logxor crc byte) #xFF))
                              (ash crc -8))))
  crc)

(defun u32-bytes (value)
  (let ((bytes (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 bytes)
      (setf (aref bytes i) (ldb (byte 8 (* 8 (- 3 i))) value)))))

(defun chunk-octets (type data)
  "The bytes of one PNG chunk of TYPE (a string of four letters) holding DATA."
  (let ((type-bytes (map '(simple-array (unsigned-byte 8) (*)) #'char-code type)))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (u32-bytes (length data))
                 type-bytes
                 data
                 (u32-bytes (logxor #xFFFFFFFF (crc32 data (crc32 type-bytes)))))))

(defun zlib-compress (raw)
  "RAW as a zlib stream, deflated by the system's zlib."
  (declare (type (simple-array (unsigned-byte 8) (*)) raw))
  (let* ((bound (cffi:foreign-funcall "compressBound" :unsigned-long (length raw) :unsigned-long))
         (out (make-array bound :element-type '(unsigned-byte 8))))
    (cffi:with-foreign-object (size :unsigned-long)
      (setf (cffi:mem-ref size :unsigned-long) bound)
      (cffi:with-pointer-to-vector-data (destination out)
        (cffi:with-pointer-to-vector-data (source raw)
          (let ((status (cffi:foreign-funcall "compress2"
                                              :pointer destination :pointer size
                                              :pointer source :unsigned-long (length raw)
                                              :int 6 :int)))
            (unless (zerop status)
              (error "zlib could not compress the image (status ~d)." status))))
        (subseq out 0 (cffi:mem-ref size :unsigned-long))))))

(defun png-octets (width height bgra)
  "A PNG of WIDTH by HEIGHT pixels, as a vector of bytes, from BGRA, a vector of four bytes a
pixel (blue, green, red, alpha), row by row from the top."
  (let* ((row (* 4 width))
         (raw (make-array (* height (1+ row)) :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
    (dotimes (y height)
      (let ((out (* y (1+ row)))
            (in (* y row)))
        (setf (aref raw out) 0)
        (dotimes (x width)
          (let ((o (+ out 1 (* 4 x)))
                (i (+ in (* 4 x))))
            (setf (aref raw o) (aref bgra (+ i 2))
                  (aref raw (+ o 1)) (aref bgra (+ i 1))
                  (aref raw (+ o 2)) (aref bgra i)
                  (aref raw (+ o 3)) (aref bgra (+ i 3)))))))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 #(137 80 78 71 13 10 26 10)
                 (chunk-octets "IHDR" (concatenate '(simple-array (unsigned-byte 8) (*))
                                                   (u32-bytes width) (u32-bytes height)
                                                   (vector 8 6 0 0 0)))
                 (chunk-octets "IDAT" (zlib-compress raw))
                 (chunk-octets "IEND" (make-array 0 :element-type '(unsigned-byte 8))))))

(defun write-png (path width height bgra)
  "Write a PNG of WIDTH by HEIGHT pixels to PATH from BGRA (see PNG-OCTETS)."
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence (png-octets width height bgra) stream))
  path)
