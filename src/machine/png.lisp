(defpackage #:almighty-bolt/machine/png
  (:use #:cl)
  (:export #:png-octets
           #:write-png))

(in-package #:almighty-bolt/machine/png)

;;; A PNG writer with no compression: the pixels go into stored deflate blocks, which
;;; every PNG reader accepts, so that no library is needed. For looking at what was
;;; drawn, not for keeping.

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

(defun adler32 (bytes)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((a 1) (b 0))
    (declare (type (unsigned-byte 32) a b))
    (loop :for byte :across bytes
          :do (setf a (mod (+ a byte) 65521)
                    b (mod (+ b a) 65521)))
    (logior (ash b 16) a)))

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

(defun zlib-stored (raw)
  "RAW wrapped as a zlib stream of stored blocks."
  (declare (type (simple-array (unsigned-byte 8) (*)) raw))
  (let* ((blocks (max 1 (ceiling (length raw) 65535)))
         (out (make-array (+ 2 (length raw) (* 5 blocks) 4) :element-type '(unsigned-byte 8)))
         (position 0))
    (flet ((put (byte) (setf (aref out position) byte) (incf position)))
      (put #x78) (put #x01)
      (dotimes (block blocks)
        (let* ((start (* block 65535))
               (end (min (length raw) (+ start 65535)))
               (size (- end start)))
          (put (if (= block (1- blocks)) 1 0))
          (put (ldb (byte 8 0) size)) (put (ldb (byte 8 8) size))
          (put (ldb (byte 8 0) (logxor size #xFFFF))) (put (ldb (byte 8 8) (logxor size #xFFFF)))
          (replace out raw :start1 position :start2 start :end2 end)
          (incf position size)))
      (let ((adler (adler32 raw)))
        (dotimes (i 4)
          (put (ldb (byte 8 (* 8 (- 3 i))) adler)))))
    out))

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
                 (chunk-octets "IDAT" (zlib-stored raw))
                 (chunk-octets "IEND" (make-array 0 :element-type '(unsigned-byte 8))))))

(defun write-png (path width height bgra)
  "Write a PNG of WIDTH by HEIGHT pixels to PATH from BGRA (see PNG-OCTETS)."
  (with-open-file (stream path :direction :output :if-exists :supersede
                               :element-type '(unsigned-byte 8))
    (write-sequence (png-octets width height bgra) stream))
  path)
