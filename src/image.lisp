(defpackage #:almighty-bolt/image
  (:use #:cl)
  (:export #:image
           #:image-p
           #:make-image
           #:make-premultiplied-image
           #:image-id
           #:image-pixel-width
           #:image-pixel-height
           #:image-scale
           #:image-width
           #:image-height
           #:image-pixels))

(in-package #:almighty-bolt/image)

;;; An image is its pixels, kept in the Lisp image until the renderer first draws it and copies
;;; them to the GPU, where they stay. Each image made has an id of its own, by which a renderer
;;; finds it again, so an image is made once (at top level, or kept with STATE-LET) and drawn as
;;; often as wanted: one made every frame would be copied to the GPU every frame.
;;;
;;; The pixels are red, green, blue and alpha, a byte each, row by row from the top, with red,
;;; green and blue already multiplied by alpha, as the GPU blends them. MAKE-IMAGE takes them
;;; unmultiplied, as image files and colour pickers have them.

(defvar *next-id* (list 0)
  "The id the next image gets, in a cons so that it can be taken atomically.")

(defstruct (image (:constructor %make-image (id pixel-width pixel-height scale pixels))
                  (:copier nil))
  (id 0 :type fixnum :read-only t)
  (pixel-width 1 :type (integer 1) :read-only t)
  (pixel-height 1 :type (integer 1) :read-only t)
  (scale 1f0 :type single-float :read-only t)
  (pixels nil :type (simple-array (unsigned-byte 8) (*)) :read-only t))

(defmethod print-object ((image image) stream)
  (print-unreadable-object (image stream :type t)
    (format stream "~d by ~d pixels at ~a a point" (image-pixel-width image)
            (image-pixel-height image) (image-scale image))))

(defun make-image (width height pixels &key (scale 1))
  "An image WIDTH by HEIGHT pixels, from PIXELS: a vector of WIDTH times HEIGHT times four bytes,
red, green, blue and alpha for each pixel, row by row from the top, with red, green and blue not
multiplied by alpha. SCALE is how many pixels it has to a point: an image made for a screen of two
pixels a point (an \"@2x\" image) has 2, and is drawn half as many points across as it has pixels."
  (check-type width (integer 1))
  (check-type height (integer 1))
  (check-type scale (real (0)))
  (let ((count (* 4 width height)))
    (unless (and (typep pixels 'vector) (= (length pixels) count))
      (error "An image ~d by ~d pixels has ~d bytes (four a pixel), not ~a." width height count
             (if (typep pixels 'vector) (length pixels) pixels)))
    (let ((premultiplied (make-array count :element-type '(unsigned-byte 8))))
      (loop :for i :from 0 :below count :by 4
            :for alpha = (aref pixels (+ i 3))
            :do (dotimes (channel 3)
                  (setf (aref premultiplied (+ i channel))
                        (round (* (aref pixels (+ i channel)) alpha) 255)))
                (setf (aref premultiplied (+ i 3)) alpha))
      (make-premultiplied-image width height premultiplied scale))))

(defun make-premultiplied-image (width height pixels scale)
  "An image from PIXELS whose red, green and blue are already multiplied by alpha, which it keeps."
  (%make-image (sb-ext:atomic-incf (car *next-id*)) width height (float scale 1f0) pixels))

(defun image-width (image)
  "How wide IMAGE is drawn at its own size, in points."
  (/ (image-pixel-width image) (image-scale image)))

(defun image-height (image)
  "How tall IMAGE is drawn at its own size, in points."
  (/ (image-pixel-height image) (image-scale image)))
