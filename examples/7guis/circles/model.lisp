(defpackage #:almighty-bolt-7guis/circles/model
  (:use #:cl)
  (:export #:circle
           #:circle-id
           #:circle-x
           #:circle-y
           #:circle-diameter
           #:+default-diameter+
           #:drawing
           #:make-drawing
           #:drawing-circles
           #:drawing-can-undo-p
           #:drawing-can-redo-p
           #:drawing-add
           #:drawing-resize
           #:drawing-undo
           #:drawing-redo
           #:drawing-find
           #:drawing-circle-at))

(in-package #:almighty-bolt-7guis/circles/model)

;;; The circle drawer's logic: circles on a canvas, with undo and redo. A drawing is a
;;; value; every change returns a new one that remembers the one before, so that undo is
;;; going back to it and redo is going forward again.

(defconstant +default-diameter+ 40)

(defstruct (circle (:constructor make-circle (id x y diameter)) (:copier nil))
  (id 0 :type fixnum :read-only t)
  (x 0 :type real :read-only t)
  (y 0 :type real :read-only t)
  (diameter +default-diameter+ :type real :read-only t))

(defstruct (drawing (:constructor %make-drawing (circles next-id past future))
                    (:copier nil))
  (circles '() :type list :read-only t)
  (next-id 0 :type fixnum :read-only t)
  ;; The drawings before this one, nearest first, and those undone, nearest first.
  (past '() :type list :read-only t)
  (future '() :type list :read-only t))

(defun make-drawing ()
  (%make-drawing '() 0 '() '()))

(defun drawing-can-undo-p (drawing)
  (and (drawing-past drawing) t))

(defun drawing-can-redo-p (drawing)
  (and (drawing-future drawing) t))

(defun change (drawing circles next-id)
  "A drawing with CIRCLES that remembers DRAWING as what came before. A change ends the
future: what was undone cannot be redone after it."
  (%make-drawing circles next-id (cons drawing (drawing-past drawing)) '()))

(defun drawing-add (drawing x y &optional (diameter +default-diameter+))
  "DRAWING with a circle of DIAMETER centred on X, Y."
  (change drawing
          (append (drawing-circles drawing)
                  (list (make-circle (drawing-next-id drawing) x y diameter)))
          (1+ (drawing-next-id drawing))))

(defun drawing-find (drawing id)
  (find id (drawing-circles drawing) :key #'circle-id))

(defun drawing-resize (drawing id diameter)
  "DRAWING with circle ID given DIAMETER, as one change that can be undone."
  (let ((circle (drawing-find drawing id)))
    (if (or (null circle) (= diameter (circle-diameter circle)))
        drawing
        (change drawing
                (mapcar (lambda (c)
                          (if (= (circle-id c) id)
                              (make-circle id (circle-x c) (circle-y c) diameter)
                              c))
                        (drawing-circles drawing))
                (drawing-next-id drawing)))))

(defun drawing-undo (drawing)
  "The drawing before the last change, which can redo it; DRAWING if there is none."
  (let ((previous (first (drawing-past drawing))))
    (if previous
        (%make-drawing (drawing-circles previous) (drawing-next-id previous)
                       (drawing-past previous)
                       (cons drawing (drawing-future drawing)))
        drawing)))

(defun drawing-redo (drawing)
  (let ((next (first (drawing-future drawing))))
    (if next
        (%make-drawing (drawing-circles next) (drawing-next-id next)
                       (cons drawing (drawing-past drawing))
                       (rest (drawing-future drawing)))
        drawing)))

(defun drawing-circle-at (drawing x y)
  "The circle whose area holds the point X, Y and whose centre is nearest it, or NIL.
Later circles win a tie, as they are drawn on top."
  (let ((best nil) (best-distance nil))
    (dolist (circle (drawing-circles drawing) best)
      (let* ((dx (- x (circle-x circle)))
             (dy (- y (circle-y circle)))
             (distance (+ (* dx dx) (* dy dy)))
             (radius (/ (circle-diameter circle) 2)))
        (when (and (<= distance (* radius radius))
                   (or (null best-distance) (<= distance best-distance)))
          (setf best circle
                best-distance distance))))))
