(defpackage #:almighty-bolt-variant-support/compare
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:display-list-count
                #:display-list-kind
                #:display-list-x
                #:display-list-y
                #:display-list-width
                #:display-list-height
                #:display-list-parameter
                #:display-list-color)
  (:export #:commands-of
           #:compare-display-lists))

(in-package #:almighty-bolt-variant-support/compare)

;;; Two display lists that draw the same thing need not be the same bit for bit: a front end
;;; that adds its places up in another order rounds a single float another way, one that draws
;;; a form row by row draws the same commands in another order, and one that puts text on whole
;;; pixels another way puts some of it a pixel off. This says how far apart two lists are, with
;;; the order of the commands not counting: each command of one is matched to the nearest
;;; command of the other that is the same kind, colour and parameter (the glyph).

(defun commands-of (list)
  "The commands of LIST as (kind color parameter x y width height), sorted."
  (let ((commands (loop :for i :below (display-list-count list)
                        :collect (list (display-list-kind list i) (display-list-color list i)
                                       (display-list-parameter list i)
                                       (display-list-x list i) (display-list-y list i)
                                       (display-list-width list i) (display-list-height list i)))))
    (sort commands
          (lambda (a b)
            (loop :for x :in a
                  :for y :in b
                  :do (cond ((< x y) (return t))
                            ((> x y) (return nil))))))))

(defun buckets (list)
  "The commands of LIST by kind, colour and parameter: a hash table of lists of (x y w h)."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (command (commands-of list) table)
      (push (nthcdr 3 command) (gethash (subseq command 0 3) table)))))

(defun distance (a b)
  (reduce #'max (mapcar (lambda (p q) (abs (- p q))) a b)))

(defun compare-display-lists (a b)
  "How far apart A and B are, as two values: the largest distance from a command of one to the
command of the other it is matched to (the largest difference in x, y, width or height), and a
keyword saying more if they are not even the same commands: :count if they have a different
number, :commands if they differ in how many there are of some kind, colour and parameter."
  (if (/= (display-list-count a) (display-list-count b))
      (values nil :count)
      (let ((buckets-a (buckets a))
            (buckets-b (buckets b))
            (largest 0f0))
        (maphash (lambda (key commands-a)
                   (let ((commands-b (copy-list (gethash key buckets-b))))
                     (unless (= (length commands-a) (length commands-b))
                       (return-from compare-display-lists (values nil :commands)))
                     (dolist (command commands-a)
                       (let ((nearest (first commands-b)))
                         (dolist (candidate (rest commands-b))
                           (when (< (distance command candidate) (distance command nearest))
                             (setf nearest candidate)))
                         (setf largest (max largest (distance command nearest))
                               commands-b (remove nearest commands-b :count 1 :test #'eq))))))
                 buckets-a)
        (values largest nil))))
