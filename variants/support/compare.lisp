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
;;; that adds its places up in another order rounds a single float another way, and one that
;;; draws a form row by row draws the same commands in another order. This says how far apart
;;; two lists are, with the order of the commands not counting.

(defun commands-of (list)
  "The commands of LIST as (kind color parameter x y width height), sorted by kind, colour,
parameter, then place."
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

(defun compare-display-lists (a b)
  "How far apart A and B are, as two values: the largest difference in any coordinate of any
command, and a keyword saying more if they are not even the same commands: :count if they have
a different number, :commands if the same number of commands differ in kind, colour or
parameter."
  (let ((count-a (display-list-count a))
        (count-b (display-list-count b)))
    (if (/= count-a count-b)
        (values nil :count)
        (let ((largest 0f0))
          (loop :for x :in (commands-of a)
                :for y :in (commands-of b)
                :do (unless (and (= (first x) (first y)) (= (second x) (second y))
                                 (= (third x) (third y)))
                      (return-from compare-display-lists (values nil :commands)))
                    (loop :for p :in (nthcdr 3 x)
                          :for q :in (nthcdr 3 y)
                          :do (setf largest (max largest (abs (- p q))))))
          (values largest nil)))))
