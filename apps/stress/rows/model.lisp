(defpackage #:almighty-bolt-stress/rows/model
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:+points-per-line+
                #:+key-up+
                #:+key-down+
                #:+key-page-up+
                #:+key-page-down+
                #:+key-home+
                #:+key-end+)
  (:export #:+row-count+
           #:+row-height+
           #:+visible-rows+
           #:+list-width+
           #:+list-height+
           #:+bar-width+
           #:+rows-per-line+
           #:+thumb-height+
           #:clamp-top
           #:row-at
           #:thumb-top
           #:top-for-thumb
           #:on-bar-p
           #:key-move
           #:row-id-text
           #:row-name-text
           #:row-value-text
           #:status-text))

(in-package #:almighty-bolt-stress/rows/model)

;;; The million-row list (the third stress app): a list of a million rows of which 18 show, a
;;; scroll bar at its right, a row selected. The rows are not kept: what a row shows is worked out
;;; from its index when it is drawn, so that the memory a variant uses is its own and not a
;;; million rows'. Everything here is on plain numbers (the index of the first row that shows, the
;;; index of the selected row), so that each variant holds those as it likes.

(defconstant +row-count+ 1000000)
(defconstant +row-height+ 22)
(defconstant +visible-rows+ 18)
(defconstant +list-width+ 600)
(defconstant +list-height+ (* +visible-rows+ +row-height+))
(defconstant +bar-width+ 14)
(defconstant +rows-per-line+ (float (/ +points-per-line+ +row-height+) 1f0)
  "How far a line of the wheel scrolls the list, in rows: a line is 24 points and a row 22.")
(defconstant +thumb-height+ 24 "The thumb of the bar: a million rows would make it a hair.")

(defun clamp-top (top)
  "TOP, the index of the first row that shows, kept so that the list does not scroll past its
end."
  (max 0 (min top (- +row-count+ +visible-rows+))))

(defun on-bar-p (x)
  "True if the point at X across the list is in the scroll bar."
  (>= x (- +list-width+ +bar-width+)))

(defun row-at (x y top)
  "The index of the row at the point X, Y of the list when it is scrolled to TOP, or NIL if the
point is in the bar or outside."
  (when (and (<= 0 x) (< x (- +list-width+ +bar-width+)) (<= 0 y) (< y +list-height+))
    (+ top (floor y +row-height+))))

(defun thumb-top (top)
  "How far down the bar the thumb is when the list is scrolled to TOP."
  (let ((room (- +list-height+ +thumb-height+))
        (span (- +row-count+ +visible-rows+)))
    (* room (/ (float top 1f0) span))))

(defun top-for-thumb (y)
  "The first row for a thumb whose middle is at Y of the bar."
  (let ((room (- +list-height+ +thumb-height+))
        (span (- +row-count+ +visible-rows+)))
    (clamp-top (round (* span (/ (- y (/ +thumb-height+ 2)) room))))))

(defun key-move (key selected top)
  "What the key KEY does to the selected row and the first row: the new SELECTED, the new TOP and
whether the key was one that moves. A selected row that would go off the list scrolls it just far
enough."
  (flet ((to (new)
           (let* ((new (max 0 (min (1- +row-count+) new)))
                  (top (cond ((< new top) new)
                             ((>= new (+ top +visible-rows+)) (1+ (- new +visible-rows+)))
                             (t top))))
             (values new (clamp-top top) t))))
    (let ((selected (or selected 0)))
      (cond ((eql key +key-up+) (to (1- selected)))
            ((eql key +key-down+) (to (1+ selected)))
            ((eql key +key-page-up+) (to (- selected +visible-rows+)))
            ((eql key +key-page-down+) (to (+ selected +visible-rows+)))
            ((eql key +key-home+) (to 0))
            ((eql key +key-end+) (to (1- +row-count+)))
            (t (values selected top nil))))))

;;; What a row shows. The names and values are from small tables; only the number of the row is
;;; formatted, and the last 64 are kept so that drawing the same rows again formats nothing.

(defparameter +first-names+
  #("Ada" "Alan" "Barbara" "Brian" "Dennis" "Donald" "Edsger" "Frances" "Grace" "Guy"
    "John" "Ken" "Leslie" "Linus" "Margaret" "Niklaus" "Radia" "Rich" "Tim" "Yukihiro"))
(defparameter +last-names+
  #("Lovelace" "Turing" "Liskov" "Kernighan" "Ritchie" "Knuth" "Dijkstra" "Allen" "Hopper"
    "Steele" "McCarthy" "Thompson" "Lamport" "Torvalds" "Hamilton" "Wirth" "Perlman" "Hickey"
    "Berners-Lee" "Matsumoto"))

(defparameter +names+
  (let ((table (make-array 400)))
    (dotimes (i 400 table)
      (setf (svref table i)
            (format nil "~a ~a" (svref +first-names+ (mod i 20)) (svref +last-names+ (floor i 20)))))))

(defparameter +values+
  (let ((table (make-array 1000)))
    (dotimes (i 1000 table)
      (setf (svref table i) (format nil "~d.~d" (floor i 10) (mod i 10))))))

(defparameter +ids+ (make-array 64 :initial-element nil)
  "The text of the number of the last 64 rows, each as (index . text), by index mod 64.")

(defun row-id-text (index)
  "The text of the number of row INDEX, as 'Row 0001234' (the first row is row 1)."
  (let* ((slot (mod index 64))
         (entry (svref +ids+ slot)))
    (if (and entry (eql (car entry) index))
        (cdr entry)
        (cdr (setf (svref +ids+ slot)
                   (cons index (format nil "Row ~7,'0d" (1+ index))))))))

(defun row-name-text (index)
  (svref +names+ (mod (* index 31) 400)))

(defun row-value-text (index)
  (svref +values+ (mod (* index 7919) 1000)))

(defun status-text (selected)
  "What the status line says for the selected row, or for none."
  (if selected
      (format nil "Row ~:d of ~:d selected: ~a" (1+ selected) +row-count+ (row-name-text selected))
      "No row selected"))
