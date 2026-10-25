(defpackage #:almighty-bolt-7guis/t/cells
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-7guis/cells/model)))

(in-package #:almighty-bolt-7guis/t/cells)

(defun sheet-with (&rest cells)
  "A sheet with CELLS, each (reference text)."
  (let ((sheet (m:make-sheet)))
    (dolist (cell cells sheet)
      (multiple-value-bind (column row) (m:parse-ref (first cell))
        (m:sheet-set sheet column row (second cell))))))

(defun show (sheet reference)
  (multiple-value-bind (column row) (m:parse-ref reference)
    (m:sheet-display sheet column row)))

(defun value (sheet reference)
  (multiple-value-bind (column row) (m:parse-ref reference)
    (m:sheet-value sheet column row)))

(define-test references-name-a-column-and-a-row ()
  (assert-equal '(0 0) (multiple-value-list (m:parse-ref "A0")))
  (assert-equal '(25 99) (multiple-value-list (m:parse-ref "Z99")))
  (assert-equal '(1 12) (multiple-value-list (m:parse-ref "b12")))
  (dolist (text '("" "A" "A100" "AA1" "1A" "A-1" "Z"))
    (assert-false (m:parse-ref text) text))
  (assert-equal "C7" (m:format-ref 2 7)))

(define-test an-empty-cell-shows-nothing-and-is-zero-in-a-formula ()
  (let ((sheet (sheet-with '("B0" "=A0+1"))))
    (assert-equal "" (show sheet "A0"))
    (assert-equal "1" (show sheet "B0"))))

(define-test text-and-numbers-show-as-they-are ()
  (let ((sheet (sheet-with '("A0" "hello") '("A1" "42") '("A2" "3.50"))))
    (assert-equal "hello" (show sheet "A0"))
    (assert-equal "42" (show sheet "A1"))
    (assert-equal "3.5" (show sheet "A2"))
    (assert-equal 42d0 (value sheet "A1"))
    (assert-equal "hello" (value sheet "A0"))))

(define-test formulas-do-arithmetic-with-the-usual-precedence ()
  (let ((sheet (sheet-with '("A0" "=1+2*3") '("A1" "=(1+2)*3") '("A2" "=10/4") '("A3" "=-2+5")
                           '("A4" "=2*-3") '("A5" "=1 - 1 - 1"))))
    (assert-equal "7" (show sheet "A0"))
    (assert-equal "9" (show sheet "A1"))
    (assert-equal "2.5" (show sheet "A2"))
    (assert-equal "3" (show sheet "A3"))
    (assert-equal "-6" (show sheet "A4"))
    (assert-equal "-1" (show sheet "A5"))))

(define-test formulas-use-other-cells-and-follow-their-changes ()
  (let ((sheet (sheet-with '("A0" "5") '("A1" "=A0*2") '("A2" "=A1+A0"))))
    (assert-equal "15" (show sheet "A2"))
    (m:sheet-set sheet 0 0 "10")
    (assert-equal "30" (show sheet "A2"))))

(define-test the-functions-work-over-ranges ()
  (let ((sheet (sheet-with '("A0" "1") '("A1" "2") '("A2" "3") '("A3" "text") '("B0" "=SUM(A0:A3)")
                           '("B1" "=AVG(A0:A2)") '("B2" "=MIN(A0:A2)") '("B3" "=MAX(A0:A2, 10)")
                           '("B4" "=COUNT(A0:A3)"))))
    (assert-equal "6" (show sheet "B0"))
    (assert-equal "2" (show sheet "B1"))
    (assert-equal "1" (show sheet "B2"))
    (assert-equal "10" (show sheet "B3"))
    (assert-equal "3" (show sheet "B4"))))

(define-test a-range-can-be-written-backwards-or-across ()
  (let ((sheet (sheet-with '("A0" "1") '("B0" "2") '("A1" "3") '("B1" "4")
                           '("C0" "=SUM(B1:A0)"))))
    (assert-equal "10" (show sheet "C0"))))

(define-test a-formula-that-refers-to-itself-is-a-cycle ()
  (let ((sheet (sheet-with '("A0" "=A0+1") '("B0" "=C0") '("C0" "=B0") '("D0" "=A0+1"))))
    (assert-eql :cycle (value sheet "A0"))
    (assert-equal "#CYCLE" (show sheet "A0"))
    (assert-equal "#CYCLE" (show sheet "B0"))
    (assert-equal "#CYCLE" (show sheet "C0"))
    ;; And what depends on a cycle is one too.
    (assert-equal "#CYCLE" (show sheet "D0"))
    ;; Breaking the cycle mends it.
    (m:sheet-set sheet 0 0 "1")
    (assert-equal "2" (show sheet "D0"))))

(define-test faults-are-errors ()
  (let ((sheet (sheet-with '("A0" "=1/0") '("A1" "=1+") '("A2" "=(1") '("A3" "=NOPE(1)")
                           '("A4" "=A5+1") '("A5" "words") '("A6" "=SUM()") '("A7" "=1 2")
                           '("A8" "=A0+1") '("A9" "=#"))))
    (dolist (reference '("A0" "A1" "A2" "A3" "A4" "A6" "A7" "A8" "A9"))
      (assert-equal "#ERR" (show sheet reference) reference))))

(define-test the-version-counts-edits ()
  (let ((sheet (m:make-sheet)))
    (assert-eql 0 (m:sheet-version sheet))
    (m:sheet-set sheet 0 0 "1")
    (m:sheet-set sheet 0 0 "2")
    (assert-eql 2 (m:sheet-version sheet))))

(define-test the-text-of-a-cell-is-what-was-typed ()
  (let ((sheet (sheet-with '("A0" "=1+1"))))
    (assert-equal "=1+1" (m:sheet-text sheet 0 0))
    (assert-equal "" (m:sheet-text sheet 1 0))
    (m:sheet-set sheet 0 0 "")
    (assert-equal "" (m:sheet-text sheet 0 0))))

(define-test the-whole-sheet-can-hold-a-long-chain ()
  ;; 99 cells each adding one to the one above.
  (let ((sheet (m:make-sheet)))
    (m:sheet-set sheet 0 0 "1")
    (loop :for row :from 1 :below 100
          :do (m:sheet-set sheet 0 row (format nil "=A~d+1" (1- row))))
    (assert-equal "100" (show sheet "A99"))))
