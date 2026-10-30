(defpackage #:almighty-bolt-7guis/cells/model
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/numbers
                #:parse-number
                #:format-number)
  (:export #:+columns+
           #:+rows+
           #:column-name
           #:parse-ref
           #:format-ref
           #:sheet
           #:make-sheet
           #:sheet-version
           #:sheet-text
           #:sheet-set
           #:sheet-value
           #:sheet-display))

(in-package #:almighty-bolt-7guis/cells/model)

;;; The cells task's logic: a sheet of 26 columns (A to Z) and 100 rows (0 to 99), each
;;; cell holding text. Text that starts with = is a formula over numbers, other cells
;;; (A0, Z99), ranges (A0:A9) and the functions SUM, AVG, MIN, MAX and COUNT, with + - * /
;;; and brackets. A cell's value is worked out when it is asked for and kept until
;;; anything is edited. A formula that refers to itself, directly or not, has the error
;;; :CYCLE, and any other fault (a bad formula, text where a number is wanted, a division
;;; by zero) the error :ERROR.

(defconstant +columns+ 26)
(defconstant +rows+ 100)

(defun column-name (column)
  (string (code-char (+ (char-code #\A) column))))

(defun format-ref (column row)
  (format nil "~a~d" (column-name column) row))

(defun parse-ref (text)
  "The column and row a reference such as B12 names, as two values, or NIL."
  (when (and (>= (length text) 2) (<= (length text) 3)
             (char<= #\A (char-upcase (char text 0)) #\Z)
             (every #'digit-char-p (subseq text 1)))
    (let ((column (- (char-code (char-upcase (char text 0))) (char-code #\A)))
          (row (parse-integer text :start 1)))
      (when (< row +rows+)
        (values column row)))))

;;; --- the sheet ---

(defstruct (sheet (:constructor make-sheet ()) (:copier nil))
  (texts (make-array (* +columns+ +rows+) :initial-element nil) :type simple-vector :read-only t)
  ;; The parsed formulas and the values, kept until the next edit; a value being
  ;; worked out is :VISITING.
  (asts (make-array (* +columns+ +rows+) :initial-element nil) :type simple-vector :read-only t)
  (values (make-array (* +columns+ +rows+) :initial-element nil) :type simple-vector
          :read-only t)
  ;; Counts edits, so that a UI can tell whether anything changed.
  (version 0 :type fixnum))

(declaim (inline index))
(defun index (column row)
  (+ (* row +columns+) column))

(defun sheet-text (sheet column row)
  "What was typed into the cell, or the empty string."
  (or (svref (sheet-texts sheet) (index column row)) ""))

(defun sheet-set (sheet column row text)
  "Put TEXT in a cell. Everything worked out so far is forgotten."
  (let ((i (index column row)))
    (setf (svref (sheet-texts sheet) i) (if (zerop (length text)) nil text)
          (svref (sheet-asts sheet) i) nil)
    (fill (sheet-values sheet) nil)
    (incf (sheet-version sheet))
    sheet))

;;; --- reading a formula ---

(defun fail (error)
  (throw 'cell-error error))

(defun tokenize (text)
  "The tokens of TEXT: numbers, references, names, and the characters + - * / ( ) : ,."
  (let ((tokens '()) (i 0) (end (length text)))
    (loop :while (< i end)
          :do (let ((c (char text i)))
                (cond
                  ((char= c #\Space) (incf i))
                  ((find c "+-*/(),:") (push c tokens) (incf i))
                  ((or (digit-char-p c) (char= c #\.))
                   (let ((start i))
                     (loop :while (and (< i end) (or (digit-char-p (char text i))
                                                     (char= (char text i) #\.)))
                           :do (incf i))
                     (let ((number (parse-number (subseq text start i))))
                       (unless number (fail :error))
                       (push number tokens))))
                  ((alpha-char-p c)
                   (let ((start i))
                     (loop :while (and (< i end) (alphanumericp (char text i))) :do (incf i))
                     (let ((name (string-upcase (subseq text start i))))
                       (multiple-value-bind (column row) (parse-ref name)
                         (push (if column (list :ref column row) (list :name name)) tokens)))))
                  (t (fail :error)))))
    (nreverse tokens)))

(defvar *tokens*)

(defun peek () (first *tokens*))
(defun next () (pop *tokens*))
(defun expect (token)
  (unless (eql (next) token) (fail :error)))

(defun parse-expression ()
  (let ((left (parse-term)))
    (loop :while (member (peek) '(#\+ #\-))
          :do (let ((op (next)))
                (setf left (list :op op left (parse-term)))))
    left))

(defun parse-term ()
  (let ((left (parse-factor)))
    (loop :while (member (peek) '(#\* #\/))
          :do (let ((op (next)))
                (setf left (list :op op left (parse-factor)))))
    left))

(defun parse-factor ()
  (let ((token (next)))
    (cond
      ((numberp token) token)
      ((eql token #\-) (list :negate (parse-factor)))
      ((eql token #\() (prog1 (parse-expression) (expect #\))))
      ((and (consp token) (eq (first token) :ref))
       (if (eql (peek) #\:)
           (progn (next)
                  (let ((end (next)))
                    (unless (and (consp end) (eq (first end) :ref)) (fail :error))
                    (list :range (second token) (third token) (second end) (third end))))
           token))
      ((and (consp token) (eq (first token) :name))
       (expect #\()
       (let ((arguments '()))
         (unless (eql (peek) #\))
           (loop (push (parse-expression) arguments)
                 (if (eql (peek) #\,) (next) (return))))
         (expect #\))
         (list* :call (second token) (nreverse arguments))))
      (t (fail :error)))))

(defun parse-formula (text)
  "The expression tree of the formula TEXT (which starts with =), or the error."
  (catch 'cell-error
    (let ((*tokens* (tokenize (subseq text 1))))
      (prog1 (parse-expression)
        (when *tokens* (fail :error))))))

;;; --- working out values ---

(defun cell-value (sheet column row)
  "The value of a cell: a double-float, a string, or an error keyword."
  (let* ((i (index column row))
         (cached (svref (sheet-values sheet) i)))
    (cond
      ((eq cached :visiting) :cycle)
      (cached cached)
      (t
       (setf (svref (sheet-values sheet) i) :visiting)
       (let ((value (compute-cell sheet i)))
         (setf (svref (sheet-values sheet) i) value))))))

(defun compute-cell (sheet i)
  (let ((text (svref (sheet-texts sheet) i)))
    (cond
      ((null text) 0d0)
      ((char= (char text 0) #\=)
       (let ((ast (or (svref (sheet-asts sheet) i)
                      (setf (svref (sheet-asts sheet) i) (parse-formula text)))))
         (if (keywordp ast)
             ast
             (catch 'cell-error (evaluate sheet ast)))))
      (t (or (parse-number text) text)))))

(defun numeric (value)
  "VALUE as a number for arithmetic: an error propagates, and text is an error."
  (cond ((numberp value) value)
        ((keywordp value) (fail value))
        (t (fail :error))))

(defun range-values (sheet c1 r1 c2 r2)
  "The numbers in a range of cells; text is left out, and an error in a cell propagates."
  (let ((numbers '()))
    (loop :for row :from (min r1 r2) :to (max r1 r2)
          :do (loop :for column :from (min c1 c2) :to (max c1 c2)
                    :do (let ((value (cell-value sheet column row)))
                          (cond ((numberp value) (push value numbers))
                                ((keywordp value) (fail value))))))
    (nreverse numbers)))

(defun argument-numbers (sheet arguments)
  (loop :for argument :in arguments
        :append (if (and (consp argument) (eq (first argument) :range))
                    (destructuring-bind (c1 r1 c2 r2) (rest argument)
                      (range-values sheet c1 r1 c2 r2))
                    (list (numeric (evaluate sheet argument))))))

(defun evaluate (sheet ast)
  (cond
    ((numberp ast) (float ast 1d0))
    ((eq (first ast) :ref)
     (let ((value (cell-value sheet (second ast) (third ast))))
       (if (stringp value) (fail :error) (numeric value))))
    ((eq (first ast) :negate) (- (numeric (evaluate sheet (second ast)))))
    ((eq (first ast) :op)
     (destructuring-bind (op a b) (rest ast)
       (let ((x (numeric (evaluate sheet a)))
             (y (numeric (evaluate sheet b))))
         (ecase op
           (#\+ (+ x y))
           (#\- (- x y))
           (#\* (* x y))
           (#\/ (if (zerop y) (fail :error) (/ x y)))))))
    ((eq (first ast) :call)
     ;; A function needs at least one argument.
     (when (null (cddr ast)) (fail :error))
     (let ((numbers (argument-numbers sheet (cddr ast)))
           (name (second ast)))
       (cond
         ((string= name "SUM") (float (reduce #'+ numbers) 1d0))
         ((string= name "COUNT") (float (length numbers) 1d0))
         ((null numbers) (fail :error))
         ((string= name "AVG") (/ (reduce #'+ numbers) (length numbers)))
         ((string= name "MIN") (reduce #'min numbers))
         ((string= name "MAX") (reduce #'max numbers))
         (t (fail :error)))))
    (t (fail :error))))

(defun sheet-value (sheet column row)
  "The value of a cell: a double-float, a string (text that is not a number or a
formula), or :ERROR or :CYCLE."
  (let ((value (cell-value sheet column row)))
    (if (integerp value) (float value 1d0) value)))

(defun sheet-display (sheet column row)
  "What a cell shows: a number formatted, text as it is, and #ERR or #CYCLE for the
errors."
  (let ((value (sheet-value sheet column row)))
    (cond ((numberp value)
           (if (null (svref (sheet-texts sheet) (index column row)))
               ""
               (format-number value)))
          ((eq value :error) "#ERR")
          ((eq value :cycle) "#CYCLE")
          (t value))))
