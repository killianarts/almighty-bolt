(defpackage #:almighty-bolt-studio/code-edit
  (:use #:cl)
  (:export #:view-form-start
           #:set-clause
           #:add-stubs
           #:rename-id
           #:compile-form-at
           #:completions))

(in-package #:almighty-bolt-studio/code-edit)

;;; The studio's light editing of the code that wires a view: a clause written in, a stub for every
;;; id, an id renamed. Each is an edit of the file's text, where the reader says the VIEW form and
;;; its clauses are, so that the rest of the file (its comments, how it is laid out) is as the
;;; person wrote it; then the form at the left edge that has the VIEW form is read and compiled, so
;;; that the running app has it. Nothing is evaluated in reading: *READ-EVAL* is off.

(defun skip-blank (text start)
  "Where the next thing that is read in TEXT from START begins: past spaces, line breaks and
comments of a line."
  (loop :with at = start
        :while (< at (length text))
        :do (let ((c (char text at)))
              (cond ((member c '(#\Space #\Tab #\Newline #\Return)) (incf at))
                    ((char= c #\;) (setf at (or (position #\Newline text :start at) (length text))))
                    (t (return at))))
        :finally (return at)))

(defun read-at (text start package)
  "The form in TEXT that begins at START, read in PACKAGE without evaluating, and where it ends."
  (let ((*read-eval* nil) (*package* package))
    (read-from-string text t nil :start start)))

(defun elements (text open package)
  "The elements of the list in TEXT whose parenthesis is at OPEN: a list of (form start end), and
where the list's closing parenthesis is, as a second value."
  (let ((at (1+ open)) (found '()))
    (loop
      (setf at (skip-blank text at))
      (when (or (>= at (length text)) (char= (char text at) #\)))
        (return (values (nreverse found) at)))
      (multiple-value-bind (form end) (read-at text at package)
        (push (list form at end) found)
        (setf at end)))))

(defun view-form-start (text name)
  "Where in TEXT the parenthesis of the VIEW form of the view NAME (a string, as written) is, or NIL:
(view NAME ..., (bolt:view NAME ..., or (view (NAME :id) ...."
  (let ((target (string-downcase name)))
    (flet ((bare (token)
             (string-downcase (subseq token (1+ (or (position #\: token :from-end t) -1)))))
           (word-at (from)
             (let ((end (or (position-if (lambda (c) (member c '(#\Space #\Tab #\Newline #\( #\))))
                                         text :start from)
                            (length text))))
               (values (subseq text from end) end))))
      (loop :for open = (position #\( text) :then (position #\( text :start (1+ open))
            :while open
            :when (multiple-value-bind (operator end) (word-at (1+ open))
                    (and (string= (bare operator) "view")
                         (let ((next (position-if-not (lambda (c) (member c '(#\Space #\Tab #\Newline #\()))
                                                      text :start end)))
                           (and next (string= (bare (word-at next)) target)))))
              :return open))))

(defun top-form-start (text at)
  "Where the form at the left edge of TEXT that AT is in begins: the last parenthesis at the start of
a line at or before AT."
  (loop :for i :from at :downto 0
        :when (and (char= (char text i) #\() (or (zerop i) (char= (char text (1- i)) #\Newline)))
          :return i
        :finally (return 0)))

(defun line-start (text at)
  (let ((newline (position #\Newline text :end at :from-end t)))
    (if newline (1+ newline) 0)))

(defun clause-indent (text open clauses)
  "What a new clause of the VIEW form at OPEN is indented by: as its last clause is, or two more than
the form."
  (let ((last (car (last clauses))))
    (make-string (if last
                     (- (second last) (line-start text (second last)))
                     (+ 2 (- open (line-start text open))))
                 :initial-element #\Space)))

(defun splice (text start end new)
  (concatenate 'string (subseq text 0 start) new (subseq text end)))

(defun view-clauses-at (text open package)
  "The clauses of the VIEW form at OPEN in TEXT: each (form start end), and where it closes."
  (multiple-value-bind (all close) (elements text open package)
    ;; The operator and the view's name (or its name and id) are not clauses.
    (values (cddr all) close)))

(defun find-clause (clauses id)
  (find-if (lambda (clause)
             (let ((form (first clause)))
               (and (consp form) (consp (cdr form)) (eq (second form) id))))
           clauses))

(defun set-clause (text name package kind id key value)
  "TEXT with the clause of the widget ID (of KIND, a keyword) in the VIEW form of the view NAME giving
KEY the form VALUE (a string, as written): the value it gave replaced, or the key added at the end of
the clause, or a clause added at the end of the form. NIL if the text has no such form."
  (let ((open (view-form-start text name)))
    (when open
      (multiple-value-bind (clauses close) (view-clauses-at text open package)
        (let ((clause (find-clause clauses id)))
          (if clause
              (multiple-value-bind (parts clause-close) (elements text (second clause) package)
                (let ((at (position key parts :key #'first)))
                  (if (and at (nth (1+ at) parts))
                      (let ((old (nth (1+ at) parts)))
                        (splice text (second old) (third old) value))
                      (splice text clause-close clause-close
                              (format nil " ~(~s~) ~a" key value)))))
              (splice text close close
                      (format nil "~%~a(~(~a ~s ~s~) ~a)" (clause-indent text open clauses)
                              kind id key value))))))))

(defun add-stubs (text name package kinds)
  "TEXT with a clause, of no keys, added at the end of the VIEW form of the view NAME for each of
KINDS (a list of (id kind)) that has none; and how many were added, as a second value. NIL if the
text has no such form."
  (let ((open (view-form-start text name)))
    (when open
      (multiple-value-bind (clauses close) (view-clauses-at text open package)
        (let ((missing (remove-if (lambda (entry) (find-clause clauses (first entry))) kinds))
              (indent (clause-indent text open clauses)))
          (values (splice text close close
                          (format nil "~{~a~}"
                                  (loop :for (id kind) :in missing
                                        :collect (format nil "~%~a(~(~a ~s~))" indent kind id))))
                  (length missing)))))))

(defun rename-id (text name package old new)
  "TEXT with the id OLD written NEW in the clause of the VIEW form of the view NAME that has it, and
true if there was one; TEXT and NIL if not."
  (let ((open (view-form-start text name)))
    (if (null open)
        (values text nil)
        (let ((clause (find-clause (view-clauses-at text open package) old)))
          (if (null clause)
              (values text nil)
              (let ((id (second (elements text (second clause) package))))
                (values (splice text (second id) (third id) (format nil "~(~s~)" new)) t)))))))

(defun compile-form-at (text name package file)
  "Read and compile the form at the left edge of TEXT that has the VIEW form of the view NAME, as
loading it from FILE would (so that the wiring is known to come from FILE). NIL if all is well, and
otherwise what went wrong, as a sentence."
  (let ((open (view-form-start text name)))
    (if (null open)
        (format nil "~a has no form that draws the view." (file-namestring file))
        (handler-case
            (let ((form (read-at text (top-form-start text open) package))
                  (*package* package)
                  (*load-truename* (truename file))
                  (*load-pathname* (pathname file)))
              (handler-bind ((warning #'muffle-warning))
                (eval form))
              nil)
          (error (condition)
            (format nil "Not compiled: ~a" condition))))))

(defun completions (prefix package &optional (most 6))
  "The names, in small letters, of the functions and variables of PACKAGE's own (its home is
PACKAGE) that begin with PREFIX, the shortest first, as many as MOST."
  (let ((found '()))
    (when (plusp (length prefix))
      (do-symbols (symbol package)
        (when (and (eq (symbol-package symbol) package)
                   (or (fboundp symbol) (boundp symbol))
                   (eql 0 (search prefix (symbol-name symbol) :test #'char-equal)))
          (pushnew (string-downcase (symbol-name symbol)) found :test #'string=))))
    (let ((sorted (sort found (lambda (a b) (or (< (length a) (length b))
                                               (and (= (length a) (length b)) (string< a b)))))))
      (subseq sorted 0 (min most (length sorted))))))
