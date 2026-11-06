(defpackage #:almighty-bolt-studio/wire
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:code-edit #:almighty-bolt-studio/code-edit)
                    (#:studio-theme #:almighty-bolt-studio/theme)
                    (#:chrome #:almighty-bolt-studio/chrome))
  (:export #:views-list
           #:state-list
           #:view-code
           #:wiring-code
           #:view-clauses
           #:wires-list
           #:wire-detail
           #:edit-code))

(in-package #:almighty-bolt-studio/wire)

;;; Wire's left pane, as the prototype draws it: the views the image has, each with how many of its
;;; widgets the code wires, the one open in the designer chosen; and under them the state of the app
;;; on the canvas, each variable its STATE-LET keeps, live, to edit and to pin. In the middle, the
;;; view as its file is written and the code that wires it, read from its file as it is now (saved
;;; in Emacs, say), each marking the selected widget's line; a press on a widget's line selects it.

(defun views-list (views open)
  "VIEWS (the GUI designer's :views: each a name and how many of its widgets are wired), each a row
to press, OPEN (the name of the view open, or NIL) chosen. Returns the name of the one pressed on
the frame it is, or NIL."
  (let ((pressed nil))
    (bolt:column (:id :views-list :gap 2 :fill t :cross-align :stretch)
      (chrome:section-header :views-heading "Views" (length views))
      (loop :for (name count) :in views
            :do (when (bolt:pressable (:id (list :view-row name) :selected (eq name open)
                                       :radius 0 :left 8 :right 8 :top 4 :bottom 4
                                       :name (format nil "View ~(~a~), ~d wire~:p" name count)
                                       :tooltip (format nil "~(~s~): ~d of its widgets wired. ~
                                                             Press to open it."
                                                        name count))
                      ;; Chosen, on the selection's fill, in the colour words on it take.
                      (let ((dim (if (eq name open) :button-text :dim-text)))
                        (bolt:row (:id :view-row-parts :gap 8 :cross-align :center :fill t)
                          (bolt:label "VIEW" :id :view-kind :font chrome:+small+ :color dim)
                          (bolt:label (string-downcase (symbol-name name)) :id :view-name :flex 1
                                                                           :color (if (eq name open)
                                                                                      :button-text
                                                                                      :text))
                          (bolt:label (princ-to-string count) :id :view-wires :font chrome:+small+
                                                                               :color dim))))
                  (setf pressed name))))
    pressed))


(defun read-text (text package)
  "The value TEXT says, read in PACKAGE with no evaluation, and true; or NIL and NIL if it is not
one whole form."
  (handler-case
      (let ((*package* package) (*read-eval* nil))
        (multiple-value-bind (value end) (read-from-string text)
          (if (every (lambda (c) (member c '(#\Space #\Tab #\Newline))) (subseq text end))
              (values value t)
              (values nil nil))))
    (error () (values nil nil))))

(defun readable-text (value package)
  "VALUE printed so that it reads back, in PACKAGE, with no evaluation, or NIL if it does not print
so: the text is read again to see (SBCL prints a hash table readably as #.(...), which only reads
by evaluating it)."
  (let ((text (handler-case (let ((*package* package) (*print-readably* t))
                              (prin1-to-string value))
                (error () nil))))
    (and text (nth-value 1 (read-text text package)) text)))

(defun state-list (app drafts)
  "The variables APP (the app running on the canvas, or NIL) keeps with STATE-LET, each its name, its
value, live, in a field, and a pin. A value typed there and entered with Return is the variable's:
the app shows it. DRAFTS, a table the caller keeps, holds what is typed until then. A pinned
variable keeps its value when the app is made again even if its first value was edited."
  (let ((variables (bolt:app-pane-variables app)))
    (bolt:column (:id :state-list :gap 6 :fill t :cross-align :stretch)
      (chrome:section-header :state-heading "State" (and app (length variables)))
      (cond ((null app)
             (bolt:paragraph "No app is running on the canvas: name one in its bar, and pick Live, to see its state here."
                             :id :state-note :color :dim-text))
            ((null variables)
             (bolt:paragraph "The app on the canvas keeps no state with STATE-LET."
                             :id :state-note :color :dim-text))
            (t
             (dolist (variable variables)
               (destructuring-bind (&key name occurrence value pinned &allow-other-keys) variable
                 (let* ((package (or (symbol-package name) *package*))
                        (key (list name occurrence))
                        (printed (readable-text value package))
                        (draft (gethash key drafts)))
                   (bolt:row (:id (list :state-row name occurrence) :gap 8 :cross-align :center
                              :fill t)
                     (bolt:label (format nil "~a~@[ ~d~]" (symbol-name name)
                                         (and (> occurrence 1) occurrence))
                                 :id :state-name :font chrome:+small+ :color :dim-text)
                     (if printed
                         (multiple-value-bind (text edited entered)
                             (bolt:text-field (or draft printed) :id :state-value :width 80 :flex 1
                                                                 :labelled-by :state-name
                                                                 :font :code
                                                                 :invalid (and draft
                                                                               (not (nth-value
                                                                                     1 (read-text draft package)))))
                           (when edited
                             (if (string= text printed)
                                 (remhash key drafts)
                                 (setf (gethash key drafts) text)))
                           (when entered
                             (multiple-value-bind (new ok) (read-text text package)
                               (when ok
                                 (remhash key drafts)
                                 (bolt:set-app-pane-variable app name new :occurrence occurrence)))))
                         ;; What does not print to be read back is shown, not edited.
                         (bolt:label (let ((*package* package) (*print-length* 8) (*print-level* 3))
                                       (prin1-to-string value))
                                     :id :state-value :flex 1 :font :code))
                     (when (bolt:pressable (:id :state-pin :selected pinned :radius 4 :all 4
                                            :name (format nil "Pin ~(~a~)" name)
                                            :tooltip "Pinned, it keeps its value when the app is made again, even if its first value was edited.")
                             (bolt:label "PIN" :id :state-pin-words :font chrome:+small+))
                       (bolt:pin-app-pane-variable app name (not pinned) :occurrence occurrence))))))
             (bolt:paragraph "Live, from the running app. Type a value and press Return, and the app shows it; pin one to keep it when its first value is edited."
                             :id :state-note :color :dim-text))))))

;;; --- the view's file and the code's ---

(defun lines-of (text)
  "TEXT's lines, as a vector of strings."
  (coerce (uiop:split-string text :separator '(#\Newline)) 'vector))

(defun tokens (line start)
  "The first two words of LINE from START, each up to a space, a parenthesis or the end."
  (flet ((word-at (from)
           (let* ((from (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line :start from))
                  (end (and from (or (position-if (lambda (c) (member c '(#\Space #\Tab #\( #\))))
                                                  line :start from)
                                     (length line)))))
             (values (and from (subseq line from end)) end))))
    (multiple-value-bind (first end) (word-at start)
      (values first (and first (word-at end))))))

(defun clause-id (line ids)
  "The id, of IDS, of the widget whose clause begins LINE: (kind :id ..., or NIL."
  (let ((open (position-if-not (lambda (c) (member c '(#\Space #\Tab))) line)))
    (when (and open (char= (char line open) #\())
      (multiple-value-bind (kind id) (tokens line (1+ open))
        (declare (ignore kind))
        (and id (> (length id) 1) (char= (char id 0) #\:)
             (find (subseq id 1) ids :key #'symbol-name :test #'string-equal))))))

(defun bare (token)
  (string-downcase (subseq token (1+ (or (position #\: token :from-end t) -1)))))

(defun view-form-at (line name)
  "Where on LINE a VIEW form of the view NAME (a string, as written) begins, or NIL."
  (loop :for open = (position #\( line) :then (position #\( line :start (1+ open))
        :while open
        :when (multiple-value-bind (operator next) (tokens line (1+ open))
                (and operator (string= (bare operator) "view") next
                     (string= (bare (string-left-trim "(" next)) (string-downcase name))))
          :return open))

(defun view-form-line-p (line name)
  "True if a VIEW form of the view NAME (a string, as written) begins on LINE."
  (and (view-form-at line name) t))

(defun clause-lines (lines ids &key from)
  "For each of LINES (from the index FROM, to the next form at the left edge after it, if FROM is
given), the id of IDS whose clause begins on it: a list of (line-number id), from 1."
  (loop :for i :from (or from 0) :below (length lines)
        :for line = (aref lines i)
        :until (and from (> i from) (plusp (length line)) (char= (char line 0) #\())
        :for id = (clause-id line ids)
        :when id :collect (list (1+ i) id)))

(defun code-pane (id heading title place text lines-ids selected memory)
  "TEXT in a code view, under a header that says TITLE and PLACE, the line of SELECTED (one of
LINES-IDS, a list of (line id)) marked. Returns the id on the line the caret was moved to, on the
frame it was, or NIL. MEMORY keeps the caret's line from one frame to the next."
  (chrome:section-header heading title place)
  (let ((line (first (find selected lines-ids :key #'second))))
    (multiple-value-bind (now edited caret)
        (bolt:code-view text :id id :flex 1 :rows 20 :width 300 :labelled-by heading
                             :marks (and line (list (list line :ring
                                                          (format nil "~(~s~), selected" selected)))))
      (declare (ignore now edited))
      (let ((before (gethash id memory)))
        (setf (gethash id memory) caret)
        (and before (/= before caret)
             (second (find caret lines-ids :key #'first)))))))

(defun foot (id parts)
  "A line of PARTS under a pane, each (dot-p words): with a green dot, or an empty one."
  (bolt:row (:id id :gap 8 :cross-align :center)
    (loop :for (on words) :in parts
          :for i :from 0
          :do (bolt:panel (:id (list :dot i) :radius 4 :width 8 :height 8
                           :color (studio-theme:tool-color (if on :ok :line-strong))))
              (bolt:label (chrome:caps words) :id (list :words i) :font chrome:+small+
                                              :color :dim-text))))

(defun view-code (about memory)
  "The view being designed, as the file it is saved as (ABOUT, the GUI designer's, says it), with the
selected widget's line marked. Returns the id of the widget whose line the caret was moved to, on
the frame it was, or NIL. MEMORY is a table the caller keeps."
  (let* ((text (getf about :source))
         (file (getf about :file))
         (picked nil))
    (bolt:column (:id :view-code :gap 8 :flex 1 :fill t :cross-align :stretch)
      (setf picked (code-pane :view-code-text :view-code-heading "View"
                              (if file (file-namestring file) "not saved") text
                              (clause-lines (lines-of text) (getf about :ids))
                              (getf about :selected) memory))
      (foot :view-code-foot (list (list (getf about :live)
                                        (if (getf about :live) "Defined in the image" "Not in the image"))
                                  (list nil "Written by the designer, readable by you"))))
    picked))

(defun file-text (file memory)
  "What FILE has in it now, read again when it was written since it was last read; NIL if it cannot
be read."
  (let ((date (ignore-errors (file-write-date file)))
        (known (gethash :file memory)))
    (if (and known (equal (first known) file) (eql (second known) date))
        (third known)
        (let ((text (ignore-errors (uiop:read-file-string file))))
          (setf (gethash :file memory) (list file date text))
          text))))

(defun top-form-around (lines index)
  "The lines of the form at the left edge that the line INDEX of LINES is in: the index of its first
line and of the line after its last, which is the next such form's, less the blank lines and the
comments before that."
  (let* ((start (or (position-if (lambda (line) (and (plusp (length line)) (char= (char line 0) #\()))
                                 lines :end (1+ index) :from-end t)
                    index))
         (next (or (position-if (lambda (line) (and (plusp (length line)) (char= (char line 0) #\()))
                                lines :start (1+ start))
                   (length lines)))
         (end (1+ (or (position-if (lambda (line)
                                     (let ((trimmed (string-left-trim '(#\Space #\Tab) line)))
                                       (and (plusp (length trimmed)) (char/= (char trimmed 0) #\;))))
                                   lines :start start :end next :from-end t)
                      start))))
    (values start end)))

(defun wiring-code (about memory)
  "The code that wires the view being designed: the form at the left edge of its file, as the file is
now, that has the VIEW form (ABOUT, the GUI designer's, says which file), with the clause of the
selected widget marked; or why there is none. Returns the id of the widget whose clause the caret
was moved to, on the frame it was, or NIL."
  (let* ((file (getf about :wiring-file))
         (text (and file (file-text file memory)))
         (lines (and text (lines-of text)))
         (at (and lines (position-if (lambda (line) (view-form-line-p line (getf about :name)))
                                     lines)))
         (picked nil))
    (bolt:column (:id :wiring-code :gap 8 :flex 1 :fill t :cross-align :stretch)
      (cond ((null at)
             (chrome:section-header :code-heading "Code")
             (bolt:paragraph (cond ((not (getf about :live))
                                    "The view is not live: open a view of the image (in Views) to see the code that wires it.")
                                   ((null file) "No code from a file wires this view.")
                                   ((null text) (format nil "~a cannot be read." (file-namestring file)))
                                   (t (format nil "~a no longer has the form that wires the view."
                                              (file-namestring file))))
                             :id :code-note :color :dim-text))
            (t
             (multiple-value-bind (start end) (top-form-around lines at)
               (let ((form (subseq lines start end)))
                 (setf picked (code-pane :code-text :code-heading "Code"
                                         (format nil "~a:~d" (file-namestring file) (1+ start))
                                         ;; Ending in a line break, so that the caret, at
                                         ;; the end until the person moves it, is on no clause.
                                         (format nil "~{~a~%~}" (coerce form 'list))
                                         (clause-lines form (getf about :ids) :from (- at start))
                                         (getf about :selected) memory))))
             (foot :code-foot
                   (list (list t "Compiled")
                         (list (null (getf about :problems))
                               (if (getf about :problems)
                                   (format nil "~d problem~:p with the view"
                                           (length (getf about :problems)))
                                   "Checked against the view"))
                         (list t (format nil "~d wire~:p" (getf about :wires))))))))
    picked))

;;; --- the wires ---

(defun file-package (text default)
  "The package TEXT's first IN-PACKAGE names, if this Lisp has it, or else DEFAULT."
  (let ((at (search "(in-package" text :test #'char-equal)))
    (or (and at
             (ignore-errors
              (let ((*read-eval* nil))
                (find-package (second (read-from-string text t nil :start at))))))
        default)))

(defun view-clauses (about memory)
  "The clauses of the VIEW form that wires the view being designed, as its code's file has them now
(ABOUT, the GUI designer's, says which file): a list of (id kind options), OPTIONS a property list
of the forms as written; with the package they were read in as a second value. NIL if there is no
such form, or it does not read. Read with *READ-EVAL* off, again only when the file changes."
  (let* ((file (getf about :wiring-file))
         (text (and file (file-text file memory)))
         (name (getf about :name))
         (known (gethash :clauses memory)))
    (if (and known (eq (first known) text) (equal (second known) name))
        (values (third known) (fourth known))
        (let* ((package (and text (file-package text (or (find-package (string-upcase
                                                                         (getf about :package)))
                                                         (find-package :cl-user)))))
               (clauses
                 (and text
                      (let* ((lines (lines-of text))
                             (at (position-if (lambda (line) (view-form-line-p line name)) lines)))
                        (when at
                          (let* ((start (+ (loop :for i :below at :sum (1+ (length (aref lines i))))
                                           (view-form-at (aref lines at) name)))
                                 (form (ignore-errors
                                        (let ((*read-eval* nil) (*package* package))
                                          (read-from-string text t nil :start start)))))
                            (loop :for clause :in (cddr form)
                                  :when (and (consp clause) (consp (cdr clause)) (symbolp (second clause)))
                                    :collect (list (second clause) (first clause) (cddr clause)))))))))
          (setf (gethash :clauses memory) (list text name clauses package))
          (values clauses package)))))

(defun written (form package)
  (let ((*package* (or package *package*)) (*print-case* :downcase) (*print-pretty* nil))
    (prin1-to-string form)))

(defun wire-keys (clause)
  (loop :for (key) :on (third clause) :by #'cddr :collect key))

(defun wires-list (about clauses)
  "Each id of the view being designed (ABOUT, the GUI designer's), and what of it CLAUSES (VIEW-
CLAUSES's) wire, or that nothing does; the selected one chosen. Returns the id pressed, on the frame
it is, or NIL."
  (let ((pressed nil)
        (ids (getf about :ids)))
    (bolt:column (:id :wires-list :gap 2 :fill t :cross-align :stretch)
      (chrome:section-header :wires-heading "Wires" (count-if (lambda (id) (wire-keys (assoc id clauses)))
                                                              ids))
      (dolist (id ids)
        (let ((keys (wire-keys (assoc id clauses))))
          (when (bolt:pressable (:id (list :wire-row id) :selected (eq id (getf about :selected))
                                 :radius 0 :left 6 :right 6 :top 4 :bottom 4
                                 :name (format nil "~(~s~), ~:[not wired~;wired to ~:*~(~{~s~^ ~}~)~]"
                                               id keys))
                  (bolt:row (:id :wire-row-parts :gap 8 :cross-align :center)
                    (bolt:panel (:id :wire-dot :radius 4 :width 8 :height 8
                                 :color (studio-theme:tool-color (if keys :ok :line-strong))))
                    ;; Chosen, on the selection's fill, in the colour words on it take.
                    (let ((chosen (eq id (getf about :selected))))
                      (bolt:column (:id :wire-ends :gap 0)
                        (bolt:label (format nil "~(~s~)" id) :id :wire-id :font chrome:+small+
                                                             :color (cond (chosen :button-text)
                                                                          (keys :text)
                                                                          (t :dim-text)))
                        (bolt:label (if keys
                                        (format nil "TO ~:@(~{~s~^ ~}~)" keys)
                                        "TO NOT WIRED")
                                    :id :wire-to :font chrome:+small+
                                    :color (if chosen :button-text :dim-text))))))
            (setf pressed id)))))
    pressed))

(defun clock-time (universal-time)
  (multiple-value-bind (second minute hour) (decode-universal-time universal-time)
    (format nil "~2,'0d:~2,'0d:~2,'0d" hour minute second)))

(defun id-text-p (text)
  "True if TEXT is an id as written: letters, digits and dashes, after an optional colon."
  (let ((name (string-left-trim ":" text)))
    (and (plusp (length name))
         (every (lambda (c) (or (alphanumericp c) (char= c #\-))) name))))

(defun inline-clause (text package)
  "The key and the form, as written, that TEXT (\":on-press (sign-in name)\") gives, or NIL if it is
not a keyword and one whole form after it."
  (handler-case
      (let ((*read-eval* nil) (*package* package))
        (multiple-value-bind (key end) (read-from-string text)
          (let ((rest (string-trim '(#\Space #\Tab) (subseq text end))))
            (when (and (keywordp key) (plusp (length rest)) (nth-value 1 (read-text rest package)))
              (values key rest)))))
    (error () nil)))

(defun last-word (text)
  "Where the word being typed at the end of TEXT begins: after the last space or parenthesis."
  (let ((at (position-if (lambda (c) (member c '(#\Space #\( #\) #\'))) text :from-end t)))
    (if at (1+ at) 0)))

(defun wire-detail (about clauses package app note memory)
  "The selected widget of the view being designed (ABOUT, the GUI designer's): its kind and id, each
option the code wires (CLAUSES, VIEW-CLAUSES's, read in PACKAGE) as written and each the view gives,
and when it was last pressed in APP (the app on the canvas, or NIL); Go to the Code, Write the Stub
for Every Id, and Rename; and a clause to write into the code, with the names of the code's package
that complete the word typed. NOTE (a string, or NIL) is said under them. MEMORY keeps what is typed.
Returns what to do, on the frame it is asked for: :go, :stubs, (:rename new-id) or (:inline key
value), VALUE as written; or NIL."
  (let* ((id (getf about :selected))
         (clause (and id (assoc id clauses)))
         (wired (third clause))
         (given (getf about :selected-options))
         (code (and (getf about :live) (getf about :wiring-file) package t))
         (action nil))
    (bolt:column (:id :wire-detail :gap 8 :fill t :cross-align :stretch)
      (chrome:section-header :wire-heading "Selected wire")
      (cond ((null id)
             (bolt:paragraph "Select a widget, here or on the canvas, to see what wires it."
                             :id :wire-note :color :dim-text))
            (t
             (bolt:row (:id :wire-head :gap 10 :cross-align :center)
               (bolt:label (chrome:caps (or (getf about :selected-kind) "")) :id :wire-kind
                                                                              :font '(:heading :size 20 :weight :bold))
               (bolt:label (format nil "~(~s~)" id) :id :wire-head-id :font chrome:+small+ :color :dim-text))
             (flet ((row (key value source)
                      (bolt:row (:id (list :wire-option key) :gap 8 :cross-align :center :fill t)
                        (bolt:label (format nil "~(~s~)" key) :id :wire-key :font chrome:+small+
                                                              :color :dim-text)
                        (bolt:label value :id :wire-value :font :code :flex 1)
                        (bolt:label source :id :wire-source :font chrome:+small+ :color :dim-text))))
               (loop :for (key form) :on wired :by #'cddr
                     :do (row key (written form package) "WIRED"))
               (loop :for (key value) :on given :by #'cddr
                     :unless (getf wired key)
                       :do (row key (written value package) "VIEW"))
               (multiple-value-bind (frame time) (bolt:app-pane-last-press app (getf about :live) id)
                 (row :last-press (if frame
                                      (format nil "frame ~d ~a ~a" frame chrome:+separator+
                                              (clock-time time))
                                      "not pressed")
                      "TRACE")))
             (when (bolt:button "GO TO THE CODE" :id :wire-go-to-code :enabled (and clause code))
               (setf action :go))
             (when (bolt:button "WRITE THE STUB FOR EVERY ID" :id :wire-stubs :enabled code)
               (setf action :stubs))
             ;; Renamed in the view and in the code that wires it.
             (let ((to (gethash :rename-to memory "")))
               (bolt:row (:id :wire-rename-row :gap 8 :cross-align :center :fill t)
                 (multiple-value-bind (text edited entered)
                     (bolt:text-field to :id :wire-rename-to :flex 1 :width 80 :font :code
                                         :invalid (and (plusp (length to)) (not (id-text-p to))))
                   (when edited (setf (gethash :rename-to memory) text))
                   (when (or (bolt:button (format nil "RENAME ~:@(~s~) EVERYWHERE" id)
                                          :id :wire-rename :enabled (and code (id-text-p to)))
                             (and entered code (id-text-p text)))
                     (setf action (list :rename (intern (string-upcase (string-left-trim ":" text))
                                                        :keyword))
                           (gethash :rename-to memory) "")))))
             (when note
               (bolt:paragraph note :id :wire-go-note :color :dim-text))
             ;; A clause written here goes into the code, which is compiled.
             (chrome:section-header :inline-heading "Write code here" "LISP")
             (bolt:paragraph (if code
                                 (format nil "A wire written here goes into ~a as a clause, and is compiled: the app has it at once."
                                         (file-namestring (getf about :wiring-file)))
                                 "Open a view that code in a file wires (in Views) to write its wires here.")
                             :id :inline-note :color :dim-text)
             (let* ((draft (gethash :inline memory ""))
                    (start (last-word draft))
                    (names (and code (code-edit:completions (subseq draft start) package))))
               (multiple-value-bind (text edited entered)
                   (bolt:text-field draft :id :wire-inline :width 200 :font :code :enabled code
                                          :invalid (and (plusp (length draft))
                                                        (not (inline-clause draft package))))
                 (when edited (setf (gethash :inline memory) text))
                 (when entered
                   (multiple-value-bind (key value) (inline-clause text package)
                     (when key
                       (setf action (list :inline key value)
                             (gethash :inline memory) "")))))
               ;; The names that complete the word typed; one pressed is written.
               (when names
                 (bolt:row (:id :wire-completions :gap 6)
                   (dolist (name names)
                     (when (bolt:pressable (:id (list :completion name) :radius 2 :left 4 :right 4
                                            :top 1 :bottom 1 :border-color :field-border
                                            :name (format nil "Complete ~a" name))
                             (bolt:label (chrome:caps name) :id :completion-words :font chrome:+small+))
                       (setf (gethash :inline memory)
                             (concatenate 'string (subseq draft 0 start) name))))))))))
    action))

(defun write-file-text (file text memory)
  "Write TEXT to FILE, and take it as what FILE has (its date may not show it changed, within a
second)."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (write-string text out))
  (setf (gethash :file memory) (list file (ignore-errors (file-write-date file)) text)))

(defun edit-code (action about memory rename)
  "Do ACTION (WIRE-DETAIL's) to the code that wires the view being designed (ABOUT, the GUI
designer's), and compile what changed. RENAME is the designer's, to rename the selected widget in
the view. Returns what was done, or what went wrong, as a sentence."
  (multiple-value-bind (clauses package) (view-clauses about memory)
    (declare (ignore clauses))
    (let* ((file (getf about :wiring-file))
           (text (and file (file-text file memory)))
           (name (getf about :name))
           (id (getf about :selected))
           (where (and file (file-namestring file))))
      (flet ((write-and-compile (new done)
               (cond ((null new) (format nil "~a has no form that draws the view." where))
                     (t (write-file-text file new memory)
                        (or (code-edit:compile-form-at new name package file)
                            done)))))
        (cond ((or (null text) (null package))
               "There is no code in a file that wires the view.")
              ((eq action :stubs)
               (multiple-value-bind (new count) (code-edit:add-stubs text name package (getf about :kinds))
                 (write-and-compile new (format nil "Wrote ~d stub~:p into ~a, and compiled it."
                                                count where))))
              ((eq (first action) :inline)
               (destructuring-bind (key value) (rest action)
                 (write-and-compile (code-edit:set-clause text name package
                                                          (second (assoc id (getf about :kinds)))
                                                          id key value)
                                    (format nil "Wrote ~(~s~) ~a into ~a, and compiled it."
                                            key value where))))
              ((eq (first action) :rename)
               (let* ((new (second action))
                      (wrong (funcall rename id new)))
                 (or wrong
                     (multiple-value-bind (renamed found)
                         (code-edit:rename-id text name package id new)
                       (if found
                           (write-and-compile renamed (format nil "Renamed ~(~s~) to ~(~s~) in the view and in ~a, and compiled it."
                                                              id new where))
                           (format nil "Renamed ~(~s~) to ~(~s~) in the view; ~a does not wire it."
                                   id new where)))))))))))
