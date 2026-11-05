(defpackage #:almighty-bolt/emacs
  (:use #:cl)
  (:local-nicknames (#:views #:almighty-bolt/ui/views))
  (:export #:next-request
           #:emacs-connected-p
           #:view-typed
           #:point-in-view
           #:take-selection
           #:show-view
           #:show-wiring
           #:show-code
           #:view-text))

(in-package #:almighty-bolt/emacs)

;;; The image's side of the bridge to Emacs (emacs/almighty-bolt.el). The studio runs in the image Sly
;;; is connected to, so Emacs and the studio need copy nothing to each other: each points the other
;;; at things. Emacs asks, over Sly, for what to do (NEXT-REQUEST, which waits for one), and the
;;; image never evaluates anything in Emacs: the requests are data (go to this view's clause, to this
;;; code, put this form in the buffer), and what Emacs does with them is the minor mode's, which its
;;; user installed. Emacs says where point is in a view (POINT-IN-VIEW), which the studio selects, and
;;; gives a view's form as it is typed (VIEW-TYPED), which is defined in the image without being
;;; evaluated: read with *READ-EVAL* off and checked as a file of it would be. Code is still compiled
;;; with C-c C-c: a half-typed function made on a timer would run what is not yet written. Without
;;; Emacs (nothing has asked for a while), nothing is queued for it.

(defvar *lock* (sb-thread:make-mutex :name "Emacs bridge"))
(defvar *ready* (sb-thread:make-waitqueue :name "Emacs bridge requests"))

(defvar *requests* '()
  "What Emacs is asked to do, oldest first: property lists.")

(defvar *last-asked* nil
  "When Emacs last asked for a request (internal real time), or NIL if it never has.")

(defvar *selection* nil
  "What point in Emacs last chose in a view, (name . id), until the studio takes it.")

(defvar *from-emacs* nil
  "True while a view typed in Emacs is defined, so that it is not sent back to the buffer it came
from.")

(defconstant +grace+ 5
  "Seconds after a request's wait ends in which Emacs is still taken to be there: the time it has
to ask again.")

(defun emacs-connected-p ()
  "True if Emacs has asked for a request lately: it is there to be asked to do something."
  (sb-thread:with-mutex (*lock*)
    (and *last-asked*
         (< (- (get-internal-real-time) *last-asked*)
            (* (+ 20 +grace+) internal-time-units-per-second)))))

(defun request (&rest plist)
  "Queue PLIST for Emacs, if it is there. True if it was queued."
  (when (emacs-connected-p)
    (sb-thread:with-mutex (*lock*)
      (setf *requests* (append *requests* (list plist)))
      (sb-thread:condition-broadcast *ready*))
    t))

(defun next-request (&optional (seconds 10))
  "The next thing for Emacs to do, waiting up to SECONDS (at most 20) for one: a property list, or
NIL. Emacs calls it again and again, over Sly, from the minor mode."
  (let ((deadline (+ (get-internal-real-time)
                     (* (min seconds 20) internal-time-units-per-second))))
    (sb-thread:with-mutex (*lock*)
      (setf *last-asked* deadline)
      (loop
        (when *requests*
          (return (pop *requests*)))
        (let ((left (/ (- deadline (get-internal-real-time)) internal-time-units-per-second)))
          (when (<= left 0)
            (return nil))
          (sb-thread:condition-wait *ready* *lock* :timeout left))))))

;;; --- the view's form, as text ---

(defun view-text (name form)
  "FORM, the view NAME, written as the designer writes it in a file: in NAME's package, in lower
case."
  (with-standard-io-syntax
    (let ((*package* (symbol-package name))
          (*print-case* :downcase)
          (*print-pretty* t)
          (*print-right-margin* 96))
      (prin1-to-string form))))

(defun written-name (name)
  (string-downcase (symbol-name name)))

;;; A view defined in the image (in the designer, as it is edited) goes into the buffer of its file,
;;; if Emacs has one: as an edit there, which Emacs's undo takes back.
(defun view-changed (name form)
  (unless *from-emacs*
    (let ((file (views:view-file name)))
      (when file
        (request :op :replace-view :file (namestring file) :name (written-name name)
                 :text (view-text name form))))))

(pushnew 'view-changed views:*view-changed-functions*)

;;; --- what Emacs says ---

(defun view-typed (text package)
  "Define the view that TEXT, a DEFINE-VIEW form typed in Emacs in the package named PACKAGE, writes:
read with *READ-EVAL* off and checked, never evaluated. What Emacs is told: \"\" if all is well, and
otherwise what is wrong (it does not read, it is not a view that can be drawn, the code that wires
it does not fit it)."
  (handler-case
      (let ((form (with-standard-io-syntax
                    (let ((*read-eval* nil)
                          (*package* (or (find-package (string-upcase package))
                                         (find-package :cl-user))))
                      (read-from-string text)))))
        (unless (and (consp form) (symbolp (first form))
                     (string= (symbol-name (first form)) "DEFINE-VIEW")
                     (= (length form) 3) (second form) (symbolp (second form)))
          (error "This is not (define-view name form)."))
        ;; SET-VIEW checks it, as a file of it is checked when it is compiled.
        (destructuring-bind (name view) (rest form)
          (if (equal view (views:find-view name))
              ""
              (let ((problems (let ((*from-emacs* t))
                                (handler-bind ((warning #'muffle-warning))
                                  (views:set-view name view)))))
                (format nil "~{~a~^ ~}" problems)))))
    (error (condition)
      (princ-to-string condition))))

(defun point-in-view (name package id)
  "Point in Emacs is on the widget ID (its name, as written) of the view NAME (as written, in the
package named PACKAGE): the studio selects it. True if there is such a view."
  (let* ((home (or (find-package (string-upcase package)) (find-package :cl-user)))
         (symbol (find-symbol (string-upcase (string-left-trim ":" (let ((colon (search "::" name)))
                                                                     (if colon
                                                                         (subseq name (+ colon 2))
                                                                         name))))
                              home))
         (id (and id (plusp (length id))
                  (intern (string-upcase (string-left-trim ":" id)) :keyword))))
    (when (and symbol (views:find-view symbol))
      (sb-thread:with-mutex (*lock*)
        (setf *selection* (cons symbol id)))
      ;; The studio sees it at its next frame.
      #+darwin (almighty-bolt/platform/macos/host:request-all-frames)
      t)))

(defun take-selection ()
  "What point in Emacs last chose, and forget it: a view's name and the id of a widget in it (or NIL
for the view as a whole), as two values; NIL if nothing was chosen since."
  (let ((selection (sb-thread:with-mutex (*lock*)
                     (shiftf *selection* nil))))
    (and selection (values (car selection) (cdr selection)))))

;;; --- what Emacs is asked to show ---

(defun show-view (name &optional id)
  "Ask Emacs to show the clause of the widget ID in the view NAME, in the file it was defined in.
NIL and why, if it cannot be: Emacs is not there, or the view has no file."
  (let ((file (views:view-file name)))
    (cond ((not (emacs-connected-p)) (values nil "Emacs is not connected (almighty-bolt-bridge-mode)."))
          ((null file) (values nil (format nil "~(~a~) was not defined from a file." name)))
          (t (request :op :show-view :file (namestring file) :name (written-name name)
                      :widget (and id (string-downcase (symbol-name id))))))))

(defun show-wiring (name &optional id)
  "Ask Emacs to show where the code wires the widget ID of the view NAME: its clause in the VIEW
form, in the file the code was compiled from."
  (let ((file (views:wiring-file name)))
    (cond ((not (emacs-connected-p)) (values nil "Emacs is not connected (almighty-bolt-bridge-mode)."))
          ((null file) (values nil (format nil "No code from a file wires ~(~a~)." name)))
          (t (request :op :show-wiring :file (namestring file) :name (written-name name)
                      :widget (and id (string-downcase (symbol-name id))))))))

(defun definition-place (function-name)
  "The file and the character position of the definition of FUNCTION-NAME, or NIL."
  #+sbcl
  (let ((source (ignore-errors
                 (first (sb-introspect:find-definition-sources-by-name function-name :function)))))
    (when (and source (sb-introspect:definition-source-pathname source))
      (values (namestring (translate-logical-pathname
                           (sb-introspect:definition-source-pathname source)))
              (sb-introspect:definition-source-character-offset source))))
  #-sbcl
  (progn function-name nil))

(defun show-code (function-name &optional id)
  "Ask Emacs to show the definition of FUNCTION-NAME (an app's constructor), at the widget ID in it if
it names one there."
  (multiple-value-bind (file offset) (definition-place function-name)
    (cond ((not (emacs-connected-p)) (values nil "Emacs is not connected (almighty-bolt-bridge-mode)."))
          ((null file) (values nil (format nil "Where ~(~s~) was defined is not known." function-name)))
          (t (request :op :show-code :file file :offset (or offset 0)
                      :search (and id (keywordp id)
                                   (format nil ":~(~a~)" (symbol-name id))))))))
