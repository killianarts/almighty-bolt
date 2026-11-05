(defpackage #:almighty-bolt/t/emacs
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:emacs #:almighty-bolt/emacs)
                    (#:views #:almighty-bolt/ui/views)))

(in-package #:almighty-bolt/t/emacs)

;;; The image's side of the bridge to Emacs, used as the minor mode uses it over Sly: Emacs asks for
;;; requests (NEXT-REQUEST), says where point is (POINT-IN-VIEW) and gives a view as it is typed
;;; (VIEW-TYPED). What Emacs does with a request is tested in emacs/almighty-bolt-tests.el.

(defun emacs-there ()
  "Be Emacs, asking for a request: then it is there to be asked. Whatever is queued is dropped."
  (loop :while (emacs:next-request 0)))

(defun emacs-gone ()
  (setf emacs::*last-asked* nil
        emacs::*requests* '()))

(defun view-file-with (text)
  "A view defined from a file in the temporary directory, as an app's system loads one: its name
and the file."
  (let* ((name (intern (format nil "BRIDGED-~d" (random 1000000000)) :cl-user))
         (path (merge-pathnames (format nil "~(~a~).lisp" name) (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (format out "(in-package #:common-lisp-user)~%~%(almighty-bolt:define-view ~(~a~)~%  ~
                   (:column :page (:gap 4) (:label :words (:text ~s))))~%"
              name text))
    (load path)
    (values name path)))

(defmacro with-view ((name path text) &body body)
  `(multiple-value-bind (,name ,path) (view-file-with ,text)
     (unwind-protect (progn ,@body)
       (remhash ,name views::*views*)
       (emacs-gone)
       (when (probe-file ,path) (delete-file ,path)))))

(define-test a-view-defined-in-the-image-goes-to-emacs-to-be-put-in-its-buffer ()
  (with-view (name path "Before")
    (emacs-there)
    (views:set-view name '(:column :page (:gap 4) (:label :words (:text "After"))))
    (let ((request (emacs:next-request 1)))
      (assert-eq :replace-view (getf request :op))
      (assert-equal (namestring (truename path)) (namestring (truename (getf request :file))))
      (assert-equal (string-downcase (symbol-name name)) (getf request :name))
      ;; Written as the view is: it reads back as the form.
      (assert-equal '(:column :page (:gap 4) (:label :words (:text "After")))
                    (let ((*package* (find-package :cl-user)))
                      (read-from-string (getf request :text)))))))

(define-test nothing-is-queued-for-emacs-when-it-is-not-there ()
  (with-view (name path "Before")
    (emacs-gone)
    (views:set-view name '(:column :page (:gap 4) (:label :words (:text "After"))))
    (assert-false emacs::*requests*)
    (multiple-value-bind (done why) (emacs:show-view name :words)
      (assert-false done)
      (assert-true (search "not connected" why) why))))

(define-test a-view-typed-in-emacs-is-defined-and-not-sent-back ()
  (with-view (name path "Before")
    (emacs-there)
    (let ((typed (format nil "(almighty-bolt:define-view ~(~a~)~%  (:column :page (:gap 4) ~
                              (:label :words (:text \"Typed\"))))" name)))
      (assert-equal "" (emacs:view-typed typed "common-lisp-user"))
      (assert-equal '(:column :page (:gap 4) (:label :words (:text "Typed")))
                    (views:find-view name))
      ;; Not back to the buffer it came from.
      (assert-false (emacs:next-request 0)))))

(defvar *evaluated* nil "Set by a form that must not be evaluated.")

(define-test a-view-typed-that-cannot-be-drawn-or-read-is-not-defined-and-nothing-is-evaluated ()
  (with-view (name path "Before")
    (let ((before (views:find-view name)))
      ;; A kind there is no widget of.
      (assert-true (plusp (length (emacs:view-typed
                                   (format nil "(define-view ~(~a~) (:gizmo :page ()))" name)
                                   "cl-user"))))
      ;; Not whole yet.
      (assert-true (plusp (length (emacs:view-typed
                                   (format nil "(define-view ~(~a~) (:column :page" name)
                                   "cl-user"))))
      ;; Read with *read-eval* off: #. is not evaluated.
      (setf *evaluated* nil)
      (assert-true (plusp (length (emacs:view-typed
                                   (format nil "(define-view ~(~a~) #.(setf ~s t))" name
                                           '*evaluated*)
                                   "cl-user"))))
      (assert-false *evaluated*)
      (assert-equal before (views:find-view name)))))

(define-test point-on-a-widget-in-emacs-is-taken-by-the-studio-once ()
  (with-view (name path "Before")
    (assert-true (emacs:point-in-view (string-downcase (symbol-name name)) "common-lisp-user"
                                      "words"))
    (assert-equal (list name :words) (multiple-value-list (emacs:take-selection)))
    (assert-false (emacs:take-selection))
    ;; The view as a whole; and a name with its package, as it may be written.
    (emacs:point-in-view (format nil "cl-user::~(~a~)" name) "cl-user" "")
    (assert-equal (list name nil) (multiple-value-list (emacs:take-selection)))
    ;; A view the image does not have is nothing to select.
    (assert-false (emacs:point-in-view "no-such-view" "cl-user" "words"))))

(defun shown-here (x)
  "A function of this file, for SHOW-CODE to find."
  (list :here x))

(define-test emacs-is-asked-to-show-a-view-s-clause-and-a-function-s-definition ()
  (with-view (name path "Before")
    (emacs-there)
    (assert-true (emacs:show-view name :words))
    (let ((request (emacs:next-request 0)))
      (assert-eq :show-view (getf request :op))
      (assert-equal (namestring (truename path)) (namestring (truename (getf request :file))))
      (assert-equal "words" (getf request :widget)))
    (assert-true (emacs:show-code 'shown-here :here))
    (let* ((request (emacs:next-request 0))
           (file (getf request :file)))
      (assert-eq :show-code (getf request :op))
      (assert-true (search "emacs.lisp" file) file)
      ;; The definition is the first form at the offset (SBCL's offset is where the form before
      ;; it ended).
      (assert-equal '(defun shown-here)
                    (subseq (let ((*package* (find-package :almighty-bolt/t/emacs)))
                              (read-from-string (uiop:read-file-string file) t nil
                                                :start (getf request :offset)))
                            0 2))
      (assert-equal ":here" (getf request :search)))))
