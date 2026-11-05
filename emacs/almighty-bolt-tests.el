;;; almighty-bolt-tests.el --- Its tests -*- lexical-binding: t -*-

;;; Commentary:

;; What the minor mode does with what the image asks, and what it tells the
;; image, in real buffers of real files; the image is a function that notes
;; the forms it is given (`almighty-bolt-eval-async-function').  The image's
;; side is tested in t/emacs.lisp.  From the repository root:
;;
;;   make emacs-test

;;; Code:

(require 'ert)
(require 'almighty-bolt)

(defconst abt-text "(in-package #:my-app)

;; A comment
(defun before () 1)

(almighty-bolt:define-view sign-in
  (:column :page (:main-align :end)
   (:label :heading (:text \"Sign in\"))
   (:row :actions (:gap 8)
    (:button :go (:text \"Go\")))))

(defun make-app ()
  (bolt:view sign-in
    (label :heading :text \"Hi\")
    (button :go :on-press (go))))
"
  "A file of an app with a view and the code that wires it.")

(defmacro abt-with-file (var &rest body)
  "Run BODY with VAR a file holding `abt-text', visited in the current buffer."
  (declare (indent 1))
  `(let ((,var (make-temp-file "abt-" nil ".lisp" abt-text)))
     (unwind-protect
         (with-current-buffer (find-file-noselect ,var)
           (lisp-mode)
           ,@body)
       (let ((buffer (find-buffer-visiting ,var)))
         (when buffer
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-file ,var))))

(defun abt-at (text)
  "Move to the start of the first TEXT in the buffer."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (match-beginning 0)))

(defun abt-call-of (form)
  "What FORM (from `almighty-bolt--call') calls: (NAME . ARGUMENTS)."
  (let ((call (nth 2 (nth 2 form))))
    (cons (nth 1 (nth 1 call)) (nthcdr 2 call))))

(ert-deftest abt-point-names-the-view-and-the-innermost-widget ()
  (abt-with-file file
    (abt-at "\"Go\"")
    (should (equal (almighty-bolt-view-at-point) '("sign-in" "go")))
    (abt-at "(:button :go")
    (should (equal (almighty-bolt-view-at-point) '("sign-in" "go")))
    (abt-at "(:gap 8)\n")
    (should (equal (almighty-bolt-view-at-point) '("sign-in" "actions")))
    ;; A widget's options, (:main-align :end), look like a clause, and are not
    ;; one: point in them is on the widget, and :end is no widget's id.
    (abt-at ":main-align")
    (should (equal (almighty-bolt-view-at-point) '("sign-in" "page")))
    (save-excursion
      (almighty-bolt--find-define-view "sign-in")
      (let ((bounds (almighty-bolt--view-form-bounds)))
        (should-not (almighty-bolt--goto-clause "end"
                                                (car bounds) (cdr bounds)))))
    ;; The view's name, and outside the view.
    (abt-at "sign-in\n")
    (should (equal (almighty-bolt-view-at-point) '("sign-in" nil)))
    (abt-at "(defun make-app")
    (should (null (almighty-bolt-view-at-point)))
    (should (equal (almighty-bolt--package) "my-app"))))

(ert-deftest abt-the-image-shows-a-clause-the-wiring-and-a-definition ()
  (abt-with-file file
    (almighty-bolt-handle-request
     (list :op :show-view :file file :name "sign-in" :widget "go"))
    (should (looking-at "(:button :go"))
    (almighty-bolt-handle-request
     (list :op :show-wiring :file file :name "sign-in" :widget "go"))
    (should (looking-at "(button :go"))
    ;; SBCL's offset is where the form before ended: blanks and comments after.
    (let ((offset (progn (abt-at "(defun before () 1)")
                         (- (match-end 0) 1))))
      (almighty-bolt-handle-request
       (list :op :show-code :file file :offset offset))
      (should (looking-at "(almighty-bolt:define-view")))
    (let ((offset (progn (abt-at "(defun make-app") (- (point) 2))))
      (almighty-bolt-handle-request
       (list :op :show-code :file file :offset offset :search ":go"))
      (should (looking-at ":go :on-press")))))

(ert-deftest abt-the-image-s-view-goes-in-the-buffer-as-an-edit-to-undo ()
  (abt-with-file file
    (almighty-bolt-mode 1)
    (buffer-enable-undo)
    (undo-boundary)
    (almighty-bolt-handle-request
     (list :op :replace-view :file file :name "sign-in"
           :text "(:column :page () (:label :heading (:text \"Hello\")))"))
    (should (search-forward "(:label :heading (:text \"Hello\"))" nil t))
    ;; The rest of the file as it was.
    (should (string-prefix-p
             "(in-package #:my-app)\n\n;; A comment\n(defun before () 1)"
                             (buffer-string)))
    (should (string-suffix-p "(button :go :on-press (go))))\n" (buffer-string)))
    ;; Not to be sent back to the image it came from.
    (should-not almighty-bolt--typed)
    (should (buffer-modified-p))
    (undo-boundary)
    (undo)
    (should (equal (buffer-string) abt-text))))

(ert-deftest abt-a-view-typed-and-where-point-is-go-to-the-image ()
  (abt-with-file file
    (let* ((sent '())
           (almighty-bolt-eval-async-function
            (lambda (form callback)
              (push (abt-call-of form) sent)
              (funcall callback ""))))
      (almighty-bolt-mode 1)
      (abt-at "\"Go\"")
      (forward-char 3)
      (insert "o")
      (almighty-bolt--idle)
      (let ((typed (assoc "VIEW-TYPED" sent))
            (point (assoc "POINT-IN-VIEW" sent)))
        ;; The whole form, in the buffer's package.
        (should (string-prefix-p "(almighty-bolt:define-view sign-in"
                                 (nth 1 typed)))
        (should (string-suffix-p "(:text \"Goo\")))))" (nth 1 typed)))
        (should (equal (nth 2 typed) "my-app"))
        (should (equal (cdr point) '("sign-in" "my-app" "go"))))
      ;; Said once: point has not moved, and nothing was typed since.
      (setq sent '())
      (almighty-bolt--idle)
      (should (null sent))
      ;; A form that is not whole (its last parenthesis not typed) is not sent.
      (abt-at "(:text \"Goo\")))))")
      (goto-char (match-end 0))
      (delete-char -1)
      (abt-at "\"Goo\"")
      (almighty-bolt--idle)
      (should-not (assoc "VIEW-TYPED" sent)))))

(ert-deftest abt-the-bridge-asks-again-and-again-until-the-image-is-gone ()
  (abt-with-file file
    (let* ((answers (list (list :op :show-view :file file :name "sign-in"
                                :widget "heading")
                          nil
                          :not-connected))
           (asked 0)
           (almighty-bolt-eval-async-function
            (lambda (form callback)
              (should (equal (car (abt-call-of form)) "NEXT-REQUEST"))
              (setq asked (1+ asked))
              (funcall callback (pop answers)))))
      (almighty-bolt-bridge-mode 1)
      (unwind-protect
          (progn
            (should (= asked 3))
            (should (looking-at "(:label :heading")))
        (almighty-bolt-bridge-mode -1)))))

;;; almighty-bolt-tests.el ends here
