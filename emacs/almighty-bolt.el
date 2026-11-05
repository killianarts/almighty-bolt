;;; almighty-bolt.el --- The studio and Emacs -*- lexical-binding: t -*-

;; Author: Micah Killian
;; License: MIT
;; Package-Requires: ((emacs "27.1") (sly "1.0"))

;;; Commentary:

;; The studio of almighty-bolt runs in the Lisp that Sly is connected to.
;; This keeps a buffer and the studio in step through that Lisp:
;;
;; - Edit Source in the studio's inspector, on a widget picked in the running
;;   app, brings Emacs to its clause in the view's file, or to the code that
;;   drew it; Edit Wiring, to where the code wires it.
;; - Point on a widget's clause in a `define-view' form selects that widget
;;   in the studio.
;; - An edit in the studio's designer rewrites the view's form in its buffer,
;;   as an edit that undo takes back (the buffer is then modified; save it as
;;   usual).
;; - A `define-view' form typed in a buffer is defined in the image after a
;;   pause, so the running app and the designer follow without C-c C-c.  It
;;   is read with *read-eval* off and checked, never evaluated; other code is
;;   compiled as before, with C-c C-c.
;;
;; Emacs asks the Lisp what to do (a request: data, not code), so nothing is
;; evaluated in Emacs from Lisp, and `slynk:*enable-evaluate-in-emacs*' can
;; stay off.
;;
;; In the Lisp, load the system "almighty-bolt/emacs" (the studio does).  In
;; Emacs:
;;
;;   (load "/path/to/almighty-bolt/emacs/almighty-bolt.el")
;;   (add-hook 'lisp-mode-hook #'almighty-bolt-mode)
;;
;; and, once Sly is connected, M-x almighty-bolt-bridge-mode.

;;; Code:

(require 'cl-lib)

(declare-function sly-eval-async "sly")
(declare-function sly-connected-p "sly")

(defgroup almighty-bolt nil
  "The almighty-bolt studio and Emacs, in step through the image."
  :group 'lisp)

(defcustom almighty-bolt-idle-delay 0.6
  "Seconds idle before what was typed, and where point is, go to the image."
  :type 'number)

(defvar almighty-bolt-bridge-mode)
(defvar almighty-bolt-mode)

(defvar almighty-bolt-eval-async-function #'almighty-bolt--sly-eval-async
  "A function of a Lisp form and a function to call with its value.
The form is a list, as Sly sends it to the image.")

(defun almighty-bolt--sly-eval-async (form callback)
  "Evaluate FORM in the image, over Sly, and call CALLBACK with its value."
  (if (and (fboundp 'sly-connected-p) (sly-connected-p))
      (sly-eval-async form callback)
    (funcall callback :not-connected)))

(defun almighty-bolt--eval (form &optional callback)
  "Evaluate FORM in the image, and call CALLBACK, if given, with its value."
  (funcall almighty-bolt-eval-async-function form (or callback #'ignore)))

(defun almighty-bolt--call (name &rest arguments)
  "A form that calls the bridge's function NAME (a string) with ARGUMENTS.
It is :not-loaded if the image does not have the system almighty-bolt/emacs."
  `(cl:let ((package (cl:find-package "ALMIGHTY-BOLT/EMACS")))
     (cl:if package
            (cl:funcall (cl:find-symbol ,name package) ,@arguments)
            :not-loaded)))

;;; --- finding a view and a widget in a buffer ---

(defconst almighty-bolt--define-view-regexp
  "(\\(?:[^ \t\n()]+:\\)?define-view[ \t\n]+"
  "The start of a `define-view' form, with any package prefix, to its name.")

(defconst almighty-bolt--clause-regexp
  (concat "(:[^ \t\n()]+[ \t\n]+\\(:[^ \t\n()]+\\)[ \t\n]*"
          "\\(?:(\\|nil\\_>\\|)\\)")
  "A widget's clause in a view's form: (kind id (options ...) ...).
Its id is the first group.")

(defun almighty-bolt--name-regexp (name)
  "The name NAME, with or without a package prefix, as a whole symbol."
  (concat "\\(?:[^ \t\n()]+::?\\)?" (regexp-quote name) "\\_>"))

(defun almighty-bolt--find-define-view (name)
  "Move to the start of the `define-view' form of the view NAME.
Return point, or NIL if this buffer has none."
  (let ((case-fold-search t)
        (found nil))
    (save-excursion
      (goto-char (point-min))
      (while (and (not found)
                  (re-search-forward
                   (concat almighty-bolt--define-view-regexp
                           (almighty-bolt--name-regexp name))
                   nil t))
        (unless (nth 8 (syntax-ppss (match-beginning 0)))
          (setq found (match-beginning 0)))))
    (when found (goto-char found))))

(defun almighty-bolt--view-form-bounds ()
  "The start and end of the view's form, the third element of a `define-view'.
Point is at the start of the `define-view' form."
  (save-excursion
    (down-list 1)
    (forward-sexp 2)
    (skip-chars-forward " \t\n")
    (let ((start (point)))
      (forward-sexp 1)
      (cons start (point)))))

(defun almighty-bolt--options-p (position)
  "True if the list at POSITION is the options of a clause, not a clause.
A clause's options, (:main-align :end), look like a clause of their own."
  (save-excursion
    (goto-char position)
    (let ((parent (ignore-errors (backward-up-list 1 t t) (point))))
      (and parent
           (progn (goto-char parent) (looking-at almighty-bolt--clause-regexp))
           (ignore-errors
             (down-list 1)
             (forward-sexp 2)
             (skip-chars-forward " \t\n")
             (= (point) position))))))

(defun almighty-bolt--clause-id (position)
  "The id of the widget whose clause starts at POSITION, or NIL."
  (save-excursion
    (goto-char position)
    (and (looking-at almighty-bolt--clause-regexp)
         (let ((id (substring (match-string-no-properties 1) 1)))
           (and (not (almighty-bolt--options-p position)) id)))))

(defun almighty-bolt--goto-clause (id start end)
  "Move to the clause of the widget ID between START and END.
True if there is one."
  (let ((case-fold-search t)
        (found nil))
    (save-excursion
      (goto-char start)
      (while (and (not found)
                  (re-search-forward almighty-bolt--clause-regexp end t))
        ;; A clause's options are never matched: the match of the clause
        ;; takes their opening parenthesis.
        (when (string-equal (substring (match-string 1) 1) id)
          (setq found (match-beginning 0)))))
    (when found (goto-char found) t)))

(defun almighty-bolt--package ()
  "The package at point: of the last `in-package' before it, or cl-user."
  (save-excursion
    (if (re-search-backward
         (concat "^(\\(?:[^ \t\n()]+:\\)?in-package[ \t\n]+"
                 "\\(?:#?:\\|'\\)?\\([^ \t\n()]+\\)")
         nil t)
        (match-string-no-properties 1)
      "cl-user")))

(defun almighty-bolt--top-start (&optional position)
  "The start of the top-level form POSITION (or point) is in or at, or NIL."
  (let* ((position (or position (point)))
         (ppss (syntax-ppss position)))
    (cond ((nth 9 ppss) (car (nth 9 ppss)))
          ((eq (char-after position) ?\() position))))

(defun almighty-bolt-view-at-point ()
  "The view point is in, and the widget, as a list (NAME ID), or NIL.
NAME is the view's name, as written, and ID the id of the widget whose
clause point is innermost in, or NIL outside every clause."
  (save-excursion
    (let* ((here (point))
           (top (almighty-bolt--top-start here))
           (case-fold-search t))
      (when top
        (goto-char top)
        (when (looking-at (concat almighty-bolt--define-view-regexp
                                  "\\(?:[^ \t\n()]+::?\\)?\\([^ \t\n()]+\\)"))
          (let ((name (match-string-no-properties 1))
                (bounds (ignore-errors (almighty-bolt--view-form-bounds)))
                (id nil))
            (when (and bounds (<= (car bounds) here) (< here (cdr bounds)))
              ;; The lists point is in, and the one it is at, innermost first.
              (let ((opens (reverse (nth 9 (syntax-ppss here)))))
                (when (eq (char-after here) ?\() (push here opens))
                (cl-loop for open in opens
                         while (>= open (car bounds))
                         do (setq id (almighty-bolt--clause-id open))
                         until id)))
            (list name id)))))))

;;; --- what the image asks for ---

(defvar almighty-bolt--inhibit nil
  "True while this changes a buffer for the image: not to be sent back.")

(defun almighty-bolt--show (file)
  "Show FILE's buffer, and return it."
  (let ((buffer (find-file-noselect file)))
    (pop-to-buffer buffer)
    buffer))

(defun almighty-bolt--do-show-view (request)
  "Show the clause of REQUEST's widget in its view's file."
  (almighty-bolt--show (plist-get request :file))
  (if (not (almighty-bolt--find-define-view (plist-get request :name)))
      (message "No (define-view %s ...) in %s."
               (plist-get request :name) (plist-get request :file))
    (let ((bounds (almighty-bolt--view-form-bounds))
          (id (plist-get request :widget)))
      (when id
        (almighty-bolt--goto-clause id (car bounds) (cdr bounds))))))

(defun almighty-bolt--do-show-wiring (request)
  "Show where the code wires REQUEST's widget: its clause in a `view' form."
  (almighty-bolt--show (plist-get request :file))
  (let ((case-fold-search t)
        (name (plist-get request :name))
        (id (plist-get request :widget))
        (found nil))
    (goto-char (point-min))
    (while (and (not found)
                (re-search-forward
                 (concat "(\\(?:[^ \t\n()]+:\\)?view[ \t\n]+(?"
                         (almighty-bolt--name-regexp name))
                 nil t))
      (unless (nth 8 (syntax-ppss (match-beginning 0)))
        (setq found (match-beginning 0))))
    (if (not found)
        (message "No (view %s ...) in %s." name (plist-get request :file))
      (goto-char found)
      (let ((end (save-excursion (forward-sexp 1) (point))))
        (when (and id
                   (re-search-forward
                    (concat "([^ \t\n()]+[ \t\n]+:" (regexp-quote id) "\\_>")
                    end t))
          (goto-char (match-beginning 0)))))))

(defun almighty-bolt--do-show-code (request)
  "Show the definition REQUEST names, at what it says to look for in it."
  (almighty-bolt--show (plist-get request :file))
  (goto-char (1+ (or (plist-get request :offset) 0)))
  ;; SBCL's offset is where the form before it ended: past blanks and comments.
  (forward-comment (buffer-size))
  (let ((search (plist-get request :search)))
    (when search
      (let ((end (save-excursion (ignore-errors (forward-sexp 1)) (point)))
            (case-fold-search t))
        (when (re-search-forward (concat (regexp-quote search) "\\_>") end t)
          (goto-char (match-beginning 0)))))))

(defun almighty-bolt--do-replace-view (request)
  "Put REQUEST's view, as the image has it, in the buffer of its file.
Only if Emacs has a buffer of the file; as an edit there."
  (let ((buffer (find-buffer-visiting (plist-get request :file))))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (when (almighty-bolt--find-define-view (plist-get request :name))
            (let* ((bounds (almighty-bolt--view-form-bounds))
                   (old (buffer-substring-no-properties
                         (car bounds) (cdr bounds)))
                   (new (plist-get request :text)))
              (unless (string= old new)
                (let ((almighty-bolt--inhibit t))
                  (goto-char (car bounds))
                  (delete-region (car bounds) (cdr bounds))
                  (insert new)
                  (indent-region (car bounds) (point)))))))))))

(defun almighty-bolt-handle-request (request)
  "Do REQUEST, a property list from the image."
  (pcase (plist-get request :op)
    (:show-view (almighty-bolt--do-show-view request))
    (:show-wiring (almighty-bolt--do-show-wiring request))
    (:show-code (almighty-bolt--do-show-code request))
    (:replace-view (almighty-bolt--do-replace-view request))))

;;; --- asking the image, again and again ---

(defvar almighty-bolt--last-poll 0
  "When a request was last asked for (`float-time').")

(defvar almighty-bolt--watchdog nil
  "The timer that asks again when asking has stopped.")

(defun almighty-bolt--poll ()
  "Ask the image for a request, wait for it, do it, and ask again."
  (setq almighty-bolt--last-poll (float-time))
  (almighty-bolt--eval
   (almighty-bolt--call "NEXT-REQUEST" 10)
   (lambda (request)
     (cond ((not almighty-bolt-bridge-mode))
           ;; The watchdog asks again later.
           ((memq request '(:not-loaded :not-connected)))
           (t
            (when request
              (with-demoted-errors "almighty-bolt: %S"
                (almighty-bolt-handle-request request)))
            (almighty-bolt--poll))))))

(defun almighty-bolt--watch ()
  "Ask again if nothing has been asked for a while.
The image was not ready, or a request failed."
  (when (and almighty-bolt-bridge-mode
             (> (- (float-time) almighty-bolt--last-poll) 15))
    (almighty-bolt--poll)))

;;;###autoload
(define-minor-mode almighty-bolt-bridge-mode
  "Keep the almighty-bolt studio, in the image Sly is connected to, in step."
  :global t
  :lighter " Bolt"
  (when almighty-bolt--watchdog
    (cancel-timer almighty-bolt--watchdog)
    (setq almighty-bolt--watchdog nil))
  (when almighty-bolt-bridge-mode
    (setq almighty-bolt--watchdog (run-at-time 5 5 #'almighty-bolt--watch))
    (almighty-bolt--poll)))

;;; --- what Emacs says ---

(defvar-local almighty-bolt--typed nil
  "True if the buffer was changed since its view was last sent.")

(defvar-local almighty-bolt--last-point nil
  "The view and widget point was last said to be on, (NAME ID).")

(defvar almighty-bolt--idle-timer nil
  "The timer that sends what was typed, and where point is, when idle.")

(defun almighty-bolt--after-change (&rest _)
  "Note that the buffer changed, unless this changed it for the image."
  (unless almighty-bolt--inhibit
    (setq almighty-bolt--typed t)))

(defun almighty-bolt-send-view-at-point ()
  "Send the `define-view' form point is in to the image, to be defined there."
  (interactive)
  (save-excursion
    (let ((top (and (almighty-bolt-view-at-point) (almighty-bolt--top-start))))
      (when top
        (goto-char top)
        (let ((text (buffer-substring-no-properties
                     (point) (save-excursion (forward-sexp 1) (point)))))
          (almighty-bolt--eval
           (almighty-bolt--call "VIEW-TYPED" text (almighty-bolt--package))
           (lambda (said)
             (when (and (stringp said) (> (length said) 0))
               (message "almighty-bolt: %s" said)))))))))

(defun almighty-bolt--idle ()
  "Send what point is on, and what was typed, to the image."
  (when (and almighty-bolt-mode (derived-mode-p 'lisp-mode))
    (let ((at (almighty-bolt-view-at-point)))
      (when (and at almighty-bolt--typed)
        (setq almighty-bolt--typed nil)
        ;; A form that is not whole does not read; it is sent when it is.
        (when (ignore-errors
                (save-excursion (goto-char (almighty-bolt--top-start))
                                (forward-sexp 1)
                                t))
          (almighty-bolt-send-view-at-point)))
      (when (and at (not (equal at almighty-bolt--last-point)))
        (setq almighty-bolt--last-point at)
        (almighty-bolt--eval
         (almighty-bolt--call "POINT-IN-VIEW" (car at) (almighty-bolt--package)
                              (or (cadr at) "")))))))

(defun almighty-bolt--idle-in-selected ()
  "`almighty-bolt--idle', in the buffer of the selected window."
  (with-current-buffer (window-buffer (selected-window))
    (almighty-bolt--idle)))

;;;###autoload
(define-minor-mode almighty-bolt-mode
  "Tell the studio where point is in a view, and give it views as typed."
  :lighter ""
  (if almighty-bolt-mode
      (progn
        (add-hook 'after-change-functions #'almighty-bolt--after-change
                  nil t)
        (unless almighty-bolt--idle-timer
          (setq almighty-bolt--idle-timer
                (run-with-idle-timer almighty-bolt-idle-delay t
                                     #'almighty-bolt--idle-in-selected))))
    (remove-hook 'after-change-functions #'almighty-bolt--after-change t)))

(provide 'almighty-bolt)
;;; almighty-bolt.el ends here
