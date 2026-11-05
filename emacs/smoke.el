;;; smoke.el --- The bridge to Emacs, over a real Sly -*- lexical-binding: t -*-

;;; Commentary:

;; Run by emacs/smoke.lisp (make emacs-smoke), which starts the Lisp and its
;; Slynk server and gives `bolt-smoke-port' and `bolt-smoke-file', a file that
;; defines the view smoke-view.  It says "The Emacs bridge check passed" when
;; every check holds, and exits with a status that says so.

;;; Code:

(require 'sly)
(require 'almighty-bolt)

(defvar bolt-smoke-port)
(defvar bolt-smoke-file)
(defvar bolt-smoke-failures 0)

(defun bolt-smoke-check (what ok &optional detail)
  "Say WHAT, and whether it holds (OK); DETAIL is shown when it does not."
  (unless ok (setq bolt-smoke-failures (1+ bolt-smoke-failures)))
  (princ (format "%s %s%s\n" (if ok "ok  " "FAIL") what
                 (if (and (not ok) detail) (format " (%S)" detail) "")))
  ok)

(defun bolt-smoke-wait (predicate seconds)
  "Wait up to SECONDS for PREDICATE to hold, letting Sly's replies in.
Return what it returned when it held, or NIL; it is not called again after."
  (let ((end (+ (float-time) seconds))
        (held nil))
    (while (and (not (setq held (funcall predicate))) (< (float-time) end))
      (accept-process-output nil 0.05))
    held))

(defun bolt-smoke-text ()
  "The view smoke-view's words, as the image has them."
  (sly-eval '(cl:getf (cl:third (cl:fourth (almighty-bolt/ui/views:find-view
                                            'cl-user::smoke-view)))
                      :text)))

(let ((inhibit-message t))
  (sly-connect "127.0.0.1" bolt-smoke-port))
(bolt-smoke-check "Sly connects to the Lisp"
                  (bolt-smoke-wait #'sly-connected-p 60))

(let ((buffer (find-file-noselect bolt-smoke-file)))
  (with-current-buffer buffer
    (lisp-mode)
    (almighty-bolt-mode 1))
  (almighty-bolt-bridge-mode 1)
  (bolt-smoke-check
   "the bridge asks the image what to do"
   (bolt-smoke-wait (lambda ()
                      (sly-eval '(almighty-bolt/emacs:emacs-connected-p)))
                    10))
  ;; Edit Source: the image asks Emacs to show a widget's clause.
  (sly-eval '(almighty-bolt/emacs:show-view 'cl-user::smoke-view :go))
  (bolt-smoke-check
   "Edit Source brings Emacs to the widget's clause in the view's file"
   (bolt-smoke-wait (lambda () (with-current-buffer buffer
                                 (looking-at "(:button :go")))
                    10)
   (with-current-buffer buffer
     (buffer-substring (point) (min (point-max) (+ (point) 20)))))
  ;; An edit in the studio: the view defined again in the image goes into the
  ;; buffer, as an edit.
  (sly-eval '(almighty-bolt/ui/views:set-view
              'cl-user::smoke-view
              '(:column :page (:gap 4)
                        (:label :words (:text "After"))
                        (:button :go (:text "Go")))))
  (bolt-smoke-check
   "a view defined in the image is put in the buffer of its file"
   (bolt-smoke-wait (lambda () (with-current-buffer buffer
                                 (save-excursion
                                   (goto-char (point-min))
                                   (search-forward "\"After\"" nil t))))
                    10)
   (with-current-buffer buffer (buffer-string)))
  (bolt-smoke-check "as an edit, and not sent back"
                    (with-current-buffer buffer
                      (and (buffer-modified-p) (not almighty-bolt--typed))))
  ;; Typed in the buffer: the view is defined in the image after a pause.
  (with-current-buffer buffer
    (goto-char (point-min))
    (search-forward "\"After\"")
    (replace-match "\"Typed\"")
    (goto-char (match-beginning 0))
    (almighty-bolt--idle))
  (bolt-smoke-check
   "a view typed in the buffer is defined in the image"
   (bolt-smoke-wait (lambda () (equal (bolt-smoke-text) "Typed")) 10)
   (bolt-smoke-text))
  ;; Point on a clause: the studio is told which widget.
  (let ((selected nil))
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "(:button :go")
      (goto-char (match-beginning 0))
      (almighty-bolt--idle))
    (bolt-smoke-check
     "point on a widget's clause is what the studio selects"
     (bolt-smoke-wait
      (lambda ()
        (setq selected
              (sly-eval '(cl:format nil "~(~s~)"
                                    (cl:multiple-value-list
                                     (almighty-bolt/emacs:take-selection)))))
        (equal selected "(smoke-view :go)"))
      10)
     selected))
  (with-current-buffer buffer (set-buffer-modified-p nil))
  (almighty-bolt-bridge-mode -1))

(princ (if (zerop bolt-smoke-failures)
           "The Emacs bridge check passed.\n"
         (format "FAIL: %d check%s did not hold.\n" bolt-smoke-failures
                 (if (= bolt-smoke-failures 1) "" "s"))))
(kill-emacs (if (zerop bolt-smoke-failures) 0 1))

;;; smoke.el ends here
