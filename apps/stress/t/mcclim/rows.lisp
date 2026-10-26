(defpackage #:almighty-bolt-stress/t/mcclim/rows
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-stress/rows/model)
                    (#:r #:almighty-bolt-stress/rows/mcclim)
                    (#:s #:almighty-bolt-stress/t/mcclim/support)
                    (#:clim #:clim)
                    (#:clime #:clim-extensions)))

(in-package #:almighty-bolt-stress/t/mcclim/rows)

;;; The million rows in McCLIM, with a thousand of them: what the list pane holds and shows, what
;;; selecting a row says in the status line, and what the keys do through the model's arithmetic.

(defun maker (&rest initargs)
  (apply #'r:make-rows-frame :row-count 1000 initargs))

(defun rows (frame) (clim:find-pane-named frame 'almighty-bolt-stress/rows/mcclim::rows))
(defun status (frame) (clim:find-pane-named frame 'almighty-bolt-stress/rows/mcclim::status))

(defun press (frame command)
  (let ((clim:*application-frame* frame))
    (funcall command)))

(define-test the-list-pane-holds-every-row ()
  (s:with-running-frame (frame #'maker)
    (assert-eql 1000 (length (clime:list-pane-items (rows frame))))
    (assert-equal "Row 0000001  Ada Lovelace            0.0"
                  (almighty-bolt-stress/rows/mcclim::row-line 0))))

(define-test the-status-line-says-no-row-is-selected-until-one-is ()
  (s:with-running-frame (frame #'maker)
    (assert-equal (list (m:status-text nil)) (s:pane-strings (status frame)))))

(define-test selecting-a-row-puts-it-in-the-status-line ()
  (s:with-running-frame (frame #'maker)
    (setf (clim:gadget-value (rows frame) :invoke-callback t) 5)
    (assert-equal (list (m:status-text 5)) (s:pane-strings (status frame)))))

(define-test the-keys-move-the-selection-by-the-model ()
  (s:with-running-frame (frame #'maker)
    (press frame 'almighty-bolt-stress/rows/mcclim::com-down)
    (assert-eql 1 (clim:gadget-value (rows frame)))
    (press frame 'almighty-bolt-stress/rows/mcclim::com-page-down)
    (assert-eql (+ 1 m:+visible-rows+) (clim:gadget-value (rows frame)))
    (press frame 'almighty-bolt-stress/rows/mcclim::com-up)
    (assert-eql m:+visible-rows+ (clim:gadget-value (rows frame)))
    (press frame 'almighty-bolt-stress/rows/mcclim::com-home)
    (assert-eql 0 (clim:gadget-value (rows frame)))
    (assert-equal (list (m:status-text 0)) (s:pane-strings (status frame)))))
