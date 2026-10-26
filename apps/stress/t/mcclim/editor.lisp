(defpackage #:almighty-bolt-stress/t/mcclim/editor
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-stress/editor/model)
                    (#:e #:almighty-bolt-stress/editor/mcclim)
                    (#:s #:almighty-bolt-stress/t/mcclim/support)
                    (#:clim #:clim)))

(in-package #:almighty-bolt-stress/t/mcclim/editor)

;;; The text editor in McCLIM, with two hundred lines: that the gadget holds the sample document and
;;; takes another. (A typed character is not tested here: McCLIM's null port has no keyboard focus,
;;; so a key event cannot be handled on it; scripts/mcclim-measure.lisp types on the CLX port. What
;;; the app does not have, a gutter and a status line, cannot be tested.)

(defun maker (&rest initargs)
  (apply #'e:make-editor-frame :line-count 200 initargs))

(defun editor (frame) (clim:find-pane-named frame 'almighty-bolt-stress/editor/mcclim::editor))

(define-test the-gadget-holds-the-sample-document ()
  (s:with-running-frame (frame #'maker)
    (assert-equal (m:generate-text 200) (clim:gadget-value (editor frame)))))

(define-test the-gadget-takes-another-text-in-place-of-the-document ()
  (s:with-running-frame (frame #'maker)
    (setf (clim:gadget-value (editor frame)) (format nil "one~%two"))
    (assert-equal (format nil "one~%two") (clim:gadget-value (editor frame)))))
