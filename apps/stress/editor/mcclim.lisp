(defpackage #:almighty-bolt-stress/editor/mcclim
  (:use #:cl)
  (:import-from #:almighty-bolt-stress/editor/model
                #:generate-text
                #:default-line-count)
  (:import-from #:clim
                #:define-application-frame
                #:make-application-frame
                #:*application-frame*
                #:run-frame-top-level
                #:make-text-style)
  (:export #:make-editor-frame
           #:run-editor))

(in-package #:almighty-bolt-stress/editor/mcclim)

;;; The text editor, McCLIM: the TEXT-EDITOR gadget, with the sample document in it, in the viewport
;;; that McCLIM makes for it (24 lines of 90 characters, scrolling by the wheel and to the caret,
;;; with no scroll bar drawn). The gadget has the caret, the selection, the keys, the mouse, a kill
;;; ring, and an input method's composition if the backend sends one. What is not done, because
;;; McCLIM does not have it: the gutter of line numbers and the highlight of the current line (the
;;; gadget draws text, a selection and a caret, and nothing more), and the status line (the position
;;; of the caret is not in what McCLIM exports: EDIT-CURSOR is an internal function, and nothing is
;;; called when the caret moves, only when the text changes). See notes/mcclim-stress-apps.org.

(define-application-frame editor-frame ()
  ((line-count :initarg :line-count :reader line-count))
  (:panes (editor :text-editor :ncolumns 90 :nlines 24
                               :text-style (make-text-style :fix :roman 14)
                               :value (generate-text (line-count *application-frame*))))
  (:layouts (default editor))
  (:default-initargs :line-count (default-line-count) :width 740 :height 560))

(defun make-editor-frame (&rest initargs)
  (apply #'make-application-frame 'editor-frame initargs))

(defun run-editor (&rest initargs)
  (run-frame-top-level (apply #'make-editor-frame initargs)))
