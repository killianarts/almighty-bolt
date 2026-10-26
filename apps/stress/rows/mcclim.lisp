(defpackage #:almighty-bolt-stress/rows/mcclim
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:+key-up+
                #:+key-down+
                #:+key-page-up+
                #:+key-page-down+
                #:+key-home+
                #:+key-end+)
  (:import-from #:almighty-bolt-stress/rows/model
                #:+row-count+
                #:+visible-rows+
                #:row-id-text
                #:row-name-text
                #:row-value-text
                #:key-move
                #:status-text)
  (:import-from #:clim
                #:define-application-frame
                #:*application-frame*
                #:make-application-frame
                #:run-frame-top-level
                #:redisplay-frame-pane
                #:find-pane-named
                #:pane-frame
                #:vertically
                #:scrolling
                #:make-pane
                #:list-pane
                #:gadget-value
                #:make-text-style
                #:text-style-height
                #:pane-text-style
                #:scroll-extent
                #:pane-viewport-region
                #:bounding-rectangle-min-y)
  (:export #:make-rows-frame
           #:run-rows))

(in-package #:almighty-bolt-stress/rows/mcclim)

;;; The million rows, McCLIM: a LIST-PANE of the rows in a scroller. A list pane has one string for
;;; each item (so the three columns are one string, aligned by a text style of equal widths), keeps
;;; every item (so a million are made when the pane is), selects one by a click and calls back when
;;; the selection changes. It takes no keys, so they are keystrokes of the frame's commands, which
;;; use the model's arithmetic. It has no stripes. McCLIM has no way to give it the million as a
;;; function of an index: that is what is not done here (notes/mcclim-stress-apps.org).

(defun row-line (index)
  (format nil "~a  ~20a ~6@a" (row-id-text index) (row-name-text index) (row-value-text index)))

(defun row-items (count)
  (let ((items (make-array count)))
    (dotimes (index count items)
      (setf (svref items index) index))))

(defun display-status (frame stream)
  (write-string (status-text (gadget-value (find-pane-named frame 'rows))) stream))

(defun move-selection (frame key)
  "What KEY does to the selected row and to the row at the top of the list, by the model."
  (let* ((pane (find-pane-named frame 'rows))
         (height (text-style-height (pane-text-style pane) pane))
         (top (floor (bounding-rectangle-min-y (pane-viewport-region pane)) height)))
    (multiple-value-bind (selected new-top handled) (key-move key (gadget-value pane) top)
      (when handled
        (setf (gadget-value pane :invoke-callback t) selected)
        (scroll-extent pane 0 (* new-top height))))))

(define-application-frame rows-frame ()
  ((row-count :initarg :row-count :reader row-count))
  (:panes (status :application :display-function 'display-status :scroll-bars nil
                  :height 24 :max-height 24)
          (rows (make-pane 'list-pane :mode :exclusive :name-key 'row-line
                                      :items (row-items (row-count *application-frame*))
                                      :text-style (make-text-style :fix :roman 14)
                                      :value-changed-callback
                                      (lambda (pane value)
                                        (declare (ignore value))
                                        (redisplay-frame-pane (pane-frame pane) 'status
                                                              :force-p t)))))
  (:layouts (default (vertically (:spacing 12)
                       status
                       (scrolling (:width 600 :height 396) rows))))
  (:default-initargs :row-count +row-count+ :width 640 :height 470))

(define-rows-frame-command (com-up :keystroke (:up)) ()
  (move-selection *application-frame* +key-up+))
(define-rows-frame-command (com-down :keystroke (:down)) ()
  (move-selection *application-frame* +key-down+))
(define-rows-frame-command (com-page-up :keystroke (:prior)) ()
  (move-selection *application-frame* +key-page-up+))
(define-rows-frame-command (com-page-down :keystroke (:next)) ()
  (move-selection *application-frame* +key-page-down+))
(define-rows-frame-command (com-home :keystroke (:home)) ()
  (move-selection *application-frame* +key-home+))
(define-rows-frame-command (com-end :keystroke (:end)) ()
  (move-selection *application-frame* +key-end+))

(defun make-rows-frame (&rest initargs)
  (apply #'make-application-frame 'rows-frame initargs))

(defun run-rows (&rest arguments)
  (run-frame-top-level (apply #'make-rows-frame arguments)))
