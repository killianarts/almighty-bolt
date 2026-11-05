(defpackage #:almighty-bolt-stress/editor/paint
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt)
                    (#:model #:almighty-bolt-stress/editor/model))
  (:export #:paint-editor
           #:+text-inset+))

(in-package #:almighty-bolt-stress/editor/paint)

;;; How the editor looks: a gutter of line numbers, the lines that show
;;; in a font of equal widths, the selection behind them, the caret, and the text an input method is
;;; composing underlined where it will go. It also says where the caret is (in the window) so that the
;;; input method can put its candidates beside it, and what a character is wide so that a click can
;;; be turned into a column.

(defparameter +ground+ (bolt:pack-color 27 29 38))
(defparameter +gutter+ (bolt:pack-color 34 37 49))
(defparameter +current+ (bolt:pack-color 36 40 54))
(defparameter +selection+ (bolt:pack-color 62 90 170))
(defparameter +text+ (bolt:pack-color 226 229 240))
(defparameter +dim+ (bolt:pack-color 118 124 146))
(defparameter +caret+ (bolt:pack-color 255 214 102))
(defparameter +marked+ (bolt:pack-color 150 196 255))

(defconstant +text-inset+ 8 "How far from the gutter the text starts.")

(defun paint-editor (context list x y width height document)
  "Append the commands that draw DOCUMENT in the area X, Y, WIDTH by HEIGHT, and say where the
caret is in the area, which is what the input method needs and the canvas's app puts in the
window."
  (declare (ignore width height))
  (let* ((font (bolt:context-font context model:+font-name+ model:+font-size+))
         (number-font (bolt:context-font context model:+font-name+ model:+number-font-size+))
         (text-x (+ x model:+gutter-width+ +text-inset+))
         (top (model:document-top-line document))
         (caret (model:document-caret document))
         (caret-line (model:position-line document caret))
         (marked (model:document-marked-text document))
         (caret-x nil) (caret-y nil))
    (multiple-value-bind (cell ascent descent) (bolt:context-measure context font "M")
      (let ((baseline (+ (/ (- model:+line-height+ (+ ascent descent)) 2) ascent)))
        (bolt:push-rect list x y model:+editor-width+ model:+editor-height+ +ground+)
        (bolt:push-rect list x y model:+gutter-width+ model:+editor-height+ +gutter+)
        (dotimes (slot model:+visible-lines+)
          (let ((line (+ top slot))
                (row-y (+ y (* slot model:+line-height+))))
            (when (< line (model:document-line-count document))
              (let* ((string (model:cached-line-string document line))
                     (start (model:line-start document line))
                     (composing (and (= line caret-line) (plusp (length marked))))
                     (column (- caret start))
                     (shown (if composing
                                (concatenate 'string (subseq string 0 column) marked
                                             (subseq string column))
                                string))
                     (number (model:line-number-text (1+ line))))
                (when (= line caret-line)
                  (bolt:push-rect list (+ x model:+gutter-width+) row-y (- model:+editor-width+ model:+gutter-width+)
                                  model:+line-height+ +current+))
                ;; The gutter holds the number of the line, against the text.
                (bolt:context-push-text context list number-font number
                                        (- (+ x model:+gutter-width+) 10
                                           (bolt:context-measure context number-font number))
                                        (+ row-y baseline) (if (= line caret-line) +text+ +dim+))
                (bolt:push-clip list (+ x model:+gutter-width+) row-y (- model:+editor-width+ model:+gutter-width+)
                                model:+line-height+)
                (when (and (model:selection-p document) (not composing)
                           (< (model:selection-start document) (1+ (model:line-end document line)))
                           (> (model:selection-end document) start))
                  (let* ((from (max 0 (- (model:selection-start document) start)))
                         (to (min (length string) (- (model:selection-end document) start)))
                         (past-end (> (model:selection-end document) (model:line-end document line)))
                         (left (bolt:context-caret-x context font string from))
                         (right (+ (bolt:context-caret-x context font string to)
                                   (if past-end (* 0.5 cell) 0))))
                    (bolt:push-rect list (+ text-x left) row-y (max 1f0 (- right left)) model:+line-height+
                                    +selection+)))
                (bolt:context-push-text context list font shown text-x (+ row-y baseline) +text+)
                (when composing
                  (let ((left (bolt:context-caret-x context font shown column))
                        (right (bolt:context-caret-x context font shown (+ column (length marked)))))
                    (bolt:push-rect list (+ text-x left) (+ row-y baseline 2) (- right left) 1f0
                                    +marked+)))
                (bolt:push-unclip list)
                (when (= line caret-line)
                  (let ((at (bolt:context-caret-x context font shown
                                                  (+ column (if composing
                                                                (model:document-marked-start document)
                                                                0)))))
                    (setf caret-x (+ text-x at)
                          caret-y (+ row-y 2))
                    (bolt:push-rect list caret-x caret-y 1.5f0 (- model:+line-height+ 4) +caret+)))))))
        ;; Where the input method puts its window: at the caret, or at the top if it is not shown.
        (bolt:set-text-input-caret context
                                   (- (or caret-x text-x) x)
                                   (- (or caret-y y) y)
                                   1.5f0 (float (- model:+line-height+ 4) 1f0))))))
