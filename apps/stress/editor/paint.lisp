(defpackage #:almighty-bolt-stress/editor/paint
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:context-font
                #:context-measure
                #:context-caret-x
                #:context-push-text
                #:set-text-input-caret
                #:push-rect
                #:push-clip
                #:push-unclip
                #:pack-color)
  (:import-from #:almighty-bolt-stress/editor/model
                #:+line-height+
                #:+visible-lines+
                #:+gutter-width+
                #:+editor-width+
                #:+editor-height+
                #:document-line-count
                #:document-caret
                #:document-top-line
                #:document-marked-text
                #:document-marked-start
                #:document-marked-length
                #:document-char-width
                #:line-start
                #:line-end
                #:position-line
                #:selection-p
                #:selection-start
                #:selection-end
                #:cached-line-string
                #:line-number-text)
  (:export #:paint-editor
           #:+text-inset+))

(in-package #:almighty-bolt-stress/editor/paint)

;;; How the editor looks, the same in every variant: a gutter of line numbers, the lines that show
;;; in a font of equal widths, the selection behind them, the caret, and the text an input method is
;;; composing underlined where it will go. It also says where the caret is (in the window) so that the
;;; input method can put its candidates beside it, and what a character is wide so that a click can
;;; be turned into a column.

(defparameter +ground+ (pack-color 27 29 38))
(defparameter +gutter+ (pack-color 34 37 49))
(defparameter +current+ (pack-color 36 40 54))
(defparameter +selection+ (pack-color 62 90 170))
(defparameter +text+ (pack-color 226 229 240))
(defparameter +dim+ (pack-color 118 124 146))
(defparameter +caret+ (pack-color 255 214 102))
(defparameter +marked+ (pack-color 150 196 255))

(defconstant +text-inset+ 8 "How far from the gutter the text starts.")

(defun paint-editor (context list x y width height document &optional (origin-x x) (origin-y y))
  "Append the commands that draw DOCUMENT in the area X, Y, WIDTH by HEIGHT. ORIGIN-X and ORIGIN-Y
are where the area is in the window, if that is not where X and Y are."
  (declare (ignore width height))
  (let* ((font (context-font context "Menlo" 12))
         (number-font (context-font context "Menlo" 11))
         (text-x (+ x +gutter-width+ +text-inset+))
         (top (document-top-line document))
         (caret (document-caret document))
         (caret-line (position-line document caret))
         (marked (document-marked-text document))
         (caret-x nil) (caret-y nil))
    (multiple-value-bind (cell ascent descent) (context-measure context font "M")
      (setf (document-char-width document) (float cell 1f0))
      (let ((baseline (+ (/ (- +line-height+ (+ ascent descent)) 2) ascent)))
        (push-rect list x y +editor-width+ +editor-height+ +ground+)
        (push-rect list x y +gutter-width+ +editor-height+ +gutter+)
        (dotimes (slot +visible-lines+)
          (let ((line (+ top slot))
                (row-y (+ y (* slot +line-height+))))
            (when (< line (document-line-count document))
              (let* ((string (cached-line-string document line))
                     (start (line-start document line))
                     (composing (and (= line caret-line) (plusp (length marked))))
                     (column (- caret start))
                     (shown (if composing
                                (concatenate 'string (subseq string 0 column) marked
                                             (subseq string column))
                                string))
                     (number (line-number-text (1+ line))))
                (when (= line caret-line)
                  (push-rect list (+ x +gutter-width+) row-y (- +editor-width+ +gutter-width+)
                             +line-height+ +current+))
                ;; The gutter holds the number of the line, against the text.
                (context-push-text context list number-font number
                                   (- (+ x +gutter-width+) 10
                                      (context-measure context number-font number))
                                   (+ row-y baseline) (if (= line caret-line) +text+ +dim+))
                (push-clip list (+ x +gutter-width+) row-y (- +editor-width+ +gutter-width+)
                           +line-height+)
                (when (and (selection-p document) (not composing)
                           (< (selection-start document) (1+ (line-end document line)))
                           (> (selection-end document) start))
                  (let* ((from (max 0 (- (selection-start document) start)))
                         (to (min (length string) (- (selection-end document) start)))
                         (past-end (> (selection-end document) (line-end document line)))
                         (left (context-caret-x context font string from))
                         (right (+ (context-caret-x context font string to)
                                   (if past-end (* 0.5 cell) 0))))
                    (push-rect list (+ text-x left) row-y (max 1f0 (- right left)) +line-height+
                               +selection+)))
                (context-push-text context list font shown text-x (+ row-y baseline) +text+)
                (when composing
                  (let ((left (context-caret-x context font shown column))
                        (right (context-caret-x context font shown (+ column (length marked)))))
                    (push-rect list (+ text-x left) (+ row-y baseline 2) (- right left) 1f0
                               +marked+)))
                (push-unclip list)
                (when (= line caret-line)
                  (let ((at (context-caret-x context font shown
                                             (+ column (if composing
                                                           (document-marked-start document)
                                                           0)))))
                    (setf caret-x (+ text-x at)
                          caret-y (+ row-y 2))
                    (push-rect list caret-x caret-y 1.5f0 (- +line-height+ 4) +caret+)))))))
        ;; Where the input method puts its window: at the caret, or at the top if it is not shown.
        (set-text-input-caret context
                              (+ origin-x (- (or caret-x text-x) x))
                              (+ origin-y (- (or caret-y y) y))
                              1.5f0 (float (- +line-height+ 4) 1f0))))))
