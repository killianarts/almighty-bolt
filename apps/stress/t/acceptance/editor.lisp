(defpackage #:almighty-bolt-stress/t/acceptance/editor
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:m #:almighty-bolt-stress/editor/model)
                    (#:p #:almighty-bolt-stress/editor/paint)
                    (#:fake #:almighty-bolt-variant-support/fake-context)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-stress/t/acceptance/editor)

;;; The text editor: a text of 20,000 lines of which 24 show, with the number of each line at the
;;; left; keys, text and compositions from the input method, the wheel and the mouse edit and move
;;; around it, and a status line says where the caret is. Controls: :editor and :status. The first
;;; line is empty. The text is the one that GENERATE-TEXT makes, which is kept here to compare with.

(defparameter *reference* (m:make-document (m:generate-text 20000)))

(defun status (driver) (d:text-in driver :status))
(defun screen (driver) (d:text-in driver :editor))

(defun shows-p (driver line)
  "True if the text of LINE (which is not empty) shows in the editor."
  (and (search (m:line-string *reference* line) (screen driver)) t))

(defun click-at (driver line column)
  "Click on the text of LINE (counted from the top of the window) at COLUMN."
  (d:click-at driver :editor
              (/ (+ m:+gutter-width+ p:+text-inset+ (* column fake:+char-width+) 1) m:+editor-width+)
              (/ (+ (* line m:+line-height+) 10) m:+editor-height+)))

(defun at (line column &optional (lines "20,000") selected)
  "What the status line says for the caret at LINE and COLUMN (counted from 1)."
  (format nil "Ln ~:d, Col ~:d    ~a lines~@[    ~a selected~]" line column lines selected))

(d:define-acceptance the-first-lines-show-with-the-caret-at-the-start :editor
  (assert-equal (at 1 1) (status driver))
  (assert-true (shows-p driver 1))
  (assert-true (shows-p driver 14))
  (assert-true (shows-p driver 23))
  (assert-false (shows-p driver 24))
  (assert-true (search "23" (screen driver))))

(d:define-acceptance a-click-puts-the-caret-on-the-line-and-column-it-is-at :editor
  (click-at driver 2 3)
  (assert-equal (at 3 4) (status driver))
  (click-at driver 5 0)
  (assert-equal (at 6 1) (status driver))
  ;; Past the end of a line, the caret is at its end.
  (click-at driver 1 60)
  (assert-equal (at 2 (1+ (length (m:line-string *reference* 1)))) (status driver)))

(d:define-acceptance the-arrows-home-and-end-move-the-caret :editor
  (click-at driver 1 0)
  (d:press-key driver core:+key-right+)
  (d:press-key driver core:+key-right+)
  (assert-equal (at 2 3) (status driver))
  (d:press-key driver core:+key-down+)
  (assert-equal (at 3 3) (status driver))
  (d:press-key driver core:+key-up+)
  (d:press-key driver core:+key-up+)
  (assert-equal (at 1 1) (status driver))                 ; line 1 is empty: the caret is at its end
  (d:press-key driver core:+key-down+)
  (assert-equal (at 2 3) (status driver))                 ; and goes back to the column it wanted
  (d:press-key driver core:+key-end+)
  (assert-equal (at 2 (1+ (length (m:line-string *reference* 1)))) (status driver))
  (d:press-key driver core:+key-home+)
  (assert-equal (at 2 1) (status driver)))

(d:define-acceptance typing-puts-text-at-the-caret-and-backspace-takes-it-away :editor
  (click-at driver 1 0)
  (d:type-text driver "xyz")
  (assert-equal (at 2 4) (status driver))
  (assert-true (search (concatenate 'string "xyz" (m:line-string *reference* 1)) (screen driver)))
  (d:press-key driver #\Backspace)
  (assert-equal (at 2 3) (status driver))
  (assert-true (search (concatenate 'string "xy" (m:line-string *reference* 1)) (screen driver)))
  (d:press-key driver #\Backspace)
  (d:press-key driver #\Backspace)
  (assert-true (shows-p driver 1))
  (assert-equal (at 2 1) (status driver)))

(d:define-acceptance return-breaks-a-line-and-backspace-joins-two :editor
  (click-at driver 1 2)
  (d:press-key driver #\Return)
  (assert-equal (at 3 1 "20,001") (status driver))
  (assert-true (search (subseq (m:line-string *reference* 1) 0 2) (screen driver)))
  (assert-true (search (subseq (m:line-string *reference* 1) 2) (screen driver)))
  (d:press-key driver #\Backspace)
  (assert-equal (at 2 3) (status driver))
  (assert-true (shows-p driver 1)))

(d:define-acceptance shift-and-the-arrows-select-and-typing-replaces-the-selection :editor
  (click-at driver 1 0)
  (dotimes (i 3)
    (d:press-key driver core:+key-right+ :modifiers core:+modifier-shift+))
  (assert-equal (at 2 4 "20,000" 3) (status driver))
  (d:type-text driver "Q")
  (assert-equal (at 2 2) (status driver))
  (assert-true (search (concatenate 'string "Q" (subseq (m:line-string *reference* 1) 3))
                       (screen driver))))

(d:define-acceptance command-a-selects-everything-and-a-delete-empties-the-text :editor
  (click-at driver 1 0)
  (d:press-key driver #\a :modifiers core:+modifier-super+)
  (assert-equal (at 20000 (1+ (length (m:line-string *reference* 19999))) "20,000"
                    (format nil "~:d" (m:document-length *reference*)))
                (status driver))
  (d:press-key driver #\Backspace)
  (assert-equal (at 1 1 "1") (status driver))
  (assert-false (shows-p driver 1)))

(d:define-acceptance the-wheel-scrolls-by-lines-of-the-text :editor
  (d:scroll driver :editor -10)
  (assert-false (shows-p driver 1))
  (assert-false (shows-p driver 11))
  (assert-true (shows-p driver 12))
  (assert-true (shows-p driver 35))
  (assert-false (shows-p driver 36))
  (d:scroll driver :editor 20)
  (assert-true (shows-p driver 1)))

(d:define-acceptance the-part-of-a-line-that-a-wheel-step-leaves-is-not-lost :editor
  ;; A wheel line is 24 points and a text line 20: three of them are 3.6 lines.
  (dotimes (i 3)
    (d:scroll driver :editor -1))
  (assert-true (shows-p driver 3))
  (assert-false (shows-p driver 2)))

(d:define-acceptance page-down-moves-the-caret-a-page-and-scrolls-to-it :editor
  (click-at driver 1 0)
  (d:press-key driver core:+key-page-down+)
  (assert-equal (at 26 1) (status driver))
  (assert-true (shows-p driver 25))                       ; the caret's line is the last that shows
  (assert-false (shows-p driver 27))
  (d:press-key driver core:+key-page-up+)
  (assert-equal (at 2 1) (status driver)))

(d:define-acceptance text-from-the-input-method-is-inserted-as-text :editor
  (click-at driver 1 0)
  (d:input-text driver "héllo")
  (assert-equal (at 2 6) (status driver))
  (assert-true (search (concatenate 'string "héllo" (m:line-string *reference* 1)) (screen driver))))

(d:define-acceptance a-composition-shows-where-it-will-go-and-the-text-is-theirs-when-it-is-committed :editor
  (click-at driver 1 0)
  (let ((plain (length (d:shapes-in driver :editor))))
    (d:compose driver :started "ni")
    (assert-equal (at 2 1) (status driver))                 ; not in the text yet
    (assert-true (search (concatenate 'string "ni" (m:line-string *reference* 1)) (screen driver)))
    (assert-eql (1+ plain) (length (d:shapes-in driver :editor)))   ; underlined
    (d:compose driver :changed "你")
    (assert-true (search (concatenate 'string "你" (m:line-string *reference* 1)) (screen driver)))
    (d:compose driver :committed "你")
    (assert-equal (at 2 2) (status driver))
    (assert-true (search (concatenate 'string "你" (m:line-string *reference* 1)) (screen driver)))
    (assert-eql plain (length (d:shapes-in driver :editor)))))

(d:define-acceptance the-input-method-is-told-where-the-caret-is :editor
  (click-at driver 3 5)
  (d:redraw driver)                                         ; once more: a variant may know a frame late
  (destructuring-bind (x y width height) (d:rectangle driver :editor)
    (declare (ignore width height))
    (multiple-value-bind (on rect) (d:text-input-caret driver)
      (assert-true on)
      (destructuring-bind (cx cy cw ch) rect
        (assert-true (< (abs (- cx (+ x m:+gutter-width+ p:+text-inset+ (* 5 fake:+char-width+)))) 1.0))
        (assert-true (< (abs (- cy (+ y (* 3 m:+line-height+) 2))) 1.0))
        (assert-true (plusp cw))
        (assert-true (plusp ch))))
    ;; While something is composed the candidates go at the end of what is typed.
    (d:compose driver :started "abc")
    (multiple-value-bind (on rect) (d:text-input-caret driver)
      (declare (ignore on))
      (assert-true (< (abs (- (first rect)
                              (+ x m:+gutter-width+ p:+text-inset+ (* 8 fake:+char-width+))))
                      1.0)))))

(d:define-acceptance dragging-with-the-mouse-selects :editor
  (d:press-at driver :editor (/ (+ m:+gutter-width+ p:+text-inset+ 1) m:+editor-width+)
              (/ (+ m:+line-height+ 10) m:+editor-height+))
  (d:drag-to driver :editor (/ (+ m:+gutter-width+ p:+text-inset+ 1) m:+editor-width+)
             (/ (+ (* 3 m:+line-height+) 10) m:+editor-height+))
  (d:release-at driver :editor 0.1 0.1)
  (assert-equal (at 4 1 "20,000" (format nil "~:d" (- (m:line-start *reference* 3)
                                                      (m:line-start *reference* 1))))
                (status driver)))

(d:define-acceptance a-lot-of-typing-and-deleting-leaves-the-text-as-it-was :editor
  (click-at driver 14 0)
  (let ((typed (apply #'concatenate 'string (loop :repeat 30 :collect "abcdefghij"))))
    (d:type-text driver typed)
    (assert-equal (at 15 301) (status driver))
    (dotimes (i 300)
      (d:press-key driver #\Backspace)))
  (assert-equal (at 15 1) (status driver))
  (assert-true (shows-p driver 14)))
