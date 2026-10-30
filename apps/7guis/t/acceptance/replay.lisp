(defpackage #:almighty-bolt-7guis/t/acceptance/replay
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:g #:almighty-bolt-7guis/cells/geometry)
                    (#:fake #:almighty-bolt/test-support/fake-context)
                    (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:core #:almighty-bolt/internals)))

(in-package #:almighty-bolt-7guis/t/acceptance/replay)

;;; Replay scripts, which name the controls they act on, drive a task's app. And a script replays
;;; to the same frames every time, which has to hold before any measurement is trusted.

(defun make-app (variant task)
  (registry:make-app-object (registry:find-app variant task)))

(defun make-context (variant task)
  (let ((app (registry:find-app variant task)))
    (fake:make-fake-context :width (float (registry:app-width app) 1f0)
                            :height (float (registry:app-height app) 1f0))))

(defmacro define-replay-test (name task (new) &body body)
  "A test of NAME for each variant that has an app for TASK. In BODY, (NEW) is a function that
makes a new variant of that app and a context for it, as two values."
  `(progn
     ,@(loop :for which :in (d:available-variants task)
             :collect `(lisp-unit2:define-test ,(intern (format nil "~a/~a" name which)) ()
                         (flet ((,new ()
                                  (values (make-app ,which ,task) (make-context ,which ,task))))
                           ,@body)))))

(defun click (id &key (at 0) (fx 0.5d0) (fy 0.5d0) (count 1))
  "The entries of a click on the control ID, the press 1 ms after the move."
  (let ((target (list :target id :fx fx :fy fy)))
    (list (list at (list* :pointer :kind :move target))
          (list (+ at 1000000) (append (list :pointer :kind :down) target
                                       (list :button 0 :buttons 1 :click-count count)))
          (list (+ at 2000000) (append (list :pointer :kind :up) target
                                       (list :button 0 :click-count count))))))

(defun keys (string &optional (at 0))
  (loop :for character :across string
        :for time :from at :by 1000000
        :collect (list time (list :key :kind :down :logical-key character))))

(defun glyph-string (list left top width height)
  (coerce (loop :for index :below (core:display-list-count list)
                :when (and (= (core:display-list-kind list index) core:+command-glyph+)
                           (<= left (core:display-list-x list index) (+ left width -1))
                           (<= top (core:display-list-y list index) (+ top height -1)))
                  :collect (code-char (round (core:display-list-parameter list index))))
          'string))

(defun text-in (variant result id)
  (multiple-value-bind (x y width height) (core:lookup-target variant id)
    (glyph-string (core:replay-result-list result) x y width height)))

(defun run (script new)
  "Replay SCRIPT into a new variant. Returns the variant and the result."
  (multiple-value-bind (variant context) (funcall new)
    (values variant (core:replay script variant context))))

(defun hashes (result)
  (map 'list #'cdr (core:replay-result-hashes result)))

;;; --- the counter ---

(defun counter-script ()
  (core:make-script
   :header '(:width 360 :height 160 :scale 1.0d0)
   :entries (loop :for i :below 3
                  :append (click :count-button :at (* i 100000000)))))

(define-replay-test one-script-clicks-the-counter-in-every-variant :counter (new)
  (multiple-value-bind (variant result) (run (counter-script) #'new)
    (assert-equal "3" (text-in variant result :count))
    (assert-true (core:replay-ok-p result))))

(define-replay-test a-script-replays-to-the-same-frames-every-time :counter (new)
  (let ((script (counter-script)))
    (assert-equal (hashes (nth-value 1 (run script #'new)))
                  (hashes (nth-value 1 (run script #'new))))))

(define-replay-test checkpoints-made-from-one-run-hold-in-the-next :counter (new)
  (let* ((with (core:script-with-checkpoints (counter-script)))
         (golden (core:freeze-checkpoints with (nth-value 1 (run with #'new)))))
    (assert-true (core:replay-ok-p (nth-value 1 (run golden #'new))))))

;;; --- the timer: time is the script's ---

(defun timer-script ()
  (core:make-script :header '(:width 460 :height 250 :scale 1.0d0)
                    :entries '((2500000000 (:checkpoint :name "2.5 s")))))

(define-replay-test the-timer-shows-the-time-of-the-script-not-the-time-it-took :timer (new)
  (multiple-value-bind (variant result) (run (timer-script) #'new)
    (assert-equal "2.5s" (text-in variant result :elapsed))))

(define-replay-test the-timer-replays-to-the-same-frames-every-time :timer (new)
  (assert-equal (hashes (nth-value 1 (run (timer-script) #'new)))
                (hashes (nth-value 1 (run (timer-script) #'new)))))

;;; --- cells: a double click, and keys ---

(defun cells-script ()
  "Type abc into A0, finish with Return, double-click A0, add d and finish."
  (let ((fx (/ (+ (g:cell-left 0 0) 40d0) g:+grid-width+))
        (fy (/ (+ (g:cell-top 0 0) 12d0) g:+grid-height+)))
    (core:make-script
     :header '(:width 640 :height 440 :scale 1.0d0)
     :entries (append (click :grid :at 0 :fx fx :fy fy)
                      (keys "abc" 10000000)
                      (keys (string #\Return) 20000000)
                      (click :grid :at 100000000 :fx fx :fy fy)
                      (click :grid :at 101000000 :fx fx :fy fy :count 2)
                      (keys "d" 200000000)
                      (keys (string #\Return) 210000000)))))

(define-replay-test a-script-types-and-double-clicks-in-the-cells :cells (new)
  (multiple-value-bind (variant result) (run (cells-script) #'new)
    (multiple-value-bind (x y) (core:lookup-target variant :grid)
      (assert-equal "abcd" (glyph-string (core:replay-result-list result)
                                         (+ x (g:cell-left 0 0)) (+ y (g:cell-top 0 0))
                                         g:+cell-width+ g:+cell-height+)))
    ;; Return after the second edit selects the cell below.
    (assert-equal "A1" (text-in variant result :cell-name))))
