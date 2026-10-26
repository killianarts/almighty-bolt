(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system "almighty-bolt-core")

(defpackage #:almighty-bolt-scripts/make-stress-scripts
  (:use #:cl)
  (:local-nicknames (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-scripts/make-stress-scripts)

;;;; Writes the replay scripts of the stress apps into apps/stress/replay/, from the repository root:
;;;;   sbcl --script scripts/make-stress-scripts.lisp
;;;; They are made here, in the script format of core/src/replay/script.lisp, and not recorded from a
;;;; session, because a session of a person's is not the same twice and these have to be: each is a
;;;; fixed timeline of events at the control that they name, so that every variant is driven the same.
;;;; Times are in nanoseconds; an event is at a fraction of a control.

(defconstant +ms+ 1000000)

(defun write-script (name header entries)
  (let ((path (format nil "apps/stress/replay/~a.script" name)))
    (core:save-script (core:make-script :header header :entries entries) path)
    (format t "~&~a: ~d entries~%" path (length entries))))

(defun rows-scroll ()
  "Scrolling the million rows: a click for the keyboard, 120 wheel steps, 40 key presses, a drag of
the bar."
  (let ((time 0) (entries '()))
    (flet ((at (delta form) (push (list (incf time delta) form) entries))
           (key (character) (list :key :kind :down :logical-key character)))
      (at 0 '(:focus :focused-p t))
      (at (* 100 +ms+) '(:pointer :kind :move :target :list :fx 0.2d0 :fy 0.1d0))
      (at (* 16 +ms+) '(:pointer :kind :down :target :list :fx 0.2d0 :fy 0.1d0 :button 0 :buttons 1
                        :click-count 1))
      (at (* 16 +ms+) '(:pointer :kind :up :target :list :fx 0.2d0 :fy 0.1d0 :button 0))
      (dotimes (i 120)
        (at (* 16 +ms+) (list :wheel :target :list :fx 0.5d0 :fy 0.5d0 :dx 0d0
                              :dy (if (< i 100) -3d0 4d0))))
      (dotimes (i 40)
        (at (* 25 +ms+) (key core:+key-down+)))
      (dotimes (i 5)
        (at (* 50 +ms+) (key core:+key-page-down+)))
      (at (* 50 +ms+) (key core:+key-end+))
      (at (* 50 +ms+) (key core:+key-home+))
      (at (* 16 +ms+) '(:pointer :kind :move :target :list :fx 0.988d0 :fy 0.1d0))
      (at (* 16 +ms+) '(:pointer :kind :down :target :list :fx 0.988d0 :fy 0.1d0 :button 0
                        :buttons 1 :click-count 1))
      (dotimes (i 40)
        (at (* 16 +ms+) (list :pointer :kind :move :target :list :fx 0.988d0
                              :fy (+ 0.1d0 (* i 0.02d0)) :buttons 1)))
      (at (* 16 +ms+) '(:pointer :kind :up :target :list :fx 0.988d0 :fy 0.9d0 :button 0))
      (at (* 100 +ms+) '(:checkpoint :name "end")))
    (write-script "rows-scroll" '(:variant :immediate :app :rows
                                  :width 640 :height 470)
                  (nreverse entries))))

(defun editor-session ()
  "Editing the text: a click, 160 characters typed as keys, 40 words as text from the input method,
lines broken, the caret moved about and a range selected and deleted, pages down and up, the wheel,
five compositions of a few steps each, and a drag over several lines."
  (let ((time 0) (entries '())
        (sentence "the quick brown fox jumps over the lazy dog while the editor keeps up. "))
    (flet ((at (delta form) (push (list (incf time delta) form) entries))
           (key (character &optional (modifiers 0))
             (list :key :kind :down :logical-key character :modifiers modifiers)))
      (at 0 '(:focus :focused-p t))
      (at (* 100 +ms+) '(:pointer :kind :move :target :editor :fx 0.2d0 :fy 0.2d0))
      (at (* 16 +ms+) '(:pointer :kind :down :target :editor :fx 0.2d0 :fy 0.2d0 :button 0
                        :buttons 1 :click-count 1))
      (at (* 16 +ms+) '(:pointer :kind :up :target :editor :fx 0.2d0 :fy 0.2d0 :button 0))
      (dotimes (i 160)
        (at (* 20 +ms+) (key (char sentence (mod i (length sentence))))))
      (dotimes (i 40)
        (at (* 30 +ms+) (list :text :text (if (zerop (mod i 10)) (format nil "line ~d " i) "word "))))
      (dotimes (i 6)
        (at (* 40 +ms+) (key #\Return)))
      (dotimes (i 30)
        (at (* 25 +ms+) (key core:+key-left+)))
      (dotimes (i 12)
        (at (* 25 +ms+) (key core:+key-up+)))
      (dotimes (i 20)
        (at (* 25 +ms+) (key core:+key-right+ core:+modifier-shift+)))
      (at (* 50 +ms+) (key #\Backspace))
      (dotimes (i 3)
        (at (* 100 +ms+) (key core:+key-page-down+)))
      (dotimes (i 3)
        (at (* 100 +ms+) (key core:+key-page-up+)))
      (dotimes (i 60)
        (at (* 16 +ms+) (list :wheel :target :editor :fx 0.5d0 :fy 0.5d0 :dx 0d0
                              :dy (if (< i 40) -2d0 3d0))))
      (dotimes (i 5)
        (at (* 80 +ms+) (list :composition :kind :started :text "n"))
        (at (* 80 +ms+) (list :composition :kind :changed :text "ni"))
        (at (* 80 +ms+) (list :composition :kind :changed :text (format nil "~c" (code-char #x4F60))
                              :selection-start 1))
        (at (* 80 +ms+) (list :composition :kind :committed
                              :text (format nil "~c~c" (code-char #x4F60) (code-char #x597D)))))
      (at (* 100 +ms+) '(:pointer :kind :move :target :editor :fx 0.2d0 :fy 0.1d0))
      (at (* 16 +ms+) '(:pointer :kind :down :target :editor :fx 0.2d0 :fy 0.1d0 :button 0
                        :buttons 1 :click-count 1))
      (dotimes (i 30)
        (at (* 16 +ms+) (list :pointer :kind :move :target :editor :fx (+ 0.2d0 (* i 0.01d0))
                              :fy (+ 0.1d0 (* i 0.025d0)) :buttons 1)))
      (at (* 16 +ms+) '(:pointer :kind :up :target :editor :fx 0.5d0 :fy 0.85d0 :button 0))
      (at (* 50 +ms+) (key #\a core:+modifier-super+))
      (at (* 100 +ms+) '(:checkpoint :name "end")))
    (write-script "editor-session" '(:variant :immediate :app :editor
                                     :width 740 :height 560)
                  (nreverse entries))))

(rows-scroll)
(editor-session)
