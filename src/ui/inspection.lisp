(defpackage #:almighty-bolt/ui/inspection
  (:use #:cl)
  (:local-nicknames (#:hash #:almighty-bolt/ui/hash)
                    (#:box #:almighty-bolt/ui/box)
                    (#:ui #:almighty-bolt/ui/ui)
                    (#:theme #:almighty-bolt/ui/theme)
                    (#:widgets #:almighty-bolt/ui/widgets))
  (:export #:*inspector-opener*
           #:inspection-take-input
           #:describe-outlines
           #:box-at
           #:box-by-key))

(in-package #:almighty-bolt/ui/inspection)

;;; The inspector is a window of its own, beside the app's (see inspector.lisp), and it is only
;;; there while the app is developed. What it does in the app's window is here: Command-Option-I
;;; opens it, the press that picks a widget is taken from the app before the app's function is
;;; called, and the outlines that show what the pointer is over and what was picked are drawn over
;;; the app after it, as floats that are not in the semantic tree. What it asked for is an
;;; INSPECTION, which the inspector puts in the app's ui; the ui is handed whole to the app made
;;; again when code is compiled, and the pick with it.
;;;
;;; What it says of a widget is from the last frame that was laid out: the boxes are still those of
;;; that frame when the input of a new one is taken, and between frames, when the inspector's window
;;; reads them. The pick is kept by the box's key, which is the same from frame to frame while the
;;; widget is described in the same place. (The INSPECTION itself is in ui.lisp, where the pointer's
;;; events are told that it picks.)

(defvar *inspector-opener* nil
  "A function of the app, called when Command-Option-I is pressed in a window that may have an
inspector, which opens the inspector's window or closes it; set by the app (see app.lisp). NIL
where there are no windows to open, and then the keys are the app's.")

;;; --- finding boxes in the frame before ---

(defun walk-frame (ui function)
  "Call FUNCTION with each box of the last frame laid out, and where it is in the window (x and y),
the part of the window it shows in (left, top, right and bottom: a scroll container's window, or
all of it), in the order they were described: the screen, then the app's floats."
  (labels ((walk (box x y left top right bottom)
             (let ((x (+ x (box:box-x box))) (y (+ y (box:box-y box))))
               (funcall function box x y left top right bottom)
               (if (member (box:box-kind box) '(:scroll :lazy-list))
                   (let ((left (max x left)) (top (max y top))
                         (right (min (+ x (box:box-w box)) right))
                         (bottom (min (+ y (box:box-h box)) bottom)))
                     (loop :for child :across (box:box-children box)
                           :do (walk child x y left top right bottom)))
                   (loop :for child :across (box:box-children box)
                         :do (walk child x y left top right bottom))))))
    (when (ui:ui-root ui)
      (walk (ui:ui-root ui) 0f0 0f0 -1f30 -1f30 1f30 1f30))
    (loop :for float :across (ui:ui-floats ui)
          :unless (eq (box:box-kind float) :outline)
            :do (walk float 0f0 0f0 -1f30 -1f30 1f30 1f30))))

(defun box-at (ui px py)
  "The box of the last frame that is under the point PX, PY and shows there, the last described (so
the innermost, and a float before the screen under it), and where it is: three values, or NIL."
  (let ((found nil) (found-x 0f0) (found-y 0f0))
    (walk-frame ui (lambda (box x y left top right bottom)
                     (when (and (<= (max x left) px) (< px (min (+ x (box:box-w box)) right))
                                (<= (max y top) py) (< py (min (+ y (box:box-h box)) bottom)))
                       (setf found box found-x x found-y y))))
    (values found found-x found-y)))

(defun box-by-key (ui key)
  "The box of the last frame with KEY, and where it is: three values, or NIL."
  (walk-frame ui (lambda (box x y left top right bottom)
                   (declare (ignore left top right bottom))
                   (when (= (box:box-key box) key)
                     (return-from box-by-key (values box x y)))))
  nil)

;;; --- what it takes before the app ---

(defun inspection-take-input (ui instance)
  "Take from the frame's input, before the app's function, what is the inspector's: Command-Option-I,
which calls *INSPECTOR-OPENER* with INSTANCE (if INSTANCE is not NIL: the app may have tools), and
while it picks, Escape and the press that picks; then note where the outlines go, from the frame
before, whose boxes the ui still has. Called on the first build of a frame. Returns true if it took
any of the frame's input: then the frame builds again, and the other windows (the inspector's) are
drawn after it, as after any frame that took input."
  (let ((ui:*ui* ui)
        (taken nil))
    (when (and instance *inspector-opener* (widgets:shortcut #\i :command :option))
      (setf taken t)
      (funcall *inspector-opener* instance))
    (let ((inspection (ui:ui-inspection ui)))
      (when inspection
        (when (and (ui:inspection-picking inspection) (widgets:shortcut #\Escape))
          (setf taken t
                (ui:inspection-picking inspection) nil))
        (setf (ui:inspection-hover inspection) nil)
        (when (ui:inspection-picking inspection)
          (multiple-value-bind (box x y) (box-at ui (ui:ui-pointer-x ui) (ui:ui-pointer-y ui))
            (when box
              (setf (ui:inspection-hover inspection) (list x y (box:box-w box) (box:box-h box)))
              ;; The press picks, and the app does not have it.
              (when (ui:ui-pressed ui)
                (ui:consume-press ui)
                (setf taken t
                      (ui:inspection-picked inspection) (box:box-key box)
                      (ui:inspection-picking inspection) nil
                      (ui:inspection-hover inspection) nil)))))
        (multiple-value-bind (box x y) (if (zerop (ui:inspection-picked inspection))
                                           nil
                                           (box-by-key ui (ui:inspection-picked inspection)))
          (setf (ui:inspection-shown inspection) (and box (list x y (box:box-w box) (box:box-h box)))))))
    taken))

;;; --- what it describes after the app ---

(defun outline (ui id rect color)
  "A frame of COLOR round RECT (x, y, width, height in the window)."
  (when rect
    (destructuring-bind (x y w h) rect
      (multiple-value-bind (float open)
          (ui:open-float ui :outline hash:+seed+ id
                         (ui:make-float-spec :placement :at :x (float x 1f0) :y (float y 1f0) :tool t))
        (setf (box:box-pref-w float) (float w 1f0)
              (box:box-pref-h float) (float h 1f0)
              (box:box-color float) color)
        (ui:close-float ui open)))))

(defun describe-outlines (ui)
  "The outlines round what the pointer is over while the inspector picks, and round what it picked,
above everything else. Called after the app's function, the toasts and the tooltip."
  (let ((inspection (ui:ui-inspection ui)))
    (when inspection
      (let ((color (theme:theme-ring (ui:inspection-theme inspection))))
        (outline ui :inspector-hover (ui:inspection-hover inspection) color)
        (outline ui :inspector-picked (ui:inspection-shown inspection) color)))))
