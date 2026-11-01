(defpackage #:almighty-bolt-theme-designer
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:export #:make-designer
           #:open-designer
           #:theme-source))

(in-package #:almighty-bolt-theme-designer)

;;; A theme designer: the settings of a theme on the left, and on the right some of every kind of
;;; widget drawn in it, as they are changed. A theme starts as the dark or the light one, its three
;;; seeds (the background, the text and the accent) are picked by hue, saturation and lightness or
;;; typed as #rrggbb, and any colour worked out from them can be given instead. It says how readable
;;; the text is, and writes the theme down as a DEFINE-THEME form, to copy or to save in a file.
;;; It uses the public package only.
;;;
;;;   (almighty-bolt-theme-designer:open-designer)

;;; --- colours as hue, saturation and lightness ---

(defun rgb-to-hsl (color)
  "The hue (0 to 360), saturation and lightness (0 to 100) of the packed COLOR."
  (let* ((r (/ (ldb (byte 8 24) color) 255d0))
         (g (/ (ldb (byte 8 16) color) 255d0))
         (b (/ (ldb (byte 8 8) color) 255d0))
         (high (max r g b)) (low (min r g b))
         (l (/ (+ high low) 2))
         (d (- high low)))
    (if (zerop d)
        (values 0d0 0d0 (* 100 l))
        (let ((s (/ d (- 1 (abs (- (* 2 l) 1)))))
              (h (cond ((= high r) (mod (/ (- g b) d) 6))
                       ((= high g) (+ (/ (- b r) d) 2))
                       (t (+ (/ (- r g) d) 4)))))
          (values (* 60 h) (* 100 s) (* 100 l))))))

(defun hsl-to-rgb (h s l)
  "The packed colour of hue H (0 to 360), saturation S and lightness L (0 to 100)."
  (let* ((s (/ s 100d0)) (l (/ l 100d0))
         (c (* (- 1 (abs (- (* 2 l) 1))) s))
         (h (/ (mod h 360d0) 60))
         (x (* c (- 1 (abs (- (mod h 2) 1)))))
         (m (- l (/ c 2))))
    (multiple-value-bind (r g b)
        (cond ((< h 1) (values c x 0)) ((< h 2) (values x c 0)) ((< h 3) (values 0 c x))
              ((< h 4) (values 0 x c)) ((< h 5) (values x 0 c)) (t (values c 0 x)))
      (flet ((byte-of (v) (max 0 (min 255 (round (* 255 (+ v m)))))))
        (bolt:pack-color (byte-of r) (byte-of g) (byte-of b))))))

;;; --- the design ---

(defparameter +bases+ '(:dark :light))

(defparameter +derived-settings+
  (remove-if (lambda (setting)
               (or (member setting bolt:+theme-seeds+)
                   (member setting '(:font-name :font-size :radius))))
             bolt:+theme-settings+)
  "The colours a theme works out from its seeds, which can be given instead.")

(defparameter +readability+
  '(("Text on the background" :text :background 4.5)
    ("Text on a panel" :text :panel 4.5)
    ("Dim text on a panel" :dim-text :panel 3)
    ("Words on a button" :button-text :button 4.5)
    ("Words on a chosen row" :button-text :row-selected 4.5)
    ("Text in a field" :text :field-fill 4.5)
    ("A tooltip" :tooltip-text :tooltip 4.5))
  "Pairs of a theme's colours that text is read in, and the contrast each needs: 4.5 for text, 3 for
text that is less important (WCAG's level AA).")

(defstruct (seed-editor (:constructor make-seed-editor (seed id hex hue saturation lightness)))
  "What the editor of one seed holds: the hue, saturation and lightness the sliders are at (kept,
so that a grey does not lose its hue), and what its field says, which can be a colour half typed."
  (seed :accent :read-only t)
  (id :accent :read-only t)
  (hex "" :type string)
  (hue 0d0) (saturation 0d0) (lightness 0d0))

(defun set-editor-color (editor color)
  (multiple-value-bind (h s l) (rgb-to-hsl color)
    (setf (seed-editor-hue editor) h
          (seed-editor-saturation editor) s
          (seed-editor-lightness editor) l
          (seed-editor-hex editor) (bolt:color-hex color))))

(defun editor-color (editor)
  (hsl-to-rgb (seed-editor-hue editor) (seed-editor-saturation editor)
              (seed-editor-lightness editor)))

(defun editors-of (base)
  "An editor for each seed, at BASE's colour."
  (loop :for seed :in bolt:+theme-seeds+
        :collect (let ((editor (make-seed-editor seed seed "" 0d0 0d0 0d0)))
                   (set-editor-color editor (bolt:theme-setting (bolt:find-theme base) seed))
                   editor)))

(defun theme-keyword (name)
  "The keyword a theme called NAME (what was typed) is defined as."
  (let ((clean (string-trim "-" (substitute-if #\- (lambda (c) (not (alphanumericp c)))
                                               (string-downcase name)))))
    (intern (string-upcase (if (string= clean "") "my-theme" clean)) :keyword)))

(defun design-theme (name base editors changed font-name font-size radius overrides)
  "The theme the design describes: BASE, with the seeds in CHANGED, the font, its size and the
radius where they are not BASE's, and the OVERRIDES (a property list of colours)."
  (let ((base-theme (bolt:find-theme base)))
    (apply #'bolt:make-theme
           :name (theme-keyword name) :base base
           (append (loop :for editor :in editors
                         :when (member (seed-editor-seed editor) changed)
                           :collect (seed-editor-seed editor)
                           :and :collect (editor-color editor))
                   (unless (string= font-name (bolt:theme-setting base-theme :font-name))
                     (list :font-name font-name))
                   (unless (= font-size (bolt:theme-setting base-theme :font-size))
                     (list :font-size font-size))
                   (unless (= radius (bolt:theme-setting base-theme :radius))
                     (list :radius radius))
                   overrides))))

(defun theme-source (theme)
  "THEME written down as the DEFINE-THEME form that defines it again, as text that reads back in any
package: a setting to a line."
  (destructuring-bind (operator name (&key base) &rest settings) (bolt:theme-form theme)
    (declare (ignore operator))
    (with-output-to-string (out)
      (let ((*print-case* :downcase))
        (format out "(almighty-bolt:define-theme ~s (:base ~s)" name base)
        (loop :for (key value) :on settings :by #'cddr
              :do (format out "~%  ~s ~s" key value))
        (format out ")")))))

;;; --- pieces of the screen ---

(defun swatch (color id)
  "A small square of COLOR."
  (bolt:canvas 28 22 (lambda (list context width height)
                       (declare (ignore context))
                       (bolt:push-rounded-rect list 0 0 width height 4
                                               (bolt:pack-color 128 128 128))
                       (bolt:push-rounded-rect list 1 1 (- width 2) (- height 2) 3 color))
               :id id))

(defmacro section ((title id) &body body)
  `(bolt:panel (:id ,id :all 14)
     (bolt:column (:gap 10)
       (bolt:label ,title :id :title :size 17)
       ,@body)))

(defun id (&rest parts)
  "A keyword made of PARTS, for the id of one of several alike things."
  (intern (string-upcase (format nil "~{~a~^-~}" parts)) :keyword))

(defun seed-section (editor)
  "The editor of a seed. Returns true if its colour changed."
  (let ((seed (seed-editor-seed editor))
        (changed nil))
    (bolt:column (:gap 6 :id seed)
      (bolt:row (:gap 8 :cross-align :center)
        (swatch (editor-color editor) (id seed "swatch"))
        (bolt:label (string-capitalize (string seed)) :id (id seed "name"))
        (multiple-value-bind (text edited)
            (bolt:text-field (seed-editor-hex editor) :id (id seed "hex") :width 110
                             :invalid (not (ignore-errors (bolt:parse-color
                                                           (seed-editor-hex editor)))))
          (when edited
            (setf (seed-editor-hex editor) text)
            (let ((color (ignore-errors (bolt:parse-color text))))
              (when color
                (multiple-value-bind (h s l) (rgb-to-hsl color)
                  (setf (seed-editor-hue editor) h (seed-editor-saturation editor) s
                        (seed-editor-lightness editor) l))
                (setf changed t))))))
      (flet ((slider (title value maximum part)
               ;; The new value if the person moved it, or NIL.
               (let ((result nil))
                 (bolt:row (:gap 10 :cross-align :center)
                   (multiple-value-bind (new moved)
                       (bolt:slider value 0 maximum :id (id seed part) :width 200)
                     (when moved
                       (setf changed t
                             result new)))
                   (bolt:label (format nil "~a ~d" title (round (or result value)))
                               :id (id seed part "name")))
                 result)))
        (let ((hue (slider "Hue" (seed-editor-hue editor) 360 "hue"))
              (saturation (slider "Saturation" (seed-editor-saturation editor) 100 "saturation"))
              (lightness (slider "Lightness" (seed-editor-lightness editor) 100 "lightness")))
          (when hue (setf (seed-editor-hue editor) (float hue 1d0)))
          (when saturation (setf (seed-editor-saturation editor) (float saturation 1d0)))
          (when lightness (setf (seed-editor-lightness editor) (float lightness 1d0)))
          (when (or hue saturation lightness)
            (setf (seed-editor-hex editor) (bolt:color-hex (editor-color editor)))))))
    changed))

(defun readability-section (theme)
  (section ("Readability" :readability)
    (loop :for (title foreground background need) :in +readability+
          :for i :from 0
          :do (let ((ratio (bolt:contrast-ratio (bolt:theme-setting theme foreground)
                                                (bolt:theme-setting theme background))))
                (bolt:label (format nil "~a: ~,1f, ~a" title ratio
                                    (if (>= ratio need) "easy to read" "too low"))
                            :id (id "contrast" i))))))

(defun preview (theme state)
  "Some of every kind of widget, in THEME. STATE is a vector of what they hold."
  (bolt:with-theme (theme)
    (bolt:padding (:id :preview :all 24)
      (bolt:column (:gap 16)
        (bolt:label "Preview" :id :preview-title :size 26)
        (bolt:paragraph "What the widgets look like in this theme. They work: press, type and drag."
                        :id :preview-intro)
        (bolt:panel (:id :preview-panel :all 16)
          (bolt:column (:gap 12)
            (bolt:row (:gap 10)
              (when (bolt:button "Primary" :id :sample-button)
                (incf (aref state 0)))
              (bolt:button "Disabled" :id :sample-disabled :enabled nil)
              (bolt:label (format nil "Pressed ~d times." (aref state 0)) :id :sample-presses))
            (setf (aref state 1) (bolt:text-field (aref state 1) :id :sample-field))
            (bolt:text-field "Not valid" :id :sample-invalid :invalid t)
            (bolt:text-field "Not enabled" :id :sample-field-disabled :enabled nil)
            (setf (aref state 2) (bolt:checkbox "A check box" (aref state 2) :id :sample-check))
            (setf (aref state 3) (bolt:switch "A switch" (aref state 3) :id :sample-switch))
            (setf (aref state 4) (bolt:choice '("Small" "Medium" "Large") (aref state 4)
                                              :id :sample-choice))
            (setf (aref state 5) (bolt:slider (aref state 5) 0 100 :id :sample-slider))
            (bolt:progress (/ (aref state 5) 100) :id :sample-progress)
            (setf (aref state 6) (bolt:dropdown '("Apple" "Banana" "Cherry") (aref state 6)
                                                :id :sample-dropdown))
            (setf (aref state 7) (bolt:list-box '("Apple" "Banana" "Cherry" "Damson" "Elderberry")
                                                (aref state 7) :id :sample-list :rows 4))
            (setf (aref state 8) (bolt:text-area (aref state 8) :id :sample-area :width 300
                                                                 :rows 3))))))))

(defun make-designer ()
  (bolt:state-let ((name "my-theme")
                   (base 0)
                   (editors (editors-of :dark))
                   (changed '())
                   (font-name "Helvetica")
                   (font-size 15)
                   (radius 6)
                   (overrides '())
                   (picked 0)
                   (override-hex "")
                   (saving nil)
                   (saved "Not saved yet.")
                   (samples (vector 0 "Some text" t nil 1 40d0 0 1
                                    (format nil "Several lines~%of text."))))
    ;; The theme, worked out from the design when it changes (and so not state: it is made again
    ;; with the code).
    (let ((theme nil))
      (lambda ()
        (unless theme
          (setf theme (design-theme name (nth base +bases+) editors changed font-name font-size
                                    radius overrides)))
        (let ((dirty nil))
          (bolt:row (:id :designer :fill t :cross-align :stretch)
            (bolt:scroll (:id :settings)
              (bolt:padding (:all 18)
                (bolt:column (:gap 16)
                  (bolt:label "Theme designer" :id :heading :size 24)
                  (section ("Theme" :theme)
                    (bolt:row (:gap 8 :cross-align :center)
                      (bolt:label "Name" :id :name-label)
                      (multiple-value-bind (text edited) (bolt:text-field name :id :name :width 200)
                        (when edited (setf name text dirty t))))
                    (bolt:row (:gap 8 :cross-align :center)
                      (bolt:label "Start from" :id :base-label)
                      (multiple-value-bind (index moved) (bolt:choice '("Dark" "Light") base :id :base)
                        (when moved
                          (let ((theme (bolt:find-theme (nth index +bases+))))
                            (setf base index
                                  editors (editors-of (nth index +bases+))
                                  changed '()
                                  overrides '()
                                  override-hex ""
                                  font-name (bolt:theme-setting theme :font-name)
                                  font-size (bolt:theme-setting theme :font-size)
                                  radius (bolt:theme-setting theme :radius)
                                  dirty t))))))
                  (section ("Colours" :colours)
                    (dolist (editor editors)
                      (when (seed-section editor)
                        (pushnew (seed-editor-seed editor) changed)
                        (setf dirty t))))
                  (section ("Font and corners" :font)
                    (bolt:row (:gap 8 :cross-align :center)
                      (bolt:label "Font" :id :font-label)
                      (multiple-value-bind (text edited) (bolt:text-field font-name :id :font-name)
                        (when edited (setf font-name text dirty t))))
                    (bolt:row (:gap 8 :cross-align :center)
                      (bolt:label (format nil "Size ~d" font-size) :id :size-label)
                      (multiple-value-bind (value moved) (bolt:slider font-size 10 24 :id :font-size)
                        (when moved (setf font-size (round value) dirty t))))
                    (bolt:row (:gap 8 :cross-align :center)
                      (bolt:label (format nil "Corners ~d" radius) :id :radius-label)
                      (multiple-value-bind (value moved) (bolt:slider radius 0 14 :id :radius)
                        (when moved (setf radius (round value) dirty t)))))
                  (section ("Other colours" :others)
                    (bolt:paragraph "Each is worked out from the colours above. Give one to use it instead."
                                    :id :others-note)
                    (let ((setting (nth picked +derived-settings+)))
                      (bolt:row (:gap 8 :cross-align :center)
                        (multiple-value-bind (index moved)
                            (bolt:dropdown (mapcar (lambda (s) (string-downcase (string s)))
                                                   +derived-settings+)
                                           picked :id :override-which :width 170)
                          (when moved
                            (setf picked index
                                  setting (nth index +derived-settings+)
                                  override-hex (bolt:color-hex (bolt:theme-setting theme setting)))))
                        (swatch (bolt:theme-setting theme setting) :override-swatch))
                      ;; Showing the colour the theme has, unless one is half typed.
                      (when (or (string= override-hex "")
                                (and (not (getf overrides setting))
                                     (ignore-errors (bolt:parse-color override-hex))))
                        (setf override-hex (bolt:color-hex (bolt:theme-setting theme setting))))
                      (bolt:row (:gap 8 :cross-align :center)
                        (multiple-value-bind (text edited)
                            (bolt:text-field override-hex :id :override-hex :width 110
                                                          :invalid (not (ignore-errors
                                                                         (bolt:parse-color override-hex))))
                          (when edited
                            (setf override-hex text)
                            (let ((color (ignore-errors (bolt:parse-color text))))
                              (when color
                                (setf (getf overrides setting) color dirty t)))))
                        (when (bolt:button "Use the worked-out one" :id :override-clear
                                           :enabled (and (getf overrides setting) t))
                          (remf overrides setting)
                          (setf dirty t
                                override-hex "")))))
                  (readability-section theme)
                  (section ("Definition" :definition)
                    (bolt:text-area (theme-source theme) :id :source :width 380 :rows 12)
                    (bolt:row (:gap 8 :cross-align :center)
                      (when (bolt:button "Save…" :id :save)
                        (setf saving t))
                      (bolt:label saved :id :saved))
                    (let ((path (bolt:save-file-dialog :id :save-panel :show saving :types '("lisp")
                                                       :name (format nil "~(~a~).lisp"
                                                                     (bolt:theme-name theme)))))
                      (setf saving nil)
                      (when path
                        (with-open-file (out path :direction :output :if-exists :supersede)
                          (format out ";;; A theme made with the theme designer of almighty-bolt.~%~a~%"
                                  (theme-source theme)))
                        (setf saved (format nil "Saved to ~a." path))))))))
            ;; The preview on the theme's background, all down the right.
            (bolt:panel (:id :preview-back :color (bolt:theme-setting theme :background) :radius 0
                         :flex 1)
              (bolt:scroll (:id :preview-page)
                (preview theme samples))))
          (when dirty
            (setf theme nil)
            (bolt:ask-for-frame)))))))

(defun open-designer ()
  "Open the theme designer in a window, from the REPL."
  (bolt:open-app 'make-designer :title "Theme designer" :width 1180 :height 800))
