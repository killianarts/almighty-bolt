(defpackage #:almighty-bolt/ui/theme
  (:use #:cl)
  (:import-from #:almighty-bolt/internals
                #:pack-color)
  (:import-from #:almighty-bolt/ui/hash
                #:mix
                #:hash-of
                #:+seed+)
  (:export #:theme
           #:theme-p
           #:make-theme
           #:define-theme
           #:find-theme
           #:theme-name
           #:theme-id
           #:theme-names
           #:+theme-settings+
           #:+theme-seeds+
           #:+theme-colors+
           #:+theme-sizes+
           #:theme-setting
           #:theme-color
           #:theme-form
           #:theme-background
           #:theme-panel
           #:theme-text
           #:theme-dim-text
           #:theme-button-text
           #:theme-accent
           #:theme-ring
           #:theme-field-border
           #:theme-field-fill
           #:theme-field-invalid
           #:theme-field-disabled
           #:theme-selection
           #:theme-caret
           #:theme-track
           #:theme-thumb
           #:theme-row-selected
           #:theme-row-hover
           #:theme-scrim
           #:theme-tooltip
           #:theme-tooltip-text
           #:theme-button
           #:theme-button-hover
           #:theme-button-pressed
           #:theme-button-disabled
           #:theme-font-name
           #:theme-font-size
           #:theme-radius
           #:theme-spacing
           #:spaced
           #:theme-button-padding-x
           #:theme-button-padding-y
           #:theme-field-padding-x
           #:theme-field-padding-y
           #:theme-row-height
           #:theme-menu-padding
           #:theme-popover-padding
           #:theme-popover-gap
           #:theme-dialog-padding
           #:theme-dialog-gap
           #:theme-option-gap
           #:theme-option-row-gap
           #:theme-control-height
           #:theme-slider-height
           #:theme-progress-height
           #:button-color
           #:parse-color
           #:color-hex
           #:mix-colors
           #:contrast-ratio
           #:*theme-defined-hook*
           #:*theme*))

(in-package #:almighty-bolt/ui/theme)

;;; How the widgets look. A theme is a value: its colours, the font and its size, how round the
;;; corners of a control are, and how much room there is in and round the controls. A theme is made
;;; from a few seeds (the background, the text and the accent), from which the other colours are
;;; worked out, and one more (the spacing) from which the sizes are; any of those can be given as well.
;;; Themes have names, and a part of a screen is drawn in the theme it was described in. A theme
;;; has an id, a hash of everything in it, worked out when it is made, so that what was drawn in
;;; one theme is not kept for another.

;;; --- colours ---

(defun parse-color (designator)
  "The packed colour of DESIGNATOR: a packed colour (an integer, as PACK-COLOR makes), or a string
\"#rrggbb\" or \"#rrggbbaa\"."
  (etypecase designator
    ((unsigned-byte 32) designator)
    (string
     (let ((digits (string-left-trim "#" designator)))
       (unless (and (member (length digits) '(6 8))
                    (every (lambda (c) (digit-char-p c 16)) digits))
         (error "~s is not a colour: write it as #rrggbb or #rrggbbaa." designator))
       (let ((value (parse-integer digits :radix 16)))
         (if (= (length digits) 6) (logior (ash value 8) 255) value))))))

(defun color-hex (color)
  "COLOR as \"#rrggbb\", or \"#rrggbbaa\" if it is not opaque."
  (if (= (ldb (byte 8 0) color) 255)
      (format nil "#~(~6,'0x~)" (ash color -8))
      (format nil "#~(~8,'0x~)" color)))

(defun channels (color)
  (values (ldb (byte 8 24) color) (ldb (byte 8 16) color) (ldb (byte 8 8) color)
          (ldb (byte 8 0) color)))

(defun mix-colors (a b fraction)
  "The colour FRACTION of the way from A to B (0 is A, 1 is B), alpha too."
  (multiple-value-bind (r1 g1 b1 a1) (channels a)
    (multiple-value-bind (r2 g2 b2 a2) (channels b)
      (flet ((between (x y) (round (+ x (* fraction (- y x))))))
        (pack-color (between r1 r2) (between g1 g2) (between b1 b2) (between a1 a2))))))

(defun with-alpha (color alpha)
  (dpb alpha (byte 8 0) color))

(defun luminance (color)
  "The relative luminance of COLOR, as WCAG defines it: 0 for black, 1 for white."
  (flet ((linear (channel)
           (let ((c (/ channel 255d0)))
             (if (<= c 0.04045d0) (/ c 12.92d0) (expt (/ (+ c 0.055d0) 1.055d0) 2.4d0)))))
    (multiple-value-bind (r g b) (channels color)
      (+ (* 0.2126d0 (linear r)) (* 0.7152d0 (linear g)) (* 0.0722d0 (linear b))))))

(defun contrast-ratio (a b)
  "The WCAG contrast ratio of the colours A and B, from 1 (the same) to 21 (black and white). Text
is easy to read at 4.5 and above, and large text at 3."
  (let ((la (luminance a)) (lb (luminance b)))
    (/ (+ (max la lb) 0.05d0) (+ (min la lb) 0.05d0))))

(defparameter +white+ (pack-color 255 255 255))
(defparameter +black+ (pack-color 0 0 0))
(defparameter +red+ (pack-color 210 60 70))

;;; --- the theme ---

(defparameter +theme-seeds+ '(:background :text :accent)
  "The colours a theme is worked out from.")

(defparameter +theme-settings+
  '(:background :text :accent
    :panel :dim-text :button-text :ring :field-border :field-fill :field-invalid :field-disabled
    :selection :caret :track :thumb :row-selected :row-hover :scrim :tooltip :tooltip-text
    :button :button-hover :button-pressed :button-disabled
    :font-name :font-size :radius
    :spacing
    :button-padding-x :button-padding-y :field-padding-x :field-padding-y :row-height :menu-padding
    :popover-padding :popover-gap :dialog-padding :dialog-gap :option-gap :option-row-gap
    :control-height :slider-height :progress-height)
  "Everything a theme sets, in order: the seeds, the colours worked out from them, the font, its
size and the corner radius, then the spacing and the sizes worked out from it.")

(defparameter +theme-colors+
  (subseq +theme-settings+ 0 (position :font-name +theme-settings+))
  "The settings that are colours: the seeds and the colours worked out from them.")

(defparameter +theme-sizes+
  '((:button-padding-x 14) (:button-padding-y 7) (:field-padding-x 8) (:field-padding-y 6)
    (:row-height 24) (:menu-padding 4) (:popover-padding 12) (:popover-gap 8)
    (:dialog-padding 20) (:dialog-gap 12) (:option-gap 20) (:option-row-gap 6)
    (:control-height 22) (:slider-height 24) (:progress-height 14))
  "The sizes a theme works out from its spacing, in points, and what each is at a spacing of 1:
- the padding of a button, a menu button and a dropdown, at the sides and at the top and bottom;
- the padding of a text field, a combo box and a text area, at the sides and at the top and bottom;
- the height of a row of a list box or a menu, and the padding round the rows of a menu;
- the padding inside a popover and a dialog, and the gap between what is in them;
- the gap between the options of a choice, side by side and one under another;
- the height of a checkbox, a switch and an option of a choice, of a slider, and of a progress bar.")

(defparameter +theme-defaults+ '(:font-name "Helvetica" :font-size 15 :radius 6 :spacing 1)
  "What a theme with no base is if it does not give these.")

(defstruct (theme (:constructor %make-theme) (:copier nil))
  (name nil :type symbol :read-only t)
  ;; The seeds.
  (background 0 :type (unsigned-byte 32) :read-only t)
  (text 0 :type (unsigned-byte 32) :read-only t)
  (accent 0 :type (unsigned-byte 32) :read-only t)
  ;; Worked out from the seeds unless given.
  (panel 0 :type (unsigned-byte 32) :read-only t)
  (dim-text 0 :type (unsigned-byte 32) :read-only t)
  (button-text 0 :type (unsigned-byte 32) :read-only t)
  ;; The ring round what has the keyboard.
  (ring 0 :type (unsigned-byte 32) :read-only t)
  (field-border 0 :type (unsigned-byte 32) :read-only t)
  (field-fill 0 :type (unsigned-byte 32) :read-only t)
  (field-invalid 0 :type (unsigned-byte 32) :read-only t)
  (field-disabled 0 :type (unsigned-byte 32) :read-only t)
  (selection 0 :type (unsigned-byte 32) :read-only t)
  (caret 0 :type (unsigned-byte 32) :read-only t)
  (track 0 :type (unsigned-byte 32) :read-only t)
  (thumb 0 :type (unsigned-byte 32) :read-only t)
  (row-selected 0 :type (unsigned-byte 32) :read-only t)
  (row-hover 0 :type (unsigned-byte 32) :read-only t)
  (scrim 0 :type (unsigned-byte 32) :read-only t)
  (tooltip 0 :type (unsigned-byte 32) :read-only t)
  (tooltip-text 0 :type (unsigned-byte 32) :read-only t)
  (button 0 :type (unsigned-byte 32) :read-only t)
  (button-hover 0 :type (unsigned-byte 32) :read-only t)
  (button-pressed 0 :type (unsigned-byte 32) :read-only t)
  (button-disabled 0 :type (unsigned-byte 32) :read-only t)
  (font-name "Helvetica" :type simple-string :read-only t)
  (font-size 15 :type (integer 1 200) :read-only t)
  (radius 6 :type (integer 0 100) :read-only t)
  ;; The seed of the sizes, and the sizes worked out from it (+THEME-SIZES+), in points.
  (spacing 1f0 :type single-float :read-only t)
  (button-padding-x 0f0 :type single-float :read-only t)
  (button-padding-y 0f0 :type single-float :read-only t)
  (field-padding-x 0f0 :type single-float :read-only t)
  (field-padding-y 0f0 :type single-float :read-only t)
  (row-height 0f0 :type single-float :read-only t)
  (menu-padding 0f0 :type single-float :read-only t)
  (popover-padding 0f0 :type single-float :read-only t)
  (popover-gap 0f0 :type single-float :read-only t)
  (dialog-padding 0f0 :type single-float :read-only t)
  (dialog-gap 0f0 :type single-float :read-only t)
  (option-gap 0f0 :type single-float :read-only t)
  (option-row-gap 0f0 :type single-float :read-only t)
  (control-height 0f0 :type single-float :read-only t)
  (slider-height 0f0 :type single-float :read-only t)
  (progress-height 0f0 :type single-float :read-only t)
  ;; What was given rather than worked out, for writing the theme down again (THEME-FORM).
  (given '() :type list :read-only t)
  ;; A hash of everything above, for the hashes of the boxes drawn in it.
  (id 0 :type (unsigned-byte 62) :read-only t))

(defmethod print-object ((theme theme) stream)
  (print-unreadable-object (theme stream :type t)
    (format stream "~s" (theme-name theme))))

(defun theme-setting (theme setting)
  "The value of SETTING (one of +THEME-SETTINGS+) in THEME."
  (ecase setting
    (:background (theme-background theme)) (:text (theme-text theme))
    (:accent (theme-accent theme)) (:panel (theme-panel theme))
    (:dim-text (theme-dim-text theme)) (:button-text (theme-button-text theme))
    (:ring (theme-ring theme)) (:field-border (theme-field-border theme))
    (:field-fill (theme-field-fill theme)) (:field-invalid (theme-field-invalid theme))
    (:field-disabled (theme-field-disabled theme)) (:selection (theme-selection theme))
    (:caret (theme-caret theme)) (:track (theme-track theme)) (:thumb (theme-thumb theme))
    (:row-selected (theme-row-selected theme)) (:row-hover (theme-row-hover theme))
    (:scrim (theme-scrim theme)) (:tooltip (theme-tooltip theme))
    (:tooltip-text (theme-tooltip-text theme)) (:button (theme-button theme))
    (:button-hover (theme-button-hover theme)) (:button-pressed (theme-button-pressed theme))
    (:button-disabled (theme-button-disabled theme)) (:font-name (theme-font-name theme))
    (:font-size (theme-font-size theme)) (:radius (theme-radius theme))
    (:spacing (theme-spacing theme))
    (:button-padding-x (theme-button-padding-x theme))
    (:button-padding-y (theme-button-padding-y theme))
    (:field-padding-x (theme-field-padding-x theme))
    (:field-padding-y (theme-field-padding-y theme))
    (:row-height (theme-row-height theme)) (:menu-padding (theme-menu-padding theme))
    (:popover-padding (theme-popover-padding theme)) (:popover-gap (theme-popover-gap theme))
    (:dialog-padding (theme-dialog-padding theme)) (:dialog-gap (theme-dialog-gap theme))
    (:option-gap (theme-option-gap theme)) (:option-row-gap (theme-option-row-gap theme))
    (:control-height (theme-control-height theme))
    (:slider-height (theme-slider-height theme))
    (:progress-height (theme-progress-height theme))))

(defparameter +derived-from+
  '((:panel :background :text) (:dim-text :background :text) (:button-text :accent)
    (:ring :background :text :accent) (:field-border :background :text)
    (:field-fill :background :text) (:field-invalid :background)
    (:field-disabled :background :text) (:selection :accent) (:caret :text)
    (:track :background :text) (:thumb :accent) (:row-selected :accent)
    (:row-hover :background :text) (:scrim :background :text) (:tooltip :text)
    (:tooltip-text :background) (:button :accent) (:button-hover :accent)
    (:button-pressed :accent) (:button-disabled :background :text))
  "Each colour worked out from the seeds, and the seeds it is worked out from: a theme that gives a
seed works out again the colours that come from it, and keeps its base's others.")

(defun derive (background text accent)
  "The colours worked out from the seeds BACKGROUND, TEXT and ACCENT, as a property list. (Whether
the theme is dark, which some depend on, is from the background and the text.)"
  (let ((dark (< (luminance background) (luminance text))))
    (list :panel (if dark (mix-colors background text 0.07) (mix-colors background +white+ 0.75))
          :dim-text (mix-colors text background 0.45)
          :button-text (if (>= (contrast-ratio +white+ accent) (contrast-ratio +black+ accent))
                           +white+ +black+)
          :ring (if dark (mix-colors accent +white+ 0.5) (mix-colors accent background 0.3))
          :field-border (mix-colors background text 0.3)
          :field-fill (if dark (mix-colors background +black+ 0.33) (mix-colors background +white+ 0.8))
          :field-invalid (mix-colors background +red+ 0.35)
          :field-disabled (mix-colors background text 0.03)
          :selection (with-alpha accent 160)
          :caret text
          :track (if dark (mix-colors background +black+ 0.33) (mix-colors background text 0.15))
          :thumb (mix-colors accent +white+ 0.25)
          :row-selected accent
          :row-hover (mix-colors background text 0.12)
          :scrim (with-alpha +black+ (if dark 130 90))
          :tooltip text
          :tooltip-text background
          :button accent
          :button-hover (mix-colors accent +white+ 0.12)
          :button-pressed (mix-colors accent +black+ 0.18)
          :button-disabled (mix-colors background text (if dark 0.15 0.08)))))

(defun derive-sizes (spacing)
  "The sizes worked out from SPACING, as a property list: each of +THEME-SIZES+ times SPACING, to
the nearest whole point (a half up)."
  (loop :for (key size) :in +theme-sizes+
        :collect key :collect (float (floor (+ (* spacing size) 1/2)) 1f0)))

(defun size-value (key value)
  "VALUE, given as the setting KEY (the spacing or a size), as a theme keeps it."
  (unless (and (realp value) (if (eq key :spacing) (plusp value) (not (minusp value))))
    (error "~s is ~s, and it must be a number~:[ of points, 0 or more~; more than 0~]." key value
           (eq key :spacing)))
  (float value 1f0))

(defun theme-hash (values name)
  (let ((hash (mix +seed+ (hash-of name))))
    (dolist (value values hash)
      (setf hash (mix hash (hash-of value))))))

(defvar *themes* (make-hash-table :test 'eq #+sbcl :synchronized #+sbcl t)
  "The themes that have names, by name. The window's thread reads it every frame while a REPL may
define a theme, so it is synchronized.")

(defun find-theme (designator)
  "The theme DESIGNATOR: a theme, or the name of one (:dark, :light, or one made by DEFINE-THEME)."
  (if (theme-p designator)
      designator
      (or (gethash designator *themes*)
          (error "There is no theme called ~s. The themes are ~{~s~^, ~}." designator
                 (theme-names)))))

(defun theme-names ()
  "The names of the themes there are."
  (sort (loop :for name :being :the :hash-keys :of *themes* :collect name) #'string<))

(defun make-theme (&rest settings &key name (base :dark) &allow-other-keys)
  "A theme like BASE (a theme or its name; :dark if not given) with SETTINGS, which are any of
+THEME-SETTINGS+ (colours as packed colours or \"#rrggbb\" strings, sizes as numbers of points). If
a seed (the background, the text or the accent) is given, the colours that are worked out from it
are worked out again (see +DERIVED-FROM+), except those given too; if the spacing is given, so are
the sizes (+THEME-SIZES+), except those given too. The others are BASE's."
  (let* ((base (and base (find-theme base)))
         (given (loop :for (key value) :on settings :by #'cddr
                      :unless (member key '(:name :base))
                        :do (unless (member key +theme-settings+)
                              (error "~s is not something a theme sets. A theme sets ~{~s~^, ~}."
                                     key +theme-settings+))
                        :and :collect key :and :collect value))
         (seeds-given (remove-if-not (lambda (seed) (getf given seed)) +theme-seeds+)))
    (flet ((seed (key)
             (let ((value (getf given key)))
               (cond (value (parse-color value))
                     (base (theme-setting base key))
                     (t (error "A theme with no base needs ~s." key)))))
           (plain (key)
             (let ((value (getf given key)))
               (cond (value value)
                     (base (theme-setting base key))
                     (t (getf +theme-defaults+ key))))))
      (let* ((background (seed :background)) (text (seed :text)) (accent (seed :accent))
             (derived (derive background text accent))
             (spacing (size-value :spacing (plain :spacing)))
             (sizes (derive-sizes spacing))
             (values
               (loop :for key :in +theme-settings+
                     :collect (let ((value (getf given key)))
                                (cond ((member key +theme-seeds+) (seed key))
                                      ((eq key :font-name) (coerce (plain key) 'simple-string))
                                      ((member key '(:font-size :radius)) (plain key))
                                      ((eq key :spacing) spacing)
                                      ((assoc key +theme-sizes+)
                                       (cond (value (size-value key value))
                                             ;; Worked out again if the spacing is given.
                                             ((or (null base) (getf given :spacing))
                                              (getf sizes key))
                                             (t (theme-setting base key))))
                                      (value (parse-color value))
                                      ;; Worked out again if a seed it comes from is given.
                                      ((or (null base)
                                           (intersection seeds-given
                                                         (rest (assoc key +derived-from+))))
                                       (getf derived key))
                                      (t (theme-setting base key)))))))
        (apply #'%make-theme
               :name name
               :given (append (and base (list :base (theme-name base))) given)
               :id (theme-hash values name)
               (loop :for key :in +theme-settings+
                     :for value :in values
                     :collect key :collect value))))))

(defvar *theme-defined-hook* nil
  "A function called with no arguments after DEFINE-THEME has defined a theme, or NIL: the host sets
it so that an open window shows a theme changed in the editor.")

(defun register-theme (theme)
  (setf (gethash (theme-name theme) *themes*) theme)
  (when *theme-defined-hook* (funcall *theme-defined-hook*))
  (theme-name theme))

(defmacro define-theme (name (&key (base :dark)) &body settings)
  "Define the theme called NAME (a keyword), like BASE (:dark if not given) with SETTINGS, as
MAKE-THEME takes them. Defining it again changes it, and a window that shows it shows the change."
  `(register-theme (make-theme :name ,name :base ,base ,@settings)))

(defun theme-form (theme)
  "A DEFINE-THEME form that defines THEME again: its base and what was given rather than worked
out, with colours as \"#rrggbb\" strings."
  (let ((given (theme-given theme)))
    `(define-theme ,(theme-name theme) (:base ,(getf given :base))
       ,@(loop :for (key value) :on given :by #'cddr
               :unless (eq key :base)
                 :collect key
                 :and :collect (if (member key +theme-colors+)
                                   (color-hex (parse-color value))
                                   value)))))

(defun button-color (theme state)
  "The colour of a button of THEME that is STATE (:normal, :hover, :pressed or :disabled)."
  (ecase state
    (:normal (theme-button theme))
    (:hover (theme-button-hover theme))
    (:pressed (theme-button-pressed theme))
    (:disabled (theme-button-disabled theme))))

;;; --- the themes that come with the library ---

;;; The dark theme is every colour given, as it was before themes could be made.
(define-theme :dark (:base nil)
  :background (pack-color 36 40 56)
  :text (pack-color 232 234 242)
  :accent (pack-color 72 104 200)
  :panel (pack-color 48 53 74)
  :dim-text (pack-color 130 136 156)
  :button-text (pack-color 255 255 255)
  :ring (pack-color 150 200 255)
  :field-border (pack-color 86 92 118)
  :field-fill (pack-color 24 27 40)
  :field-invalid (pack-color 96 34 40)
  :field-disabled (pack-color 40 43 58)
  :selection (pack-color 72 104 200 160)
  :caret (pack-color 240 242 250)
  :track (pack-color 24 27 40)
  :thumb (pack-color 110 150 235)
  :row-selected (pack-color 72 104 200)
  :row-hover (pack-color 58 64 90)
  :scrim (pack-color 0 0 0 130)
  :tooltip (pack-color 232 234 240)
  :tooltip-text (pack-color 30 33 46)
  :button (pack-color 72 104 200)
  :button-hover (pack-color 92 126 224)
  :button-pressed (pack-color 52 80 164)
  :button-disabled (pack-color 62 66 86)
  :font-name "Helvetica"
  :font-size 15
  :radius 6)

;;; The light theme is its seeds, and the rest worked out.
(define-theme :light (:base :dark)
  :background (pack-color 240 241 245)
  :text (pack-color 28 30 38)
  :accent (pack-color 40 96 210))

(defvar *theme* (find-theme :dark)
  "The theme that what is described, laid out or drawn now is in: bound around a frame's
description to the window's, by WITH-THEME to another, and to a box's own while it is laid out
and drawn.")

(defun theme-color (designator &optional (theme *theme*))
  "The packed colour of DESIGNATOR in THEME (the one being described in, by default):
  - a packed colour, or a string \"#rrggbb\" or \"#rrggbbaa\", which is itself;
  - a colour setting of the theme, :accent, :dim-text, :panel ... (+THEME-COLORS+);
  - (DESIGNATOR :alpha FRACTION), a colour with that much of it showing, 0 to 1;
  - (:mix A B FRACTION), the colour FRACTION of the way from A to B, each a designator.
NIL is NIL: the widget's own colour from the theme. A colour given as a setting follows the theme,
so it suits a dark window and a light one, and what WITH-THEME draws."
  (etypecase designator
    (null nil)
    ((or (unsigned-byte 32) string) (parse-color designator))
    (keyword (theme-setting theme designator))
    (cons (if (eq (first designator) :mix)
              (destructuring-bind (a b fraction) (rest designator)
                (mix-colors (theme-color a theme) (theme-color b theme) fraction))
              (destructuring-bind (color &key (alpha 1)) designator
                (with-alpha (theme-color color theme)
                            (max 0 (min 255 (round (* 255 alpha))))))))))

(defun spaced (size &optional (theme *theme*))
  "SIZE points (at a spacing of 1) in THEME's spacing, to the nearest whole point: for the room in
and round what has no size of its own in a theme, such as a toast or a tooltip."
  (float (floor (+ (* (theme-spacing theme) size) 1/2)) 1f0))
