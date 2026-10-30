(defpackage #:almighty-bolt-variant-immediate/theme
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:pack-color)
  (:export #:+background+
           #:+panel-color+
           #:+text-color+
           #:+dim-text-color+
           #:+button-text-color+
           #:+accent-color+
           #:+field-border-color+
           #:+field-fill-color+
           #:+field-invalid-color+
           #:+field-disabled-color+
           #:+selection-color+
           #:+caret-color+
           #:+track-color+
           #:+thumb-color+
           #:+row-selected-color+
           #:+row-hover-color+
           #:+scrim-color+
           #:+font-name+
           #:+font-size+
           #:+control-radius+
           #:button-color))

(in-package #:almighty-bolt-variant-immediate/theme)

;;; How the widgets look: a dark theme, with one blue.

(defparameter +background+ (pack-color 36 40 56))
(defparameter +panel-color+ (pack-color 48 53 74))
(defparameter +text-color+ (pack-color 232 234 242))
(defparameter +dim-text-color+ (pack-color 130 136 156))
(defparameter +button-text-color+ (pack-color 255 255 255))
(defparameter +accent-color+ (pack-color 150 200 255))
(defparameter +field-border-color+ (pack-color 86 92 118))
(defparameter +field-fill-color+ (pack-color 24 27 40))
(defparameter +field-invalid-color+ (pack-color 96 34 40))
(defparameter +field-disabled-color+ (pack-color 40 43 58))
(defparameter +selection-color+ (pack-color 72 104 200 160))
(defparameter +caret-color+ (pack-color 240 242 250))
(defparameter +track-color+ (pack-color 24 27 40))
(defparameter +thumb-color+ (pack-color 110 150 235))
(defparameter +row-selected-color+ (pack-color 72 104 200))
(defparameter +row-hover-color+ (pack-color 58 64 90))
(defparameter +scrim-color+ (pack-color 0 0 0 130))
(defparameter +font-name+ "Helvetica")
(defparameter +font-size+ 15)
(defconstant +control-radius+ 6)

(defparameter *button-colors*
  (list :normal (pack-color 72 104 200)
        :hover (pack-color 92 126 224)
        :pressed (pack-color 52 80 164)
        :disabled (pack-color 62 66 86)))

(defun button-color (state)
  (getf *button-colors* state))
