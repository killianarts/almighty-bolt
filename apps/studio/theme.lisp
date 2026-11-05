(defpackage #:almighty-bolt-studio/theme
  (:use #:cl)
  (:local-nicknames (#:bolt #:almighty-bolt))
  (:export #:studio-theme
           #:tool-color
           #:+popover-shadow+))

(in-package #:almighty-bolt-studio/theme)

;;; The studio's own theme, in a light and a dark, imported once from the design system of the
;;; prototype in Figma (its "theme" variables, in their Light and Dark modes, and its text styles)
;;; and the studio's from then on: nothing is kept in step with Figma.
;;;
;;; It is grey, black and white: white or black behind, panels a step off it, lines a step further,
;;; and what is chosen, the buttons and the ring in the text's colour. The sizes are the library's
;;; own, but for corners of 4. The fonts are the system's (San Francisco, and its monospaced
;;; design for labels and code) at the prototype's sizes, weights, tracking and line heights, in
;;; place of its families, which are not on every Mac.

(bolt:define-theme :studio-light (:base :light)
  :background "#ffffff"
  :text "#000000"
  :accent "#000000"
  :panel "#f2f2f2"
  :dim-text "#6c6c6c"
  :button-text "#ffffff"
  :ring "#000000"
  :field-border "#c3c3c3"
  :field-fill "#ffffff"
  :field-invalid "#e5484d"
  :field-disabled "#f2f2f2"
  :selection "#d2d2d2"
  :caret "#000000"
  :track "#e4e4e4"
  :thumb "#000000"
  :row-selected "#000000"
  :row-hover "#e4e4e4"
  ;; The prototype's scrim is black; how much shows is the library's.
  :scrim "#0000005a"
  :tooltip "#000000"
  :tooltip-text "#ffffff"
  :button "#000000"
  :button-hover "#3a3a3a"
  :button-pressed "#565656"
  :button-disabled "#d2d2d2"
  :font-name "System"
  :font-size 15
  :label-font '("System Mono" :size 11 :tracking 1.5 :line-height 14)
  :heading-font '("System" :weight :medium :size 20 :tracking -0.1 :line-height 24)
  :code-font '("System Mono" :size 13 :line-height 20)
  :button-font '("System" :weight :medium :size 16 :tracking 0.8)
  :radius 4)

(bolt:define-theme :studio-dark (:base :dark)
  :background "#000000"
  :text "#ffffff"
  :accent "#ffffff"
  :panel "#1e1e1e"
  :dim-text "#8f8f8f"
  :button-text "#000000"
  :ring "#ffffff"
  :field-border "#565656"
  :field-fill "#131313"
  :field-invalid "#e5484d"
  :field-disabled "#1e1e1e"
  :selection "#565656"
  :caret "#ffffff"
  :track "#2b2b2b"
  :thumb "#ffffff"
  :row-selected "#ffffff"
  :row-hover "#2b2b2b"
  :scrim "#00000082"
  :tooltip "#ffffff"
  :tooltip-text "#000000"
  :button "#ffffff"
  :button-hover "#e4e4e4"
  :button-pressed "#c3c3c3"
  :button-disabled "#3a3a3a"
  :font-name "System"
  :font-size 15
  :label-font '("System Mono" :size 11 :tracking 1.5 :line-height 14)
  :heading-font '("System" :weight :medium :size 20 :tracking -0.1 :line-height 24)
  :code-font '("System Mono" :size 13 :line-height 20)
  :button-font '("System" :weight :medium :size 16 :tracking 0.8)
  :radius 4)

(defun studio-theme ()
  "The studio's theme for the window it is drawn in: its light one, or its dark one in a dark
window."
  (if (eq (bolt:appearance) :dark) :studio-dark :studio-light))

(defparameter +tool-colors+
  '(:light (:line "#e4e4e4" :line-strong "#b4b4b4" :grid "#d2d2d2" :raised "#ffffff"
            :ok "#3dd68c" :warn "#ffc53d" :danger "#e5484d")
    :dark (:line "#2b2b2b" :line-strong "#565656" :grid "#3a3a3a" :raised "#131313"
           :ok "#3dd68c" :warn "#ffc53d" :danger "#e5484d"))
  "The colours of the studio's own chrome that a theme has no setting for, in the light and in the
dark: lines between parts and a stronger one, the canvas's grid, what is raised off a panel, and what
is well, a warning and what is wrong.")

(defun tool-color (name)
  "The studio's colour NAME (:line, :line-strong, :grid, :raised, :ok, :warn or :danger) for the
window it is drawn in, as \"#rrggbb\"."
  (or (getf (getf +tool-colors+ (bolt:appearance)) name)
      (error "The studio has no colour ~s; it has ~{~s~^, ~}." name
             (loop :for (key) :on (getf +tool-colors+ :light) :by #'cddr :collect key))))

(defparameter +popover-shadow+ '(:y 8 :blur 24 :color "#00000073")
  "The shadow of what floats over the studio (the prototype's elevation/popover), as :shadow takes
it.")
