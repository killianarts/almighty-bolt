(defpackage #:almighty-bolt-7guis/layout/text
  (:use #:cl)
  (:export #:+sidebar-text+
           #:+top-text+
           #:+small-text+
           #:+margin+
           #:+gap+))

(in-package #:almighty-bolt-7guis/layout/text)

;;; What the layout screen shows: three paragraphs that are
;;; broken into lines to the width of the pane they are in, so that the height of a pane
;;; depends on its width, which depends on the window.

(defparameter +sidebar-text+
  "The sidebar holds a paragraph that is broken into lines to the width of the sidebar, so that it is taller when the window is narrow and shorter when it is wide.")

(defparameter +top-text+
  "The top pane is the larger part of the right side. Its paragraph is longer than the others, and what it needs from its parent is a width to break the text for.")

(defparameter +small-text+
  "Two small panes share the bottom, each with a few words.")

(defconstant +margin+ 12)
(defconstant +gap+ 8)
