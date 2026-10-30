(defpackage #:almighty-bolt-7guis/dialog/text
  (:use #:cl)
  (:export #:+header+
           #:+close+
           #:+title+
           #:+labels+
           #:+values+
           #:+body+
           #:+status+
           #:+margin+
           #:+gap+))

(in-package #:almighty-bolt-7guis/dialog/text)

;;; The dialog screen: a header with something at its right edge, a card in the middle of the window that is as wide
;;; and as tall as what is in it, with labels lined up to the right against their values, and
;;; a line at the bottom. What decides where the card goes is its size, and what decides its
;;; size is the paragraph, which is broken to the width of the window.

(defparameter +header+ "Settings")
(defparameter +close+ "Close")
(defparameter +title+ "Rename the file")
(defparameter +labels+ '("Name" "Folder" "Description"))
(defparameter +values+ '("draft.txt" "Documents" "none yet"))

(defparameter +body+
  "A card that is as wide as its widest line and as tall as its lines, in the middle of the window. Where it goes depends on its size, and its size depends on the width of the window.")

(defparameter +status+ "Resize the window; the card stays in the middle.")

(defconstant +margin+ 12)
(defconstant +gap+ 8)
