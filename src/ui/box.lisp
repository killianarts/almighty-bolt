(defpackage #:almighty-bolt-variant-immediate/box
  (:use #:cl)
  (:export #:box
           #:make-box
           #:box-key
           #:box-id
           #:box-kind
           #:box-parent
           #:box-children
           #:box-text
           #:box-size
           #:box-color
           #:box-flex
           #:box-gap
           #:box-main-align
           #:box-cross-align
           #:box-fill
           #:box-pad-left
           #:box-pad-top
           #:box-pad-right
           #:box-pad-bottom
           #:box-pref-w
           #:box-pref-h
           #:box-enabled
           #:box-look
           #:box-n1
           #:box-n2
           #:box-n3
           #:box-data
           #:box-interactive
           #:box-focusable
           #:box-moves
           #:box-x
           #:box-y
           #:box-w
           #:box-h
           #:box-layout-hash
           #:box-paint-hash
           #:reset-box))

(in-package #:almighty-bolt-variant-immediate/box)

;;; A box is one thing the interface described this frame: a kind, what it holds, how it is
;;; to be sized, and, after layout, where it is. Boxes are made once and reused from one
;;; frame to the next, so describing a frame allocates nothing for them, and they are in the
;;; order the frame described them, which is the order they are drawn in.

(defstruct (box (:constructor make-box ()))
  (key 0 :type fixnum)
  (id nil)
  (kind :none :type keyword)
  (parent nil)
  (children (make-array 4 :adjustable t :fill-pointer 0))
  ;; What it holds.
  (text "" :type string)
  (size 15 :type fixnum)
  (color 0 :type (unsigned-byte 32))
  (flex 0f0 :type single-float)
  (gap 0f0 :type single-float)
  (main-align :start :type keyword)
  (cross-align :start :type keyword)
  (fill nil)
  (pad-left 0f0 :type single-float)
  (pad-top 0f0 :type single-float)
  (pad-right 0f0 :type single-float)
  (pad-bottom 0f0 :type single-float)
  (pref-w 0f0 :type single-float)
  (pref-h 0f0 :type single-float)
  (enabled t)
  ;; How it looks to the pointer and keyboard, a small number: 0 plain, 1 hovered, 2 pressed,
  ;; 3 disabled, and 4 added if it has the keyboard.
  (look 0 :type fixnum)
  ;; Numbers and an object for the widget to use (a fraction, a caret, a list of items).
  (n1 0f0 :type single-float)
  (n2 0f0 :type single-float)
  (n3 0 :type fixnum)
  (data nil)
  (interactive nil)
  (focusable nil)
  ;; Whether it wants to be told of the pointer moving over it.
  (moves nil)
  ;; Where it is in its parent and how large, from layout.
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (w 0f0 :type single-float)
  (h 0f0 :type single-float)
  (layout-hash 0 :type (unsigned-byte 62))
  (paint-hash 0 :type (unsigned-byte 62)))

(defun reset-box (box kind)
  "Make BOX ready to describe a new thing of KIND: everything but its storage is as it was
made."
  (setf (box-kind box) kind
        (box-id box) nil
        (box-parent box) nil
        (fill-pointer (box-children box)) 0
        (box-text box) ""
        (box-size box) 15
        (box-color box) 0
        (box-flex box) 0f0
        (box-gap box) 0f0
        (box-main-align box) :start
        (box-cross-align box) :start
        (box-fill box) nil
        (box-pad-left box) 0f0
        (box-pad-top box) 0f0
        (box-pad-right box) 0f0
        (box-pad-bottom box) 0f0
        (box-pref-w box) 0f0
        (box-pref-h box) 0f0
        (box-enabled box) t
        (box-look box) 0
        (box-n1 box) 0f0
        (box-n2 box) 0f0
        (box-n3 box) 0
        (box-data box) nil
        (box-interactive box) nil
        (box-focusable box) nil
        (box-moves box) nil
        (box-x box) 0f0
        (box-y box) 0f0
        (box-w box) 0f0
        (box-h box) 0f0)
  box)
