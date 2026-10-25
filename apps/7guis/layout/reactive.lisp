(defpackage #:almighty-bolt-7guis/layout/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/layout/text
                #:+sidebar-text+
                #:+top-text+
                #:+small-text+
                #:+margin+
                #:+gap+)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:label
                #:paragraph
                #:padding
                #:panel
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-reactive/reactive
                #:make-reactive-variant)
  (:export #:make-layout))

(in-package #:almighty-bolt-7guis/layout/reactive)

;;; The layout spike, reactive: a description of the screen with no state in it, so it is
;;; described once. What the window's width does is the retained nodes' layout, as in the
;;; retained version.

(defun make-layout ()
  (make-reactive-variant
   (lambda ()
     (padding (row (panel (column (label "Sidebar" :id :sidebar-title)
                                  (paragraph +sidebar-text+ :id :sidebar-text)
                                  :gap +gap+)
                          :all +gap+ :flex 1 :id :sidebar)
                   (column (panel (column (label "Top" :id :top-title)
                                          (paragraph +top-text+ :id :top-text)
                                          :gap +gap+)
                                  :all +gap+ :flex 1 :id :top)
                           (row (panel (paragraph +small-text+ :id :left-text)
                                       :all +gap+ :flex 1 :id :bottom-left)
                                (panel (paragraph +small-text+ :id :right-text)
                                       :all +gap+ :flex 1 :id :bottom-right)
                                :gap +gap+ :flex 1 :cross-align :stretch)
                           :gap +gap+ :flex 2 :cross-align :stretch)
                   :gap +gap+ :cross-align :stretch :fill t)
              :all +margin+))))

(register-app :reactive :layout #'make-layout :width 640 :height 420)
