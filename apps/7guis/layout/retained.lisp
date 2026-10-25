(defpackage #:almighty-bolt-7guis/layout/retained
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/layout/text
                #:+sidebar-text+
                #:+top-text+
                #:+small-text+
                #:+margin+
                #:+gap+)
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:paragraph
                #:padding
                #:panel
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-layout))

(in-package #:almighty-bolt-7guis/layout/retained)

;;; The layout spike, retained: a tree of nodes made once. A split is a row or a column whose
;;; children have flex factors; the paragraphs are told the width of their pane and answer with
;;; the height of their lines.

(defun make-layout ()
  (make-retained-variant
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
            :all +margin+)))

(register-app :retained :layout #'make-layout :width 640 :height 420)
