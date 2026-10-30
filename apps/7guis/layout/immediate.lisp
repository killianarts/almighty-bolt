(defpackage #:almighty-bolt-7guis/layout/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/layout/text
                #:+sidebar-text+
                #:+top-text+
                #:+small-text+
                #:+margin+
                #:+gap+)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:paragraph
                #:padding
                #:panel
                #:column
                #:row)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant)
  (:export #:make-layout))

(in-package #:almighty-bolt-7guis/layout/immediate)

;;; The layout spike, immediate: the function describes the screen with the parents opened
;;; before their children, and layout runs afterwards over the boxes it made, so a pane's
;;; paragraph is told its width and answers with its height in the same frame, with no extra
;;; build and no lag.

(defun make-layout ()
  (make-immediate-variant
   (lambda ()
     (padding (:all +margin+)
       (row (:gap +gap+ :cross-align :stretch :fill t)
         (panel (:all +gap+ :flex 1 :id :sidebar)
           (column (:gap +gap+)
             (label "Sidebar" :id :sidebar-title)
             (paragraph +sidebar-text+ :id :sidebar-text)))
         (column (:gap +gap+ :flex 2 :cross-align :stretch)
           (panel (:all +gap+ :flex 1 :id :top)
             (column (:gap +gap+)
               (label "Top" :id :top-title)
               (paragraph +top-text+ :id :top-text)))
           (row (:gap +gap+ :flex 1 :cross-align :stretch)
             (panel (:all +gap+ :flex 1 :id :bottom-left)
               (paragraph +small-text+ :id :left-text))
             (panel (:all +gap+ :flex 1 :id :bottom-right)
               (paragraph +small-text+ :id :right-text)))))))))

(register-app :immediate :layout 'make-layout :width 640 :height 420)
