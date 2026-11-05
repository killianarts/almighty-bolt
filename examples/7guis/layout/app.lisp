(defpackage #:almighty-bolt-7guis/layout/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:text #:almighty-bolt-7guis/layout/text)
                    (#:bolt #:almighty-bolt))
  (:export #:make-layout))

(in-package #:almighty-bolt-7guis/layout/app)

;;; The layout screen: the function describes the screen with the parents opened
;;; before their children, and layout runs afterwards over the boxes it made, so a pane's
;;; paragraph is told its width and answers with its height in the same frame, with no extra
;;; build and no lag.

(defun make-layout ()
  (lambda ()
    (bolt:padding (:all text:+margin+)
      (bolt:row (:gap text:+gap+ :cross-align :stretch :fill t)
        (bolt:panel (:all text:+gap+ :flex 1 :id :sidebar)
          (bolt:column (:gap text:+gap+)
                 (bolt:label "Sidebar" :id :sidebar-title)
                 (bolt:paragraph text:+sidebar-text+ :id :sidebar-text)))
        (bolt:column (:gap text:+gap+ :flex 2 :cross-align :stretch)
          (bolt:panel (:all text:+gap+ :flex 1 :id :top)
                 (bolt:column (:gap text:+gap+)
                   (bolt:label "Top" :id :top-title)
                   (bolt:paragraph text:+top-text+ :id :top-text)))
          (bolt:row (:gap text:+gap+ :flex 1 :cross-align :stretch)
                 (bolt:panel (:all text:+gap+ :flex 1 :id :bottom-left)
                   (bolt:paragraph text:+small-text+ :id :left-text))
                 (bolt:panel (:all text:+gap+ :flex 1 :id :bottom-right)
                   (bolt:paragraph text:+small-text+ :id :right-text))))))))

(registry:register-app :layout 'make-layout :width 640 :height 420)
