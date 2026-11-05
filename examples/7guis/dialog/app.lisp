(defpackage #:almighty-bolt-7guis/dialog/app
  (:use #:cl)
  (:local-nicknames (#:registry #:almighty-bolt-7guis/common/registry)
                    (#:text #:almighty-bolt-7guis/dialog/text)
                    (#:bolt #:almighty-bolt))
  (:export #:make-dialog))

(in-package #:almighty-bolt-7guis/dialog/app)

;;; The dialog screen, immediate: the card is described inside a centre, and layout, which
;;; runs after the description, centres what it finds.

(defun make-dialog ()
  (lambda ()
    (bolt:padding (:all text:+margin+)
      (bolt:column (:gap text:+gap+ :fill t :cross-align :stretch)
        (bolt:row (:gap text:+gap+ :fill t)
          (bolt:label text:+header+ :id :header-title)
          (bolt:label "" :id :spacer :flex 1)
          (bolt:label text:+close+ :id :close))
        (bolt:center (:flex 1)
          (bolt:panel (:all text:+gap+ :id :card)
            (bolt:column (:gap text:+gap+)
                   (bolt:label text:+title+ :id :card-title)
                   (bolt:row (:gap text:+gap+)
                     (bolt:column (:cross-align :end)
                       (dolist (text text:+labels+) (bolt:label text :id text)))
                     (bolt:column ()
                       (dolist (text text:+values+) (bolt:label text :id text))))
                   (bolt:paragraph text:+body+ :id :card-text))))
        (bolt:label text:+status+ :id :status)))))

(registry:register-app :dialog 'make-dialog :width 640 :height 420)
