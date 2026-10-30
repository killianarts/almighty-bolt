(defpackage #:almighty-bolt-7guis/dialog/immediate
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/dialog/text
                #:+header+
                #:+close+
                #:+title+
                #:+labels+
                #:+values+
                #:+body+
                #:+status+
                #:+margin+
                #:+gap+)
  (:import-from #:almighty-bolt/ui/widgets
                #:label
                #:paragraph
                #:padding
                #:panel
                #:center
                #:column
                #:row)
  (:import-from #:almighty-bolt/ui/immediate
                #:make-immediate-variant)
  (:export #:make-dialog))

(in-package #:almighty-bolt-7guis/dialog/immediate)

;;; The dialog screen, immediate: the card is described inside a centre, and layout, which
;;; runs after the description, centres what it finds.

(defun make-dialog ()
  (make-immediate-variant
   (lambda ()
     (padding (:all +margin+)
       (column (:gap +gap+ :fill t :cross-align :stretch)
         (row (:gap +gap+ :fill t)
           (label +header+ :id :header-title)
           (label "" :flex 1)
           (label +close+ :id :close))
         (center (:flex 1)
           (panel (:all +gap+ :id :card)
             (column (:gap +gap+)
               (label +title+ :id :card-title)
               (row (:gap +gap+)
                 (column (:cross-align :end)
                   (dolist (text +labels+) (label text)))
                 (column ()
                   (dolist (text +values+) (label text))))
               (paragraph +body+ :id :card-text))))
         (label +status+ :id :status))))))

(register-app :immediate :dialog 'make-dialog :width 640 :height 420)
