(defpackage #:almighty-bolt-7guis/dialog/retained
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
  (:import-from #:almighty-bolt-variant-retained/widgets
                #:label
                #:paragraph
                #:padding
                #:panel
                #:center
                #:column
                #:row)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant)
  (:export #:make-dialog))

(in-package #:almighty-bolt-7guis/dialog/retained)

;;; The dialog screen, retained: a tree of nodes made once, in which the card is the child of
;;; a centre.

(defun make-dialog ()
  (make-retained-variant
   (padding (column (row (label +header+ :id :header-title)
                         (label "" :flex 1)
                         (label +close+ :id :close)
                         :gap +gap+ :fill t)
                    (center (panel (column (label +title+ :id :card-title)
                                           (row (apply #'column
                                                       (append (mapcar #'label +labels+)
                                                               '(:cross-align :end)))
                                                (apply #'column (mapcar #'label +values+))
                                                :gap +gap+)
                                           (paragraph +body+ :id :card-text)
                                           :gap +gap+)
                                   :all +gap+ :id :card)
                            :flex 1)
                    (label +status+ :id :status)
                    :gap +gap+ :fill t :cross-align :stretch)
            :all +margin+)))

(register-app :retained :dialog 'make-dialog :width 640 :height 420)
