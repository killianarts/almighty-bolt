(defpackage #:almighty-bolt-7guis/dialog/cursor
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/cursor/mode
                #:cursor-mode)
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
  (:import-from #:almighty-bolt-variant-cursor/widgets
                #:label
                #:paragraph
                #:text-width
                #:line-height
                #:avail-height
                #:room-width
                #:skip
                #:move-to-x
                #:with-column
                #:with-row
                #:with-panel
                #:centered)
  (:import-from #:almighty-bolt-variant-cursor/cursor
                #:make-cursor-variant)
  (:export #:make-dialog))

(in-package #:almighty-bolt-7guis/dialog/cursor)

;;; The dialog screen, cursor style. The two things at the sides are placed by sizes that
;;; are text and can be measured first (the width of Close, the widest of the labels); the
;;; card is placed by a size that is its children's, which are not drawn yet.

(defun dialog-screen ()
  (with-column (:inset +margin+ :gap +gap+)
    (with-row (:gap +gap+)
      (label +header+ :id :header-title)
      (move-to-x (- (room-width) (text-width +close+)))
      (label +close+ :id :close))
    (centered (:id :card :height (- (avail-height) +gap+ (line-height)))
      (with-panel (:id :card :pad +gap+ :gap +gap+)
        (label +title+ :id :card-title)
        (with-row (:gap +gap+)
          (let ((widest (reduce #'max +labels+ :key #'text-width)))
            (with-column ()
              (dolist (text +labels+)
                (skip (- widest (text-width text)) 0)
                (label text))))
          (with-column ()
            (dolist (text +values+)
              (label text))))
        (paragraph +body+ :id :card-text)))
    (label +status+ :id :status)))

(defun make-dialog (&optional (mode (cursor-mode)))
  (make-cursor-variant #'dialog-screen :mode mode))

(register-app :cursor :dialog #'make-dialog :width 640 :height 420
              :title (format nil "dialog (cursor, ~(~a~))" (cursor-mode)))
