(defpackage #:almighty-bolt-7guis/layout/cursor
  (:use #:cl)
  (:import-from #:almighty-bolt-7guis/common/registry
                #:register-app)
  (:import-from #:almighty-bolt-7guis/cursor/mode
                #:cursor-mode)
  (:import-from #:almighty-bolt-7guis/layout/text
                #:+sidebar-text+
                #:+top-text+
                #:+small-text+
                #:+margin+
                #:+gap+)
  (:import-from #:almighty-bolt-variant-cursor/widgets
                #:label
                #:paragraph
                #:avail-width
                #:avail-height
                #:split-sizes
                #:with-column
                #:with-row
                #:with-panel)
  (:import-from #:almighty-bolt-variant-cursor/cursor
                #:make-cursor-variant)
  (:export #:make-layout))

(in-package #:almighty-bolt-7guis/layout/cursor)

;;; The layout spike, cursor style: every size comes from the window, down, so each pane
;;; works out how much room its children have before it draws any of them. There is no flex
;;; factor to give; the split is done where it is described, with SPLIT-SIZES.

(defun layout-screen ()
  (with-column (:inset +margin+)
    (destructuring-bind (side main) (split-sizes (avail-width) +gap+ 1 2)
      (with-row (:gap +gap+ :height (avail-height))
        (with-panel (:id :sidebar :pad +gap+ :gap +gap+ :width side :height (avail-height))
          (label "Sidebar" :id :sidebar-title)
          (paragraph +sidebar-text+ :id :sidebar-text))
        (with-column (:gap +gap+ :width main :height (avail-height))
          (destructuring-bind (top bottom) (split-sizes (avail-height) +gap+ 1 1)
            (with-panel (:id :top :pad +gap+ :gap +gap+ :width main :height top)
              (label "Top" :id :top-title)
              (paragraph +top-text+ :id :top-text))
            (with-row (:gap +gap+ :width main :height bottom)
              (destructuring-bind (left right) (split-sizes (avail-width) +gap+ 1 1)
                (with-panel (:id :bottom-left :pad +gap+ :width left :height bottom)
                  (paragraph +small-text+ :id :left-text))
                (with-panel (:id :bottom-right :pad +gap+ :width right :height bottom)
                  (paragraph +small-text+ :id :right-text))))))))))

(defun make-layout (&optional (mode (cursor-mode)))
  (make-cursor-variant #'layout-screen :mode mode))

(register-app :cursor :layout 'make-layout :width 640 :height 420
              :title (format nil "layout (cursor, ~(~a~))" (cursor-mode)))
