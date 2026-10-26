(defpackage #:almighty-bolt-stress/t/acceptance/rows
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:m #:almighty-bolt-stress/rows/model)
                    (#:core #:almighty-bolt-core)))

(in-package #:almighty-bolt-stress/t/acceptance/rows)

;;; The million-row list: 18 of the rows show, with the number of the row, a name and a value; the
;;; wheel scrolls it, the keys move the selected row, a click selects one, and the bar at its right
;;; is dragged. Controls: :list (the rows and the bar) and :status. The first row is "Row 0000001".

(defun listed (driver) (d:text-in driver :list))

(defun shows-p (driver index)
  "True if the row INDEX is among those that show."
  (and (search (m:row-id-text index) (listed driver)) t))

(defun status (driver) (d:text-in driver :status))

(defun click-row (driver slot)
  (d:click-at driver :list 0.2 (/ (+ (* slot m:+row-height+) 11) m:+list-height+)))


(d:define-acceptance the-first-rows-show-and-none-is-selected :rows
  (assert-equal "No row selected" (status driver))
  (assert-true (shows-p driver 0))
  (assert-true (shows-p driver 17))
  (assert-false (shows-p driver 18))
  (assert-true (search (m:row-name-text 0) (listed driver)))
  (assert-true (search (m:row-value-text 0) (listed driver))))

(d:define-acceptance the-wheel-scrolls-by-rows :rows
  (d:scroll driver :list -10)
  (assert-false (shows-p driver 9))
  (assert-true (shows-p driver 10))
  (assert-true (shows-p driver 27))
  (assert-false (shows-p driver 28)))

(d:define-acceptance the-part-of-a-row-that-a-wheel-step-leaves-is-not-lost :rows
  ;; A line is 24 points and a row 22: 5 lines are 5.45 rows, and the .45 is kept for the next.
  (d:scroll driver :list -5)
  (assert-true (shows-p driver 5))
  (assert-false (shows-p driver 4))
  (d:scroll driver :list -5)
  (assert-true (shows-p driver 10))
  (assert-false (shows-p driver 9)))

(d:define-acceptance the-wheel-does-not-scroll-past-the-first-row :rows
  (d:scroll driver :list 7)
  (assert-true (shows-p driver 0))
  (assert-true (shows-p driver 17)))

(d:define-acceptance a-click-selects-the-row-it-is-on-and-says-so :rows
  (click-row driver 3)
  (assert-equal (m:status-text 3) (status driver))
  (click-row driver 0)
  (assert-equal (m:status-text 0) (status driver)))

(d:define-acceptance a-click-on-a-scrolled-list-selects-that-row :rows
  ;; 20 lines are 21.8 rows: the list is at row 21.
  (d:scroll driver :list -20)
  (click-row driver 2)
  (assert-equal (m:status-text 23) (status driver)))

(d:define-acceptance the-selected-row-looks-different-from-the-rest :rows
  (let ((before (d:colors-in driver :list)))
    (click-row driver 4)
    (assert-false (equal before (d:colors-in driver :list)))))

(d:define-acceptance the-down-key-selects-the-next-row-and-the-up-key-the-one-before :rows
  ;; The keys go to the list once it has the keyboard, which a click gives it.
  (click-row driver 0)
  (d:press-key driver core:+key-down+)
  (assert-equal (m:status-text 1) (status driver))
  (d:press-key driver core:+key-down+)
  (assert-equal (m:status-text 2) (status driver))
  (d:press-key driver core:+key-up+)
  (assert-equal (m:status-text 1) (status driver)))

(d:define-acceptance a-selected-row-that-goes-off-the-list-scrolls-it :rows
  (click-row driver 17)
  (d:press-key driver core:+key-down+)
  (assert-equal (m:status-text 18) (status driver))
  (assert-true (shows-p driver 18))
  (assert-false (shows-p driver 0))
  (assert-true (shows-p driver 1)))

(d:define-acceptance page-down-moves-a-page-and-home-and-end-go-to-the-ends :rows
  (click-row driver 0)
  (d:press-key driver core:+key-page-down+)
  (assert-equal (m:status-text 18) (status driver))
  (assert-true (shows-p driver 18))
  (d:press-key driver core:+key-end+)
  (assert-equal (m:status-text (1- m:+row-count+)) (status driver))
  (assert-true (shows-p driver (1- m:+row-count+)))
  (assert-false (shows-p driver m:+row-count+))
  (d:press-key driver core:+key-home+)
  (assert-equal (m:status-text 0) (status driver))
  (assert-true (shows-p driver 0)))

(d:define-acceptance dragging-the-bar-scrolls-to-where-it-goes :rows
  (let ((x (/ (- m:+list-width+ (/ m:+bar-width+ 2)) m:+list-width+)))
    (d:press-at driver :list x 0.5)
    (let ((top (m:top-for-thumb 198)))
      (assert-true (< 400000 top 600000))
      (assert-true (shows-p driver top))
      (assert-false (shows-p driver (1- top))))
    (d:drag-to driver :list x 0.0)
    (assert-true (shows-p driver 0))
    (d:drag-to driver :list x 1.0)
    (assert-true (shows-p driver (1- m:+row-count+)))
    (d:release-at driver :list x 1.0)
    ;; Released, moving over the bar scrolls nothing.
    (d:drag-to driver :list x 0.0)
    (assert-true (shows-p driver (1- m:+row-count+)))))

(d:define-acceptance a-press-on-the-bar-does-not-select-a-row :rows
  (d:press-at driver :list (/ (- m:+list-width+ 7) m:+list-width+) 0.2)
  (assert-equal "No row selected" (status driver)))

(d:define-acceptance a-million-rows-take-no-more-than-a-few-frames-of-memory :rows
  (let ((before (sb-ext:get-bytes-consed)))
    (dotimes (i 200)
      (d:scroll driver :list -9))
    (assert-true (< (- (sb-ext:get-bytes-consed) before) 300000000))))
