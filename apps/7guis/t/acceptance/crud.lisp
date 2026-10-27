(defpackage #:almighty-bolt-7guis/t/acceptance/crud
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)))

(in-package #:almighty-bolt-7guis/t/acceptance/crud)

;;; CRUD: a filter by surname prefix, a list of "Surname, Name" (starting with Emil, Hans;
;;; Mustermann, Max; and Tisch, Roman), fields for a name and a surname, and Create, Update
;;; and Delete, the last two only with something picked. Controls: :filter, :list (rows of
;;; 24 points from the top, six showing), :name, :surname, :create, :update and :delete.

(defun row-fraction (index)
  "How far down the list the middle of row INDEX is, 0 to 1, for a list of six rows."
  (/ (+ 1 (* 24 index) 12) 146))

(defun pick-row (driver index)
  (d:click-at driver :list 0.3 (row-fraction index)))

(defun replace-text (driver id text)
  (d:click driver id)
  (d:press-key driver #\a :modifiers almighty-bolt-core:+modifier-super+)
  (when (plusp (length text))
    (d:type-text driver text))
  (when (zerop (length text))
    (d:press-key driver #\Backspace)))

(defun listed (driver)
  (d:text-in driver :list))

(d:define-acceptance the-list-starts-with-three-people :crud
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance picking-a-person-fills-the-fields :crud
  (pick-row driver 1)
  (assert-equal "Max" (d:text-in driver :name))
  (assert-equal "Mustermann" (d:text-in driver :surname)))

(d:define-acceptance create-adds-a-person-and-picks-them :crud
  (replace-text driver :name "Anna")
  (replace-text driver :surname "Zeus")
  (d:click driver :create)
  (assert-equal "Emil, HansMustermann, MaxTisch, RomanZeus, Anna" (listed driver))
  ;; The new person is picked, so Delete works on them.
  (d:click driver :delete)
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance update-changes-the-picked-person :crud
  (pick-row driver 2)
  (replace-text driver :surname "Tischer")
  (d:click driver :update)
  (assert-equal "Emil, HansMustermann, MaxTischer, Roman" (listed driver)))

(d:define-acceptance delete-removes-the-picked-person-and-clears-the-fields :crud
  (pick-row driver 0)
  (d:click driver :delete)
  (assert-equal "Mustermann, MaxTisch, Roman" (listed driver))
  (assert-equal "" (d:text-in driver :name))
  (assert-equal "" (d:text-in driver :surname)))

(d:define-acceptance update-and-delete-do-nothing-with-nobody-picked :crud
  (d:click driver :delete)
  (d:click driver :update)
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance create-needs-a-name-or-a-surname :crud
  (d:click driver :create)
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance the-filter-keeps-the-surnames-that-start-with-it :crud
  (replace-text driver :filter "m")
  (assert-equal "Mustermann, Max" (listed driver))
  (replace-text driver :filter "Q")
  (assert-equal "" (listed driver))
  (replace-text driver :filter "")
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance a-filter-that-hides-the-picked-person-unpicks-them :crud
  (pick-row driver 2)
  (replace-text driver :filter "e")
  (assert-equal "Emil, Hans" (listed driver))
  (d:click driver :delete)
  (replace-text driver :filter "")
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance a-filtered-list-is-picked-by-its-own-rows :crud
  (replace-text driver :filter "t")
  (pick-row driver 0)
  (assert-equal "Roman" (d:text-in driver :name))
  (replace-text driver :name "Ronald")
  (d:click driver :update)
  (assert-equal "Tisch, Ronald" (listed driver)))

;;; --- E4 again (notes/change-cost.org, 2026-10-27): the filter row is hidden behind a checkbox
;;; labelled "Filter", :filter-switch, checked at first. One click turns the filter off, another
;;; on. While it is off the row is not on the screen and the list shows everybody. ---

(defun filter-shown-p (driver)
  "True if the filter row is on the screen: its label is drawn."
  (and (search "Filter prefix" (d:screen-text driver)) t))

(defun toggle-filter (driver)
  (d:click driver :filter-switch))

(d:define-acceptance the-filter-has-a-checkbox-and-its-row-is-shown-at-first :crud
  (assert-true (d:found-p driver :filter-switch))
  (assert-true (filter-shown-p driver)))

(d:define-acceptance unchecking-the-filter-hides-its-row-and-the-checkbox-stays :crud
  (toggle-filter driver)
  (assert-false (filter-shown-p driver))
  (assert-true (d:found-p driver :filter-switch)))

(d:define-acceptance with-the-filter-off-the-list-shows-everybody :crud
  (replace-text driver :filter "m")
  (assert-equal "Mustermann, Max" (listed driver))
  (toggle-filter driver)
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman" (listed driver)))

(d:define-acceptance checking-the-filter-again-brings-back-the-row-and-what-it-held :crud
  (replace-text driver :filter "t")
  (toggle-filter driver)
  (toggle-filter driver)
  (assert-true (filter-shown-p driver))
  (assert-equal "t" (d:text-in driver :filter))
  (assert-equal "Tisch, Roman" (listed driver)))
