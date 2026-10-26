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

;;; --- E1 (notes/change-cost.org): an Email field below Surname. The control is :email. ---

(d:define-acceptance email-is-kept-by-create-and-shown-on-picking :crud
  (replace-text driver :name "Anna")
  (replace-text driver :surname "Zeus")
  (replace-text driver :email "anna@zeus.org")
  (d:click driver :create)
  (pick-row driver 0)
  (assert-equal "" (d:text-in driver :email))
  (pick-row driver 3)
  (assert-equal "Anna" (d:text-in driver :name))
  (assert-equal "anna@zeus.org" (d:text-in driver :email)))

(d:define-acceptance update-keeps-a-changed-email :crud
  (pick-row driver 1)
  (replace-text driver :email "max@example.org")
  (d:click driver :update)
  (pick-row driver 0)
  (pick-row driver 1)
  (assert-equal "max@example.org" (d:text-in driver :email))
  (assert-equal "Mustermann, Max" (subseq (listed driver) 11 26)))

(d:define-acceptance delete-clears-the-email-too :crud
  (pick-row driver 1)
  (replace-text driver :email "max@example.org")
  (d:click driver :update)
  (d:click driver :delete)
  (assert-equal "" (d:text-in driver :email)))

(d:define-acceptance create-can-be-pressed-with-only-an-email :crud
  (replace-text driver :email "someone@example.org")
  (d:click driver :create)
  (assert-equal "Emil, HansMustermann, MaxTisch, Roman, " (listed driver)))
