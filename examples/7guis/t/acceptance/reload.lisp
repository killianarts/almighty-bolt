(defpackage #:almighty-bolt-7guis/t/acceptance/reload
  (:use #:cl #:lisp-unit2)
  (:import-from #:almighty-bolt
                #:label
                #:button
                #:padding
                #:row)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:core #:almighty-bolt/internals)
                    (#:app #:almighty-bolt/app)))

(in-package #:almighty-bolt-7guis/t/acceptance/reload)

;;; An app that is made again when its code changed is the same app to the person using it:
;;; the CRUD is driven through the same keystrokes with and without a
;;; reload in the middle, and ends in the same place. A reload here is the app made again by the
;;; same code; what an edit shows is checked by the tests below.

(defun row-fraction (index)
  (/ (+ 1 (* 24 index) 12) 146))

(defun pick-row (driver index)
  (d:click-at driver :list 0.3 (row-fraction index)))

(defun replace-text (driver id text)
  (d:click driver id)
  (d:press-key driver #\a :modifiers almighty-bolt/internals:+modifier-super+)
  (if (plusp (length text))
      (d:type-text driver text)
      (d:press-key driver #\Backspace)))

(defun listed (driver)
  (d:text-in driver :list))

(defun the-same-with-and-without-a-reload (task before after)
  "Run BEFORE and then AFTER on a driver for each variant, and again with a reload between them,
and return, for each variant, what the two ended showing."
  (loop :for variant :in (d:available-variants task)
        :collect (let ((plain (d:start variant task))
                       (reloaded (d:start variant task)))
                   (funcall before plain)
                   (funcall before reloaded)
                   (d:reload reloaded)
                   (funcall after plain)
                   (funcall after reloaded)
                   (list variant (d:screen-text plain) (d:screen-text reloaded)))))

(defun assert-no-difference (results)
  (dolist (result results)
    (destructuring-bind (variant plain reloaded) result
      (assert-equal plain reloaded variant))))

(define-test what-the-person-did-to-the-crud-is-still-there-after-a-reload ()
  (assert-no-difference
   (the-same-with-and-without-a-reload
    :crud
    (lambda (driver)
      ;; A person is added, the list is filtered, someone is picked, and a field is half edited.
      (replace-text driver :name "Anna")
      (replace-text driver :surname "Zeus")
      (d:click driver :create)
      (replace-text driver :filter "z")
      (pick-row driver 0)
      (d:click driver :name)
      (d:type-text driver "bel"))
    (lambda (driver)
      ;; And it goes on: the field that had the keyboard takes the next key, Update works on who
      ;; was picked, and the filter is still the one that was typed.
      (d:type-text driver "!")
      (d:click driver :update)
      (replace-text driver :filter "")))))

(define-test the-keyboard-and-the-caret-are-where-they-were ()
  (assert-no-difference
   (the-same-with-and-without-a-reload
    :crud
    (lambda (driver)
      (replace-text driver :name "Max")
      ;; A press inside the text, near its start, puts the caret there.
      (d:click-at driver :name 0.1 0.5))
    (lambda (driver)
      (d:type-text driver "X")))))

(define-test the-picked-person-is-still-picked ()
  (dolist (variant (d:available-variants :crud))
    (let ((driver (d:start variant :crud)))
      (pick-row driver 1)
      (d:reload driver)
      (assert-equal "Max" (d:text-in driver :name) variant)
      (assert-equal "Mustermann" (d:text-in driver :surname) variant)
      ;; Delete is enabled for them, so it removes them.
      (d:click driver :delete)
      (assert-equal "Emil, HansTisch, Roman" (listed driver) variant))))

(define-test the-people-added-are-still-there ()
  (dolist (variant (d:available-variants :crud))
    (let ((driver (d:start variant :crud)))
      (replace-text driver :name "Anna")
      (replace-text driver :surname "Zeus")
      (d:click driver :create)
      (d:reload driver)
      (d:reload driver)
      (assert-equal "Emil, HansMustermann, MaxTisch, RomanZeus, Anna" (listed driver) variant))))

(define-test an-edited-way-to-show-a-person-is-shown-after-the-reload ()
  ;; The edit is to a function that the CRUD calls, as compiling a definition in the
  ;; editor would make it, and the list was worked out before it by a key typed in the filter.
  (let* ((name 'almighty-bolt-7guis/crud/model:entry-display)
         (original (fdefinition name)))
    (unwind-protect
         (dolist (variant (d:available-variants :crud))
           (let ((driver (d:start variant :crud)))
             (replace-text driver :filter "m")
             (replace-text driver :filter "")
             (pick-row driver 1)
             (setf (fdefinition name)
                   (lambda (entry)
                     (format nil "~a ~a" (almighty-bolt-7guis/crud/model:entry-name entry)
                             (almighty-bolt-7guis/crud/model:entry-surname entry))))
             (d:reload driver)
             (assert-equal "Hans EmilMax MustermannRoman Tisch" (listed driver) variant)
             (assert-equal "Max" (d:text-in driver :name) variant)
             (setf (fdefinition name) original)))
      (setf (fdefinition name) original))))

(defun plain-counter ()
  "The Counter written with LET, which keeps nothing, made as the host makes an app."
  (app:make-app-instance
   (lambda ()
     (let ((count 0))
       (lambda ()
         (padding (:all 24)
           (row (:gap 16 :cross-align :center)
             (label (princ-to-string count) :id :count :size 32)
             (when (button "Count" :id :count-button)
               (incf count)))))))))

(define-test an-app-written-with-let-keeps-nothing-and-starts-again ()
  (dolist (variant (d:available-variants :counter))
    (let ((driver (d:start variant :counter)))
      (d:reload driver #'plain-counter)
      (d:click driver :count-button)
      (assert-equal "1" (d:text-in driver :count) variant)
      (d:reload driver #'plain-counter)
      (assert-equal "0" (d:text-in driver :count) variant))))

(define-test an-app-written-with-state-let-keeps-its-count ()
  (dolist (variant (d:available-variants :counter))
    (let ((driver (d:start variant :counter)))
      (d:click driver :count-button)
      (d:click driver :count-button)
      (d:reload driver)
      (assert-equal "2" (d:text-in driver :count) variant))))
