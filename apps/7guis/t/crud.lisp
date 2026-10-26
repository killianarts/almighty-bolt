(defpackage #:almighty-bolt-7guis/t/crud
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:m #:almighty-bolt-7guis/crud/model)))

(in-package #:almighty-bolt-7guis/t/crud)

(defun people ()
  (m:make-entries '("Hans" "Emil") '("Max" "Mustermann") '("Roman" "Tisch")))

(defun names (entries)
  (mapcar #'m:entry-display (m:entries-list entries)))

(define-test entries-are-shown-surname-first ()
  (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (names (people))))

(define-test adding-puts-an-entry-at-the-end-with-a-new-id ()
  (multiple-value-bind (entries entry) (m:entries-add (people) "Anna" "Zeus" "anna@zeus.org")
    (assert-equal "Zeus, Anna" (m:entry-display entry))
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman" "Zeus, Anna") (names entries))
    (assert-eql 3 (m:entry-id entry))
    (assert-eql 3 (length (m:entries-list (people))))))

(define-test updating-changes-an-entry-in-place ()
  (let* ((entries (people))
         (id (m:entry-id (second (m:entries-list entries)))))
    (assert-equal '("Emil, Hans" "Muster, Maria" "Tisch, Roman")
                  (names (m:entries-update entries id "Maria" "Muster" "maria@muster.org")))))

(define-test an-entry-keeps-its-email-through-adding-and-updating ()
  (multiple-value-bind (entries entry) (m:entries-add (people) "Anna" "Zeus" "anna@zeus.org")
    (assert-equal "anna@zeus.org" (m:entry-email entry))
    (assert-equal "" (m:entry-email (first (m:entries-list entries))))
    (let ((updated (m:entries-update entries (m:entry-id entry) "Anna" "Zeus" "az@example.org")))
      (assert-equal "az@example.org" (m:entry-email (m:entries-find updated (m:entry-id entry)))))))

(define-test removing-takes-an-entry-out-and-ids-are-not-reused ()
  (let* ((entries (people))
         (id (m:entry-id (first (m:entries-list entries))))
         (fewer (m:entries-remove entries id)))
    (assert-equal '("Mustermann, Max" "Tisch, Roman") (names fewer))
    (assert-false (m:entries-find fewer id))
    (assert-eql 3 (m:entry-id (nth-value 1 (m:entries-add fewer "A" "B" ""))))))

(define-test the-filter-keeps-surnames-that-start-with-the-prefix ()
  (assert-equal '("Mustermann, Max") (names-of (m:entries-filtered (people) "Mu")))
  (assert-equal '("Mustermann, Max") (names-of (m:entries-filtered (people) "mu")))
  (assert-equal 3 (length (m:entries-filtered (people) "")))
  (assert-equal '() (m:entries-filtered (people) "Q")))

(defun names-of (list)
  (mapcar #'m:entry-display list))
