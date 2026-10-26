(defpackage #:almighty-bolt-7guis/t/mcclim/crud
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:c #:almighty-bolt-7guis/crud/mcclim)
                    (#:g #:almighty-bolt-7guis/crud/mcclim-gadgets)
                    (#:m #:almighty-bolt-7guis/crud/model)
                    (#:s #:almighty-bolt-7guis/t/mcclim/support)
                    (#:base #:almighty-bolt-stress/t/mcclim/support)
                    (#:clim #:clim)
                    (#:clime #:clim-extensions)))

(in-package #:almighty-bolt-7guis/t/mcclim/crud)

;;; The crud in McCLIM, twice, by the acceptance tests of the other variants
;;; (apps/7guis/t/acceptance/ crud.lisp). The version with the presentation layer first: its list is
;;; what is presented, a click on a row is the command COM-PICK (the translator is checked by a
;;; click on XQuartz and not here: the null port has no text to click on), the fields are an
;;; ACCEPT-VALUES form; then the version with gadgets.

(defun form (frame) (clim:find-pane-named frame 'almighty-bolt-7guis/crud/mcclim::form))
(defun listing (frame) (clim:find-pane-named frame 'almighty-bolt-7guis/crud/mcclim::listing))

(defun listed (frame)
  (remove "" (base:pane-strings (listing frame)) :test #'equal))

(defun slot (frame name)
  (slot-value frame (find-symbol (string name) '#:almighty-bolt-7guis/crud/mcclim)))

(defmacro with-crud ((frame) &body body)
  `(base:with-running-frame (,frame #'c:make-crud-frame)
     (s:await (lambda () (s:query-present-p (form ,frame) "Filter prefix")))
     (sleep 0.5)
     ,@body))

(defun entry-at (frame index)
  (nth index (m:entries-filtered (slot frame :entries) (slot frame :filter))))

(defun pick (frame index)
  (s:run-command frame 'almighty-bolt-7guis/crud/mcclim::com-pick (entry-at frame index)))

(defun identifier (frame prompt)
  "Under what the form asks for PROMPT now: a field is a new one for each entry that is picked."
  (list prompt (slot frame :picked)))

(defun field (frame prompt text)
  (s:type-into (form frame) (identifier frame prompt) text))

(defun fields (frame name surname)
  (field frame "Name" name)
  (field frame "Surname" surname))

(define-test the-list-starts-with-three-people ()
  (with-crud (frame)
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (listed frame))))

(define-test picking-a-person-fills-the-fields ()
  (with-crud (frame)
    (pick frame 1)
    (assert-equal "Max" (slot frame :name))
    (assert-equal "Mustermann" (slot frame :surname))))

(define-test create-adds-a-person-and-picks-them ()
  (with-crud (frame)
    (fields frame "Anna" "Zeus")
    (s:press-button (form frame) "Create")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman" "Zeus, Anna") (listed frame))
    (s:press-button (form frame) "Delete")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (listed frame))))

(define-test update-changes-the-picked-person ()
  (with-crud (frame)
    (pick frame 2)
    (field frame "Surname" "Tischer")
    (s:press-button (form frame) "Update")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tischer, Roman") (listed frame))))

(define-test delete-removes-the-picked-person-and-clears-the-fields ()
  (with-crud (frame)
    (pick frame 0)
    (s:press-button (form frame) "Delete")
    (assert-equal '("Mustermann, Max" "Tisch, Roman") (listed frame))
    (assert-equal "" (slot frame :name))
    (assert-equal "" (slot frame :surname))))

(define-test update-and-delete-are-not-offered-with-nobody-picked--differently ()
  (with-crud (frame)
    (assert-false (s:button-offered-p (form frame) "Update"))
    (assert-false (s:button-offered-p (form frame) "Delete"))
    (pick frame 0)
    (assert-true (s:button-offered-p (form frame) "Update"))
    (assert-true (s:button-offered-p (form frame) "Delete"))))

(define-test create-is-offered-only-with-a-name-or-a-surname--differently ()
  (with-crud (frame)
    (assert-false (s:button-offered-p (form frame) "Create"))
    (fields frame "" "X")
    (assert-true (s:button-offered-p (form frame) "Create"))))

(define-test the-filter-keeps-the-surnames-that-start-with-it ()
  (with-crud (frame)
    (s:type-into (form frame) "Filter prefix" "m")
    (assert-equal '("Mustermann, Max") (listed frame))
    (s:type-into (form frame) "Filter prefix" "Q")
    (assert-equal '() (listed frame))
    (s:type-into (form frame) "Filter prefix" "")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (listed frame))))

(define-test a-filter-that-hides-the-picked-person-unpicks-them ()
  (with-crud (frame)
    (pick frame 2)
    (s:type-into (form frame) "Filter prefix" "e")
    (assert-equal '("Emil, Hans") (listed frame))
    (assert-false (s:button-offered-p (form frame) "Delete"))
    (s:type-into (form frame) "Filter prefix" "")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (listed frame))))

(define-test a-filtered-list-is-picked-by-its-own-rows ()
  (with-crud (frame)
    (s:type-into (form frame) "Filter prefix" "t")
    (pick frame 0)
    (assert-equal "Roman" (slot frame :name))
    (field frame "Name" "Ronald")
    (s:press-button (form frame) "Update")
    (assert-equal '("Tisch, Ronald") (listed frame))))

(define-test the-presented-rows-are-entries-that-a-click-can-pick ()
  (with-crud (frame)
    (let ((types '()))
      (labels ((walk (record)
                 (when (typep record 'clim:presentation)
                   (push (clim:presentation-type record) types))
                 (clim:map-over-output-records #'walk record)))
        (walk (clim:stream-output-history (listing frame))))
      (assert-equal 3 (count 'almighty-bolt-7guis/crud/mcclim::entry types :test #'eq)))))

;;; The same, with McCLIM's gadgets.

(defun gform (frame name)
  (clim:find-pane-named frame
                        (find-symbol (string name) '#:almighty-bolt-7guis/crud/mcclim-gadgets)))

(defun glisted (frame)
  (map 'list #'m:entry-display (clime:list-pane-items (gform frame :entries-list))))

(defun gtype (frame name text)
  (let ((field (gform frame name)))
    (setf (clim:gadget-value field) text)
    (clim:value-changed-callback field (clim:gadget-client field) (clim:gadget-id field) text)))

(defun gpick (frame index)
  (setf (clim:gadget-value (gform frame :entries-list) :invoke-callback t)
        (m:entry-id (nth index (m:entries-filtered
                                (slot-value frame 'almighty-bolt-7guis/crud/mcclim-gadgets::entries)
                                (clim:gadget-value (gform frame :filter)))))))

(defun gpress (frame name)
  "A click on the button, which an inactive button does not take."
  (let ((button (gform frame name)))
    (when (clim:gadget-active-p button)
      (clim:activate-callback button (clim:gadget-client button) (clim:gadget-id button)))))

(defmacro with-gadget-crud ((frame) &body body)
  `(base:with-running-frame (,frame #'g:make-crud-frame)
     (s:await (lambda () (gform ,frame :create)))
     ,@body))

(define-test gadgets--the-list-starts-with-three-people ()
  (with-gadget-crud (frame)
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (glisted frame))))

(define-test gadgets--picking-a-person-fills-the-fields ()
  (with-gadget-crud (frame)
    (gpick frame 1)
    (assert-equal "Max" (clim:gadget-value (gform frame :name)))
    (assert-equal "Mustermann" (clim:gadget-value (gform frame :surname)))))

(define-test gadgets--create-adds-a-person-and-picks-them ()
  (with-gadget-crud (frame)
    (gtype frame :name "Anna")
    (gtype frame :surname "Zeus")
    (gpress frame :create)
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman" "Zeus, Anna") (glisted frame))
    (gpress frame :delete)
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (glisted frame))))

(define-test gadgets--update-changes-the-picked-person ()
  (with-gadget-crud (frame)
    (gpick frame 2)
    (gtype frame :surname "Tischer")
    (gpress frame :update)
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tischer, Roman") (glisted frame))))

(define-test gadgets--delete-removes-the-picked-person-and-clears-the-fields ()
  (with-gadget-crud (frame)
    (gpick frame 0)
    (gpress frame :delete)
    (assert-equal '("Mustermann, Max" "Tisch, Roman") (glisted frame))
    (assert-equal "" (clim:gadget-value (gform frame :name)))
    (assert-equal "" (clim:gadget-value (gform frame :surname)))))

(define-test gadgets--update-and-delete-do-nothing-with-nobody-picked ()
  (with-gadget-crud (frame)
    (assert-false (clim:gadget-active-p (gform frame :update)))
    (assert-false (clim:gadget-active-p (gform frame :delete)))
    (gpress frame :delete)
    (gpress frame :update)
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (glisted frame))))

(define-test gadgets--create-needs-a-name-or-a-surname ()
  (with-gadget-crud (frame)
    (assert-false (clim:gadget-active-p (gform frame :create)))
    (gtype frame :surname "X")
    (assert-true (clim:gadget-active-p (gform frame :create)))))

(define-test gadgets--the-filter-keeps-the-surnames-that-start-with-it ()
  (with-gadget-crud (frame)
    (gtype frame :filter "m")
    (assert-equal '("Mustermann, Max") (glisted frame))
    (gtype frame :filter "Q")
    (assert-equal '() (glisted frame))
    (gtype frame :filter "")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (glisted frame))))

(define-test gadgets--a-filter-that-hides-the-picked-person-unpicks-them ()
  (with-gadget-crud (frame)
    (gpick frame 2)
    (gtype frame :filter "e")
    (assert-equal '("Emil, Hans") (glisted frame))
    (assert-false (clim:gadget-active-p (gform frame :delete)))
    (gtype frame :filter "")
    (assert-equal '("Emil, Hans" "Mustermann, Max" "Tisch, Roman") (glisted frame))))

(define-test gadgets--a-filtered-list-is-picked-by-its-own-rows ()
  (with-gadget-crud (frame)
    (gtype frame :filter "t")
    (gpick frame 0)
    (assert-equal "Roman" (clim:gadget-value (gform frame :name)))
    (gtype frame :name "Ronald")
    (gpress frame :update)
    (assert-equal '("Tisch, Ronald") (glisted frame))))
