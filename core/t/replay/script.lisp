(defpackage #:almighty-bolt-core/t/replay/script
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:event #:almighty-bolt-core/event)
                    (#:script #:almighty-bolt-core/replay/script)))

(in-package #:almighty-bolt-core/t/replay/script)

(defun every-kind-of-event ()
  (list (event:make-pointer-event :timestamp 100 :kind :down :x 12.5d0 :y 40d0 :button 0
                                  :buttons 1 :modifiers event:+modifier-shift+ :click-count 2)
        (event:make-pointer-event :timestamp 101 :kind :move :x 13d0 :y 41d0)
        (event:make-wheel-event :timestamp 102 :x 5d0 :y 6d0 :dx 0d0 :dy -3.5d0 :precise-p t)
        (event:make-key-event :timestamp 103 :kind :down :physical-key 36 :logical-key #\Return
                              :modifiers event:+modifier-super+)
        (event:make-key-event :timestamp 104 :kind :up :physical-key 123)
        (event:make-key-event :timestamp 105 :kind :down :logical-key (code-char #xF702)
                              :repeat-p t)
        (event:make-text-input-event :timestamp 106 :text "héllo \"quoted\"")
        (event:make-composition-event :timestamp 107 :kind :changed :text "かな"
                                      :selection-start 1 :selection-length 1)
        (event:make-resize-event :timestamp 108 :width 400d0 :height 300d0)
        (event:make-scale-event :timestamp 109 :scale 2d0)
        (event:make-focus-event :timestamp 110 :focused-p t)))

(define-test every-kind-of-event-goes-to-a-form-and-back ()
  (dolist (event (every-kind-of-event))
    (let* ((form (script:event-form event))
           (back (script:form-event form (event:event-timestamp event))))
      (assert-equalp event back)
      (assert-equal form (script:event-form back)))))

(define-test what-is-the-default-is-left-out-of-a-form ()
  (assert-equal '(:pointer :kind :move :x 13d0 :y 41d0)
                (script:event-form (event:make-pointer-event :timestamp 1 :kind :move
                                                             :x 13d0 :y 41d0)))
  (assert-equal '(:key :kind :down :logical-key #\a)
                (script:event-form (event:make-key-event :timestamp 1 :kind :down
                                                         :logical-key #\a))))

(defun round-trip (script)
  (let ((text (with-output-to-string (stream) (script:write-script script stream))))
    (values (with-input-from-string (stream text) (script:read-script stream))
            text)))

(define-test a-script-survives-being-written-and-read-back ()
  (let* ((entries (loop :for event :in (every-kind-of-event)
                        :for time :from 0 :by 1000
                        :collect (list time (script:event-form event))))
         (entries (append entries
                          '((20000 (:pointer :kind :down :target :count-button :fx 0.5d0
                                    :fy 0.25d0 :button 0))
                            (30000 (:checkpoint :name "after the click" :hash 123456789012)))))
         (script (script:make-script :header '(:variant :immediate :app :counter :width 360
                                               :height 160 :scale 2.0d0)
                                     :entries entries)))
    (multiple-value-bind (back text) (round-trip script)
      (assert-equalp (script:script-header script) (script:script-header back))
      (assert-equalp entries (script:script-entries back))
      ;; The header is on the first line and each entry on a line of its own.
      (assert-eql (1+ (length entries))
                  (count #\Newline text)))))

(define-test the-reader-evaluates-nothing ()
  (assert-error 'reader-error
                (with-input-from-string (stream "(:almighty-bolt-replay 1)
(0 (:key :kind :down :logical-key #.(code-char 65)))")
                  (script:read-script stream))))

(define-test a-file-that-is-not-a-script-of-this-version-is-refused ()
  (flet ((read-text (text)
           (with-input-from-string (stream text) (script:read-script stream))))
    (assert-error 'error (read-text "(:something-else 1)"))
    (assert-error 'error (read-text "(:almighty-bolt-replay 2)"))
    (assert-error 'error (read-text "(:almighty-bolt-replay 1)
(10 (:focus :focused-p t))
(5 (:focus :focused-p nil))"))
    (assert-error 'error (read-text "(:almighty-bolt-replay 1)
(\"soon\" (:focus :focused-p t))"))
    (assert-true (typep (read-text "(:almighty-bolt-replay 1 :app :counter)") 'script:script))))

(define-test a-script-can-be-saved-and-loaded ()
  (let ((path (merge-pathnames "almighty-bolt-replay-test.script" (uiop:temporary-directory)))
        (script (script:make-script :header '(:app :counter)
                                    :entries '((0 (:focus :focused-p t))))))
    (unwind-protect
         (progn (script:save-script script path)
                (assert-equalp script (script:load-script path)))
      (ignore-errors (delete-file path)))))
