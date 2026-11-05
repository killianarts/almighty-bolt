(defpackage #:almighty-bolt/replay/script
  (:use #:cl)
  (:local-nicknames (#:event #:almighty-bolt/event))
  (:export #:script
           #:make-script
           #:script-header
           #:script-entries
           #:entry-time
           #:entry-form
           #:event-form
           #:form-event
           #:write-script
           #:read-script
           #:save-script
           #:load-script
           #:+script-version+))

(in-package #:almighty-bolt/replay/script)

;;; A replay script is a list of s-expressions, so that it can be read, written and edited by
;;; hand. The first is the header, =(:almighty-bolt-replay 1 :app :counter :width 360 :height 160
;;; :scale 2.0)=, and each of the others is an entry, =(TIME FORM)=. (Scripts recorded before 0.3
;;; have =:variant :immediate= in the header too, from when there was more than one kind of ui; it
;;; is not read.) TIME is nanoseconds on the virtual clock from the start, and FORM is one of
;;;
;;;   (:pointer :kind :down :x 12.5d0 :y 40.0d0 :button 0 :buttons 1 :click-count 1)
;;;   (:pointer :kind :down :target :count-button :fx 0.5d0 :fy 0.5d0 :button 0 ...)
;;;   (:wheel :x .. :y .. :dx .. :dy .. :precise-p t)         (also with :target, :fx, :fy)
;;;   (:key :kind :down :physical-key 36 :logical-key #\Return :modifiers 1)
;;;   (:text :text "hello")        (:composition :kind :changed :text "..")
;;;   (:resize :width 400d0 :height 300d0)   (:scale :scale 2d0)   (:focus :focused-p t)
;;;   (:checkpoint :name "after the click" :hash 123456789)
;;;
;;; An event with a :TARGET is placed at the fraction :FX, :FY across and down the rectangle
;;; that the app's LOOKUP-TARGET gives for that id at the time it is delivered, so a script
;;; still drives an app whose layout has moved by a few pixels. Anything that is its
;;; default (no button, no modifiers) is left out. Only keywords, numbers, characters and
;;; strings appear, and the reader never evaluates anything.

(defconstant +script-version+ 1)

(defstruct (script (:constructor make-script (&key header entries)))
  "A header (a plist) and the entries, each a list (TIME FORM), in order of time."
  (header '() :type list)
  (entries '() :type list))

(defun entry-time (entry) (first entry))
(defun entry-form (entry) (second entry))

;;; --- events and forms ---

(defun plist-without-defaults (&rest keys-and-values)
  "A plist of KEYS-AND-VALUES, as (KEY VALUE DEFAULT) triples, without those that are default."
  (loop :for (key value default) :on keys-and-values :by #'cdddr
        :unless (equal value default)
          :append (list key value)))

(defun event-form (event)
  "The form that stands for EVENT in a script, without its time, with raw coordinates."
  (etypecase event
    (event:pointer-event
     (list* :pointer :kind (event:pointer-event-kind event)
            :x (event:pointer-event-x event) :y (event:pointer-event-y event)
            (plist-without-defaults :button (event:pointer-event-button event) nil
                                    :buttons (event:pointer-event-buttons event) 0
                                    :modifiers (event:pointer-event-modifiers event) 0
                                    :click-count (event:pointer-event-click-count event) 0)))
    (event:wheel-event
     (list* :wheel :x (event:wheel-event-x event) :y (event:wheel-event-y event)
            :dx (event:wheel-event-dx event) :dy (event:wheel-event-dy event)
            (plist-without-defaults :precise-p (event:wheel-event-precise-p event) nil
                                    :modifiers (event:wheel-event-modifiers event) 0)))
    (event:key-event
     (list* :key :kind (event:key-event-kind event)
            (plist-without-defaults :physical-key (event:key-event-physical-key event) 0
                                    :logical-key (event:key-event-logical-key event) nil
                                    :modifiers (event:key-event-modifiers event) 0
                                    :repeat-p (event:key-event-repeat-p event) nil)))
    (event:text-input-event
     (list :text :text (event:text-input-event-text event)))
    (event:composition-event
     (list* :composition :kind (event:composition-event-kind event)
            (plist-without-defaults :text (event:composition-event-text event) ""
                                    :selection-start (event:composition-event-selection-start event) 0
                                    :selection-length (event:composition-event-selection-length event)
                                    0)))
    (event:resize-event
     (list :resize :width (event:resize-event-width event) :height (event:resize-event-height event)))
    (event:scale-event
     (list :scale :scale (event:scale-event-scale event)))
    (event:focus-event
     (list :focus :focused-p (event:focus-event-focused-p event)))))

(defun double (number)
  (float number 1d0))

(defun form-event (form timestamp)
  "The event that FORM stands for, given TIMESTAMP. A pointer or wheel form must have :X and :Y
by now (see the player, which turns a :TARGET into them)."
  (destructuring-bind (kind &rest options) form
    (ecase kind
      (:pointer
       (destructuring-bind (&key kind x y button (buttons 0) (modifiers 0) (click-count 0))
           options
         (event:make-pointer-event :timestamp timestamp :kind kind :x (double x) :y (double y)
                                   :button button :buttons buttons :modifiers modifiers
                                   :click-count click-count)))
      (:wheel
       (destructuring-bind (&key x y (dx 0) (dy 0) precise-p (modifiers 0)) options
         (event:make-wheel-event :timestamp timestamp :x (double x) :y (double y) :dx (double dx)
                                 :dy (double dy) :precise-p precise-p :modifiers modifiers)))
      (:key
       (destructuring-bind (&key kind (physical-key 0) logical-key (modifiers 0) repeat-p)
           options
         (event:make-key-event :timestamp timestamp :kind kind :physical-key physical-key
                               :logical-key logical-key :modifiers modifiers :repeat-p repeat-p)))
      (:text
       (destructuring-bind (&key text) options
         (event:make-text-input-event :timestamp timestamp :text text)))
      (:composition
       (destructuring-bind (&key kind (text "") (selection-start 0) (selection-length 0))
           options
         (event:make-composition-event :timestamp timestamp :kind kind :text text
                                       :selection-start selection-start
                                       :selection-length selection-length)))
      (:resize
       (destructuring-bind (&key width height) options
         (event:make-resize-event :timestamp timestamp :width (double width) :height (double height))))
      (:scale
       (destructuring-bind (&key scale) options
         (event:make-scale-event :timestamp timestamp :scale (double scale))))
      (:focus
       (destructuring-bind (&key focused-p) options
         (event:make-focus-event :timestamp timestamp :focused-p focused-p))))))

;;; --- reading and writing ---

(defun write-script (script stream)
  "Write SCRIPT to STREAM, the header on the first line and an entry on each of the others."
  (with-standard-io-syntax
    (let ((*print-pretty* nil)
          (*print-case* :downcase))
      (prin1 `(:almighty-bolt-replay ,+script-version+ ,@(script-header script)) stream)
      (terpri stream)
      (dolist (entry (script-entries script))
        (prin1 entry stream)
        (terpri stream))))
  script)

(defun read-script (stream)
  "Read a script from STREAM. Nothing in it is evaluated. An error if it is not a script of
this version or an entry is not (TIME FORM) with the times in order."
  (with-standard-io-syntax
    (let* ((*read-eval* nil)
           (head (read stream nil nil))
           (entries (loop :for entry = (read stream nil stream)
                          :until (eq entry stream)
                          :collect entry)))
      (unless (and (consp head) (eq (first head) :almighty-bolt-replay))
        (error "This is not a replay script: it does not start with (:almighty-bolt-replay ...)."))
      (unless (eql (second head) +script-version+)
        (error "Replay script version ~s: only ~d is understood." (second head) +script-version+))
      (let ((previous 0))
        (dolist (entry entries)
          (unless (and (consp entry) (integerp (entry-time entry)) (not (minusp (entry-time entry)))
                       (consp (entry-form entry)) (keywordp (first (entry-form entry))))
            (error "A replay entry must be (TIME FORM), not ~s." entry))
          (when (< (entry-time entry) previous)
            (error "The times of a replay script must not go back: ~s." entry))
          (setf previous (entry-time entry))))
      (make-script :header (cddr head) :entries entries))))

(defun save-script (script path)
  (with-open-file (stream path :direction :output :if-exists :supersede
                              :external-format :utf-8)
    (write-script script stream))
  path)

(defun load-script (path)
  (with-open-file (stream path :external-format :utf-8)
    (read-script stream)))
