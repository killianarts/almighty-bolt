(defpackage #:almighty-bolt-core/replay/recorder
  (:use #:cl)
  (:import-from #:almighty-bolt-core/event
                #:event-timestamp)
  (:import-from #:almighty-bolt-core/variant
                #:hit-target
                #:lookup-target)
  (:import-from #:almighty-bolt-core/replay/script
                #:make-script
                #:event-form)
  (:export #:recorder
           #:make-recorder
           #:record-event
           #:recorder-script))

(in-package #:almighty-bolt-core/replay/recorder)

;;; Write a script from a live session: each event the variant is given is noted with its time
;;; since the first event, and a pointer or wheel event over a control that has an id is
;;; written as a place in that control, so that the script can be replayed in a variant whose
;;; layout is a few pixels different. Call RECORD-EVENT before the variant handles the event,
;;; so that the control found is the one the variant will find. Checkpoints are made afterwards,
;;; from a replay (SCRIPT-WITH-CHECKPOINTS, FREEZE-CHECKPOINTS), because a hash taken in a live
;;; session depends on when the display happened to ask for frames.

(defstruct (recorder (:constructor make-recorder (&key header)))
  (header '() :type list)
  (start nil)
  (last 0 :type integer)
  (entries '() :type list))

(defun remove-keys (plist &rest keys)
  (loop :for (key value) :on plist :by #'cddr
        :unless (member key keys)
          :append (list key value)))

(defun retarget (form variant)
  "FORM, a pointer or wheel form with :X and :Y, with the control under it as :TARGET, :FX and
:FY if there is one that has an id and a place."
  (if (not (member (first form) '(:pointer :wheel)))
      form
      (let* ((options (rest form))
             (x (getf options :x))
             (y (getf options :y))
             (id (hit-target variant (float x 1f0) (float y 1f0))))
        (if (null id)
            form
            (multiple-value-bind (left top width height) (lookup-target variant id)
              (if (and left (plusp width) (plusp height))
                  (list* (first form)
                         :target id :fx (/ (- x left) width) :fy (/ (- y top) height)
                         (remove-keys options :x :y))
                  form))))))

(defun record-event (recorder variant event)
  "Note EVENT, about to be given to VARIANT. Returns the form that was written."
  (let* ((timestamp (event-timestamp event))
         (start (or (recorder-start recorder)
                    (setf (recorder-start recorder) timestamp)))
         (time (max (recorder-last recorder) (- timestamp start)))
         (form (retarget (event-form event) variant)))
    (setf (recorder-last recorder) time)
    (push (list time form) (recorder-entries recorder))
    form))

(defun recorder-script (recorder)
  "The script of what has been recorded so far."
  (make-script :header (recorder-header recorder)
               :entries (reverse (recorder-entries recorder))))
