(defpackage #:almighty-bolt-variant-reactive/reactive
  (:use #:cl)
  (:import-from #:almighty-bolt-core
                #:variant
                #:frame
                #:handle-event
                #:hit-target
                #:lookup-target
                #:request-frame
                #:context-now-ns
                #:with-phase
                #:+phase-update+)
  (:import-from #:almighty-bolt-variant-reactive/views
                #:component)
  (:import-from #:almighty-bolt-variant-reactive/elements
                #:mount
                #:rebuild
                #:element-node
                #:element-live-p
                #:element-depth
                #:element-queued)
  (:import-from #:almighty-bolt-variant-retained/retained
                #:make-retained-variant
                #:retained-variant-session
                #:retained-variant-focus)
  (:export #:reactive-variant
           #:make-reactive-variant
           #:reactive-variant-inner
           #:reactive-variant-focus
           #:add-ticker
           #:flush))

(in-package #:almighty-bolt-variant-reactive/reactive)

;;; The reactive variant. An app is a function that describes the screen from signals; the
;;; variant runs it once into a persistent tree of retained nodes (elements.lisp), and again,
;;; one component at a time, when a signal that a component read has changed. A frame first
;;; runs the tickers (which write signals as time passes), then describes again every
;;; component that was told, outermost first, and then lays out and paints the retained tree
;;; as the retained variant does. Events are handled by the retained nodes, whose handlers
;;; write signals.

(defclass reactive-variant (variant)
  ((inner :initarg :inner :accessor reactive-variant-inner)
   (root :accessor root)
   ;; The components told that a signal they read changed, since they were last described: a
   ;; vector that is filled and emptied again and again, and the one that is not being filled
   ;; while the others are described, so that none of it is made again in a frame.
   (dirty :initform (make-array 16 :adjustable t :fill-pointer 0) :accessor dirty)
   (describing :initform (make-array 16 :adjustable t :fill-pointer 0) :accessor describing)
   (tickers :initform '() :accessor tickers)
   (context :initform nil :accessor context)
   ;; True while an event or a frame is being handled, when asking for a frame is the
   ;; host's business.
   (busy :initform nil :accessor busy))
  (:documentation "A variant whose screen is described by functions of signals."))

(defun make-reactive-variant (thunk &key (caching t))
  "A variant that shows what THUNK describes (a function of no arguments that returns a view),
described again whenever a signal it read changes."
  (let* ((variant (make-instance 'reactive-variant))
         (root (mount (component thunk) nil nil
                      (lambda (element)
                        (unless (element-queued element)
                          (setf (element-queued element) t)
                          (vector-push-extend element (dirty variant)))
                        ;; A signal written outside an event or a frame asks for one.
                        (when (and (context variant) (not (busy variant)))
                          (request-frame (context variant)))))))
    (setf (root variant) root
          (reactive-variant-inner variant) (make-retained-variant (element-node root)
                                                                  :caching caching))
    variant))

(defun reactive-variant-focus (variant)
  (retained-variant-focus (reactive-variant-inner variant)))

(defun add-ticker (variant function)
  "Run FUNCTION at the start of every frame with the time in nanoseconds; it may write
signals. If it returns true another frame is asked for."
  (setf (tickers variant) (append (tickers variant) (list function)))
  function)

(defun flush (variant)
  "Describe again every component that was told it changed, outermost first, and the ones
that doing so tells in turn, until none is left."
  (loop :repeat 50
        :while (plusp (length (dirty variant)))
        :do (let ((batch (dirty variant)))
              ;; What describing these tells goes in the other vector.
              (setf (dirty variant) (describing variant)
                    (describing variant) batch)
              (sort batch #'< :key #'element-depth)
              (dotimes (i (length batch))
                (setf (element-queued (aref batch i)) nil))
              (unwind-protect
                   (dotimes (i (length batch))
                     (let ((element (aref batch i)))
                       (when (element-live-p element)
                         (rebuild element))))
                (setf (fill-pointer batch) 0))))
  (when (plusp (length (dirty variant)))
    (error "The components do not settle: they keep changing the signals they read.")))

(defmethod frame ((variant reactive-variant) context list)
  (setf (context variant) context)
  (let ((again nil)
        (now (context-now-ns context)))
    (setf (busy variant) t)
    (unwind-protect
         (progn
           ;; The signals the tickers write, and the components described again for them.
           (with-phase (context +phase-update+)
             (dolist (ticker (tickers variant))
               (when (funcall ticker now)
                 (setf again t)))
             (flush variant))
           ;; Laying out and painting the retained tree are the inner variant's phases.
           (frame (reactive-variant-inner variant) context list))
      (setf (busy variant) nil))
    (when again
      (request-frame context))
    nil))

(defmethod handle-event ((variant reactive-variant) event)
  (setf (busy variant) t)
  (let ((changed (unwind-protect (handle-event (reactive-variant-inner variant) event)
                   (setf (busy variant) nil))))
    ;; What a handler did to the signals needs a frame, whether or not a node changed.
    (or changed (plusp (length (dirty variant))))))

(defmethod hit-target ((variant reactive-variant) x y)
  (hit-target (reactive-variant-inner variant) x y))

(defmethod lookup-target ((variant reactive-variant) id)
  (lookup-target (reactive-variant-inner variant) id))
