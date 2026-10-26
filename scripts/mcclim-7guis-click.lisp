(defpackage #:almighty-bolt-scripts/mcclim-7guis-click
  (:use #:cl)
  (:local-nicknames (#:flight #:almighty-bolt-7guis/flight/mcclim)
                    (#:crud #:almighty-bolt-7guis/crud/mcclim)
                    (#:circles #:almighty-bolt-7guis/circles/mcclim)
                    (#:circle #:almighty-bolt-7guis/circles/model)
                    (#:bt #:bordeaux-threads)
                    (#:clim #:clim)))

(in-package #:almighty-bolt-scripts/mcclim-7guis-click)

;;;; One real use of each of the three 7GUIs apps in McCLIM, on XQuartz, with pointer events put in
;;;; McCLIM's queue (not X events: there is no tool here that clicks an X window), and what the
;;;; frame's state is after each. From the repository root, with XQuartz running:
;;;;   APP=flight DISPLAY=:0 sbcl ... --eval '(asdf:load-system "almighty-bolt-7guis/mcclim")' \
;;;;       --load scripts/mcclim-7guis-click.lisp
;;;; APP is flight, crud or circles; SHOTS=directory also writes a screenshot of each window (xwd).

(defun say (control &rest arguments)
  (apply #'format t control arguments)
  (terpri)
  (finish-output))

(defun start (maker)
  (let ((frame (funcall maker)))
    (bt:make-thread (lambda () (clim:run-frame-top-level frame)) :name "frame")
    (loop :until (eq (clim:frame-state frame) :enabled) :do (sleep 0.01))
    (sleep 4)
    frame))

(defun shot (pattern name)
  (let ((directory (uiop:getenv "SHOTS")))
    (when (and directory (plusp (length directory)))
      (let* ((text (uiop:run-program '("/opt/X11/bin/xwininfo" "-root" "-tree")
                                     :output :string :ignore-error-status t))
             (line (find-if (lambda (line) (search pattern line :test #'char-equal))
                            (uiop:split-string text :separator '(#\Newline))))
             (id (and line (first (uiop:split-string (string-left-trim " " line) :separator " ")))))
        (when id
          (uiop:run-program (list "/opt/X11/bin/xwd" "-id" id "-out"
                                  (format nil "~a/~a.xwd" directory name))
                            :ignore-error-status t))))))

(defun click (pane x y &optional (button clim:+pointer-left-button+))
  "A press and a release of BUTTON at X, Y of PANE's own coordinates."
  (multiple-value-bind (native-x native-y)
      (clim:transform-position (clim:sheet-native-transformation pane) x y)
    (let ((port (clim:port pane)))
      (flet ((event (class)
               (make-instance class :sheet pane :pointer (clim:port-pointer port) :button button
                                    :x native-x :y native-y :modifier-state 0)))
        (clim:queue-event pane (event 'clim:pointer-button-press-event))
        (sleep 0.3)
        (clim:queue-event pane (event 'clim:pointer-button-release-event))
        (sleep 0.7)))))

(defun presentations-of (pane type-name)
  (let ((found '()))
    (labels ((walk (record)
               (when (and (typep record 'clim:presentation)
                          (string= (symbol-name (clim:presentation-type record)) type-name))
                 (push record found))
               (clim:map-over-output-records #'walk record)))
      (walk (clim:stream-output-history pane)))
    (nreverse found)))

(defun click-on (pane record &optional (button clim:+pointer-left-button+))
  (multiple-value-bind (x1 y1 x2 y2) (clim:bounding-rectangle* record)
    (click pane (/ (+ x1 x2) 2) (/ (+ y1 y2) 2) button)))

(defun sheets-of (sheet type)
  (let ((found '()))
    (labels ((walk (sheet)
               (when (typep sheet type)
                 (push sheet found))
               (when (typep sheet 'clim:sheet-parent-mixin)
                 (mapc #'walk (clim:sheet-children sheet)))))
      (walk sheet))
    (nreverse found)))

(defun slot (frame package name)
  (slot-value frame (find-symbol (string name) package)))

(defun run-flight ()
  (let* ((frame (start #'flight:make-flight-frame))
         (form (clim:find-pane-named frame 'almighty-bolt-7guis/flight/mcclim::form)))
    (shot "flight-frame" "flight-before")
    (click-on form (first (presentations-of form "ACCEPT-VALUES-COMMAND-BUTTON")))
    (say "message after a click on Book: ~s"
         (slot frame '#:almighty-bolt-7guis/flight/mcclim :message))
    (shot "flight-frame" "flight-after")))

(defun run-crud ()
  (let* ((frame (start #'crud:make-crud-frame))
         (listing (clim:find-pane-named frame 'almighty-bolt-7guis/crud/mcclim::listing)))
    (shot "crud-frame" "crud-before")
    (click-on listing (second (presentations-of listing "ENTRY")))
    (say "after a click on the second row: picked ~s, name ~s, surname ~s"
         (slot frame '#:almighty-bolt-7guis/crud/mcclim :picked)
         (slot frame '#:almighty-bolt-7guis/crud/mcclim :name)
         (slot frame '#:almighty-bolt-7guis/crud/mcclim :surname))
    (shot "crud-frame" "crud-after")))

(defun run-circles ()
  (let* ((frame (start #'circles:make-circles-frame))
         (canvas (clim:find-pane-named frame 'almighty-bolt-7guis/circles/mcclim::canvas))
         (manager (clim:frame-manager frame)))
    (flet ((circles ()
             (mapcar (lambda (circle)
                       (list (circle:circle-x circle) (circle:circle-y circle)
                             (circle:circle-diameter circle)))
                     (circle:drawing-circles
                      (slot frame '#:almighty-bolt-7guis/circles/mcclim :drawing))))
           (dialogs ()
             (remove-if-not (lambda (other)
                              (and (typep other 'clim:accept-values)
                                   (eq (clim:frame-state other) :enabled)))
                            (clim:frame-manager-frames manager))))
      (click canvas 100 80)
      (click canvas 250 150)
      (say "after two clicks on empty ground: ~s" (circles))
      (shot "circles-frame" "circles-before")
      (click canvas 100 80 clim:+pointer-right-button+)
      (sleep 3)
      (say "after a click of the secondary button on the first circle: ~d dialog~:p"
           (length (dialogs)))
      (let* ((dialog (first (dialogs)))
             (sheet (clim:frame-top-level-sheet dialog))
             (slider (first (sheets-of sheet 'clim:slider)))
             (pane (find-if (lambda (pane) (presentations-of pane "ACCEPT-VALUES-COMMAND-BUTTON"))
                            (sheets-of sheet 'clim:clim-stream-pane))))
        (shot "accept-values" "circles-dialog")
        (setf (clim:gadget-value slider :invoke-callback t) 120)
        (sleep 1)
        (click canvas 300 200)
        (say "a click on the canvas while the dialog is open is queued: ~s" (circles))
        (click-on pane (first (presentations-of pane "ACCEPT-VALUES-COMMAND-BUTTON"))))
      (sleep 3)
      (say "after the slider at 120 and OK: ~s, ~d dialog~:p" (circles) (length (dialogs)))
      (shot "circles-frame" "circles-after")
      ;; The click that the dialog held back drew the third circle; the diameter is one step before.
      (let ((clim:*application-frame* frame))
        (funcall 'almighty-bolt-7guis/circles/mcclim::com-undo)
        (say "after one undo: ~s" (circles))
        (funcall 'almighty-bolt-7guis/circles/mcclim::com-undo)
        (say "after a second undo (the diameter, in one step): ~s" (circles))))))

(let ((app (uiop:getenv "APP")))
  (cond ((equal app "flight") (run-flight))
        ((equal app "crud") (run-crud))
        ((equal app "circles") (run-circles))
        (t (error "APP is flight, crud or circles.")))
  (sb-ext:exit :code 0 :abort t))
