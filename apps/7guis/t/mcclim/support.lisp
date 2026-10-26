(defpackage #:almighty-bolt-7guis/t/mcclim/support
  (:use #:cl)
  (:import-from #:clim-internals
                #:find-av-query
                #:find-av-button
                #:av-change-query
                #:av-execute-action
                #:av-command-button-action
                #:av-command-button-resynchronize
                #:query-invalid-p
                #:av-query-ink)
  (:import-from #:almighty-bolt-stress/t/mcclim/support
                #:await)
  (:export #:type-into
           #:query-present-p
           #:press-button
           #:button-offered-p
           #:query-invalid-p
           #:query-ink
           #:click
           #:close-frame
           #:run-command
           #:gadgets-in
           #:await))

(in-package #:almighty-bolt-7guis/t/mcclim/support)

;;; What the tests of the McCLIM 7GUIs apps need beyond the stress apps' support. McCLIM's null port
;;; has no keyboard focus and no text metrics, so a person's typing into a field of an ACCEPT-VALUES
;;; pane is done by what the input editor and the field gadgets do when they commit it,
;;; AV-CHANGE-QUERY, and a click on one of its command buttons by what the click's action calls,
;;; AV-EXECUTE-ACTION; the click of the pointer is a real pointer event, through the frame's loop,
;;; for the apps that draw.

(defun type-into (pane identifier text)
  "Give the field of PANE that is asked for under IDENTIFIER (its prompt, unless the app gives it
another) the text TEXT, as the person committing it."
  (av-change-query (find-av-query identifier pane) text))

(defun query-present-p (pane identifier)
  (and (find-av-query identifier pane) t))

(defun press-button (pane prompt)
  (let ((button (find-av-button prompt pane)))
    (av-execute-action pane (av-command-button-action button)
                       (av-command-button-resynchronize button))))

(defun button-offered-p (pane prompt)
  (and (find-av-button prompt pane) t))

(defun query-ink (pane identifier)
  (av-query-ink (find-av-query identifier pane)))

(defun click (pane x y &key (button clim:+pointer-left-button+))
  "Press and release BUTTON at X, Y of PANE's own coordinates, as events through the frame's loop."
  (multiple-value-bind (native-x native-y)
      (clim:transform-position (clim:sheet-native-transformation pane) x y)
    (let ((port (clim:port pane)))
      (flet ((event (class)
               (make-instance class :sheet pane :pointer (clim:port-pointer port) :button button
                                    :x native-x :y native-y :modifier-state 0)))
        (clim:queue-event pane (event 'clim:pointer-button-press-event))
        (sleep 0.2)
        (clim:queue-event pane (event 'clim:pointer-button-release-event))
        (sleep 0.5)))))

(defun run-command (frame name &rest arguments)
  "Call the command function NAME with ARGUMENTS in this thread, with FRAME as the frame, and
redisplay its panes as the frame's loop does after a command (each by its own :DISPLAY-TIME).
(EXECUTE-FRAME-COMMAND from another thread has the frame's own thread do it, a moment later, and
that thread then redisplays at the same time as a test.)"
  (let ((clim:*application-frame* frame))
    (apply name arguments))
  (clim:redisplay-frame-panes frame))

(defun close-frame (frame)
  "Close FRAME's window, as the window manager's close does: the frame exits in the thread that
runs it (an own-window dialog is run by the thread of the frame that opened it)."
  (let ((sheet (clim:frame-top-level-sheet frame)))
    (clim:queue-event sheet (make-instance 'clim:window-manager-delete-event :sheet sheet))))

(defun gadgets-in (sheet type)
  "The gadgets of TYPE in SHEET and under it."
  (let ((found '()))
    (labels ((walk (sheet)
               (when (typep sheet type)
                 (push sheet found))
               (when (typep sheet 'clim:sheet-parent-mixin)
                 (mapc #'walk (clim:sheet-children sheet)))))
      (walk sheet))
    (nreverse found)))
