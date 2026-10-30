(defpackage #:almighty-bolt-7guis/t/acceptance/mcp
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:d #:almighty-bolt-7guis/t/acceptance/driver)
                    (#:i #:almighty-bolt/internals)
                    (#:m #:almighty-bolt/machine)
                    (#:mcp #:almighty-bolt/mcp)
                    (#:json #:com.inuoe.jzon)
                    (#:sockets #:sb-bsd-sockets)))

(in-package #:almighty-bolt-7guis/t/acceptance/mcp)

;;; An MCP client of the app, as an agent's host is one: a socket to 127.0.0.1, one JSON-RPC message
;;; to a line each way. The server works a CRUD that a test session drives, so nothing here is a window,
;;; and everything else is what goes over the wire.

(defvar *next-id* 0)

(defun connect (port)
  (let ((socket (make-instance 'sockets:inet-socket :type :stream :protocol :tcp)))
    (sockets:socket-connect socket (sockets:make-inet-address "127.0.0.1") port)
    (sockets:socket-make-stream socket :input t :output t :element-type 'character
                                       :external-format :utf-8 :buffering :line)))

(defun say (stream line)
  (write-string line stream)
  (write-char #\Newline stream)
  (finish-output stream))

(defun hear (stream)
  "The next message from the server, parsed, or NIL if nothing comes within three seconds."
  (let ((line (handler-case (sb-ext:with-timeout 3 (read-line stream nil nil))
                (sb-ext:timeout () nil))))
    (and line (json:parse line))))

(defun hears-the-end (stream)
  "True if the server has closed its end of STREAM: the end of the input comes within three seconds,
and nothing else does."
  (handler-case (eq :end (sb-ext:with-timeout 3 (read-line stream nil :end)))
    (sb-ext:timeout () nil)))

(defun field (object &rest keys)
  "What is inside OBJECT, parsed JSON, along KEYS: a string for a member of an object, an index for an
element of an array."
  (dolist (key keys object)
    (setf object (cond ((and (hash-table-p object) (stringp key)) (gethash key object))
                       ((and (vectorp object) (not (stringp object)) (integerp key)
                             (< -1 key (length object)))
                        (aref object key))
                       (t nil)))))

(defun request (stream method &optional params)
  "Send a request and return its response."
  (let ((id (incf *next-id*))
        (message (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" message) "2.0" (gethash "id" message) id (gethash "method" message) method)
    (when params (setf (gethash "params" message) params))
    (say stream (json:stringify message))
    (let ((response (hear stream)))
      (assert-eql id (field response "id") method)
      response)))

(defun table (&rest pairs)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on pairs :by #'cddr :do (setf (gethash key table) value))
    table))

(defun call (stream tool &rest pairs)
  "The result of the tool TOOL given the arguments PAIRS: the text, and whether it is an error."
  (let* ((response (request stream "tools/call" (table "name" tool "arguments" (apply #'table pairs))))
         (result (field response "result")))
    (values (field result "content" 0 "text") (field result "isError"))))

(defmacro with-client ((stream) &body body)
  "Run BODY with STREAM connected to a server that works a CRUD."
  `(let* ((driver (d:start :immediate :crud))
          (server (mcp:start-mcp-server :announce nil
                                        :session-function (lambda () (d:driver-session driver))))
          (,stream (connect (mcp:mcp-server-port server))))
     (unwind-protect (progn ,@body)
       (ignore-errors (close ,stream))
       (mcp:stop-mcp-server server))))

(define-test the-server-says-hello-and-lists-its-tools ()
  (with-client (stream)
    (let ((response (request stream "initialize"
                             (table "protocolVersion" "2025-06-18"
                                    "capabilities" (table)
                                    "clientInfo" (table "name" "a test" "version" "1")))))
      (assert-equal "2025-06-18" (field response "result" "protocolVersion"))
      (assert-equal "almighty-bolt" (field response "result" "serverInfo" "name"))
      (assert-true (field response "result" "capabilities" "tools")))
    ;; A notification gets no answer: the next thing heard is the answer to the next request.
    (say stream (json:stringify (table "jsonrpc" "2.0" "method" "notifications/initialized")))
    (assert-true (field (request stream "ping") "result"))
    (let* ((tools (field (request stream "tools/list") "result" "tools"))
           (names (map 'list (lambda (tool) (field tool "name")) tools)))
      (dolist (wanted '("snapshot" "click" "double_click" "type_text" "press_key" "set_value" "scroll"
                        "wait_idle" "screenshot"))
        (assert-true (member wanted names :test #'string=) wanted))
      (assert-equal "object" (field (aref tools 0) "inputSchema" "type"))
      (assert-true (every (lambda (tool) (stringp (field tool "description"))) tools)))
    ;; A version it does not know is answered with one it does.
    (assert-equal "2025-06-18"
                  (field (request stream "initialize" (table "protocolVersion" "1999-01-01"))
                         "result" "protocolVersion"))))

(define-test an-agent-adds-ada-lovelace-over-the-wire ()
  (with-client (stream)
    (let ((text (call stream "snapshot")))
      (assert-true (search "text-field \"Name:\"" text) text)
      (assert-true (search "button \"Create\"" text) text)
      (assert-true (search "list" text) text))
    (call stream "set_value" "role" "text-field" "name" "Name:" "value" "Ada")
    (call stream "set_value" "role" "text-field" "name" "Surname:" "value" "Lovelace")
    (let ((text (call stream "click" "role" "button" "name" "Create")))
      ;; Every action answers with the screen as it is afterwards.
      (assert-true (search "Lovelace, Ada" text) text)
      (assert-true (search "Clicked." text) text))
    (let ((text (call stream "snapshot")))
      (assert-true (search "\"Lovelace, Ada\"" text) text))))

(defun id-shown (text role name)
  "The id that the snapshot TEXT shows for the ROLE called NAME, as an agent reading it would take it."
  (let* ((marker (format nil "~a ~s id=" role name))
         (found (search marker text)))
    (when found
      (let ((start (+ found (length marker))))
        (subseq text start (position #\Space text :start start))))))

(define-test an-agent-names-things-by-id-and-types-with-the-keyboard ()
  (with-client (stream)
    ;; The id is read out of the snapshot, as an agent does, and given back.
    (let ((id (id-shown (call stream "snapshot") "text-field" "Name:")))
      (assert-equal "name" id)
      (call stream "type_text" "text" "Zed" "id" id))
    (let ((text (call stream "snapshot")))
      (assert-true (search "value=\"Zed\"" text) text)
      (assert-true (search "focused" text) text))
    (call stream "press_key" "key" "a" "modifiers" (vector "super"))
    (call stream "press_key" "key" "Backspace")
    (assert-true (search "value=\"\"" (call stream "snapshot")))))

(define-test what-goes-wrong-is-an-error-result-an-agent-can-read-and-the-server-goes-on ()
  (with-client (stream)
    (flet ((error-text (tool &rest pairs)
             (multiple-value-bind (text error-p) (apply #'call stream tool pairs)
               (assert-true error-p tool)
               text)))
      (assert-true (search "nothing on the screen" (error-text "click" "role" "button" "name" "Fly")))
      (assert-true (search "nothing on the screen with the id" (error-text "click" "id" "no-such-id")))
      (assert-true (search "disabled" (error-text "click" "role" "button" "name" "Delete")))
      (assert-true (search "2 things match" (error-text "click" "name" "Name:")))
      (assert-true (search "needed" (error-text "type_text")))
      (assert-true (search "There is no role" (error-text "click" "role" "dial")))
      (assert-true (search "There is no tool" (error-text "teleport")))
      (assert-true (search "screenshot" (error-text "screenshot")))
      ;; What nobody foresaw is an error result too, not a connection that goes quiet.
      (assert-true (search "Something went wrong" (error-text "type_text" "text" 5))))
    ;; And it still answers.
    (assert-true (search "text-field" (call stream "snapshot")))))

(define-test what-is-not-a-request-is-answered-as-json-rpc-says ()
  (with-client (stream)
    (say stream "this is not json")
    (assert-eql -32700 (field (hear stream) "error" "code"))
    (say stream "[1,2]")
    (assert-eql -32600 (field (hear stream) "error" "code"))
    (assert-eql -32601 (field (request stream "resources/list") "error" "code"))
    (say stream "")
    (assert-true (field (request stream "ping") "result"))))

(define-test every-tool-makes-the-events-of-what-a-person-would-do ()
  ;; The session is wrapped to write down what is delivered to the app and how often it is waited on,
  ;; so that each tool can be seen to make the events of what a person would do.
  (let* ((driver (d:start :immediate :crud))
         (events '())
         (settled 0)
         (server (mcp:start-mcp-server
                  :announce nil
                  :session-function
                  (lambda ()
                    (let ((session (d:driver-session driver)))
                      (m:make-session :variant (m:session-variant session)
                                      :deliver (lambda (event)
                                                 (push event events)
                                                 (funcall (m:session-deliver session) event))
                                      :settle (lambda ()
                                                (incf settled)
                                                (funcall (m:session-settle session)))
                                      :on-main (m:session-on-main session))))))
         (stream (connect (mcp:mcp-server-port server))))
    (flet ((delivered (&rest pairs)
             (setf events '())
             (apply #'call stream pairs)
             (reverse events)))
      (unwind-protect
           (progn
             (let ((presses (remove-if-not (lambda (event)
                                             (and (typep event 'i:pointer-event)
                                                  (eq (i:pointer-event-kind event) :down)))
                                           (delivered "double_click" "role" "list"))))
               (assert-equal '(1 2) (mapcar #'i:pointer-event-click-count presses)))
             (let ((wheels (remove-if-not (lambda (event) (typep event 'i:wheel-event))
                                          (delivered "scroll" "role" "list" "dy" -3 "dx" 2))))
               (assert-equal 1 (length wheels))
               (assert-equal '(-3d0 2d0) (list (i:wheel-event-dy (first wheels))
                                               (i:wheel-event-dx (first wheels)))))
             (let ((keys (delivered "press_key" "key" "a" "modifiers" (vector "super" "shift"))))
               (assert-equal 1 (length keys))
               (assert-equal #\a (i:key-event-logical-key (first keys)))
               (assert-equal (logior i:+modifier-super+ i:+modifier-shift+)
                             (i:key-event-modifiers (first keys))))
             (let ((keys (delivered "type_text" "text" "ok")))
               (assert-equal '(#\o #\k) (mapcar #'i:key-event-logical-key keys)))
             (let ((before settled))
               (assert-equal '() (delivered "wait_idle"))
               (assert-true (> settled before))))
        (ignore-errors (close stream))
        (mcp:stop-mcp-server server)))))

(define-test a-screenshot-comes-as-an-image-block-of-base64-png ()
  ;; The driver has no window to look at, so the session is given a picture: the eight bytes that
  ;; begin every PNG, and the same cut at seven and extended to nine, which are the three ways that
  ;; base64 ends (two padding signs, one, none).
  (let* ((driver (d:start :immediate :crud))
         (picture #(137 80 78 71 13 10 26 10 0))
         (length 8)
         (server (mcp:start-mcp-server
                  :announce nil
                  :session-function
                  (lambda ()
                    (let ((session (d:driver-session driver)))
                      (m:make-session :variant (m:session-variant session)
                                      :deliver (m:session-deliver session)
                                      :settle (m:session-settle session)
                                      :on-main (m:session-on-main session)
                                      :screenshot (lambda () (subseq picture 0 length)))))))
         (stream (connect (mcp:mcp-server-port server))))
    (unwind-protect
         (flet ((shot ()
                  (let* ((response (request stream "tools/call" (table "name" "screenshot" "arguments" (table))))
                         (block (field response "result" "content" 0)))
                    (assert-false (field response "result" "isError"))
                    (assert-equal "image" (field block "type"))
                    (assert-equal "image/png" (field block "mimeType"))
                    (field block "data"))))
           (setf length 8)
           (assert-equal "iVBORw0KGgo=" (shot))
           (setf length 7)
           (assert-equal "iVBORw0KGg==" (shot))
           (setf length 9)
           (assert-equal "iVBORw0KGgoA" (shot)))
      (ignore-errors (close stream))
      (mcp:stop-mcp-server server))))

(define-test the-server-listens-on-this-machine-only-and-stopping-it-closes-everything ()
  (let* ((driver (d:start :immediate :crud))
         (server (mcp:start-mcp-server :announce nil
                                       :session-function (lambda () (d:driver-session driver))))
         (port (mcp:mcp-server-port server))
         (stream (connect port)))
    (unwind-protect
         (progn
           (assert-equalp #(127 0 0 1) (sockets:socket-name (mcp::mcp-server-socket server)))
           (assert-true (field (request stream "ping") "result"))
           (mcp:stop-mcp-server server)
           ;; The client that was connected is let go, and nobody can connect any more.
           (assert-true (hears-the-end stream))
           (assert-true (null (ignore-errors (connect port)))))
      (ignore-errors (close stream))
      (mcp:stop-mcp-server server))
    ;; And a new server can be started in its place.
    (let ((again (mcp:start-mcp-server :announce nil
                                       :session-function (lambda () (d:driver-session driver)))))
      (unwind-protect
           (let ((stream (connect (mcp:mcp-server-port again))))
             (assert-true (field (request stream "ping") "result"))
             (close stream))
        (mcp:stop-mcp-server again)))))

(define-test several-clients-at-once-each-get-their-own-answers ()
  (let* ((driver (d:start :immediate :crud))
         (server (mcp:start-mcp-server :announce nil
                                       :session-function (lambda () (d:driver-session driver))))
         (a (connect (mcp:mcp-server-port server)))
         (b (connect (mcp:mcp-server-port server))))
    (unwind-protect
         (progn
           (assert-true (field (request a "ping") "result"))
           (assert-true (field (request b "ping") "result"))
           (call a "set_value" "role" "text-field" "name" "Name:" "value" "Ada")
           (assert-true (search "value=\"Ada\"" (call b "snapshot"))))
      (ignore-errors (close a))
      (ignore-errors (close b))
      (mcp:stop-mcp-server server))))
