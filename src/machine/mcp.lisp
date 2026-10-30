(defpackage #:almighty-bolt/mcp
  (:use #:cl)
  (:import-from #:almighty-bolt/ui/semantics
                #:node-id
                #:node-role
                #:node-name
                #:node-value
                #:node-state
                #:node-bounds
                #:node-children
                #:walk-nodes)
  (:import-from #:almighty-bolt/machine/host-session
                #:open-session)
  (:local-nicknames (#:json #:com.inuoe.jzon)
                    (#:sockets #:sb-bsd-sockets)
                    (#:m #:almighty-bolt/machine))
  (:export #:start-mcp-server
           #:stop-mcp-server
           #:mcp-server
           #:mcp-server-port
           #:handle-message
           #:outline
           #:base64))

(in-package #:almighty-bolt/mcp)

;;; An MCP server for the app that is open: an agent sees the screen as a tree of things with roles,
;;; names and values, works the app by what things are called, waits for it to answer and looks at it,
;;; through the machine interface (almighty-bolt/machine), which does it with the events a person's
;;; hands would make. MCP's own framing over a pipe is one JSON-RPC message to a line; this is the same
;;; on a socket that listens on 127.0.0.1 only, so a client that wants a pipe has one with netcat:
;;;     command: nc    arguments: 127.0.0.1 PORT
;;; Nothing starts it but a call to START-MCP-SERVER, and anything on this machine that can reach the
;;; port can work the app, which is why it is local and opt-in and has no more to say about it.

(defparameter +protocol-versions+ '("2025-06-18" "2025-03-26" "2024-11-05")
  "The versions of MCP that this speaks, newest first.")

(defparameter +roles+
  '("static-text" "paragraph" "progress-bar" "button" "text-field" "slider" "radio-group"
    "checkbox" "list" "canvas" "overlay" "group"))

;;; --- JSON as hash tables, which is what the library reads and writes ---

(defun obj (&rest pairs)
  "A JSON object from alternating keys (strings) and values."
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on pairs :by #'cddr
          :do (setf (gethash key table) value))
    table))

(defun unnull (value)
  "VALUE, or NIL if it is JSON's null, which the parser reads as the symbol NULL."
  (if (eq value 'null) nil value))

(defun field (object key)
  (and (hash-table-p object) (unnull (gethash key object))))

(defun send (stream object)
  (write-string (json:stringify object) stream)
  (write-char #\Newline stream)
  (finish-output stream))

;;; --- base64, for the image of a screenshot ---

(defun base64 (octets)
  "OCTETS as base64 text."
  (let ((alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
        (out (make-string-output-stream)))
    (loop :for i :from 0 :below (length octets) :by 3
          :do (let* ((remaining (- (length octets) i))
                     (a (aref octets i))
                     (b (if (> remaining 1) (aref octets (+ i 1)) 0))
                     (c (if (> remaining 2) (aref octets (+ i 2)) 0))
                     (word (logior (ash a 16) (ash b 8) c)))
                (write-char (char alphabet (ldb (byte 6 18) word)) out)
                (write-char (char alphabet (ldb (byte 6 12) word)) out)
                (write-char (if (> remaining 1) (char alphabet (ldb (byte 6 6) word)) #\=) out)
                (write-char (if (> remaining 2) (char alphabet (ldb (byte 6 0) word)) #\=) out)))
    (get-output-stream-string out)))

;;; --- the screen as text, for a reader that pays by the word ---

(defun id-text (id)
  "How an id is written in the outline and in a request: a symbol by its lower-case name."
  (if (symbolp id) (string-downcase (symbol-name id)) (princ-to-string id)))

(defun number-text (number)
  (if (and (floatp number) (/= number (ffloor number)))
      (format nil "~,2f" number)
      (format nil "~d" (round number))))

(defun outline (tree)
  "TREE, the semantic tree, as indented text: a line for each thing with its role, name, id, value, state
and where it is (x, y, width, height, in points from the top left of the window). The groups that only
lay things out, with no id and no name, are left out and what is in them is not indented for them."
  (let ((*print-pretty* nil))
    (with-output-to-string (out)
      (labels ((anonymous-group-p (node depth)
                 (and (eq (node-role node) :group) (null (node-id node)) (null (node-name node))
                      (plusp depth)))
               (line (node depth)
                 (if (anonymous-group-p node depth)
                     (dolist (child (node-children node))
                       (line child depth))
                     (progn
                       (fresh-line out)
                       (dotimes (i depth) (write-string "  " out))
                       (format out "~(~a~)" (if (zerop depth) :window (node-role node)))
                       (when (node-name node) (format out " ~s" (node-name node)))
                       (when (node-id node) (format out " id=~a" (id-text (node-id node))))
                       (let ((value (getf node :value)))
                         (when (or value (eq (node-role node) :checkbox))
                           (format out " value=~a" (cond ((eq value t) "true")
                                                         ((null value) "false")
                                                         ((stringp value) (format nil "~s" value))
                                                         ((realp value) (number-text value))
                                                         (t value)))))
                       (loop :for (key state) :on (node-state node) :by #'cddr
                             :do (case key
                                   ((:focused :disabled :invalid :hovered)
                                    (format out " ~(~a~)" key))
                                   ((:caret :first-visible) (format out " ~(~a~)=~a" key state))
                                   (:selected (when state (format out " selected=~a" state)))
                                   (:selection (format out " selection=~a-~a" (first state) (second state)))
                                   ((:minimum :maximum) (format out " ~(~a~)=~a" key (number-text state)))
                                   ((:options :items) (format out " ~(~a~)=~s" key state))))
                       (format out " @~{~a~^,~}" (mapcar #'round (node-bounds node)))
                       (dolist (child (node-children node))
                         (line child (1+ depth)))))))
        (line tree 0)))))

;;; --- the tools ---

(defun target-properties ()
  (obj "id" (obj "type" "string"
                 "description" "The id of the thing, as the snapshot shows it (id=...).")
       "role" (obj "type" "string" "enum" (coerce +roles+ 'vector)
                   "description" "What kind of thing it is.")
       "name" (obj "type" "string"
                   "description" "What it is called: the words on a button, label or check box, or the label that names a field.")))

(defun schema (&key (properties (make-hash-table :test 'equal)) required)
  (obj "type" "object"
       "properties" properties
       "required" (coerce required 'vector)))

(defun merge-properties (&rest tables)
  (let ((all (make-hash-table :test 'equal)))
    (dolist (table tables all)
      (maphash (lambda (key value) (setf (gethash key all) value)) table))))

(defun resolve-id (session text)
  "The id in the tree that is written TEXT, or NIL if none."
  (when text
    (let ((found nil))
      (walk-nodes (lambda (node)
                    (when (and (null found) (node-id node) (string= (id-text (node-id node)) text))
                      (setf found (node-id node))))
                  (m:see session))
      (or found (error 'm:no-such-node
                       :message (format nil "There is nothing on the screen with the id ~a." text))))))

(defun target (session arguments)
  "The keyword arguments for the machine interface that ARGUMENTS name a thing by."
  (let ((id (field arguments "id"))
        (role (field arguments "role"))
        (name (field arguments "name")))
    (when (and role (not (member role +roles+ :test #'string=)))
      (error 'm:cannot :message (format nil "There is no role ~s; the roles are ~{~a~^, ~}." role +roles+)))
    (append (and id (list :id (resolve-id session id)))
            (and role (list :role (intern (string-upcase role) :keyword)))
            (and name (list :name name)))))

(defun required (arguments key)
  (multiple-value-bind (value present) (and (hash-table-p arguments) (gethash key arguments))
    (unless present
      (error 'm:cannot :message (format nil "The argument ~s is needed." key)))
    (unnull value)))

(defun text-result (text)
  (obj "content" (vector (obj "type" "text" "text" text))
       "isError" nil))

(defun error-result (text)
  (obj "content" (vector (obj "type" "text" "text" text))
       "isError" t))

(defun after (session what)
  "What an action answers: that it was done, and the screen as it is now."
  (text-result (format nil "~a~%~%~a" what (outline (m:see session)))))

(defparameter *tools*
  ;; (name description schema function). The function takes the session and the arguments.
  (list
   (list "snapshot"
         "The screen as a tree of things: for each its role, name, id, value, state and where it is (x, y, width, height in points from the top left). Look at this first, and again after you have done something."
         (schema)
         (lambda (session arguments)
           (declare (ignore arguments))
           (text-result (outline (m:see session)))))
   (list "click"
         "Press and release the primary button in the middle of a thing, as a person would. Name the thing by its id, or by its role and name."
         (schema :properties (target-properties))
         (lambda (session arguments)
           (apply #'m:click session (target session arguments))
           (after session "Clicked.")))
   (list "double_click"
         "Click a thing twice in a row, the second counted as a double click."
         (schema :properties (target-properties))
         (lambda (session arguments)
           (apply #'m:double-click session (target session arguments))
           (after session "Double-clicked.")))
   (list "type_text"
         "Type text, a key for each character, into the thing that has the keyboard, or, if you name a thing, into that after pressing it to give it the keyboard. Replaces nothing: use set_value to replace what a field holds."
         (schema :properties (merge-properties
                              (obj "text" (obj "type" "string" "description" "What to type."))
                              (target-properties))
                 :required '("text"))
         (lambda (session arguments)
           (apply #'m:type-text session (required arguments "text") (target session arguments))
           (after session "Typed.")))
   (list "press_key"
         "Press a key, such as Enter, Tab, Escape, Backspace, Delete, ArrowLeft, ArrowRight, ArrowUp, ArrowDown, Home, End, PageUp, PageDown, Space, or a letter, with modifiers held down (for example key a with super selects all in a field)."
         (schema :properties (obj "key" (obj "type" "string" "description" "The key's name, or one character.")
                                  "modifiers" (obj "type" "array"
                                                   "items" (obj "type" "string"
                                                                "enum" (vector "shift" "control" "alt" "super"))
                                                   "description" "Modifier keys held down while it is pressed."))
                 :required '("key"))
         (lambda (session arguments)
           (m:press-key session (required arguments "key")
                        :modifiers (coerce (or (field arguments "modifiers") #()) 'list))
           (after session "Pressed.")))
   (list "set_value"
         "Make a thing hold a value, by doing what a person would: for a text field select what is there and type the value (an empty string deletes it); for a check box press it if it is not as wanted (true or false); for a slider press where the value is (a number in its range); for a choice press the option (its text or index); for a list pick the item (its text or index)."
         (schema :properties (merge-properties
                              (obj "value" (obj "description" "The value: a string, a number or true/false."))
                              (target-properties))
                 :required '("value"))
         (lambda (session arguments)
           (let ((value (required arguments "value")))
             (apply #'m:set-value session value (target session arguments)))
           (after session "Set.")))
   (list "scroll"
         "Turn the mouse wheel over a thing, by lines; a negative dy shows what is further down."
         (schema :properties (merge-properties
                              (obj "dy" (obj "type" "number" "description" "Lines, negative to see what is below.")
                                   "dx" (obj "type" "number"))
                              (target-properties))
                 :required '("dy"))
         (lambda (session arguments)
           (apply #'m:scroll session :dy (required arguments "dy") :dx (or (field arguments "dx") 0)
                                       (target session arguments))
           (after session "Scrolled.")))
   (list "wait_idle"
         "Wait until the app has answered everything it was given and nothing more is going to be drawn. The other tools already wait; use this after something that takes time of its own, such as an animation."
         (schema)
         (lambda (session arguments)
           (declare (ignore arguments))
           (m:wait session)
           (after session "The app is idle.")))
   (list "screenshot"
         "A picture of the window as it is now, as a PNG."
         (schema)
         (lambda (session arguments)
           (declare (ignore arguments))
           (m:wait session)
           (obj "content" (vector (obj "type" "image"
                                       "data" (base64 (m:screenshot session))
                                       "mimeType" "image/png"))
                "isError" nil)))))

(defun tool-list ()
  (coerce (loop :for (name description schema) :in *tools*
                :collect (obj "name" name "description" description "inputSchema" schema))
          'vector))

(defun call-tool (session-function name arguments)
  "The result of the tool NAME with ARGUMENTS: its text or image, or, for what cannot be done, an error
result that says why and that an agent can read and go on from."
  (let ((tool (find name *tools* :key #'first :test #'string=)))
    (if (null tool)
        (error-result (format nil "There is no tool ~s; the tools are ~{~a~^, ~}." name (mapcar #'first *tools*)))
        (handler-case (funcall (fourth tool) (funcall session-function) arguments)
          (m:machine-error (condition) (error-result (m:machine-error-message condition)))
          (error (condition)
            (error-result (format nil "Something went wrong: ~a" condition)))))))

;;; --- JSON-RPC ---

(defun rpc-error (id code message)
  (obj "jsonrpc" "2.0" "id" (or id 'null) "error" (obj "code" code "message" message)))

(defun rpc-result (id result)
  (obj "jsonrpc" "2.0" "id" id "result" result))

(defun handle-request (session-function message)
  "The response to the request MESSAGE (a table), or NIL if it is a notification."
  (let ((id (field message "id"))
        (method (field message "method"))
        (params (field message "params")))
    (cond
      ((not (stringp method))
       (and id (rpc-error id -32600 "A request has a method.")))
      ((string= method "initialize")
       (let* ((asked (field params "protocolVersion"))
              (version (if (member asked +protocol-versions+ :test #'equal)
                           asked
                           (first +protocol-versions+))))
         (rpc-result id (obj "protocolVersion" version
                             "capabilities" (obj "tools" (obj "listChanged" nil))
                             "serverInfo" (obj "name" "almighty-bolt"
                                               "version" (or (asdf:component-version
                                                              (asdf:find-system "almighty-bolt"))
                                                             "0"))
                             "instructions" "This is the window of a Lisp GUI app. Call snapshot to see it as a tree, then click, type_text, press_key, set_value or scroll by naming things by id or by role and name. Every action answers with the screen as it is afterwards."))))
      ((string= method "ping") (rpc-result id (obj)))
      ((string= method "tools/list") (rpc-result id (obj "tools" (tool-list))))
      ((string= method "tools/call")
       (let ((name (field params "name")))
         (if (stringp name)
             (rpc-result id (call-tool session-function name (or (field params "arguments") (obj))))
             (rpc-error id -32602 "A tool call has a name."))))
      ((and (>= (length method) 14) (string= (subseq method 0 14) "notifications/"))
       nil)
      (id (rpc-error id -32601 (format nil "There is no method ~a." method)))
      (t nil))))

(defun handle-message (session-function line)
  "The text of the response to the JSON-RPC message LINE, or NIL if there is none (a notification)."
  (let ((message (handler-case (json:parse line)
                   (error () :invalid))))
    (cond ((eq message :invalid)
           (json:stringify (rpc-error nil -32700 "That is not JSON.")))
          ((not (hash-table-p message))
           (json:stringify (rpc-error nil -32600 "A message is a JSON object.")))
          (t (let ((response (handle-request session-function message)))
               (and response (json:stringify response)))))))

;;; --- the socket ---

(defstruct (mcp-server (:constructor %make-mcp-server))
  socket
  port
  thread
  (connections '())
  (lock (sb-thread:make-mutex :name "mcp server"))
  session-function)

(defun serve-connection (server stream)
  (unwind-protect
       (handler-case
           (loop :for line = (read-line stream nil nil)
                 :while line
                 :do (when (plusp (length (string-trim '(#\Space #\Tab #\Return) line)))
                       (let ((response (handle-message (mcp-server-session-function server) line)))
                         (when response
                           (write-string response stream)
                           (write-char #\Newline stream)
                           (finish-output stream)))))
         ;; A connection that goes away, or is closed under this thread by STOP-MCP-SERVER, ends
         ;; this connection and nothing else; SBCL signals a plain error for the second.
         (error () nil))
    (ignore-errors (close stream))))

(defun accept-loop (server)
  (loop
    (let ((connection (handler-case (sockets:socket-accept (mcp-server-socket server))
                        (error () (return)))))
      (let ((stream (sockets:socket-make-stream connection :input t :output t
                                                           :element-type 'character
                                                           :external-format :utf-8
                                                           :buffering :line)))
        (sb-thread:with-mutex ((mcp-server-lock server))
          (push connection (mcp-server-connections server)))
        (sb-thread:make-thread (lambda () (serve-connection server stream))
                               :name "mcp connection")))))

(defvar *server* nil "The server that START-MCP-SERVER started and STOP-MCP-SERVER has not stopped.")

(defun start-mcp-server (&key (port 0) (session-function #'open-session) (announce t))
  "Start an MCP server for the app that is open, on PORT of 127.0.0.1 (0, the default, takes a free
one), and return it. SESSION-FUNCTION, called for each tool call, returns the session to work on (the
window that OPEN-APP or RUN-APP opened, by default). An MCP client that wants a pipe runs
nc 127.0.0.1 PORT. Anything on this machine that can connect can work the app."
  (when *server*
    (error "An MCP server is already running, on port ~d: stop it first." (mcp-server-port *server*)))
  (let ((socket (make-instance 'sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sockets:sockopt-reuse-address socket) t)
    (sockets:socket-bind socket (sockets:make-inet-address "127.0.0.1") port)
    (sockets:socket-listen socket 8)
    (let ((server (%make-mcp-server :socket socket
                                    :port (nth-value 1 (sockets:socket-name socket))
                                    :session-function session-function)))
      (setf (mcp-server-thread server)
            (sb-thread:make-thread (lambda () (accept-loop server)) :name "mcp listener"))
      (setf *server* server)
      (when announce
        (format t "~&MCP server on 127.0.0.1:~d. For an MCP client that wants a pipe: command nc, arguments 127.0.0.1 ~:*~d~%"
                (mcp-server-port server)))
      server)))

(defun stop-mcp-server (&optional (server *server*))
  "Stop SERVER (the one that is running, by default): no new connections, and the ones open are closed."
  (when server
    (ignore-errors (sockets:socket-close (mcp-server-socket server)))
    (sb-thread:with-mutex ((mcp-server-lock server))
      (dolist (connection (mcp-server-connections server))
        (ignore-errors (sockets:socket-close connection)))
      (setf (mcp-server-connections server) '()))
    (when (eq server *server*)
      (setf *server* nil)))
  nil)
