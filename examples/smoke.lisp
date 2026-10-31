;;;; Check, in a real window, what a program that uses only the public package gets from RUN-APP: the
;;;; counter example is run on the main thread, used, redefined while it runs (as compiling it from an
;;;; editor does), and closed, which ends the process. From the repository root:
;;;;   make examples-smoke
;;;; It says "The run-app check passed" when every check holds.

(asdf:load-system "almighty-bolt-examples")
(asdf:load-system "almighty-bolt/mcp")

(defpackage #:almighty-bolt-examples/smoke
  (:use #:cl)
  (:import-from #:almighty-bolt
                #:run-app
                #:state)
  (:import-from #:almighty-bolt/machine/host-session
                #:open-session)
  (:local-nicknames (#:m #:almighty-bolt/machine)
                    (#:s #:almighty-bolt/ui/semantics)
                    (#:mcp #:almighty-bolt/mcp)
                    (#:json #:com.inuoe.jzon)
                    (#:sockets #:sb-bsd-sockets))
  (:import-from #:almighty-bolt/internals
                #:lookup-target
                #:frame-ring-total
                #:make-pointer-event)
  (:import-from #:almighty-bolt/platform/macos/main-queue
                #:call-on-main)
  (:import-from #:almighty-bolt/platform/macos/host
                #:host-variant
                #:host-ring
                #:host-deliver-event
                #:host-idle-p
                #:wait-for-idle
                #:close-host-window))

(in-package #:almighty-bolt-examples/smoke)

(defvar *failures* 0)

(defun check (what ok &optional detail)
  (unless ok (incf *failures*))
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ (~a)~]~%" ok what detail)
  (finish-output)
  ok)

(defun wait-for (predicate seconds)
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop
      (when (funcall predicate) (return t))
      (when (> (get-internal-real-time) end) (return nil))
      (sleep 0.01))))

(defun present-p (host id)
  (and (lookup-target (host-variant host) id) t))

(defun deliver (host event)
  ;; No pause after it: the events of a click arrive within a millisecond of each other, and the
  ;; ui takes one change of the button a frame, so the count is behind until the host is idle.
  (call-on-main (lambda () (host-deliver-event host event))))

(defun click (host id)
  (multiple-value-bind (x y w h) (lookup-target (host-variant host) id)
    (let ((px (float (+ x (/ w 2)) 1d0)) (py (float (+ y (/ h 2)) 1d0)))
      (deliver host (make-pointer-event :timestamp 0 :kind :move :x px :y py :button nil
                                        :buttons 0 :click-count 0))
      (deliver host (make-pointer-event :timestamp 0 :kind :down :x px :y py :button 0
                                        :buttons 1 :click-count 1))
      (deliver host (make-pointer-event :timestamp 0 :kind :up :x px :y py :button 0
                                        :buttons 0 :click-count 1)))))

(defun the-count (host)
  (declare (ignore host))
  (state 'almighty-bolt-examples/counter::count))

(defun edited-counter ()
  "What compiling the example again, with a label added, does."
  (eval '(defun almighty-bolt-examples/counter:make-counter ()
          (almighty-bolt:state-let ((almighty-bolt-examples/counter::count 0))
            (lambda ()
              (almighty-bolt:padding (:all 24)
                (almighty-bolt:row (:gap 16 :cross-align :center)
                  (almighty-bolt:label (princ-to-string almighty-bolt-examples/counter::count)
                                       :id :count :size 32)
                  (almighty-bolt:label "probe" :id :probe)
                  (when (almighty-bolt:button "Count" :id :count-button)
                    (incf almighty-bolt-examples/counter::count)))))))))

(defun png-size (octets)
  "The width and height in the header of a PNG, or NIL if OCTETS is not one."
  (and (> (length octets) 24)
       (equalp (subseq octets 0 8) #(137 80 78 71 13 10 26 10))
       (flet ((u32 (at) (reduce (lambda (a b) (+ (* a 256) b)) (subseq octets at (+ at 4)))))
         (list (u32 16) (u32 20)))))

(defun temporary-png (octets)
  "A file of OCTETS in the temporary directory, for a program that reads files."
  (let ((path (merge-pathnames (format nil "almighty-bolt-smoke-~d-~d.png" (get-universal-time) (random 100000))
                               (uiop:temporary-directory))))
    (with-open-file (out path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    path))

(defun size-the-system-reads (octets)
  "The width and height of the PNG OCTETS as macOS reads them (sips decodes the image, which the header
alone does not show), or NIL if it cannot."
  (let ((path (temporary-png octets)))
    (unwind-protect
         (let ((text (uiop:run-program (list "sips" "-g" "pixelWidth" "-g" "pixelHeight" (namestring path))
                                       :output :string :ignore-error-status t)))
           (flet ((value (key)
                    (let ((at (search key text)))
                      (and at (parse-integer text :start (+ at (length key)) :junk-allowed t)))))
             (let ((width (value "pixelWidth:")) (height (value "pixelHeight:")))
               (and width height (list width height)))))
      (ignore-errors (delete-file path)))))

(defun through-the-machine-interface ()
  "The same window worked as an agent works it: seen as a tree, pressed by what things are called,
waited for, and photographed."
  (let* ((session (open-session))
         (before (s:node-name (m:find-node session :role :static-text :id :count))))
    (check "the tree has the button, called what it says"
           (m:find-node session :role :button :name "Count"))
    (check "and the count, as text" (stringp before) before)
    (m:click session :role :button :name "Count")
    (let ((after (s:node-name (m:find-node session :role :static-text :id :count))))
      (check "pressing the button by its name counts, and waiting makes the tree true at once"
             (equal after (princ-to-string (1+ (parse-integer before)))) (list before after)))
    (let* ((png (m:screenshot session))
           (size (png-size png)))
      (check "a screenshot is a PNG of the window's size, twice over (the scale)"
             (and size (= (first size) (* 2 360)) (= (second size) (* 2 160))) size)
      (check "that macOS can decode, and that is not megabytes"
             (and (equal size (size-the-system-reads png)) (< (length png) 100000))
             (list (size-the-system-reads png) (length png)))
      (let ((path (uiop:getenv "SMOKE_SCREENSHOT")))
        (when (and path (plusp (length path)))
          (with-open-file (out path :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
            (write-sequence png out)))))))

(defun connect (port)
  (let ((socket (make-instance 'sockets:inet-socket :type :stream :protocol :tcp)))
    (sockets:socket-connect socket (sockets:make-inet-address "127.0.0.1") port)
    (sockets:socket-make-stream socket :input t :output t :element-type 'character
                                       :external-format :utf-8 :buffering :line)))

(defvar *request-id* 0)

(defun rpc (stream method &optional params)
  "The parsed response of the server on STREAM to a request for METHOD with PARAMS (a table)."
  (let ((message (make-hash-table :test 'equal)))
    (setf (gethash "jsonrpc" message) "2.0"
          (gethash "id" message) (incf *request-id*)
          (gethash "method" message) method)
    (when params (setf (gethash "params" message) params))
    (write-line (json:stringify message) stream)
    (finish-output stream)
    (json:parse (sb-ext:with-timeout 20 (read-line stream)))))

(defun table (&rest pairs)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for (key value) :on pairs :by #'cddr :do (setf (gethash key table) value))
    table))

(defun call-tool (stream tool &rest arguments)
  "The first block of the result of TOOL on STREAM: its hash table."
  (let ((response (rpc stream "tools/call" (table "name" tool "arguments" (apply #'table arguments)))))
    (aref (gethash "content" (gethash "result" response)) 0)))

(defun decoded (base64-text)
  "The octets of BASE64-TEXT, decoded by the operating system's own base64."
  (let ((path (merge-pathnames (format nil "almighty-bolt-smoke-~d.png" (get-universal-time))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (uiop:run-program '("base64" "-D") :input (make-string-input-stream base64-text)
                                              :output path :if-output-exists :supersede)
           (with-open-file (in path :element-type '(unsigned-byte 8))
             (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
               (read-sequence octets in)
               octets)))
      (ignore-errors (delete-file path)))))

(defun through-the-mcp-server ()
  "The same window worked as an agent's host works it: over a socket, in JSON-RPC, and, for a client
that wants a pipe, through netcat."
  (let* ((server (mcp:start-mcp-server :announce nil))
         (port (mcp:mcp-server-port server)))
    (unwind-protect
         (let* ((stream (connect port))
                (hello (rpc stream "initialize"
                            (table "protocolVersion" "2025-06-18" "capabilities" (table)
                                   "clientInfo" (table "name" "smoke" "version" "1"))))
                (before (the-count nil)))
           (check "the server says what it is"
                  (equal "almighty-bolt" (gethash "name" (gethash "serverInfo" (gethash "result" hello)))))
           (let ((text (gethash "text" (call-tool stream "snapshot"))))
             (check "the snapshot has the button by its role, name and id"
                    (search "button \"Count\" id=count-button" text) text))
           (let ((text (gethash "text" (call-tool stream "click" "role" "button" "name" "Count"))))
             (check "a click by name counts, and the answer is the screen afterwards"
                    (and (eql (1+ before) (the-count nil))
                         (search (format nil "static-text ~s id=count" (princ-to-string (1+ before))) text))
                    (list before text)))
           (let* ((block (call-tool stream "screenshot"))
                  (png (decoded (gethash "data" block)))
                  (size (png-size png)))
             (check "a screenshot is an image block of a PNG that the system's base64 reads"
                    (and (equal "image" (gethash "type" block))
                         (equal "image/png" (gethash "mimeType" block))
                         (equal '(720 320) size)
                         (equal size (size-the-system-reads png)))
                    (list size (length (gethash "data" block)))))
           (close stream)
           ;; A client that speaks over a pipe: nc is the pipe, the lines go in and the answers come out.
           (let ((answers (uiop:run-program
                           (list "sh" "-c"
                                 (format nil "(printf '%s\\n%s\\n' '~a' '~a'; sleep 2) | nc -w 4 127.0.0.1 ~d"
                                         "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-06-18\"}}"
                                         "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}"
                                         port))
                           :output :string :ignore-error-status t)))
             (check "and a client that has only a pipe gets the same server through nc"
                    (and (search "\"serverInfo\"" answers) (search "\"click\"" answers))
                    (subseq answers 0 (min 120 (length answers))))))
      (mcp:stop-mcp-server server))))

(defun checks (host)
  "Everything that is checked of the open window HOST."
  (wait-for (lambda () (plusp (frame-ring-total (host-ring host)))) 20)
  (sleep 1.0)
  (check "the window shows the example's button" (present-p host :count-button))
  (check "the count starts at 0" (eql 0 (the-count host)) (the-count host))
  (click host :count-button)
  (click host :count-button)
  ;; The ui takes one change of the button a frame and the host asks for the next frame, so
  ;; a quick double click is four frames from being counted: the host says when it is.
  (let* ((start (get-internal-real-time))
         (idle (wait-for-idle host))
         (took (/ (- (get-internal-real-time) start) internal-time-units-per-second 1.0)))
    (check "the window answers and rests" idle)
    (check "and it is told in well under a second, not by the wait running out" (< took 1)
           (format nil "~,3f s" took)))
  (check "two clicks count two" (eql 2 (the-count host)) (the-count host))
  (check "and a window at rest is idle" (host-idle-p host))
  (let ((before (host-variant host)))
    (edited-counter)
    (check "compiling the function again changes the window"
           (wait-for (lambda () (present-p host :probe)) 20))
    (check "the app was made again" (not (eq before (host-variant host))))
    (check "and the count is still 2" (eql 2 (the-count host)) (the-count host))
    (click host :count-button)
    (wait-for-idle host)
    (check "the new app counts on from there" (eql 3 (the-count host)) (the-count host))
    ;; Again and again: a click, and the count is right the moment the host says it is idle,
    ;; whenever during the frames that take the click in the waiting is begun. A host that
    ;; says it is idle while a frame is still wanted gives a stale count in some of them.
    (let ((wrong 0))
      (dotimes (i 30)
        (click host :count-button)
        (sleep (* 0.001 (random 45)))
        (wait-for-idle host)
        (unless (eql (+ 4 i) (the-count host))
          (incf wrong)))
      (check "thirty clicks, each counted when the host says it is idle" (zerop wrong)
             (format nil "~d wrong" wrong)))
    (through-the-machine-interface)
    (through-the-mcp-server)))

(defun drive (host)
  (unwind-protect
       (handler-case (checks host)
         (error (condition)
           (check "the checks ran to the end" nil condition)))
    (if (zerop *failures*)
        (format t "~&The run-app check passed.~%")
        (format t "~&FAIL: ~d check~:p did not hold.~%" *failures*))
    (finish-output)
    (close-host-window host)))

(run-app 'almighty-bolt-examples/counter:make-counter
         :title "Counter" :width 360 :height 160 :hot t :driver #'drive)
