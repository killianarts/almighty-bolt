(defpackage #:almighty-bolt-7guis/t/sources
  (:use #:cl #:lisp-unit2)
  (:local-nicknames (#:sources #:almighty-bolt-7guis/common/sources)))

(in-package #:almighty-bolt-7guis/t/sources)

(defun wait-until (predicate &optional (seconds 3))
  "True once PREDICATE is, checking every few milliseconds, false after SECONDS."
  (let ((end (+ (get-internal-real-time) (* seconds internal-time-units-per-second))))
    (loop :until (funcall predicate)
          :do (when (> (get-internal-real-time) end)
                (return-from wait-until nil))
              (sleep 0.005))
    t))

(defun write-file (path text)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write-string text stream))
  path)

(defmacro with-directory ((directory &optional (name '(format nil "almighty-sources-test-~d"
                                                                  (random 1000000000))))
                          &body body)
  "Run BODY with DIRECTORY the pathname of a new empty directory called NAME, and delete it
afterwards."
  `(let ((,directory (uiop:ensure-directory-pathname
                      (merge-pathnames (format nil "~a/" ,name) (uiop:temporary-directory)))))
     (ensure-directories-exist ,directory)
     (unwind-protect (progn ,@body)
       (uiop:delete-directory-tree ,directory :validate t :if-does-not-exist :ignore))))

(defmacro with-file-watcher ((calls files &key (interval 0.03)) &body body)
  "Run BODY with CALLS a list in a cons that holds what a watcher of FILES was called with,
newest first, and the watcher stopped afterwards."
  `(let* ((,calls (list nil))
          (watcher (sources:watch-files ,files (lambda (changed) (push changed (car ,calls)))
                                        :interval ,interval)))
     (unwind-protect (progn ,@body)
       (sources:stop-watching-files watcher))))

;;; --- which files ---

(define-test the-files-of-a-project-are-the-lisp-and-asd-files-without-tests-or-libraries ()
  (with-directory (directory)
    (flet ((make (relative)
             (let ((path (merge-pathnames relative directory)))
               (ensure-directories-exist path)
               (write-file path "x"))))
      (make "a.lisp")
      (make "a.asd")
      (make "notes.org")
      (make "src/b.lisp")
      (make "t/test.lisp")
      (make "src/t/test.lisp")
      (make "vendored/lib.lisp")
      (make "build/out.lisp"))
    (assert-equal '("a.asd" "a.lisp" "src/b.lisp")
                  (mapcar (lambda (file) (enough-namestring file directory))
                          (sources:lisp-files-under directory)))))

;;; --- watching them ---

(defun tables (files)
  "The tables of CHANGED-FILES for FILES as they are now: what each looked like, and nothing
pending."
  (let ((accepted (make-hash-table :test 'equal)))
    (dolist (file files)
      (setf (gethash file accepted) (almighty-bolt-7guis/common/sources::safe-signature file)))
    (values accepted (make-hash-table :test 'equal))))

(define-test a-changed-file-is-reported-at-the-second-look-that-finds-it-the-same ()
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(a)")))
      (multiple-value-bind (accepted pending) (tables (list file))
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (write-file file "(b)")
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (assert-equal (list file) (sources:changed-files (list file) accepted pending))
        ;; Reported once.
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (assert-equal '() (sources:changed-files (list file) accepted pending))))))

(define-test a-file-that-is-still-changing-is-not-reported ()
  ;; A file that is being written, or that an editor saves in two steps.
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(a)")))
      (multiple-value-bind (accepted pending) (tables (list file))
        (write-file file "(b")
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (write-file file "(b)")
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (assert-equal (list file) (sources:changed-files (list file) accepted pending))))))

(define-test a-file-put-back-before-the-second-look-is-not-reported ()
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(a)")))
      (multiple-value-bind (accepted pending) (tables (list file))
        (write-file file "(b)")
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (write-file file "(a)")
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        ;; A later change starts from a first look again.
        (write-file file "(b)")
        (assert-equal '() (sources:changed-files (list file) accepted pending))
        (assert-equal (list file) (sources:changed-files (list file) accepted pending))))))

(define-test a-changed-file-is-reported ()
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(one)")))
      (with-file-watcher (calls (list file))
        (sleep 0.1)
        (assert-false (car calls))
        (write-file file "(two, with more in it)")
        (assert-true (wait-until (lambda () (car calls))))
        ;; Once, however many looks there are after it.
        (sleep 0.3)
        (assert-equal (list (list file)) (car calls))))))

(define-test two-saves-in-the-same-second-of-the-same-size-are-both-seen ()
  ;; A write date has the resolution of a second. The watcher also looks at what a file that was
  ;; just written holds.
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(a)")))
      (with-file-watcher (calls (list file))
        (write-file file "(b)")
        (assert-true (wait-until (lambda () (= 1 (length (car calls))))))
        (write-file file "(c)")
        (assert-true (wait-until (lambda () (= 2 (length (car calls))))))))))

(define-test a-file-that-did-not-change-is-not-reported ()
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(a)")))
      (with-file-watcher (calls (list file))
        (sleep 0.5)
        (assert-false (car calls))))))

(define-test only-the-files-that-changed-are-in-the-list ()
  (with-directory (directory)
    (let ((a (write-file (merge-pathnames "a.lisp" directory) "(a)"))
          (b (write-file (merge-pathnames "b.lisp" directory) "(b)")))
      (with-file-watcher (calls (list a b))
        (write-file b "(b changed)")
        (assert-true (wait-until (lambda () (car calls))))
        (assert-equal (list (list b)) (car calls))))))

(define-test a-file-that-goes-missing-for-a-moment-is-not-an-error ()
  ;; An editor may save by writing a new file and moving it over the old one.
  (with-directory (directory)
    (let ((file (write-file (merge-pathnames "a.lisp" directory) "(a)")))
      (with-file-watcher (calls (list file))
        (delete-file file)
        (sleep 0.2)
        (assert-false (car calls))
        (write-file file "(a, back and changed)")
        (assert-true (wait-until (lambda () (car calls))))))))

(define-test an-error-in-the-function-does-not-end-the-watch ()
  (with-directory (directory)
    (let* ((file (write-file (merge-pathnames "a.lisp" directory) "(a)"))
           (count 0)
           (watcher (sources:watch-files (list file)
                                         (lambda (changed)
                                           (declare (ignore changed))
                                           (incf count)
                                           (error "the first call fails"))
                                         :interval 0.03))
           (*error-output* (make-broadcast-stream)))
      (unwind-protect
           (progn
             (write-file file "(b)")
             (assert-true (wait-until (lambda () (= 1 count))))
             (write-file file "(c)")
             (assert-true (wait-until (lambda () (= 2 count)))))
        (sources:stop-watching-files watcher)))))

(define-test a-stopped-watcher-reports-nothing ()
  (with-directory (directory)
    (let* ((file (write-file (merge-pathnames "a.lisp" directory) "(a)"))
           (calls (list nil))
           (watcher (sources:watch-files (list file) (lambda (changed) (push changed (car calls)))
                                         :interval 0.03)))
      (sources:stop-watching-files watcher)
      (sleep 0.15)
      (write-file file "(b)")
      (sleep 0.3)
      (assert-false (car calls)))))

;;; --- the systems of this project ---

(define-test the-files-of-a-system-are-found-through-asdf ()
  (let ((files (sources:system-files '("almighty-bolt-7guis"))))
    (assert-true (find "registry.lisp" files :key #'file-namestring :test #'string=))
    (assert-true (find "almighty-bolt-7guis.asd" files :key #'file-namestring :test #'string=))
    (assert-false (some (lambda (file) (member "t" (pathname-directory file) :test #'string=))
                        files))
    ;; Each file once, though the system has many modules in one directory tree.
    (assert-eql (length files) (length (remove-duplicates files :test #'equal)))))

(define-test the-loaded-systems-of-the-project-are-named ()
  (let ((names (sources:loaded-system-names)))
    (assert-true (member "almighty-bolt-7guis" names :test #'string=))
    (assert-true (every (lambda (name) (eql 0 (search "almighty-bolt" name))) names))))

;;; --- loading a system again ---

(defun answer ()
  (funcall (find-symbol "ANSWER" "ALMIGHTY-BOLT-RELOAD-TEST")))

(defun in-new-thread (function)
  "Call FUNCTION on a new thread and return what it returns. The thread that runs the tests may
be inside an ASDF operation (make test is), and ASDF takes a system loaded again inside one
for loaded already; a new thread has none."
  (sb-thread:join-thread (sb-thread:make-thread function)))

(define-test a-system-is-loaded-again-with-the-file-that-was-saved ()
  ;; A fixed name, so that the compiled file that ASDF keeps for the directory is one.
  (with-directory (directory "almighty-sources-test-reload")
    (write-file (merge-pathnames "almighty-bolt-reload-test.asd" directory)
                "(defsystem \"almighty-bolt-reload-test\" :components ((:file \"f\")))")
    (let ((file (write-file (merge-pathnames "f.lisp" directory)
                            "(defpackage #:almighty-bolt-reload-test (:use #:cl) (:export #:answer))
(in-package #:almighty-bolt-reload-test)
(defun answer () 1)")))
      (push directory asdf:*central-registry*)
      (unwind-protect
           (let ((before (in-new-thread (lambda ()
                                          (asdf:load-system "almighty-bolt-reload-test")
                                          (answer)))))
             ;; Saved in the second that the file was compiled in, which ASDF alone would take
             ;; for current.
             (write-file file "(defpackage #:almighty-bolt-reload-test (:use #:cl) (:export #:answer))
(in-package #:almighty-bolt-reload-test)
(defun answer () 2)")
             (let ((after (in-new-thread
                           (lambda ()
                             (sources:reload-systems '("almighty-bolt-reload-test")
                                                     (list (truename file)))
                             (answer)))))
               (assert-eql 1 before)
               (assert-eql 2 after)))
        (dolist (output (asdf:output-files 'asdf:compile-op
                                           (asdf:find-component "almighty-bolt-reload-test" "f")))
          (uiop:delete-file-if-exists output))
        (setf asdf:*central-registry* (remove directory asdf:*central-registry* :test #'equal))
        (asdf:clear-system "almighty-bolt-reload-test")
        (when (find-package "ALMIGHTY-BOLT-RELOAD-TEST")
          (delete-package "ALMIGHTY-BOLT-RELOAD-TEST"))))))
