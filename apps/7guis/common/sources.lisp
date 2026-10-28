(defpackage #:almighty-bolt-7guis/common/sources
  (:use #:cl)
  (:export #:lisp-files-under
           #:watch-files
           #:stop-watching-files
           #:loaded-system-names
           #:system-files
           #:reload-systems
           #:watch-sources))

(in-package #:almighty-bolt-7guis/common/sources)

;;; Saving a file is enough to change an open window (decision record 0030). A thread looks at
;;; the source files of the loaded systems; when one has changed it loads the systems again,
;;; which compiles what changed, and the definitions that come from that are what tell the window
;;; to make the app again (WATCH-REDEFINITIONS in the core). This is for someone who edits in
;;; a program that does not compile into the running image; one that does gets the same result
;;; without it.

;;; --- looking at files ---

(defun lisp-files-under (directory)
  "The .lisp and .asd files in DIRECTORY and the directories under it, except those under a
directory called t (tests), vendored or build."
  (let ((files '()))
    (labels ((walk (directory)
               (dolist (file (uiop:directory-files directory))
                 (when (member (pathname-type file) '("lisp" "asd") :test #'equal)
                   (push file files)))
               (dolist (subdirectory (uiop:subdirectories directory))
                 (unless (member (car (last (pathname-directory subdirectory)))
                                 '("t" "vendored" "build") :test #'string=)
                   (walk subdirectory)))))
      (walk directory))
    (sort files #'string< :key #'namestring)))

(defun file-signature (file)
  "What says whether FILE changed: its write date, which has the resolution of a second, and,
for a file written in the last few seconds, a hash of what it holds, because two saves within a
second leave the same date."
  (let ((date (file-write-date file)))
    (list date
          (and (< (- (get-universal-time) date) 3)
               (sxhash (uiop:read-file-string file))))))

(defun signatures-differ-p (old new)
  (or (/= (first old) (first new))
      ;; A hash is kept only for a file that is new, so one that has aged out is not a change.
      (and (second old) (second new) (/= (second old) (second new)))))

(defun safe-signature (file)
  "The signature of FILE, or NIL if it cannot be read just now, as an editor that saves by
writing a new file and moving it leaves it for a moment."
  (ignore-errors (file-signature file)))

;;; --- watching them ---

(defstruct (file-watcher (:constructor %make-file-watcher) (:copier nil))
  (thread nil)
  (stopped nil))

(defun watch-files (files function &key (interval 0.4))
  "Look at FILES every INTERVAL seconds on a thread of its own, and when some have changed call
FUNCTION with the list of them. A change made while FUNCTION runs is seen after it. Returns the
watcher, for STOP-WATCHING-FILES. This polls, so a program that watches is not asleep when idle:
it wakes a few times a second, which is for development only."
  (let* ((signatures (make-hash-table :test 'equal))
         (watcher (%make-file-watcher)))
    (dolist (file files)
      (setf (gethash file signatures) (safe-signature file)))
    (setf (file-watcher-thread watcher)
          (sb-thread:make-thread
           (lambda ()
             (loop :until (file-watcher-stopped watcher)
                   :do (sleep interval)
                       (let ((changed '()))
                         (dolist (file files)
                           (let ((old (gethash file signatures))
                                 (new (safe-signature file)))
                             (when (and new (or (null old) (signatures-differ-p old new)))
                               (push file changed))
                             (when new
                               (setf (gethash file signatures) new))))
                         (when changed
                           (handler-case (funcall function (nreverse changed))
                             (error (condition)
                               (let ((*print-pretty* nil))
                                 (format *error-output* "~&Hot reload: ~a~%" condition))
                               (finish-output *error-output*)))))))
           :name "source watcher"))
    watcher))

(defun stop-watching-files (watcher)
  "Stop WATCHER. The thread ends within one interval."
  (setf (file-watcher-stopped watcher) t)
  nil)

;;; --- loading systems again ---

(defun loaded-system-names ()
  "The names of the loaded systems that belong to this project."
  (remove-if-not (lambda (name) (eql 0 (search "almighty-bolt" name)))
                 (asdf:already-loaded-systems)))

(defun system-files (names)
  "The source files of the systems NAMES, each directory once."
  (let ((directories '()))
    (dolist (name names)
      (pushnew (asdf:system-source-directory (asdf:find-system name)) directories :test #'equal))
    (remove-duplicates (loop :for directory :in (reverse directories)
                             :append (lisp-files-under directory))
                       :test #'equal)))

(defun forget-compiled-files (names files)
  "Delete the compiled files of the components of the systems NAMES whose source is one of
FILES, so that ASDF compiles them again. ASDF takes a compiled file for current unless its
source is newer, in seconds, and a file saved in the second that it was compiled in would be
missed."
  (labels ((walk (component)
             (when (and (typep component 'asdf:cl-source-file)
                        (member (asdf:component-pathname component) files
                                :test #'equal :key #'uiop:truenamize))
               (dolist (output (asdf:output-files 'asdf:compile-op component))
                 (uiop:delete-file-if-exists output)))
             (when (typep component 'asdf:parent-component)
               (mapc #'walk (asdf:component-children component)))))
    (dolist (name names)
      (walk (asdf:find-system name)))))

(defun reload-systems (names &optional files)
  "Load the systems NAMES again: ASDF compiles the files that changed, and everything that
depends on them, and loads them. FILES are the ones known to have changed (see
FORGET-COMPILED-FILES). A function that is defined again is not worth a warning."
  (let ((*compile-verbose* nil)
        (*compile-print* nil)
        (*load-verbose* nil))
    (forget-compiled-files names files)
    (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning))
      (dolist (name names)
        (asdf:load-system name)))))

(defun watch-sources (&key (names (loaded-system-names)) (interval 0.4))
  "Load the systems NAMES again whenever one of their files changes, and say which. Returns
the watcher, for STOP-WATCHING-FILES."
  (watch-files (system-files names)
               (lambda (changed)
                 (format t "~&Hot reload: ~{~a~^, ~} changed; loading.~%"
                         (mapcar #'file-namestring changed))
                 (finish-output)
                 (reload-systems names changed)
                 (format t "~&Hot reload: loaded.~%")
                 (finish-output))
               :interval interval))
