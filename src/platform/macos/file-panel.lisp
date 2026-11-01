(defpackage #:almighty-bolt/platform/macos/file-panel
  (:use #:cl)
  (:import-from #:almighty-bolt/platform/macos/objc
                #:msg-send
                #:objc-class
                #:ns-string
                #:lisp-string
                #:release
                #:with-autorelease-pool)
  (:export #:make-file-panel
           #:file-panel-types
           #:file-panel-name
           #:run-file-panel))

(in-package #:almighty-bolt/platform/macos/file-panel)

;;; The system's Open and Save panels. Making one and setting it up is apart from running it, which
;;; waits for the person and so is for a window only; a test can make one and ask how it is set up.

(defun ns-array-of-strings (strings)
  "An autoreleased NSArray of NSStrings of STRINGS."
  (let ((array (msg-send :id (objc-class "NSMutableArray") "array")))
    (dolist (string strings array)
      (let ((ns (ns-string string)))
        (msg-send :void array "addObject:" (:id ns))
        (release ns)))))

(defun make-file-panel (kind &key types name)
  "An Open panel (KIND :open) or a Save panel (:save), autoreleased, that allows only files with an
extension in TYPES (strings, without the dot; all if NIL) and, for a Save panel, offers NAME."
  (let ((panel (ecase kind
                 (:open (msg-send :id (objc-class "NSOpenPanel") "openPanel"))
                 (:save (msg-send :id (objc-class "NSSavePanel") "savePanel")))))
    (when types
      ;; The extensions as they are, which every macOS this runs on understands.
      (msg-send :void panel "setAllowedFileTypes:" (:id (ns-array-of-strings types))))
    (when (eq kind :open)
      (msg-send :void panel "setCanChooseFiles:" (:bool t))
      (msg-send :void panel "setCanChooseDirectories:" (:bool nil))
      (msg-send :void panel "setAllowsMultipleSelection:" (:bool nil)))
    (when (and name (eq kind :save))
      (let ((ns (ns-string name)))
        (msg-send :void panel "setNameFieldStringValue:" (:id ns))
        (release ns)))
    panel))

(defun file-panel-types (panel)
  "The extensions PANEL allows, or NIL for all."
  (let ((array (msg-send :id panel "allowedFileTypes")))
    (unless (zerop array)
      (loop :for i :below (msg-send :ulong array "count")
            :collect (lisp-string (msg-send :id array "objectAtIndex:" (:ulong i)))))))

(defun file-panel-name (panel)
  (lisp-string (msg-send :id panel "nameFieldStringValue")))

(defun run-file-panel (panel)
  "Show PANEL and wait for the person: the path chosen, or NIL if they cancelled. Main thread, and
not inside a frame."
  (with-autorelease-pool
    (when (= (msg-send :long panel "runModal") 1)
      (let ((url (msg-send :id panel "URL")))
        (and (/= url 0) (lisp-string (msg-send :id url "path")))))))
