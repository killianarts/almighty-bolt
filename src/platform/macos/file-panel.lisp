(defpackage #:almighty-bolt/platform/macos/file-panel
  (:use #:cl)
  (:local-nicknames (#:objc #:almighty-bolt/platform/macos/objc))
  (:export #:make-file-panel
           #:file-panel-types
           #:file-panel-name
           #:run-file-panel))

(in-package #:almighty-bolt/platform/macos/file-panel)

;;; The system's Open and Save panels. Making one and setting it up is apart from running it, which
;;; waits for the person and so is for a window only; a test can make one and ask how it is set up.

(defun ns-array-of-strings (strings)
  "An autoreleased NSArray of NSStrings of STRINGS."
  (let ((array (objc:msg-send :id (objc:objc-class "NSMutableArray") "array")))
    (dolist (string strings array)
      (let ((ns (objc:ns-string string)))
        (objc:msg-send :void array "addObject:" (:id ns))
        (objc:release ns)))))

(defun make-file-panel (kind &key types name)
  "An Open panel (KIND :open) or a Save panel (:save), autoreleased, that allows only files with an
extension in TYPES (strings, without the dot; all if NIL) and, for a Save panel, offers NAME."
  (let ((panel (ecase kind
                 (:open (objc:msg-send :id (objc:objc-class "NSOpenPanel") "openPanel"))
                 (:save (objc:msg-send :id (objc:objc-class "NSSavePanel") "savePanel")))))
    (when types
      ;; The extensions as they are, which every macOS this runs on understands.
      (objc:msg-send :void panel "setAllowedFileTypes:" (:id (ns-array-of-strings types))))
    (when (eq kind :open)
      (objc:msg-send :void panel "setCanChooseFiles:" (:bool t))
      (objc:msg-send :void panel "setCanChooseDirectories:" (:bool nil))
      (objc:msg-send :void panel "setAllowsMultipleSelection:" (:bool nil)))
    (when (and name (eq kind :save))
      (let ((ns (objc:ns-string name)))
        (objc:msg-send :void panel "setNameFieldStringValue:" (:id ns))
        (objc:release ns)))
    panel))

(defun file-panel-types (panel)
  "The extensions PANEL allows, or NIL for all."
  (let ((array (objc:msg-send :id panel "allowedFileTypes")))
    (unless (zerop array)
      (loop :for i :below (objc:msg-send :ulong array "count")
            :collect (objc:lisp-string (objc:msg-send :id array "objectAtIndex:" (:ulong i)))))))

(defun file-panel-name (panel)
  (objc:lisp-string (objc:msg-send :id panel "nameFieldStringValue")))

(defun run-file-panel (panel)
  "Show PANEL and wait for the person: the path chosen, or NIL if they cancelled. Main thread, and
not inside a frame."
  (objc:with-autorelease-pool
    (when (= (objc:msg-send :long panel "runModal") 1)
      (let ((url (objc:msg-send :id panel "URL")))
        (and (/= url 0) (objc:lisp-string (objc:msg-send :id url "path")))))))
