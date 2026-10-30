;;;; Run some test packages without the whole suite, for quick checks.
;;;;   SYSTEM=almighty-bolt-core/tests PACKAGES="almighty-bolt-core/t/text/line ..." \
;;;;     sbcl --noinform --non-interactive --no-sysinit --no-userinit --load scripts/test-packages.lisp
;;;; Prints the failures and a summary line for each package.
(require :asdf)
(asdf:initialize-source-registry
 (list :source-registry (list :tree (uiop:getcwd)) :ignore-inherited-configuration))
(asdf:load-system (uiop:getenv "SYSTEM"))
(dolist (name (uiop:split-string (uiop:getenv "PACKAGES") :separator " "))
  (when (plusp (length name))
    (let ((db (lisp-unit2:run-tests :package (intern (string-upcase name) :keyword))))
      (lisp-unit2:print-summary db))))
