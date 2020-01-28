; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "1.0.2"
  :depends-on
  (
   "read-as-string" ; Read S-Expression as string.
   "named-readtables" ; Readtable manager.
   "alexandria" ; Public domain utilities.
   )
  :pathname
  "src/"
  :components
  ((:file "trivial-formatter")))
