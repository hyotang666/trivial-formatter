; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "0.20.1"
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
