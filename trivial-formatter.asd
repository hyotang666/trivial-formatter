; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "0.12.0"
  :depends-on
  (
   "read-as-string" ; Read S-Expression as string.
   "named-readtables" ; Readtable manager.
   )
  :pathname
  "src/"
  :components
  ((:file "trivial-formatter")))
