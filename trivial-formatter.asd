; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "0.10.0"
  :depends-on
  (
   "read-as-string" ; Read S-Expression as string.
   )
  :pathname
  "src/"
  :components
  ((:file "trivial-formatter")))
