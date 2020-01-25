; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "0.3.0"
  :depends-on
  (
   "read-as-string" ; Read S-Expression as string from stream.
   )
  :pathname
  "src/"
  :components
  ((:file "trivial-formatter")))
