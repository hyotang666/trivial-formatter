; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "0.2.0"
  :depends-on
  nil
  :pathname
  "src/"
  :components
  ((:file "trivial-formatter")))
