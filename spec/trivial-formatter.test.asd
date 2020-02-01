; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter.test"
  :version
  "0.2.5"
  :depends-on
  (:jingoh "trivial-formatter")
  :components
  ((:file "trivial-formatter"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :trivial-formatter args)))
