; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter.test"
  :version
  "1.15.6"
  :depends-on
  (:jingoh "trivial-formatter"
           #+ecl "trivial-macroexpand-all"
           )
  :components
  ((:file "trivial-formatter"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :trivial-formatter args)))
