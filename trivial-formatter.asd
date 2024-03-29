; vim: ft=lisp et
(in-package :asdf)
(defsystem "trivial-formatter"
  :version
  "10.12.1"
  :description "Code formatter for common lisp."
  :license "MIT"
  :author "SATO Shinichi"
  :source-control (:git "git@github.com:hyotang666/trivial-formatter")
  :bug-tracker "https://github.com/hyotang666/trivial-formatter/issues"
  :depends-on
  (
   "uiop"               ; Utilities, implicitly depends on via asdf.
   "lambda-fiddle"      ; Utilities for lambda-list processing.
   "read-as-string"     ; Read S-Expression as string.
   "named-readtables"   ; Readtable manager.
   "cl-ppcre"           ; Regular expression.
   "asdf"               ; System builder & compile/load external-formatter.
   "cl-store"           ; Serializer.
   )
  :pathname
  "src/"
  :components
  ((:file "trivial-formatter")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "trivial-formatter").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "trivial-formatter"))))
  (append (call-next-method) '((test-op "trivial-formatter.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
(defmethod operate :around
           ((o test-op) (c (eql (find-system "trivial-formatter")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "trivial-formatter"))))
      (with-muffled-conditions (*uninteresting-conditions*)
        (symbol-call :jingoh.documentizer :import c)))))
