(defpackage :trivial-formatter.spec
  (:use :cl :jingoh :trivial-formatter))
(in-package :trivial-formatter.spec)
(setup :trivial-formatter)

(requirements-about FMT :doc-type function)

;;;; Description:
; Format every source codes of asdf:system.

#+syntax
(FMT system &optional (if-exists nil supplied-p)) ; => result

;;;; Arguments and Values:

; system := (or symbol (vector character) (vector nil) base-string asdf/system:system)
; otherwise condition depending on implementation.
#?(fmt '(invalid type)) :signals condition

; if-exists := (member nil :append :supersede :rename :error :new-version :rename-and-delete :overwrite)
; otherwise condition depending on implementation.
#?(fmt :dummy :not-member) :signals condition

; If specified, depending on its value source file is modified.
; If not specified, formatted source code is printed to `*STANDARD-OUTPUT*`.
; Member is completely same with `CL:OPEN` keyword argument `IF-EXISTS`.

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Load asdf:system to lisp environment.
; Depends on IF-EXISTS, some side effect occurs.
; The default behavior is to print to `*STANDARD-OUTPUT*`.

;;;; Notes:

;;;; Exceptional-Situations:
; When specified system is not found, an error of asdf:missing-component is signaled.
#?(fmt :no-such-system) :signals asdf:missing-component

(requirements-about READ-AS-CODE :doc-type function)

;;;; Description:
; Almost same with `CL:READ` but
; * Can handle symbol which missing package.
; * Can handle comment.
; * Treat dot list as proper list.

#+syntax
(READ-AS-CODE &optional stream (eof-error-p t) (eof-value nil) (recursive-p nil)) ; => result

;;;; Arguments and Values:

; stream := (or null stream), otherwise condition depending on implementation.
#?(read-as-code :not-stream) :signals condition
; When NIL is specified, it means `*STANDARD-INPUT*`.
#?(with-input-from-string(*standard-input* "nil")
    (read-as-code nil))
=> NIL

; eof-error-p := boolean, otherwise condition depending on implementation.
#?(read-as-code nil :not-boolean) :signals condition
; When NIL is specified and get end-of-file, never signals end-of-file.
#?(with-input-from-string(*standard-input* "")
    (read-as-code nil nil :returned))
:invokes-debugger not
; When T is specified (the default) and get end-of-file, an error is signaled.
#?(with-input-from-string(*standard-input* "")
    (read-as-code nil t))
:signals end-of-file

; eof-value := t, when `EOF-ERROR-P` is `NIL`, and get end-of-file, this value is returned.
#?(with-input-from-string(*standard-input* "")
    (read-as-code nil nil :returned))
=> :returned

; recursive-p := boolean, internal use.

; result := t intermediate expression or eof-value is returned.
#?(with-input-from-string(s "symbol")
    (read-as-code s))
=> SYMBOL
#?(with-input-from-string(s "no-such-package:symbol")
    (read-as-code s))
:be-the trivial-formatter::broken-symbol
#?(with-input-from-string(s "; line comment.")
    (read-as-code s))
:be-the trivial-formatter::line-comment
#?(with-input-from-string(s "#|block comment|#")
    (read-as-code s))
:be-the trivial-formatter::block-comment
#?(with-input-from-string(s "(dot . list)")
    (read-as-code s))
:satisfies (lambda(result)
             (& (listp result)
                (= 3 (length result))
                (typep (second result) 'trivial-formatter::dot)
                (string= #.(format nil "(dot . list)")
                         (with-output-to-string(s)
                           (print-as-code result s)))))

;;;; Affected By:

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:
;;; Known bug.
; * Could not handle not standard reader macros.
; * Not support comment in #S, #A, #V reader macros.
; Comment will be discarded.
; * CCL could not print backquote due to CLL qackquote makes form in read time.

;;;; Exceptional-Situations:

(requirements-about PRINT-AS-CODE :doc-type function)

;;;; Description:
; Print intermediate expression as lisp source code.

#+syntax
(PRINT-AS-CODE exp &optional stream) ; => result

;;;; Arguments and Values:

; exp := t

; stream := (or null stream) otherwise condition depending on implementation.
#?(print-as-code :dummy :not-stream) :signals condition

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Output to `STREAM`.

;;;; Notes:
; This printings are heavily depending on implementations pretty printing feature.
; It means defferent output in defferent implementation.

;;;; Exceptional-Situations:

;;;; Guards for implementation specific hack.
; When test below fails, sbcl specific pretty printer may should be removed.
#+sbcl
#?(pprint-dispatch '(handler-case)) => sb-pretty::pprint-macro-call

;;;; Tests.
; 1. After line comment, newline is required.
; 2. Space may put if needed.
#?(with-input-from-string(s #.(format nil "(dummy; comment~%)"))
    (print-as-code (read-as-code s)))
:outputs
"(dummy ; comment
 )"

; Handle read time refer.
#?(with-input-from-string(s "(#0=#:hoge #0#)")
    (print-as-code (read-as-code s)))
:outputs
"(#0=#:hoge #0#)"

; Circular list.
#?(with-input-from-string(s "#0=(dummy . #0#)")
    (print-as-code (read-as-code s)))
:outputs
"#0=(dummy . #0#)"

;;;; Guards.
#?(let((*print-pretty* t))
    (format nil "~<~:@_~:>"nil))
=> "
"
,:test equal
