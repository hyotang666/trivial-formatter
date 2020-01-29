(defpackage :trivial-formatter.spec
  (:use :cl :jingoh :trivial-formatter))
(in-package :trivial-formatter.spec)
(setup :trivial-formatter)

(requirements-about *OUTPUT-HOOK* :doc-type variable)

;;;; Description:
; Bound function designator.
; TRIVIAL-FORMATTER provides hook functions.
; See `DEBUG-PRINTER`, `REPLACER`, `APPENDER`, or `RENAMER`.

;;;; Value type is (OR SYMBOL FUNCTION)
#? *OUTPUT-HOOK* :be-the (or symbol function)

; Initial value is `DEBUG-PRINTER`

;;;; Affected By:
; FMT refers.

;;;; Notes:

(requirements-about FMT :doc-type function)

;;;; Description:
; Format every source codes of asdf:system.

#+syntax
(FMT system) ; => result

;;;; Arguments and Values:

; system := (or symbol (vector character) (vector nil) base-string system)
; otherwise condition depending on implementation.
#?(fmt '(invalid type)) :signals condition

; result := null

;;;; Affected By:
; `*OUTPUT-HOOK*`

;;;; Side-Effects:
; Load asdf:system to lisp environment.
; Depends on `*OUTPUT-HOOK*`, some side effect occurs.
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
                (string= #.(format nil "(dot . list)~2%")
                         (with-output-to-string(s)
                           (print-as-code result s)))))

;;;; Affected By:

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:
;;; Known bug.
; Could not handle not standard reader macros.

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

(requirements-about DEBUG-PRINTER :doc-type function)

;;;; Description:
; The default hook function for `*OUTPUT-HOOK*`.
; Output component codes to `*STANDARD-OUTPUT*`.

#+syntax
(DEBUG-PRINTER component) ; => result

;;;; Arguments and Values:

; component := asdf:component, otherwise condition depending on implementation.
#?(debug-printer "not component") :signals condition

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Output to `*STANDARD-OUTPUT*`.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about RENAMER :doc-type function)

;;;; Description:
; Old source file is saved with renaming.
; In such case new file is 'nameYYYYMMDDhhmmss.lisp'.

#+syntax
(RENAMER component) ; => result

;;;; Arguments and Values:

; component := component, otherwise condition depending on implementation.
#?(renamer "not component") :signals condition

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Modify file system.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about APPENDER :doc-type function)

;;;; Description:
; Souce file is appended with formatted codes.

#+syntax
(APPENDER component) ; => result

;;;; Arguments and Values:

; component := component, otherwise condition depending on implementation.
#?(appender "not component") :signals condition

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Append specified file.

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REPLACER :doc-type function)

;;;; Description:
; Source file is superseded by formatted codes.

#+syntax
(REPLACER component) ; => result

;;;; Arguments and Values:

; component := component, otherwise condition depending on implementation.
#?(replacer "not component") :signals condition

; result := null

;;;; Affected By:

;;;; Side-Effects:
; Supersedes specified file.

;;;; Notes:

;;;; Exceptional-Situations:

