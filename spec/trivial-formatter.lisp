(defpackage :trivial-formatter.spec
  (:import-from :trivial-formatter #:pprint-extended-loop #:parse-loop-body)
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

(requirements-about PPRINT-EXTENDED-LOOP :doc-type function)

;;;; Description:

#+syntax
(PPRINT-EXTENDED-LOOP stream list) ; => result

;;;; Arguments and Values:

; stream := 

; list := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; TESTS
#?(pprint-extended-loop nil '(loop))
:outputs "(LOOP)"

; CLHS 6.1.1.1.1 Simple loop.
#?(pprint-extended-loop nil '(loop (print :hoge *standard-output*)
                                   (print :hoge *standard-output*)
                                   (print :hoge *standard-output*)))
:outputs "(LOOP (PRINT :HOGE *STANDARD-OUTPUT*)
      (PRINT :HOGE *STANDARD-OUTPUT*)
      (PRINT :HOGE *STANDARD-OUTPUT*))"

; CLHS 6.1.1.3
#?(pprint-extended-loop nil '(loop for i from 1 to (compute-top-value)       ; first clause
                                   while (not (unacceptable i))              ; second clause
                                   collect (square i)                        ; third clause
                                   do (format t "Working on ~D now" i)       ; fourth clause
                                   when (evenp i)                            ; fifth clause
                                     do (format t "~D is a non-odd number" i)
                                   finally (format t "About to exit!")))     ; sixth clause
:outputs "(LOOP FOR I FROM 1 TO (COMPUTE-TOP-VALUE)
      WHILE (NOT (UNACCEPTABLE I))
      COLLECT (SQUARE I)
      DO (FORMAT T \"Working on ~D now\" I)
      WHEN (EVENP I)
        DO (FORMAT T \"~D is a non-odd number\" I)
      FINALLY (FORMAT T \"About to exit!\"))"

; CLHS 6.1.1.7
#?(pprint-extended-loop nil '(loop for (x y) of-type (vector fixnum)
                                   in (list :super :long :list :like :this :more)
                                   do))
:outputs "(LOOP FOR (X Y) OF-TYPE (VECTOR FIXNUM)
          IN (LIST :SUPER :LONG :LIST :LIKE :THIS :MORE)
      DO)"

; CLHS 6.1.1.7
#?(pprint-extended-loop nil '(loop for numlist in '((1 2 4.0) (5 6 8.3) (8 9 10.4))
                                   for a of-type integer = (first numlist)
                                   and b of-type integer = (second numlist)
                                   and c of-type float = (third numlist)
                                   collect (list c b a)))
:outputs "(LOOP FOR NUMLIST IN '((1 2 4.0) (5 6 8.3) (8 9 10.4))
      FOR A OF-TYPE INTEGER = (FIRST NUMLIST)
      AND B OF-TYPE INTEGER = (SECOND NUMLIST)
      AND C OF-TYPE FLOAT = (THIRD NUMLIST)
      COLLECT (LIST C B A))"

; CLHS 6.1.1.7
#?(pprint-extended-loop nil '(loop with (a b) of-type float = '(1.0 2.0)
                                   and (c d) of-type integer = '(3 4)
                                   and (e f)
                                   return (list a b c d e f)))
:outputs "(LOOP WITH (A B) OF-TYPE FLOAT = '(1.0 2.0)
      AND (C D) OF-TYPE INTEGER = '(3 4)
      AND (E F)
      RETURN (LIST A B C D E F))"

; CLHS 6.1.3.2
#?(pprint-extended-loop nil '(loop for i upfrom 0
                                   as x in '(a b (c))
                                   nconc (if (evenp i) (list x) nil)))
:outputs "(LOOP FOR I UPFROM 0
      AS X IN '(A B (C))
      NCONC (IF (EVENP I)
                (LIST X)
                NIL))"

#?(pprint-extended-loop nil '(loop for i from 1 to 3
                                   do (print i *standard-output*)
                                   (print i *standard-output*)
                                   (print (* i i) *standard-output*)))
:outputs "(LOOP FOR I FROM 1 TO 3
      DO (PRINT I *STANDARD-OUTPUT*)
         (PRINT I *STANDARD-OUTPUT*)
         (PRINT (* I I) *STANDARD-OUTPUT*))"

; CLHS 6.1.8.1
#?(pprint-extended-loop nil '(loop for i in '(1 324 2345 323 2 4 235 252)
                                   when (oddp i)
                                   do (print i *standard-output*)
                                   and collect i into odd-numbers
                                   and do (terpri)
                                   else                              ; I is even.
                                   collect i into even-numbers
                                   finally
                                   (return (values odd-numbers even-numbers))))
:outputs "(LOOP FOR I IN '(1 324 2345 323 2 4 235 252)
      WHEN (ODDP I)
        DO (PRINT I *STANDARD-OUTPUT*)
        AND COLLECT I INTO ODD-NUMBERS
        AND DO (TERPRI)
      ELSE
        COLLECT I INTO EVEN-NUMBERS
      FINALLY (RETURN (VALUES ODD-NUMBERS EVEN-NUMBERS)))"

; CLHS 6.1.8.1
#?(pprint-extended-loop nil '(loop for i in list
                                   when (numberp i)
                                   when (floatp i)
                                   collect i into float-numbers
                                   else                                  ; Not (floatp i)
                                   collect i into other-numbers
                                   else                                    ; Not (numberp i)
                                   when (symbolp i)
                                   collect i into symbol-list
                                   else                                  ; Not (symbolp i)
                                   do (error "found a funny value in list ~S, value ~S~%" list i)
                                   finally (return (values float-numbers other-numbers symbol-list))))
:outputs "(LOOP FOR I IN LIST
      WHEN (NUMBERP I)
        WHEN (FLOATP I)
          COLLECT I INTO FLOAT-NUMBERS
        ELSE
          COLLECT I INTO OTHER-NUMBERS
      ELSE
        WHEN (SYMBOLP I)
          COLLECT I INTO SYMBOL-LIST
        ELSE
          DO (ERROR \"found a funny value in list ~S, value ~S~%\" LIST I)
      FINALLY (RETURN (VALUE FLOAT-NUMBERS OTHER-NUMBERS SYMBOL-LIST)))"

(requirements-about PARSE-LOOP-BODY :doc-type function)

;;;; Description:

#+syntax
(PARSE-LOOP-BODY body) ; => result

;;;; Arguments and Values:

; body := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests
#?(parse-loop-body nil) => NIL
#?(parse-loop-body '(:not-loop-keyword))
:satisfies (lambda(result)
             (& (listp result)
                (every #'trivial-formatter::clause-p result)
                (= 1 (length result))
                (equal '(:not-loop-keyword)
                       (trivial-formatter::clause-forms (car result)))
                (null (trivial-formatter::clause-keyword (car result)))))
