(defpackage :trivial-formatter.spec
  (:import-from :trivial-formatter #:pprint-extended-loop #:split-keywords
                #:pprint-fun-call #:pprint-define-condition #:pprint-restart-case
                #:pprint-with-open-file #:split-to-lines #:pprint-cond #:pprint-flet
                #:pprint-defgeneric #:pprint-defstruct #:pprint-defclass #:pprint-handler-case
                #:pprint-method-lambda-list)
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
; `*readtable*` `*print-pprint-dispatch*`
; `*strict-loop-keyword-p*`, when t loop macro keywrods are force to be keyword symbol.

;;;; Side-Effects:
; Load asdf:system to lisp environment.
; Depends on IF-EXISTS, some side effect occurs.
; The default behavior is to print to `*STANDARD-OUTPUT*`.

;;;; Notes:

;;;; Exceptional-Situations:
; When specified system is not found, an error of asdf:missing-component is signaled.
#?(fmt :no-such-system) :signals asdf:missing-component

;;;; Guards.
#+ecl
#?(trivial-macroexpand-all:macroexpand-all '(formatter "~<#.~;~W~:>"))
:signals condition

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
#?(with-input-from-string(s "#:uninterned")
    (read-as-code s))
:satisfies (lambda(result)
             (& (symbolp result)
                (null(symbol-package result))
                (equal "UNINTERNED" (symbol-name result))))
#?(with-input-from-string(s ":keyword")
    (read-as-code s))
=> :KEYWORD
; Double colloned keyword symbol will be canonicalized to single colloned keyword symbol.
#?(with-input-from-string(s "::double-colloned")
    (read-as-code s))
=> :DOUBLE-COLLONED
; Without package prefix, symbol is interned to current package.
#?(with-input-from-string(s "without-prefix")
    (read-as-code s))
:satisfies (lambda(result)
             (&(symbolp result)
               (eq *package* (symbol-package result))
               (equal "WITHOUT-PREFIX" (symbol-name result))))
; When prefixed,
; * If package does not exists current lisp image, uninterned symbol will be made.
#?(with-input-from-string(s "no-such-package:symbol")
    (read-as-code s))
:satisfies (lambda(result)
             (& (symbolp result)
                (null(symbol-package result))
                (equal "no-such-package:symbol" (get result 'trivial-formatter::notation))))
; ECL specific guard.
#+ecl
#?(subtypep
    (type-of
      (nth-value 1 (ignore-errors (read-from-string "no-such-package:symbol"))))
    'package-error)
=> NIL

; * If specified package name is different actual package name, such symbol is marked.
; This prevents e.g. closer-mop symbols becomes underlying implementation dependent symbol.
#?(with-input-from-string(s "asdf:find-system")
    (read-as-code s))
:satisfies (lambda(result)
             (&(eq result 'asdf:find-system)
               (equal "asdf:find-system" (get result 'trivial-formatter::notation))))

; * If explicitly specify it is internal symbol (i.e. p::s), such symbol is marked
; because there may a reason.
; E.g. It is external after compile but internal in compile time.
#?(with-input-from-string (s "asdf::defsystem")
    (read-as-code s))
:satisfies (lambda (result)
             (& (eq result 'asdf:defsystem)
                (equal "asdf::defsystem" (get result 'trivial-formatter::notation))))

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

#?(with-input-from-string(s "hoge")
    (read-as-code s nil #\h))
=> HOGE

#?(with-input-from-string (s ".3")
    (read-as-code s))
=> 0.3

#?(with-input-from-string (s ".symbol")
    (read-as-code s))
=> .SYMBOL

;;;; Affected By:
; `*readtable*`

;;;; Side-Effects:
; Consume stream contents.

;;;; Notes:
;;; Known bug.
; * Not support comment in #S, #A, reader macros.
; Comment will be discarded.

; Comment in vector is supported.
#?(with-input-from-string (s "#(;comment
)")
    (print-as-code (read-as-code s)))
:outputs "#( ; comment
  )"

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
; `*print-pprint-dispatch*`
; `*strict-loop-keyword-p*`, when t loop macro keywrods are force to be keyword symbol.

;;;; Side-Effects:
; Output to `STREAM`.

;;;; Notes:
; This printings are heavily depending on implementations pretty printing feature.
; It means defferent output in defferent implementation.

;;;; Exceptional-Situations:

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

; Corner case with symbol.
#?(print-as-code '|"|)
:outputs "|\"|"

; Corner case of declare with comma. Especially for SBCL.
#?(with-input-from-string (s "`(declare ,'nil)")
    (print-as-code (read-as-code s)))
:outputs "`(declare ,'nil)"

;; Corner case of dots, especially when *read-suppress* t.
#?(with-input-from-string (s "(a ...)")
    (print-as-code (read-as-code s)))
:outputs "(a ...)"

(requirements-about PPRINT-EXTENDED-LOOP :doc-type function
                    :around(let((*print-pretty* t))
                             (call-body)))

;;;; Description:

#+syntax
(PPRINT-EXTENDED-LOOP stream list) ; => result

;;;; Arguments and Values:

; stream := 

; list := 

; result := 

;;;; Affected By:
; `*strict-loop-keyword-p*` when t loop macro keywords are force to be keyword symbol.

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
:outputs #.(format nil "(LOOP FOR I UPFROM 0
      AS X IN '(A B (C))
      NCONC ~A)" '(if (evenp i)(list x)nil))

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

#?(trivial-formatter::make-loop-clauses (cdr '(loop for i in '(1 324 2345 323 2 4 235 252)
                                                    when (oddp i)
                                                    do (print i *standard-output*)
                                                    and collect i into odd-numbers
                                                    and do (terpri)
                                                    else                              ; I is even.
                                                    collect i into even-numbers
                                                    finally
                                                    (return (values odd-numbers even-numbers)))))
:satisfies
(lambda (list)
  (& (equalp list
             (list (trivial-formatter::make-clause :keyword 'for :forms '(i in '(1 324 2345 323 2 4 235 252)))
                   (trivial-formatter::make-nestable :keyword 'when :pred '(oddp i))
                   (trivial-formatter::make-clause :keyword 'do :forms '((print i *standard-output*)))
                   (trivial-formatter::make-clause :keyword 'and)
                   (trivial-formatter::make-clause :keyword 'collect :forms '(i into odd-numbers))
                   (trivial-formatter::make-clause :keyword 'and)
                   (trivial-formatter::make-clause :keyword 'do :forms '((terpri)))
                   (trivial-formatter::make-clause :keyword 'else)
                   (trivial-formatter::make-clause :keyword 'collect :forms '(i into even-numbers))
                   (trivial-formatter::make-clause :keyword 'finally :forms '((return (values odd-numbers even-numbers))))))))

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
      ELSE WHEN (SYMBOLP I)
        COLLECT I INTO SYMBOL-LIST
      ELSE
        DO (ERROR \"found a funny value in list ~S, value ~S~%\" LIST I)
      FINALLY (RETURN (VALUES FLOAT-NUMBERS OTHER-NUMBERS SYMBOL-LIST)))"

#?(trivial-formatter::make-loop-clauses (cdr
                                          '(loop for i in list
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
                                                 finally (return (values float-numbers other-numbers symbol-list)))))
:satisfies
(lambda (list)
  (& (equalp list
             (list (trivial-formatter::make-clause :keyword 'for :forms '(i in list))
                   (trivial-formatter::make-nestable :keyword 'when :pred '(numberp i))
                   (trivial-formatter::make-nestable :keyword 'when :pred '(floatp i))
                   (trivial-formatter::make-clause :keyword 'collect :forms '(i into float-numbers))
                   (trivial-formatter::make-clause :keyword 'else)
                   (trivial-formatter::make-clause :keyword 'collect :forms '(i into other-numbers))
                   (trivial-formatter::make-clause :keyword 'else)
                   (trivial-formatter::make-nestable :keyword 'when :pred '(symbolp i))
                   (trivial-formatter::make-clause :keyword 'collect :forms '(i into symbol-list))
                   (trivial-formatter::make-clause :keyword 'else)
                   (trivial-formatter::make-clause :keyword 'do :forms '((error "found a funny value in list ~S, value ~S~%" list i)))
                   (trivial-formatter::make-clause :keyword 'finally :forms '((return (values float-numbers other-numbers symbol-list))))))))

; CLHS 6.1.8.1
#?(pprint-extended-loop nil '(loop for x from 0 to 3
                                   do (print x)
                                   if (zerop (mod x 2))
                                   do (princ " a")
                                   and if (zerop (floor x 2))
                                   do (princ " b")
                                   end
                                   and do (princ " c")))
:outputs "(LOOP FOR X FROM 0 TO 3
      DO (PRINT X)
      IF (ZEROP (MOD X 2))
        DO (PRINC \" a\")
        AND IF (ZEROP (FLOOR X 2))
              DO (PRINC \" b\")
            END
        AND DO (PRINC \" c\"))"

#?(trivial-formatter::make-loop-clauses (cdr '(loop for x from 0 to 3
                                   do (print x)
                                   if (zerop (mod x 2))
                                   do (princ " a")
                                   and if (zerop (floor x 2))
                                   do (princ " b")
                                   end
                                   and do (princ " c"))))
:satisfies (lambda (list)
             (& (equalp list (list (trivial-formatter::make-clause :keyword 'for :forms '(x from 0 to 3))
                                   (trivial-formatter::make-clause :keyword 'do :forms '((print x)))
                                   (trivial-formatter::make-nestable :keyword 'if :pred '(zerop (mod x 2)))
                                   (trivial-formatter::make-clause :keyword 'do :forms '((princ " a")))
                                   (trivial-formatter::make-clause :keyword 'and)
                                   (trivial-formatter::make-nestable :keyword 'if :pred '(zerop (floor x 2)))
                                   (trivial-formatter::make-clause :keyword 'do :forms '((princ " b")))
                                   (trivial-formatter::make-clause :keyword 'end)
                                   (trivial-formatter::make-clause :keyword 'and)
                                   (trivial-formatter::make-clause :keyword 'do :forms '((princ " c")))))))

;; Case symbol confliction with loop macro keywords.
#?(pprint-extended-loop nil '(loop for count in counts collect count))
:outputs "(LOOP FOR COUNT IN COUNTS
      COLLECT COUNT)"

#?(trivial-formatter::make-loop-clauses '(for count in counts collect count))
:satisfies
(lambda (list)
  (& (equalp list
             (list (trivial-formatter::make-clause :keyword 'for :forms '(count in counts))
                   (trivial-formatter::make-clause :keyword 'collect :forms '(count))))))

;; Corner case. If after nil.
#?(pprint-extended-loop nil '(loop :with flag = nil :if))
:outputs "(LOOP :WITH FLAG = NIL
      :IF)"

#?(pprint-extended-loop nil '(loop :with flag = nil :if nil))
:outputs "(LOOP :WITH FLAG = NIL
      :IF NIL)"

#?(trivial-formatter::make-loop-clauses '(with flag = nil if))
:satisfies
(lambda (list)
  (& (equalp list
             (list (trivial-formatter::make-clause :keyword 'with :forms '(flag = nil))
                   (trivial-formatter::make-clause :keyword 'if)))))

#?(PPRINT-EXTENDED-LOOP NIL
                        '(LOOP :NAMED NAME
                               :FOR I :BELOW 10))
:outputs "(LOOP :NAMED NAME
      :FOR I :BELOW 10)"

;; Corner case. Nested.
#?(PRINT-AS-CODE
   '(LOOP :WHEN (ODDP X)
          :DO (LOOP :WHEN (ODDP Y)
                    :DO (PRINT (LIST X Y)))))
:outputs "(loop :when (oddp x)
        :do (loop :when (oddp y)
                    :do (print (list x y))))"

(requirements-about SPLIT-KEYWORDS :doc-type function)

;;;; Description:

#+syntax
(SPLIT-KEYWORDS exp) ; => result

;;;; Arguments and Values:

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(split-keywords '(call)) :values ((call)nil)
#?(split-keywords '(call :key)) :values ((call :key)nil)
#?(split-keywords '(call :key :value)) :values ((call)(:key :value))
#?(split-keywords '(call :k1 :v1 :k2 :v2)) :values ((call)(:k1 :v1 :k2 :v2))
#?(split-keywords '(call :k1 :k2 :k3)) :values ((call :k1)(:k2 :k3))
#?(split-keywords '(call :k1 :k2 :k3 :k4 :k5)) :values ((call :k1)(:k2 :k3 :k4 :k5))
#?(split-keywords '(call :k1 v1 rest)) :values ((call :k1 v1 rest) nil)
#?(split-keywords '(call not-key :k1 v1)) :values ((call not-key)(:k1 v1))

(requirements-about PPRINT-FUN-CALL :doc-type function
                    :around(let((*print-pretty* t))
                             (call-body)))

;;;; Description:

#+syntax
(PPRINT-FUN-CALL stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(pprint-fun-call nil '(call)) :outputs "(CALL)"
#?(pprint-fun-call nil '(call arg)) :outputs "(CALL ARG)"
#?(pprint-fun-call nil '(asdf:component-pathname component
                                                 :direction :output
                                                 :if-does-not-exist :create
                                                 :if-exists if-exists))
:outputs
"(ASDF/COMPONENT:COMPONENT-PATHNAME COMPONENT
                                   :DIRECTION :OUTPUT
                                   :IF-DOES-NOT-EXIST :CREATE
                                   :IF-EXISTS IF-EXISTS)"
#?(pprint-fun-call nil '(asdf:component-pathname :direction :output
                                                 :if-does-not-exist :create
                                                 :if-exists if-exists))
:outputs
"(ASDF/COMPONENT:COMPONENT-PATHNAME :DIRECTION :OUTPUT
                                   :IF-DOES-NOT-EXIST :CREATE
                                   :IF-EXISTS IF-EXISTS)"
#?(pprint-fun-call nil '(call :k v)) :outputs "(CALL :K V)"
#?(pprint-fun-call nil '(member nil :append :supersede :rename :error :new-version))
:outputs
"(MEMBER NIL :APPEND :SUPERSEDE :RENAME :ERROR :NEW-VERSION)"

#?(PPRINT-FUN-CALL NIL NIL)
:outputs "()"

(requirements-about PPRINT-DEFINE-CONDITION :doc-type function
                    :around (let((*print-pretty* t))
                              (call-body)))

;;;; Description:

#+syntax
(PPRINT-DEFINE-CONDITION stream exp &rest noise) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; noise := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(pprint-define-condition
    nil
    '(define-condition syntax-error(error)
       ((whole :initform nil :initarg :whole :reader whole-form<=syntax-error)
        (definitions :initform nil :initarg :definitions :reader bnf-definitions))
       (:report (lambda(condition stream)
                  (princ condition stream)))
       (:default-initargs :format-control "")))
:outputs "(DEFINE-CONDITION SYNTAX-ERROR (ERROR)
  ((WHOLE :INITFORM NIL :INITARG :WHOLE :READER WHOLE-FORM<=SYNTAX-ERROR)
   (DEFINITIONS :INITFORM NIL :INITARG :DEFINITIONS :READER BNF-DEFINITIONS))
  (:REPORT (LAMBDA (CONDITION STREAM) (PRINC CONDITION STREAM)))
  (:DEFAULT-INITARGS :FORMAT-CONTROL \"\"))"

#?(PPRINT-DEFINE-CONDITION NIL '(DEFINE-CONDITION))
:outputs "(DEFINE-CONDITION)"
#?(PPRINT-DEFINE-CONDITION NIL '(DEFINE-CONDITION NAME))
:outputs "(DEFINE-CONDITION NAME)"
#?(PPRINT-DEFINE-CONDITION NIL
                           '(DEFINE-CONDITION NAME
                                SUPERCLASSES))
:outputs "(DEFINE-CONDITION NAME SUPERCLASSES)"
#?(PPRINT-DEFINE-CONDITION NIL
                           '(DEFINE-CONDITION NAME
                                SUPERCLASSES
                                SLOTS))
:outputs "(DEFINE-CONDITION NAME SUPERCLASSES SLOTS)"

#?(PPRINT-DEFINE-CONDITION NIL
                           '(DEFINE-CONDITION NAME
                                SUPERCLASSES
                                SLOTS
                              ADDITIONAL
                              OPTIONS))
:outputs "(DEFINE-CONDITION NAME SUPERCLASSES SLOTS ADDITIONAL OPTIONS)"

(requirements-about PPRINT-RESTART-CASE :doc-type function
                    :around (let ((*print-pretty* t))
                              (call-body)))

;;;; Description:

#+syntax
(PPRINT-RESTART-CASE stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(pprint-restart-case nil '(restart-case))
:outputs "(RESTART-CASE)"
#?(pprint-restart-case nil '(restart-case form))
:outputs "(RESTART-CASE FORM)"
#?(pprint-restart-case nil '(restart-case form nil))
:outputs "(RESTART-CASE FORM NIL)"
#?(pprint-restart-case nil '(restart-case form (restart)))
:outputs "(RESTART-CASE FORM (RESTART))"
#?(pprint-restart-case nil '(restart-case form (restart nil)))
:outputs "(RESTART-CASE FORM (RESTART ()))"
#?(pprint-restart-case nil '(restart-case form (restart (var))))
:outputs "(RESTART-CASE FORM (RESTART (VAR)))"
#?(pprint-restart-case nil '(restart-case form (restart (var) var)))
:outputs "(RESTART-CASE FORM (RESTART (VAR) VAR))"
#?(pprint-restart-case nil '(restart-case (funcall *macroexpand-hook*)
                             (dribble()
                               :report "Return to dribble.")))
:outputs
"(RESTART-CASE (FUNCALL *MACROEXPAND-HOOK*)
  (DRIBBLE () :REPORT \"Return to dribble.\"))"

#?(pprint-restart-case nil '(restart-case(error 'unexpected-behavior)
                              (use-value(expected)
                                :report "Specify expected output"
                                :interactive #'read
                                expected)))
:outputs
"(RESTART-CASE (ERROR 'UNEXPECTED-BEHAVIOR)
  (USE-VALUE (EXPECTED)
      :REPORT \"Specify expected output\"
      :INTERACTIVE #'READ
    EXPECTED))"

#?(PPRINT-RESTART-CASE NIL
                       '(RESTART-CASE NIL
                          (TEST NIL :REPORT "this is report" (RETURN))))
:outputs "(RESTART-CASE NIL
  (TEST ()
      :REPORT \"this is report\"
    (RETURN)))"

(requirements-about PPRINT-WITH-OPEN-FILE :doc-type function
                    :around (let((*print-pretty* t))
                              (call-body)))

;;;; Description:

#+syntax
(PPRINT-WITH-OPEN-FILE stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:
#?(pprint-with-open-file nil '(with-open-file))
:outputs "(WITH-OPEN-FILE)"
#?(pprint-with-open-file nil '(with-open-file :not-list))
:outputs "(WITH-OPEN-FILE :NOT-LIST)"
#?(pprint-with-open-file nil '(with-open-file nil))
:outputs "(WITH-OPEN-FILE ())"
#?(pprint-with-open-file nil '(with-open-file
                                (*standard-output* #P"hoge" :direction
                                                   :output :if-does-not-exist :create :if-exists if-exists)
                                (write-string string)))
:outputs
"(WITH-OPEN-FILE (*STANDARD-OUTPUT* #P\"hoge\" :DIRECTION :OUTPUT
                 :IF-DOES-NOT-EXIST :CREATE
                 :IF-EXISTS IF-EXISTS)
  (WRITE-STRING STRING))"

#?(pprint-with-open-file nil '(with-open-file(*spec-output* *default-pathname-defaults*
                                                            :direction :output
                                                            :if-exists :append)
                                (funcall appender)))
:outputs
"(WITH-OPEN-FILE (*SPEC-OUTPUT* *DEFAULT-PATHNAME-DEFAULTS* :DIRECTION :OUTPUT
                 :IF-EXISTS :APPEND)
  (FUNCALL APPENDER))"

(requirements-about SPLIT-TO-LINES :doc-type function)

;;;; Description:

#+syntax
(SPLIT-TO-LINES string) ; => result

;;;; Arguments and Values:

; string := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Tests:

#?(split-to-lines "; |")
=> ("; |")
,:test equalp

(requirements-about PPRINT-COND :doc-type function)

;;;; Description:

#+syntax
(PPRINT-COND stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(PPRINT-COND *STANDARD-OUTPUT* '(COND NIL NIL)) :outputs "(COND () ())"
#?(PPRINT-COND NIL '(COND))
:outputs "(COND)"
#?(PPRINT-COND NIL '(COND CLAUSE))
:outputs "(COND CLAUSE)"
#?(PPRINT-COND NIL '(COND CLAUSE CLAUSE2))
:outputs "(COND CLAUSE CLAUSE2)"
#?(PPRINT-COND NIL '(COND CLAUSE (T)))
:outputs "(COND CLAUSE (T))"
#?(PPRINT-COND NIL
               '(COND ((EVENP I) (DO-IT-FOR-EVEN I))
                      ((ODDP I) (DO-IT-FOR-ODD I)) (T (DO-IT) (FOR-OTHERS))))
:outputs "(COND ((EVENP I) (DO-IT-FOR-EVEN I))
      ((ODDP I) (DO-IT-FOR-ODD I))
      (T
       (DO-IT)
       (FOR-OTHERS)))"

(requirements-about PPRINT-FLET :doc-type function)

;;;; Description:

#+syntax
(PPRINT-FLET stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(pprint-flet nil '(flet)) :outputs "(FLET)"
#?(PPRINT-FLET NIL '(FLET T))
:outputs #.(let ((*print-pretty* t))
             (prin1-to-string '(flet t)))
#+ecl ; as guard.
#?(prin1-to-string '(flet t))
=> "(FLET T
  )"
,:test equal

(requirements-about PPRINT-DEFGENERIC :doc-type function)

;;;; Description:

#+syntax (PPRINT-DEFGENERIC stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:


#?(PPRINT-DEFGENERIC NIL '(DEFGENERIC)) :outputs "(DEFGENERIC)"
#?(PPRINT-DEFGENERIC NIL '(DEFGENERIC NAME)) :outputs "(DEFGENERIC NAME)"
#?(PPRINT-DEFGENERIC NIL
                     '(DEFGENERIC NAME
                          LAMBDA-LIST)) :outputs "(DEFGENERIC NAME LAMBDA-LIST)"
#?(PPRINT-DEFGENERIC NIL
                     '(DEFGENERIC NAME
                          LAMBDA-LIST
                        BODY)) :outputs "(DEFGENERIC NAME LAMBDA-LIST BODY)"

#?(PPRINT-DEFGENERIC NIL
                     '(DEFGENERIC 1
                          2
                        3
                        4
                        5
                        6
                        7))
:outputs "(DEFGENERIC 1 2 3 4 5 6 7)"

#?(PPRINT-DEFGENERIC NIL
                     '(DEFGENERIC GENERIC-NAME
                          (LAMBDA LIST)
                        ADDITIONAL
                        OPTIONS
                        (:DOCUMENTATION "doc")))
:outputs "(DEFGENERIC GENERIC-NAME (LAMBDA LIST)
  ADDITIONAL
  OPTIONS
  (:DOCUMENTATION \"doc\"))"

#?(PPRINT-DEFGENERIC NIL
                     '(DEFGENERIC NMI
                          (OBJ)
                        (:METHOD (OBJ) (STACK-PUSH (CPU-SR OBJ) OBJ)
                         (SETF (CPU-PC OBJ) (GET-WORD 65530)))))
:outputs "(DEFGENERIC NMI (OBJ)
  (:METHOD (OBJ)
    (STACK-PUSH (CPU-SR OBJ) OBJ)
    (SETF (CPU-PC OBJ) (GET-WORD 65530))))"

(requirements-about PPRINT-DEFSTRUCT :doc-type function)

;;;; Description:

#+syntax (PPRINT-DEFSTRUCT stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(PPRINT-DEFSTRUCT NIL '(DEFSTRUCT))
:outputs "(DEFSTRUCT)"
#?(PPRINT-DEFSTRUCT NIL '(DEFSTRUCT NAME))
:outputs "(DEFSTRUCT NAME)"
#?(PPRINT-DEFSTRUCT NIL '(DEFSTRUCT NAME SLOTS))
:outputs "(DEFSTRUCT NAME SLOTS)"
#?(PPRINT-DEFSTRUCT NIL '(DEFSTRUCT (NAME OPTIONS) SLOTS))
:outputs "(DEFSTRUCT (NAME OPTIONS) SLOTS)"

#?(PPRINT-DEFSTRUCT NIL
                    '(DEFSTRUCT
                         (NAME (:CONSTRUCTOR NIL) (:PREDICATE NIL)
                          (:COPIER NIL) (:CONC-NAME NIL))
                       SLOTS))
:outputs "(DEFSTRUCT (NAME (:CONSTRUCTOR NIL)
                 (:PREDICATE NIL)
                 (:COPIER NIL)
                 (:CONC-NAME NIL))
  SLOTS)"
(requirements-about PPRINT-DEFCLASS :doc-type function)

;;;; Description:

#+syntax (PPRINT-DEFCLASS stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:


#?(PPRINT-DEFCLASS NIL '(DEFCLASS))
:outputs "(DEFCLASS)"
#?(PPRINT-DEFCLASS NIL '(DEFCLASS A))
:outputs "(DEFCLASS A)"
#?(PPRINT-DEFCLASS NIL '(DEFCLASS A SUPERCLASSES))
:outputs "(DEFCLASS A SUPERCLASSES)"
#?(PPRINT-DEFCLASS NIL '(DEFCLASS A SUPERCLASSES SLOTS))
:outputs "(DEFCLASS A SUPERCLASSES SLOTS)"
#?(PPRINT-DEFCLASS NIL '(DEFCLASS A SUPERCLASSES SLOTS OPTIONS))
:outputs "(DEFCLASS A SUPERCLASSES SLOTS OPTIONS)"

(requirements-about PPRINT-HANDLER-CASE :doc-type function)

;;;; Description:

#+syntax (PPRINT-HANDLER-CASE stream exp &rest noise) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; noise := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE))
:outputs "(HANDLER-CASE)"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE 0))
:outputs "(HANDLER-CASE 0)"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE 0 1))
:outputs "(HANDLER-CASE 0 1)"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE FORM CLAUSE1 CLAUSE2))
:outputs "(HANDLER-CASE FORM CLAUSE1 CLAUSE2)"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE FORM (CONDITION)))
:outputs "(HANDLER-CASE FORM (CONDITION))"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE FORM (CONDITION VAR)))
:outputs "(HANDLER-CASE FORM (CONDITION VAR))"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE FORM (CONDITION (VAR VAR2))))
:outputs "(HANDLER-CASE FORM (CONDITION (VAR VAR2)))"
#?(PPRINT-HANDLER-CASE NIL '(HANDLER-CASE FORM (CONDITION (VAR) BODY)))
:outputs "(HANDLER-CASE FORM
  (CONDITION (VAR)
    BODY))"

#?(PPRINT-HANDLER-CASE NIL
                       '(HANDLER-CASE FORM
                                      (:NO-ERROR (&REST ARGS)
                                       (DECLARE (IGNORE ARGS)) (VALUES))))
:outputs "(HANDLER-CASE FORM
  (:NO-ERROR (&REST ARGS)
    (DECLARE (IGNORE ARGS))
    (VALUES)))"

(requirements-about *STRICT-LOOP-KEYWORD-P* :doc-type variable)

;;;; Description:
; Control forcing to loop macro keyword to keyword symbol.

;;;; Value type is BOOLEAN
#? *STRICT-LOOP-KEYWORD-P* :be-the boolean

; Initial value is `NIL`

;;;; Affected By:
; fmt >> pprint-extended-loop >> parse-loop-body, make-clause.

;;;; Notes:

#?(LET ((*STRICT-LOOP-KEYWORD-P* T))
    (PPRINT-EXTENDED-LOOP NIL
                          '(LOOP FOR I BELOW 10
                                 COLLECT I)))
:outputs "(LOOP :FOR I :BELOW 10
      :COLLECT I)"
#?(LET ((*STRICT-LOOP-KEYWORD-P*))
    (PPRINT-EXTENDED-LOOP NIL
                          '(LOOP FOR I BELOW 10
                                 COLLECT I)))
:outputs "(LOOP FOR I BELOW 10
      COLLECT I)"

(requirements-about PPRINT-METHOD-LAMBDA-LIST :doc-type function)

;;;; Description:

#+syntax (PPRINT-METHOD-LAMBDA-LIST stream exp) ; => result

;;;; Arguments and Values:

; stream := 

; exp := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

#?(PPRINT-METHOD-LAMBDA-LIST NIL '(#'FUNCTION))
:outputs "((FUNCTION FUNCTION))"
