# TRIVIAL-FORMATTER 5.8.9
## What is this?
Code formatter for common lisp.
Please see [trivial-formatter's source code](src/trivial-formatter.lisp).
This file is formatted by trivial-formatter itself.

## Usage

```lisp
(trivial-formatter:fmt :your-system :supersede)
```

For detail, see [spec files](spec/trivial-formatter.lisp).

### Line width.
The default depends on implementation.
(Probably 80 though.)
You can control it with ordinary common lisp pretty printing system way, i.e. using `*PRINT-RIGHT-MARGIN*`.

```lisp
(let ((*print-right-margin* 100)) ; <-- Specify line width with 100.
  (trivial-formatter:fmt :your-system :supersede))
```

## From developer
### Reader.
Trivial-formatter heavily depends on readtable especially [NAMED-READTABLES](https://github.com/melisgl/named-readtables).
You can extend reader with ordinary common lisp way.

```lisp
(let ((*readtable* (named-readtables:copy-named-readtable 'as-code)))
  (set-macro-character #\! (lambda (stream char) `(,char ,(read stream))))
  (read-as-code))
!hoge
=> (#\! HOGE)
```

For details, see [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/c_reader.htm)
and [NAMED-READTABLES](https://github.com/melisgl/named-readtables).

### Printer.
Trivial-formatter heavily depends on pretty printing system.
You can extend code format with ordinary common lisp way.

```lisp
(defun !-printer (stream exp)
  (format stream "~<~A~S~:>" exp))
(set-pprint-dispatch '(cons (eql #\!) (cons * null)) '!-printer)
(print-as-code '(#\! hoge))
=> !hoge
```

For detail, see [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/22_bb.htm).

### Load foreign formatters.
Trivial-formatter can load foreign formatters.
You can write extension codes as foreign formatters.

Foreign formatters file must named as "formatters.lisp".
Trivial-formatter search file from `*foreign-formatters-directories*`.
The default is quicklisp's local-projects directory and roswell's local-projects directory.

### Package trivial-formatter-user.
In trivial-formatter-user, you can use deformatter and pprint-fun-call with ordinary common lisp symbols.

#### PPRINT-FUN-CALL
PPRINT-FUN-CALL care about key value pair.

```lisp
(pprint-fun-call nil '(asdf:component-pathname component
                                               :direction :output
                                               :if-does-not-exist :create
                                               :if-exists if-exists))
(ASDF:COMPONENT-PATHNAME COMPONENT
                         :DIRECTION :OUTPUT
                         :IF-DOES-NOT-EXIST :CREATE
                         :IF-EXISTS IF-EXISTS))
NIL
```

#### DEFORMATTER
DEFORMATTER care about package exists and symbol confliction and set-pprint-dispatch.

```lisp
(macroexpand '(deformatter package symbol (stream exp)
                (format stream "~A" exp)))

(WHEN (FIND-PACKAGE "PACKAGE")
  (DEFUN #:PPRINT-SYMBOL3440 (STREAM EXP) (FORMAT STREAM "~A" EXP))
  (SET-PPRINT-DISPATCH
   `(CONS (MEMBER ,(UIOP/PACKAGE:FIND-SYMBOL* "SYMBOL" "PACKAGE")))
   '#:PPRINT-SYMBOL3440)
  '#:PPRINT-SYMBOL3440)
```

### Product's goal

### License
MIT

### Developed with
SBCL/2.0.7

### Tested with
* SBCL/2.0.7
* CCL/1.12 ; Failed due to CCL violates ANSI standard.
* ECL/16.1.3

#### Note
Trivial-formatter works portable at least above implementation.
But it never means works samely.
For example, `IF` format is different.
SBCL prints newline even if element is short, but others.

```lisp
#+sbcl
(if a
    b
    c)

#+(or ecl ccl)
(if a b c)
```
### Known issue.
#### CLISP
CLISP is not supported.
[CLISP say](https://clisp.sourceforge.io/impnotes.html#clpp)

> The Lisp Pretty Printer implementation is not perfect yet.

#### CCL
Currently we stop to support CCL temporally due to it violates ANSI standard.

```lisp
? (pprint-dispatch t nil)
=> error
```

[CLHS says](http://www.lispworks.com/documentation/HyperSpec/Body/f_ppr_di.htm)

> table---a pprint dispatch table, or nil.

Fortunately this issue is already fixed.
Please wait next ccl release or build current ccl from source.

#### Reader.
When reader macro conflicts, such reader macros are ignored silently.
You can add new reader macros, but can not modify already existing reader macros.

#### PACKAGE-INFERRED-SYSTEM
Currently package-inferred-system is not supported.

#### FORMAT-CONTROL
Trivial-formatter can not adjust ~newline format control indentation.

## Installation
To install trivial-formatter, [roswell](https://github.com/roswell/roswell) is recommended.

```shell
$ ros install hyotang666/trivial-formatter
```

To load trivial-formatter to running lisp environment, evaluate below in the repl.

```lisp
* (ql:quickload :trivial-formatter)
```
