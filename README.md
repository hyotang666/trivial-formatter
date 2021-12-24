# TRIVIAL-FORMATTER 10.0.0
## What is this?
Code formatter for common lisp.
Please see [trivial-formatter's source code](src/trivial-formatter.lisp).
This file is formatted by trivial-formatter itself.

## Alternatives.
### [cl-syntax](https://github.com/m2ym/cl-syntax) for SLIME user.
> Reader Syntax Coventions for Common Lisp and SLIME 

### [trivial-indent](https://github.com/Shinmera/trivial-indent) for SLIME user.
> A very simple library to allow indentation hints for SWANK. 

### [cl-indentify](https://github.com/yitzchak/cl-indentify.git)
> Library and command line utility to automatically indent Common Lisp source files.

## Usage

```lisp
(trivial-formatter:fmt :your-system :supersede)
```

For detail, see [spec files](spec/trivial-formatter.lisp).

### Line width.
The default depends on implementation.
(Probably 80 though.)
You can control it with an ordinary common lisp pretty printing system way, i.e. using `*PRINT-RIGHT-MARGIN*`.

```lisp
(let ((*print-right-margin* 100)) ; <-- Specify line width with 100.
  (trivial-formatter:fmt :your-system :supersede))
```

### Strict mode.
When you invoke debugger, some implementations (e.g. ECL) are into :cl-user package.
In such cases, the `LOOP` macro backtrace form becomes ugly.

```lisp
(LOOP YOUR-PROJECT::FOR YOUR-PROJECT::I YOUR-PROJECT::UPFROM 0 ...)
```

Using keyword symbols as loop macro keywords avoid such ugly forms.

```lisp
(LOOP :FOR YOUR-PROJECT::I :UPFROM 0 ...)
```
In strict mode, trivial-formatter formats loop macro keywords into keyword symbols.
To enable it binds `*STRICT-LOOP-KEYWORD-P*` with `T`.

```lisp
* (let ((trivial-formatter:*strict-loop-keyword-p* t))
    (trivial-formatter:fmt :your-system :supersede))
```

## From developer
### Reader.
If your source codes have special reader macros, trivial-foramtter signals an error about unknown reader macros.
In such cases, you can extend the reader with an ordinary common lisp way because trivial-formatter heavily depends on readtable especially [NAMED-READTABLES](https://github.com/melisgl/named-readtables).
To extend it, your reader macro functions must return an intermediate object.

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
When you make intermediate objects, you need to make pretty print functions for it.
Trivial-formatter heavily depends on a pretty-printing system so you can extend with an ordinary common lisp way.

```lisp
(defun !-printer (stream exp)
  (format stream "~<~A~S~:>" exp))
(set-pprint-dispatch '(cons (eql #\!) (cons * null)) '!-printer)
(print-as-code '(#\! hoge))
=> !hoge
```

For detail, see [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/22_bb.htm).

### Load external formatters.
Trivial-formatter can load external formatters.
You can write extension codes as external formatters.

External formatters file must be named "formatters.lisp".
Trivial-formatter searches file from `*external-formatters-directories*`.
<del>The default is quicklisp's local-projects directory and roswell's local-projects directory.</del>

Since to share and/or to control versions of external formatters file by a version control system (e.g. git),
external-formatter directories that are directly under local-projects directories of quicklisp and roswell were
added as the default search paths.
(e.g. ~/.roswell/local-projects/external-formatter/)

Hence external formatters file directly placed under local-projects directories of quicklisp or roswell is
deprecated from version 10.

### Package trivial-formatter-user.
In trivial-formatter-user, you can use deformatter, pprint-fun-call, and pprint-linear-elt with ordinary common lisp symbols.

#### PPRINT-FUN-CALL
PPRINT-FUN-CALL cares about key-value pairs.

```lisp
(pprint-fun-call nil '(asdf:component-pathname component :direction :output :if-does-not-exist :create :if-exists if-exists))
(ASDF:COMPONENT-PATHNAME COMPONENT
                         :DIRECTION :OUTPUT
                         :IF-DOES-NOT-EXIST :CREATE
                         :IF-EXISTS IF-EXISTS))
NIL
```

#### PPRINT-LINEAR-ELT
`PPRINT-LINEAR-ELT` set indent current and put every `CDR` element with a newline.

```lisp
(pprint-linear-elt nil '(asdf:component-pathname component :direction :output :if-does-not-exist :create :if-exists if-exists))
(ASDF:COMPONENT-PATHNAME COMPONENT
                         :DIRECTION
                         :OUTPUT
                         :IF-DOES-NOT-EXIST
                         :CREATE
                         :IF-EXISTS
                         IF-EXISTS))
NIL
```

#### DEFORMATTER
DEFORMATTER care about package existence and symbol confliction and set-pprint-dispatch.

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

#### TRIVIAL-FORMATTER-USER:SET-PPRINT-DISPATCH
When you want to set pretty-printing functions temporarily, you need to bind `*PRINT-PPRINT-DISPATCH*` and use `TRIVIAL-FORMATTER-USER:SET-PPRINT-DISPATCH` instead of `CL:SET-PPRINT-DISPATCH` otherwise temporal function never worked.

`CL:SET-PPRINT-DISPATCH` is shadowed in the package `:TRIVIAL-FORMATTER-USER`.

```lisp
(deformatter sxql where (stream exp)
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(cons (member :and :or)) 'pprint-linear-elt)
    (pprint-fun-call stream exp)))
```

### Product's goal

### License
MIT

### Developed with
SBCL

### Tested with
* SBCL/2.1.11
* ECL/20.4.24
* CMUCL/21D

* CCL/1.12.1 ; Failed but pprit portability issue.
* Allegro/10.1 ; Failed but pprit portability issue.

* CLISP/2.49 ; Failed, not supported. For details see below.
* ABCL/1.8.0 ; Failed, not supported. For details see below.

#### Note
Trivial-formatter works portable at least implementations above.
But it never means works samely.
For example, the `IF` format is different.
SBCL prints a newline even if elements are short, but other implementations may not.

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

#### ABCL
ABCL is not supported due to its issues.
Related issues are [this](https://github.com/armedbear/abcl/issues/406).
And [this](https://github.com/armedbear/abcl/issues/408). (Already fixed but not released yet.)

#### Reader.
When the reader macro conflicts, such reader macros are ignored silently.
You can add new reader macros, but can not modify already existing reader macros.

#### FORMAT-CONTROL
Trivial-formatter can not adjust ~newline format control indentation.

## Installation
To install trivial-formatter, [roswell](https://github.com/roswell/roswell) is recommended.

```shell
$ ros install hyotang666/trivial-formatter
```

To load trivial-formatter to running lisp environment, evaluate below in the REPL.

```lisp
* (ql:quickload :trivial-formatter)
```
