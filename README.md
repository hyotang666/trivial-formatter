# TRIVIAL-FORMATTER 4.0.0
## What is this?
Code formatter for common lisp.

## Usage

```lisp
(fmt :your-system :supersede)
```

For detail, see spec files.

## From developer
### Reader.
Trivial-formatter heavily depends on readtable especially [NAMED-READTABLES](https://github.com/melisgl/named-readtables).
You can extend reader with ordinary common lisp way.

```lisp
(let ((*readtable* (named-readtables:copy-named-readtable 'as-code)))
  (set-macro-character #\! (lambda (stream char) `(list ,char ,(read stream))))
  (read-as-code))
!hoge
=> (LIST #\! HOGE)
```

For details, see [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/c_reader.htm)
and [NAMED-READTABLES](https://github.com/melisgl/named-readtables).

### Printer.
Trivial-formatter heavily depends on pretty printing system.
You can extend code format with ordinary common lisp way.

```lisp
(defun your-printer (stream exp)
  ...)
(set-pprint-dispatch '(cons (member your-fun)) 'your-printer)
(print-as-code '(your-fun ...))
```

For detail, see [CLHS](http://www.lispworks.com/documentation/HyperSpec/Body/22_bb.htm).

### Product's goal

### License
MIT

### Developed with
SBCL/2.0.0

### Tested with
* SBCL/2.0.0
* CCL/1.11.5
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
CCL is not recommended due to backquote works in readtime.
If your project does not have backquote, trivial-formatter will work fine.

```lisp
#+ccl
'`(a ,(cdr '(1 2 3)) b)

=> (LIST* 'A (LIST* (CDR '(1 2 3)) '(B)))
```

## Installation

