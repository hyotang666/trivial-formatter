# TRIVIAL-FORMATTER 3.26.2
## What is this?
Code formatter for common lisp.

## Usage

```lisp
(fmt :your-system :supersede)
```

For detail, see spec files.

## From developer
Trivial-formatter heavily depends on pretty printing system.
You can extend code format with ordinary common lisp way.
For detail, see CLHS.

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
CLISP is not supported due to mandatory newline does not work.

```lisp
#+clisp
(format nil "~<~:@_~:>" nil)

=> ""
```

#### CCL
CCL is not recommended due to backquote works in readtime.
If your project does not have backquote, trivial-formatter will work fine.

```lisp
#+ccl
'`(a ,(cdr '(1 2 3)) b)

=> (LIST* 'A (LIST* (CDR '(1 2 3)) '(B)))
```

## Installation

