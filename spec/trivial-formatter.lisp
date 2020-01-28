(defpackage :trivial-formatter.spec
  (:use :cl :jingoh :trivial-formatter))
(in-package :trivial-formatter.spec)
(setup :trivial-formatter)

(requirements-about *OUTPUT-HOOK* :doc-type variable)

;;;; Description:

;;;; Value type is (OR SYMBOL FUNCTION)
;#? *OUTPUT-HOOK* :be-the ???

; Initial value is `DEBUG-PRINTER`

;;;; Affected By:

;;;; Notes:

(requirements-about FMT :doc-type function)

;;;; Description:

#+syntax
(FMT system) ; => result

;;;; Arguments and Values:

; system := (or symbol (vector character) (vector nil) base-string system)

; result := null

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about READ-AS-CODE :doc-type function)

;;;; Description:

#+syntax
(READ-AS-CODE &optional stream (eof-error-p t) (eof-value nil) (recursive-p nil)) ; => result

;;;; Arguments and Values:

; stream := (or null stream)

; eof-error-p := boolean

; eof-value := t

; recursive-p := boolean

; result := t

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about PRINT-AS-CODE :doc-type function)

;;;; Description:

#+syntax
(PRINT-AS-CODE exp) ; => result

;;;; Arguments and Values:

; exp := t

; result := null

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about DEBUG-PRINTER :doc-type function)

;;;; Description:

#+syntax
(DEBUG-PRINTER component) ; => result

;;;; Arguments and Values:

; component := component

; result := null

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about RENAMER :doc-type function)

;;;; Description:

#+syntax
(RENAMER component) ; => result

;;;; Arguments and Values:

; component := component

; result := null

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about APPENDER :doc-type function)

;;;; Description:

#+syntax
(APPENDER component) ; => result

;;;; Arguments and Values:

; component := component

; result := null

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about REPLACER :doc-type function)

;;;; Description:

#+syntax
(REPLACER component) ; => result

;;;; Arguments and Values:

; component := component

; result := null

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

