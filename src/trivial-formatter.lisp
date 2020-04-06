(in-package :cl-user)

(defpackage :trivial-formatter
  (:use :cl)
  (:import-from #.(or ; To avoid #-
                      #+ecl
                      '#:agnostic-lizard
                      '#:trivial-macroexpand-all) ; As default.
                #:macroexpand-all)
  (:export ;; Main api
           #:fmt
           ;; Useful helpers
           #:read-as-code
           #:print-as-code
           ;; Readtable name.
           #:as-code
           ;; Special variable
           #:*foreign-formatters-directories*))

(in-package :trivial-formatter)

;;;; *FOREIGN-FORMATTERS-PATHNAMES*

(declaim (type list *foreign-formatters-directories*))

(defparameter *foreign-formatters-directories*
  `(,@(when (find-package :ql)
        (symbol-value (uiop:find-symbol* "*LOCAL-PROJECT-DIRECTORIES*" :ql)))
    ,@(when (find-package :roswell)
        (symbol-value
          (uiop:find-symbol* "*LOCAL-PROJECT-DIRECTORIES*" :roswell)))))

(defun load-foreign-formatters ()
  (loop :for directory :in *foreign-formatters-directories*
        :for pathname := (merge-pathnames "formatters.lisp" directory)
        :when (probe-file pathname)
          :do (load pathname)))

;;;; DEFORMTTER

(defmacro deformatter (package symbol &body body)
  (let ((pprinter (gensym (format nil "PPRINT-~A" symbol))))
    `(when (find-package ,(string package))
       (defun ,pprinter ,@body)
       (set-pprint-dispatch
         `(cons
            (member ,(uiop:find-symbol* ,(string symbol) ,(string package))))
         ',pprinter)
       ',pprinter)))

(defun pprint-deformatter (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream "~:<~W~3I~^ ~@_~W~^ ~@_~W~^ ~@_~W~1I~^~@:_~@{~W~^~:@_~}~:>"
          exp))

(set-pprint-dispatch '(cons (member deformatter)) 'pprint-deformatter)

;;;; FMT

(declaim
 (ftype (function
         ((or symbol string asdf:system) &optional
          (member nil :append
                  :supersede :rename
                  :error :new-version
                  :rename-and-delete :overwrite))
         (values null &optional))
        fmt))

(defun fmt (system &optional (if-exists nil supplied-p))
  (asdf:load-system system)
  (load-foreign-formatters)
  (dolist (component (component-children (asdf:find-system system)))
    (if (not supplied-p)
        (debug-printer component)
        (let ((string
               (with-output-to-string (*standard-output*)
                 (debug-printer component))))
          (with-open-file (*standard-output* (asdf:component-pathname
                                               component)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists if-exists)
            (write-string string))))))

(defun component-children (component)
  (labels ((rec (list acc)
             (if (endp list)
                 acc
                 (body (car list) (cdr list) acc)))
           (body (first rest acc)
             (typecase first
               (asdf:system
                (rec (append (asdf:component-children first) rest) acc))
               (asdf:module
                (rec (append (asdf:component-children first) rest) acc))
               (asdf:static-file (rec rest acc))
               (otherwise (rec rest (cons first acc))))))
    (rec (list component) nil)))

;;;; READ-AS-CODE

(declaim
 (ftype (function (&optional (or null stream) boolean t boolean)
         (values t &optional))
        read-as-code))

(defun read-as-code
       (&optional stream (eof-error-p t) (eof-value nil) (recursive-p nil))
  (let* ((*readtable*
          (handler-bind ((named-readtables:reader-macro-conflict #'continue))
            (named-readtables:merge-readtables-into (copy-readtable)
                                                    (named-readtables:copy-named-readtable
                                                      'as-code))))
         (*standard-input* (or stream *standard-input*)))
    (handler-case (peek-char t)
      (end-of-file (c)
        (if eof-error-p
            (error c)
            eof-value))
      (:no-error (char)
        (if (get-macro-character char)
            (read *standard-input* eof-error-p eof-value recursive-p)
            (let* ((notation
                    (canonicalize-case
                      (read-as-string:read-as-string nil eof-error-p eof-value
                                                     recursive-p))))
              (handler-case (values (read-from-string notation))
                #+ecl
                (error (c)
                  (if (search "There is no package with the name"
                              (princ-to-string c))
                      (make-broken-symbol notation)
                      (error c)))
                (package-error ()
                  (make-broken-symbol notation))
                (:no-error (value)
                  (unless (valid-value-p value notation)
                    (mark-it value notation))
                  value))))))))

(defun canonicalize-case (string)
  (flet ((convert-all (converter)
           (uiop:reduce/strcat
             (uiop:while-collecting (acc)
               (do ((index 0))
                   ((not (array-in-bounds-p string index)))
                 (case (char string index)
                   (#\\ ; single escape.
                    (acc (char string index))
                    (acc (char string (incf index)))
                    (incf index))
                   (#\| ; multiple escape.
                    (acc (char string index))
                    (incf index)
                    (do ((char (char string index) (char string index)))
                        ((char= #\| char)
                         (acc (char string index))
                         (incf index))
                      (case char
                        (#\\ ; single escape.
                         (acc char)
                         (acc (char string (incf index)))
                         (incf index))
                        (otherwise (acc char) (incf index)))))
                   (otherwise
                    (acc (funcall converter (char string index)))
                    (incf index))))))))
    (ecase (readtable-case *readtable*)
      ((:upcase :downcase) (convert-all #'char-downcase))
      ((:preserve :invert) string))))

(defvar *brokens* nil)

(defun mark-it (symbol notation)
  (setf (get symbol 'notation) notation)
  (push symbol *brokens*)
  symbol)

(defun make-broken-symbol (notation)
  (let ((symbol (gensym)))
    (setf (symbol-function symbol) #'make-broken-symbol) ; as dummy.
    (mark-it symbol notation)))

(defun valid-value-p (thing notation)
  (or (not (symbolp thing))
      (keywordp thing)
      (null (symbol-package thing))
      (and (nth-value 1 (find-symbol (symbol-name thing)))
           (or (not (find #\: notation)) ; Please do not use #\: as package or
                                         ; symbol name!
               (every #'char-equal (package-name (symbol-package thing))
                      notation)))))

;;;; META-OBJECT
;;; DOT

(defstruct dot)

(defmethod print-object ((dot dot) stream) (princ #\. stream))

;;; COMMENT

(defstruct comment content)

(defstruct (line-comment (:include comment)))

(defmethod print-object ((c line-comment) stream)
  (if (uiop:string-prefix-p #\; (comment-content c))
      (format stream "~:@_;~A" (comment-content c))
      (format stream "~:@_; ~A" (comment-content c)))
  (pprint-newline :mandatory stream)
  (write-char #\Nul stream))

(defstruct (block-comment (:include comment)))

(defmethod print-object ((c block-comment) stream)
  (format stream "~A" (comment-content c)))

;;; CONDITIONAL

(defstruct conditional char condition)

(defmethod print-object ((c conditional) stream)
  (format stream "~_#~A~A" (conditional-char c) (conditional-condition c)))

;;; READ-TIME-EVAL

(defstruct read-time-eval form)

(defmethod print-object ((form read-time-eval) stream)
  (format stream "~<#.~;~W~:>" (list (read-time-eval-form form))))

;;; SHARED-OBJECT

(defstruct shared-object number exp)

(defmethod print-object ((obj shared-object) stream)
  (format stream "#~D=~S" (shared-object-number obj) (shared-object-exp obj)))

;;; SHARED-REFERENCE

(defstruct shared-reference number)

(defmethod print-object ((ref shared-reference) stream)
  (format stream "#~D#" (shared-reference-number ref)))

;;; COMMA

(defstruct comma sub-char form)

(defmethod print-object ((c comma) stream)
  (format stream ",~@[~C~]~W" (comma-sub-char c) (comma-form c)))

;;; BACKQUOTE

(defstruct backquote form)

(defmethod print-object ((b backquote) stream)
  (format stream "`~W" (backquote-form b)))

;;;; MACRO CHARS

(defun |dot-reader| (stream character)
  (declare (ignore stream character))
  (make-dot))

(defun |paren-reader| (stream character)
  (declare (ignore character))
  (loop :for char = (peek-char t stream)
        ;; end check.
        :if (char= #\) char)
          :do (read-char stream)
              (loop-finish)
        ;; The default.
        :else
          :collect (read-as-code stream t t t)))

(defun |line-comment-reader| (stream character)
  (declare (ignore character))
  (make-line-comment :content (string-trim " " (read-line stream))))

(defun |block-comment-reader| (stream number character)
  (make-block-comment :content (funcall #'read-as-string::|#\|reader| stream
                                        number character)))

(defun |#+-reader| (stream character number)
  (when number
    (warn "A numeric argument is ignored in #~A~A." number character))
  (make-conditional :char character :condition (read-as-code stream)))

(defun |#.reader| (stream character number)
  (declare (ignore character))
  (when number
    (warn "A numeric argument is ignored in read time eval."))
  (make-read-time-eval :form (read-as-code stream t t t)))

(defun |#=reader| (stream character number)
  (declare (ignore character))
  (make-shared-object :number number :exp (read-as-code stream)))

(defun |##reader| (stream character number)
  (declare (ignore stream character))
  (make-shared-reference :number number))

(defun |'reader| (stream character)
  (declare (ignore character))
  (list* 'quote (list (read-as-code stream))))

(defun |#'reader| (stream character number)
  (declare (ignore character))
  (when number
    (warn "Ignore numeric argument for #~D'." number))
  (list* 'function (list (read-as-code stream))))

(defun |,reader| (stream character)
  (declare (ignore character))
  (let ((sub-char (peek-char t stream)))
    (make-comma :sub-char (when (find sub-char '(#\. #\@))
                            (read-char stream))
                :form (read-as-code stream))))

(defun |`reader| (stream character)
  (declare (ignore character))
  (make-backquote :form (read-as-code stream)))

;;;; NAMED-READTABLE

(named-readtables:defreadtable as-code
  (:merge :common-lisp)
  (:macro-char #\( '|paren-reader|)
  (:macro-char #\. '|dot-reader| t)
  (:macro-char #\; '|line-comment-reader|)
  (:macro-char #\' '|'reader|)
  (:macro-char #\` '|`reader|)
  (:macro-char #\, '|,reader|)
  (:dispatch-macro-char #\# #\| '|block-comment-reader|)
  (:dispatch-macro-char #\# #\+ '|#+-reader|)
  (:dispatch-macro-char #\# #\- '|#+-reader|)
  (:dispatch-macro-char #\# #\. '|#.reader|)
  (:dispatch-macro-char #\# #\' '|#'reader|)
  (:dispatch-macro-char #\# #\= '|#=reader|)
  (:dispatch-macro-char #\# #\# '|##reader|))

;;;; DEBUG-PRINTER

(defun debug-printer (component)
  (let ((package *package*))
    (unwind-protect
        (with-open-file (input (asdf:component-pathname component))
          (loop :with tag = '#:end
                :for exp = (read-as-code input nil tag) :then next
                :with next
                :until (eq exp tag)
                :do (let* ((*macroexpand-hook*
                            (lambda (expander form env)
                              (if (typep form '(cons (eql in-package)))
                                  (eval (funcall expander form env))
                                  (funcall expander form env))))
                           (string
                            (with-output-to-string (s) (print-as-code exp s))))
                      (when (and ;; to ignore reading top level conditional.
                                 ;; We believe there is no case e.g. #+hoge (in-package :fuga)
                                 (listp exp)
                                 ;; SBCL fails to macroexpand-all defun when
                                 ;; function declaimed as inline.
                                 ;; I beleave there is no in-package inside defun.
                                 (not (typep exp '(cons (eql defun)))))
                        (macroexpand-all (read-from-string string nil)))
                      (write-string string))
                    (setf next (read-as-code input nil tag))
                    (when (typep exp 'conditional)
                      (terpri)
                      (print-as-code next)
                      (setf exp next
                            next (read-as-code input nil tag)))
                    (typecase exp
                      (block-comment (format t "~2%"))
                      (line-comment
                       (terpri)
                       (when (not (comment-p next))
                         (terpri)))
                      (t
                       (cond ((eq next tag)) ; Do nothing.
                             ((line-comment-p next)
                              (if (uiop:string-prefix-p #\;
                                                        (comment-content next))
                                  (format t "~2%")
                                  (write-char #\Space)))
                             (t (format t "~2%")))))))
      (setf *package* package)
      (loop :for symbol = (pop *brokens*)
            :while symbol
            :do (remf (symbol-plist symbol) 'notation)))))

;;;; PRETTY PRINTERS

(defun init-table ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(eql #\Space)
                         (lambda (stream object)
                           (format stream "#\\~:C" object)))
    (set-pprint-dispatch 'symbol 'symbol-printer)
    (set-pprint-dispatch '(cons (member handler-case)) 'pprint-handler-case)
    (set-pprint-dispatch '(cons (member loop)) 'pprint-extended-loop)
    (set-pprint-dispatch '(cons (member define-condition))
                         'pprint-define-condition)
    (set-pprint-dispatch '(cons (member or and)) 'pprint-linear-elt)
    (set-pprint-dispatch '(cons (member flet labels)) 'pprint-flet)
    (set-pprint-dispatch '(cons (member when unless)) 'pprint-when)
    (set-pprint-dispatch '(cons (member restart-case)) 'pprint-restart-case)
    (set-pprint-dispatch '(cons (member cond)) 'pprint-cond)
    (set-pprint-dispatch '(cons (member with-open-file))
                         'pprint-with-open-file)
    (set-pprint-dispatch '(cons (member ftype)) 'pprint-ftype)
    (set-pprint-dispatch '(cons (member assert)) 'pprint-assert)
    (set-pprint-dispatch '(cons (member defclass)) 'pprint-defclass)
    (set-pprint-dispatch '(cons (member define-compiler-macro))
                         (pprint-dispatch '(defun) nil))
    *print-pprint-dispatch*))

(defparameter *pprint-dispatch* *print-pprint-dispatch*)

(defun pprint-defclass (stream exp)
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (apply #'format stream "~W~3I~^ ~@_~W~^ ~@_~:S~^~1I ~:_" exp)
    (when (cdddr exp)
      (if (listp (cadddr exp))
          (if (every #'listp (cadddr exp))
              (format stream
                      "~:<~@{~/trivial-formatter::pprint-fun-call/~^ ~:_~}~:>"
                      (cadddr exp))
              (format stream "~:<~@{~W~^ ~_~}~:>" (cadddr exp)))
          (format stream "~W" (cadddr exp))))
    (when (cddddr exp)
      (format stream "~:@_~{~W~^ ~_~}" (cddddr exp)))))

(defun pprint-ftype (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream "~:<~W~1I~^ ~@_~:I~^~W~^ ~_~@{~W~^ ~_~}~:>" exp))

(defun shortest-package-name (package)
  (reduce
    (lambda (champion challenger)
      (if (< (length champion) (length challenger))
          champion
          challenger))
    (cons (package-name package) (package-nicknames package))))

(defun symbol-printer (stream object)
  (let ((notation (get object 'notation)))
    (if notation
        (write-string notation stream)
        (let ((default-style
               (let ((*print-pprint-dispatch* (copy-pprint-dispatch nil)))
                 (prin1-to-string object))))
          (if (or (null (symbol-package object))
                  (eq #.(find-package :keyword) (symbol-package object))
                  (nth-value 1 (find-symbol (symbol-name object))))
              (write-string default-style stream)
              (progn
               (write-string
                 (string-downcase
                   (shortest-package-name (symbol-package object)))
                 stream)
               (write-string default-style stream :start
                             ;; Please do not ever use collon as package name!
                             (position #\: default-style))))))))

(defun pprint-handler-case (stream exp &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    (write (pprint-pop) :stream stream)
    (pprint-indent :block 3 stream)
    (pprint-exit-if-list-exhausted)
    (write-char #\Space stream)
    (pprint-newline :fill stream)
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (pprint-indent :block 1 stream)
    (pprint-newline :mandatory stream)
    (loop :for form = (pprint-pop)
          :if (atom form)
            :do (write form :stream stream)
          :else :if (= 1 (length form))
            :do (format stream "~W" form)
          :else :if (= 2 (length form))
            :do (apply #'format stream "(~W ~:A)" form)
          :else
            :do (apply #'format stream "(~1:I~W ~:A~^~:@_~@{~W~^ ~:_~})" form)
          :do (pprint-exit-if-list-exhausted)
              (pprint-indent :block 1 stream)
              (pprint-newline :mandatory stream))))

(defun pprint-define-condition (stream exp &rest noise)
  (declare (ignore noise))
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    (flet ((output ()
             (write (pprint-pop) :stream stream)
             (pprint-exit-if-list-exhausted)
             (write-char #\Space stream)))
      (output) ; operater
      (output) ; name
      (pprint-indent :block 3 stream)
      (format stream "~@_~:<~@{~W~^ ~}~:>" (pprint-pop)) ; superclasses
      (pprint-exit-if-list-exhausted)
      (pprint-indent :block 1 stream)
      (write-char #\Space stream)
      (format stream "~:_~:<~@{~^~W~^~_~}~:>" (pprint-pop)) ; slots
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (pprint-newline :linear stream)
      (loop (write (pprint-pop) :stream stream)
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)))))

(defun pprint-linear-elt (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (format stream "~:<~W~^~1:I ~@{~W~^ ~_~}~:>" exp))

(defun pprint-flet (stream exp)
  (let ((printer (pprint-dispatch exp (copy-pprint-dispatch nil)))
        (*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'list
                         (lambda (stream exp &rest noise)
                           (declare (ignore noise))
                           (if (and (symbolp (car exp))
                                    (not (keywordp (car exp)))
                                    (not (special-operator-p (car exp)))
                                    (not (macro-function (car exp))))
                               (pprint-fun-call stream exp)
                               (funcall (pprint-dispatch exp *pprint-dispatch*)
                                        stream exp))))
    (funcall printer stream exp)))

(defun pprint-cond (stream exp)
  (format stream "~:<~W ~:_~:I~@{~W~^~_~}~:>" exp))

(defun pprint-with-open-file (stream exp)
  (setf stream (or stream *standard-output*))
  (if (or (null (cdr exp)) (atom (cadr exp)))
      (format stream "~:<~@{~W~^ ~@_~}~:>" exp)
      (multiple-value-bind (pre post)
          (split-keywords (cadr exp))
        (pprint-logical-block (stream nil :prefix "(" :suffix ")")
          (write (car exp) :stream stream)
          (write-char #\Space stream)
          (pprint-newline :miser stream)
          (pprint-indent :block 3 stream)
          (format stream "~:<~@[~{~W~^ ~@_~}~]~@[ ~:_~{~W~^ ~@_~W~^ ~_~}~]~:>"
                  (list pre post))
          (pprint-indent :block 1 stream)
          (pprint-newline :mandatory stream)
          (format stream "~@[~{~W~^ ~_~}~]" (cddr exp))))))

(defun pprint-fun-call (stream exp &optional colon? &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (multiple-value-bind (pre post)
      (split-keywords exp)
    (pprint-logical-block (stream nil :prefix "(" :suffix ")")
      (format stream "~W~1I" (car pre))
      (when (or (cdr pre) post)
        (if colon?
            (format stream " ~:@_")
            (format stream " ~:_"))
        (format stream "~@[~:I~{~W~^ ~:_~}~]" (cdr pre)))
      (when post
        (format stream "~:[~:_~:I~; ~_~]~{~^~W ~@_~W~^ ~_~}" (cdr pre) post)))))

(defun pprint-list (stream exp)
  (if (and (symbolp (car exp))
           (fboundp (car exp))
           (not (special-operator-p (car exp)))
           (not (macro-function (car exp)))
           (eq (pprint-dispatch exp *pprint-dispatch*)
               (pprint-dispatch exp nil)))
      (pprint-fun-call stream exp)
      (funcall (pprint-dispatch exp *pprint-dispatch*) stream exp)))

(defun split-keywords (exp)
  (do* ((list (reverse exp) (cddr list))
        pre
        post)
       ((null list) (values pre post))
    (if (keywordp (cadr list))
        (progn (push (car list) post) (push (cadr list) post))
        (return (values (reverse list) post)))))

(defun pprint-when (stream exp)
  (format stream (formatter "~:<~W~3I~^ ~@_~W~^ ~1I~:@_~@{~^~W~^ ~_~}~:>") exp))

(defun pprint-assert (stream exp)
  (setf stream (or stream *standard-output*))
  (format stream
          "~:<~{~W~1I~^ ~@_~W~^ ~@_~:S~^ ~@_~@{~W~^ ~@_~}~}~@[ ~:_~{~W ~@_~W~^ ~_~}~]~:>"
          (multiple-value-list (split-keywords exp))))

(defun pprint-restart-case (stream exp)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (format stream "~W" (car exp))
    (when (cdr exp)
      (format stream " ~3I~@_~W" (cadr exp))
      (if (cddr exp)
          (if (some #'atom (cddr exp))
              (format stream " ~@_~{~W~^ ~@_~}" (cddr exp))
              (dolist (clause (cddr exp))
                (format stream
                        " ~1I~:_~:<~{~W~^ ~@_~:S~^ ~@_~}~@[ ~3I~{~_~W~^ ~@_~W~^ ~}~]~^ ~1I~_~@{~W~^ ~:_~}~:>"
                        (parse-restart-clause clause))))))))

(defun parse-restart-clause (clause)
  (let ((pre
         (cons (car clause)
               (when (cdr clause)
                 (list (cadr clause))))))
    (loop :for list :on (cddr clause) :by #'cddr
          :while (and (keywordp (car list)) (cdr list))
          :collect (car list) :into keys
          :collect (cadr list) :into keys
          :finally (return (list* pre keys list)))))

;;;; PRINT-AS-CODE

(defun split-to-lines (string)
  (mapcan
    (lambda (line)
      (setf line
              (remove #\Nul
                      (ppcre:regex-replace #.(format nil "~C " #\Nul) line
                                           "")))
      (unless (every (lambda (char) (char= #\Space char)) line)
        (list line)))
    (uiop:while-collecting (acc)
      (with-input-from-string (stream string)
        (loop :for char := (read-char stream nil nil)
              :with line
              :while char
              :do (case char
                    (#\\ (push char line) (push (read-char stream) line))
                    (#\"
                     (push (core-reader:read-delimited-string #\" stream)
                           line))
                    (#\Newline
                     (acc (format nil "~{~A~}" (nreverse line)))
                     (setf line nil))
                    (otherwise (push char line)))
              :finally (when line
                         (acc (format nil "~{~A~}" (nreverse line)))))))))

(defun alignment (list)
  (labels ((rec (list &optional acc)
             (if (endp list)
                 acc
                 (body (car list) (cdr list) acc)))
           (body (first rest acc)
             (if (uiop:string-prefix-p ";;" (string-left-trim " " (car rest)))
                 (rec (cons (set-align first (car rest)) (cdr rest))
                      (cons first acc))
                 (rec rest (cons first acc))))
           (set-align (current comment)
             (with-output-to-string (*standard-output*)
               (loop :for char :across current
                     :while (char= #\Space char)
                     :do (write-char char))
               (write-string (string-left-trim " " comment)))))
    (rec (reverse list))))

(defun string-as-code (exp)
  (let* ((*print-case* :downcase)
         (*print-pprint-dispatch* (init-table))
         (*pprint-dispatch* (init-table))
         (*print-pretty* t))
    (set-pprint-dispatch 'list 'pprint-list)
    (prin1-to-string exp)))

(declaim
 (ftype (function (t &optional (or null stream)) (values null &optional))
        print-as-code))

(defun print-as-code (exp &optional stream)
  (let ((*standard-output* (or stream *standard-output*)))
    (if (typep exp '(or block-comment string))
        (tagbody (prin1 exp))
        (loop :for (first . rest)
                   :on (alignment (split-to-lines (string-as-code exp)))
              :do (if (uiop:string-prefix-p "; "
                                            (string-left-trim " " (car rest)))
                      (if (uiop:string-prefix-p "; "
                                                (string-left-trim " " first))
                          ;; Both are single semicoloned line comment.
                          ;; Integrate it as one for pritty printings.
                          (setf (car rest)
                                  (format nil "~A ~A" first (car rest)))
                          ;; Next one is single semicoloned line comment but FIRST.
                          ;; Both should be printed in same line.
                          (progn
                           (format t "~A " first)
                           (rplaca rest (string-left-trim " " (car rest)))))
                      (if (uiop:string-prefix-p "; "
                                                (string-left-trim " " first))
                          ;; Next is not single semicoloned line comment but FIRST.
                          ;; Comment should be printed.
                          (format t "~<; ~@;~@{~A~^ ~:_~}~:>~:[~;~%~]"
                                  (remove ""
                                          (uiop:split-string first
                                                             :separator "; ")
                                          :test #'equal)
                                  rest) ; To avoid unneeded newline.
                          ;; Both are not single semicoloned line comment.
                          (if rest
                              ;; To avoid unneeded newline. Especially for conditional.
                              (if (= (1+ (length first))
                                     (loop :for num :upfrom 0
                                           :for char :across (car rest)
                                           :while (char= #\Space char)
                                           :finally (return num)))
                                  (rplaca rest
                                          (format nil "~A ~A" first
                                                  (string-left-trim " "
                                                                    (car
                                                                      rest))))
                                  (write-line first))
                              ;; Last line never need newline.
                              (write-string first))))))))

;;;; loop clause

(defvar *print-clause* nil)

(defvar *indent* 5)

;;; CLAUSE

(defstruct (clause (:constructor %make-clause)) keyword forms)

(defmethod print-object ((o clause) stream)
  (if (null *print-clause*)
      (call-next-method)
      (if (clause-keyword o)
          (format stream "~W~@[ ~{~W~^ ~@_~}~]" (clause-keyword o)
                  (clause-forms o))
          (format stream "~{~W~^ ~_~}" (clause-forms o)))))

;;; OPTIONAL

(defstruct (optional (:include clause)))

;;; ADDITIONAL

(defstruct (additional (:include optional)))

(defmethod print-object ((c additional) stream)
  (if (null *print-clause*)
      (call-next-method)
      (let ((*indent*
             (+ 1 (length (prin1-to-string (clause-keyword c))) *indent*)))
        (format stream "~VI~W~^ ~{~W~^ ~@_~}~5I" *indent* (clause-keyword c)
                (clause-forms c)))))

;;; VAR

(defstruct (var (:include clause)))

(defmethod print-object ((v var) stream)
  (if (null *print-clause*)
      (call-next-method)
      (apply #'format stream "~W~^ ~:I~W~@{~^ ~:_~W~^ ~W~}~5I"
             (clause-keyword v) (clause-forms v))))

;;; NESTABLE

(defstruct (nestable (:include clause)) pred else end)

(defmethod print-object ((c nestable) stream)
  (if (null *print-clause*)
      (call-next-method)
      (progn
       (format stream "~2:I~W ~W" (clause-keyword c) (nestable-pred c))
       (when (clause-forms c)
         (let ((*indent* (+ 2 *indent*)))
           (loop :for form :in (clause-forms c)
                 :do (format stream "~VI~:@_~W" *indent* form))))
       (when (nestable-else c)
         (let ((current-indent *indent*) (*indent* (+ 2 *indent*)))
           (format stream "~VI~:@_~W" current-indent (nestable-else c))))
       (when (nestable-end c)
         (format stream "~VI~:@_~W" *indent* (nestable-end c)))
       (pprint-indent :block 5 stream))))

;;; ELSE

(defstruct (else (:include optional)))

(defmethod print-object ((c else) stream)
  (if (null *print-clause*)
      (call-next-method)
      (if (nestable-p (car (clause-forms c)))
          (let ((*indent* (- *indent* 2)))
            (format stream "~2:I~W ~:_~W~@[~:@_~{~W~^~:@_~}~]~5I"
                    (clause-keyword c) (car (clause-forms c))
                    (cdr (clause-forms c))))
          (format stream "~2:I~W~:@_~{~W~^~:@_~}~5I" (clause-keyword c)
                  (clause-forms c)))))

;;; END

(defstruct (end (:include clause)))

;;; OWN-BLOCK

(defstruct (own-block (:include clause)))

(defmethod print-object ((c own-block) stream)
  (if (null *print-clause*)
      (call-next-method)
      (format stream "~W~@[ ~:I~{~W~^~:@_~}~]~5I" (clause-keyword c)
              (clause-forms c))))

;;; CONSTRUCTOR

(defun make-clause (&key keyword forms)
  (funcall
    (case (separation-keyword-p keyword)
      ((:for :with :as) #'make-var)
      ((:if :when :unless) #'make-nestable)
      ((:do :doing :finally :initially) #'make-own-block)
      ((:else) #'make-else)
      ((:and) #'make-additional)
      ((:end) #'make-end)
      (otherwise #'%make-clause))
    :keyword keyword
    :forms forms))

(defun print-clause (thing)
  (let ((*print-clause* t))
    (pprint-logical-block (nil nil) (prin1 thing))))

(defun separation-keyword-p (thing)
  (and (symbolp thing)
       (find thing
             '(:and :with :for :as :collect :collecting :append :appending
               :nconc :nconcing :count :counting :sum :summing :maximize
               :maximizing :minimize :minimizing :if :when :unless :end :while
               :until :repeat :always :never :thereis :do :doing :initially
               :finally :return :else)
             :test #'string=)))

(defun parse-loop-body (body)
  (labels ((rec (list &optional acc)
             (if (endp list)
                 (nreverse acc)
                 (multiple-value-bind (obj rest)
                     (pick-clause list)
                   (rec rest (cons obj acc))))))
    (rec (make-loop-clauses body))))

(defun make-loop-clauses (body)
  (labels ((rec (list &optional temp acc)
             (if (endp list)
                 (if temp
                     (nreconc acc (list temp))
                     (nreverse acc))
                 (body (car list) (cdr list) temp acc)))
           (body (first rest temp acc)
             (if (and (separation-keyword-p first)
                      (or (null temp)
                          (not
                            (and (symbolp (car (last (clause-forms temp))))
                                 (string= "INTO"
                                          (car (last (clause-forms temp))))))))
                 ;; Make new clause.
                 (rec rest (make-clause :keyword first)
                      (if temp
                          (cons temp acc)
                          acc))
                 (typecase temp
                   (null
                    ;; Make new junk clause.
                    (rec rest (make-clause :forms (list first)) acc))
                   (nestable
                    ;; Set nestable pred, and reset temp NIL.
                    (setf (nestable-pred temp) first)
                    (rec rest nil (cons temp acc)))
                   (otherwise
                    ;; Modify temp clause.
                    (rec rest
                         (progn
                          (setf (clause-forms temp)
                                  (nconc (clause-forms temp) (list first)))
                          temp)
                         acc))))))
    (rec body)))

(declaim
 (ftype (function (list) (values clause list &optional nil)) pick-clause))

(defun pick-clause (list)
  (typecase (car list)
    (nestable (make-nest (car list) (cadr list) (cddr list)))
    (otherwise (values (car list) (cdr list)))))

(declaim
 (ftype (function (nestable (or clause null) list)
         (values nestable list &optional nil))
        make-nest))

(defun make-nest (when first rest)
  (typecase first
    (null (values when nil))
    (end
     (cond
      ((null (nestable-end when)) (setf (nestable-end when) first)
       (values when rest))
      (t (values when (cons first rest)))))
    (nestable
     (cond
      ((null (clause-forms when))
       (multiple-value-bind (obj list)
           (make-nest first (car rest) (cdr rest))
         (setf (clause-forms when) (list obj))
         (make-nest when (car list) (cdr list))))
      ((and (additional-p (car (last (clause-forms when))))
            (null (clause-forms (car (last (clause-forms when))))))
       (multiple-value-bind (obj list)
           (make-nest first (car rest) (cdr rest))
         (setf (clause-forms (car (last (clause-forms when)))) (list obj))
         (make-nest when (car list) (cdr list))))
      ((and (nestable-else when) (null (clause-forms (nestable-else when))))
       (multiple-value-bind (obj list)
           (make-nest first (car rest) (cdr rest))
         (setf (clause-forms (nestable-else when)) (list obj))
         (make-nest when (car list) (cdr list))))
      ((and (nestable-else when)
            (additional-p (car (last (clause-forms (nestable-else when)))))
            (null (car (last (clause-forms (nestable-else when))))))
       (multiple-value-bind (obj list)
           (make-nest first (car rest) (cdr rest))
         (setf (clause-forms (nestable-else when))
                 (append (clause-forms (nestable-else when)) (list obj)))
         (make-nest when (car list) (cdr list))))
      (t (values when (cons first rest)))))
    (additional
     (cond
      ((and (nestable-else when) (clause-forms (nestable-else when)))
       (setf (clause-forms (nestable-else when))
               (append (clause-forms (nestable-else when)) (list first)))
       (make-nest when (car rest) (cdr rest)))
      ((and (nestable-forms when) (null (nestable-else when)))
       (setf (clause-forms when) (append (clause-forms when) (list first)))
       (make-nest when (car rest) (cdr rest)))
      (t (values when (cons first rest)))))
    (else
     (cond
      ((null (nestable-else when)) (setf (nestable-else when) first)
       (make-nest when (car rest) (cdr rest)))
      (t (values when (cons first rest)))))
    (otherwise
     (cond
      ((null (clause-forms when)) (setf (clause-forms when) (list first))
       (make-nest when (car rest) (cdr rest)))
      ((and (additional-p (car (last (clause-forms when))))
            (null (clause-forms (car (last (clause-forms when))))))
       (setf (clause-forms (car (last (clause-forms when)))) (list first))
       (make-nest when (car rest) (cdr rest)))
      ((and (nestable-else when) (null (clause-forms (nestable-else when))))
       (setf (clause-forms (nestable-else when)) (list first))
       (make-nest when (car rest) (cdr rest)))
      ((and (nestable-else when)
            (additional-p (car (last (clause-forms (nestable-else when)))))
            (null
              (clause-forms (car (last (clause-forms (nestable-else when)))))))
       (setf (clause-forms (car (last (clause-forms (nestable-else when)))))
               (list first))
       (make-nest when (car rest) (cdr rest)))
      (t (values when (cons first rest)))))))

(defun pprint-extended-loop (stream list)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (format stream "~W~:[~; ~:I~]" (car list) (cdr list))
    (let ((*print-clause* t))
      (format stream "~{~W~^~:@_~}" (parse-loop-body (cdr list))))))

;;;; Package TRIVIAL-FORMATTER-USER

(defpackage :trivial-formatter-user
  (:use :cl)
  (:import-from :trivial-formatter #:deformatter #:pprint-fun-call))