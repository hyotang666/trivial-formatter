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
           #:*foreign-formatters-directories*
           #:*strict-loop-keyword-p*))

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
  (funcall
    (formatter "~:<~W~^ ~3I~@_~W~^ ~@_~W~^ ~@_~W~1I~^~@:_~@{~W~^~:@_~}~:>")
    stream exp))

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
               ;; In order to open file for superseding,
               ;; we need to close it beforehand.
               (with-output-to-string (*standard-output*)
                 (debug-printer component))))
          (with-open-file (*standard-output* (asdf:component-pathname
                                               component)
                           :direction :output
                           :if-does-not-exist :create
                           :if-exists if-exists)
            (write-string string))))))

(declaim
 (ftype (function (asdf:component)
         (values list ; of-type asdf:cl-source-file
                 &optional))
        component-children))

(defun component-children (component)
  (labels ((rec (list acc primary-system-name)
             (if (endp list)
                 acc
                 (body (car list) (cdr list) acc primary-system-name)))
           (secondary-systems (first primary-system-name)
             (loop :for system :in (asdf:system-depends-on first)
                   :if (equal primary-system-name
                              (asdf:primary-system-name system))
                     :collect (asdf:find-system system)))
           (cl-source-file-p (first)
             (let ((class
                    (asdf/component:module-default-component-class first)))
               (and class (eq 'asdf:cl-source-file (class-name class)))))
           (absolute-pathname (c)
             (slot-value c 'asdf/component:absolute-pathname))
           (body (first rest acc primary-system-name)
             (typecase first
               (asdf:package-inferred-system
                (rec (nconc (secondary-systems first primary-system-name) rest)
                     (if (cl-source-file-p first)
                         (union (asdf:component-children first) acc
                                :test #'equal
                                :key #'absolute-pathname)
                         acc)
                     primary-system-name))
               (asdf:system
                (rec (append (asdf:component-children first) rest) acc
                     primary-system-name))
               (asdf:module
                (rec (append (asdf:component-children first) rest) acc
                     primary-system-name))
               (asdf:static-file (rec rest acc primary-system-name))
               (otherwise (rec rest (cons first acc) primary-system-name)))))
    (rec (list component) nil
         (when (typep component 'asdf:package-inferred-system)
           (asdf:primary-system-name component)))))

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
            (let ((notation
                   (canonicalize-case
                     (read-as-string:read-as-string nil eof-error-p eof-value
                                                    recursive-p))))
              (if (every (lambda (c) (char= #\. c)) notation)
                  (make-dot :notation notation)
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
                      value)))))))))

(defun canonicalize-case (string)
  (flet ((convert-all (converter)
           (do ((new (copy-seq string))
                (index 0))
               ((not (array-in-bounds-p string index)) new)
             (case (char string index)
               (#\\ ; single escape.
                (incf index 2))
               (#\| ; multiple escape.
                (incf index)
                (do ((char (char string index) (char string index)))
                    ((char= #\| char) (incf index))
                  (incf index
                        (if (char= #\\ char) ; single escape.
                            2
                            1))))
               (otherwise
                (setf (aref new index) (funcall converter (char string index)))
                (incf index))))))
    (ecase (readtable-case *readtable*)
      ((:upcase :downcase) (convert-all #'char-downcase))
      ((:preserve :invert) string))))

(defvar *brokens* nil)

(defun mark-it (symbol notation)
  (setf (get symbol 'notation) notation)
  (push symbol *brokens*)
  symbol)

(defun cleanup-brokens ()
  (dolist (symbol *brokens*) (remprop symbol 'notation))
  (setf *brokens* nil))

(defun make-broken-symbol (notation)
  (let ((symbol (gensym)))
    (setf (symbol-function symbol) #'make-broken-symbol) ; as dummy.
    (mark-it symbol notation)))

(defun valid-value-p (thing notation)
  (or (not (symbolp thing))
      (keywordp thing)
      (null (symbol-package thing))
      (and (nth-value 1 (find-symbol (symbol-name thing)))
           ;; If programmer specify :: explicity, there may a reason.
           ;; We should keep it.
           (not (search "::" notation))
           (or (not (find #\: notation)) ; Please do not use #\: as package or
                                         ; symbol name!
               (every #'char-equal (package-name (symbol-package thing))
                      notation)))))

;;;; META-OBJECT
;;; DOT

(defstruct dot notation)

(defmethod print-object ((dot dot) stream)
  (write-string (dot-notation dot) stream))

;;; COMMENT

(defstruct comment content)

(defstruct (line-comment (:include comment)))

(defmethod print-object ((c line-comment) stream)
  (if (uiop:string-prefix-p #\; (comment-content c))
      (funcall (formatter "~:@_;~A") stream (comment-content c))
      (funcall (formatter "~:@_; ~A") stream (comment-content c)))
  (pprint-newline :mandatory stream)
  (write-char #\Nul stream))

(defstruct (block-comment (:include comment)))

(defmethod print-object ((c block-comment) stream)
  (funcall (formatter "~A") stream (comment-content c)))

;;; CONDITIONAL

(defstruct conditional char condition)

(defmethod print-object ((c conditional) stream)
  (funcall (formatter "~_#~A~A") stream (conditional-char c)
           (conditional-condition c)))

;;; READ-TIME-EVAL

(defstruct read-time-eval form)

(defmethod print-object ((form read-time-eval) stream)
  (funcall (formatter "~<#.~;~W~:>") stream (list (read-time-eval-form form))))

;;; SHARED-OBJECT

(defstruct shared-object number exp)

(defmethod print-object ((obj shared-object) stream)
  (funcall (formatter "#~D=~S") stream (shared-object-number obj)
           (shared-object-exp obj)))

;;; SHARED-REFERENCE

(defstruct shared-reference number)

(defmethod print-object ((ref shared-reference) stream)
  (funcall (formatter "#~D#") stream (shared-reference-number ref)))

;;; COMMA

(defstruct comma sub-char form)

(defmethod print-object ((c comma) stream)
  (let (#+sbcl (sb-pretty:*pprint-quote-with-syntactic-sugar* t))
    (funcall (formatter ",~@[~C~]~W") stream (comma-sub-char c)
             (comma-form c))))

;;; BACKQUOTE

(defstruct backquote form)

(defmethod print-object ((b backquote) stream)
  (funcall (formatter "`~W") stream (backquote-form b)))

;;;; RADIX

(defstruct radix char radix number)

(defmethod print-object ((r radix) stream)
  (funcall (formatter "#~C~VR") stream (radix-char r) (radix-radix r)
           (radix-number r)))

;;;; MACRO CHARS

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

(defun |radix-reader| (stream character number)
  (let ((integer
         (funcall
           (get-dispatch-macro-character #\# character (copy-readtable nil))
           stream character number)))
    (make-radix :char (char-downcase character)
                :radix (ecase character
                         ((#\b #\B) 2)
                         ((#\o #\O) 8)
                         ((#\x #\X) 16)
                         ((#\r #\R) number))
                :number integer)))

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
  (:dispatch-macro-char #\# #\# '|##reader|)
  (:dispatch-macro-char #\# #\B '|radix-reader|)
  (:dispatch-macro-char #\# #\O '|radix-reader|)
  (:dispatch-macro-char #\# #\X '|radix-reader|)
  (:dispatch-macro-char #\# #\R '|radix-reader|))

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
                              (cond
                                ((typep form '(cons (eql in-package)))
                                 (eval (funcall expander form env)))
                                ;; SBCL fails to macroexpand-all defun when
                                ;; function declaimed as inline.
                                ;; I beleave there is no in-package inside defun.
                                ((typep form
                                        '(cons
                                           (member defun #+sbcl sb-c:xdefun)))
                                 nil)
                                (t (funcall expander form env)))))
                           (*print-length*)
                           (string
                            (with-output-to-string (s)
                              (unwind-protect (print-as-code exp s)
                                (cleanup-brokens)))))
                      ;; to ignore reading top level conditional.
                      ;; We believe there is no case e.g. #+hoge (in-package :fuga)
                      (when (listp exp)
                        (macroexpand-all (read-from-string string nil)))
                      (write-string string))
                    (setf next (read-as-code input nil tag))
                    (when (typep exp 'conditional)
                      (terpri)
                      (unwind-protect (print-as-code next) (cleanup-brokens))
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
      (setf *package* package))))

;;;; PRETTY PRINTERS

(defun init-table ()
  (let ((*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch '(eql #\Space) (formatter "#\\~:C"))
    (set-pprint-dispatch 'symbol 'symbol-printer)
    (set-pprint-dispatch '(cons (member handler-case)) 'pprint-handler-case)
    (set-pprint-dispatch '(cons (member loop)) 'pprint-extended-loop)
    (set-pprint-dispatch '(cons (member define-condition))
                         'pprint-define-condition)
    (set-pprint-dispatch '(cons (member or and values)) 'pprint-linear-elt)
    (set-pprint-dispatch '(cons (member flet labels)) 'pprint-flet)
    (set-pprint-dispatch '(cons (member when unless)) 'pprint-when)
    (set-pprint-dispatch '(cons (member restart-case)) 'pprint-restart-case)
    (set-pprint-dispatch '(cons (member restart-bind)) 'pprint-restart-bind)
    (set-pprint-dispatch '(cons (member cond)) 'pprint-cond)
    (set-pprint-dispatch '(cons (member with-open-file))
                         'pprint-with-open-file)
    (set-pprint-dispatch '(cons (member ftype)) 'pprint-ftype)
    (set-pprint-dispatch '(cons (member assert)) 'pprint-assert)
    (set-pprint-dispatch '(cons (member defclass)) 'pprint-defclass)
    (set-pprint-dispatch '(cons (member define-compiler-macro))
                         (pprint-dispatch '(defun) nil))
    (set-pprint-dispatch '(cons (member defstruct)) 'pprint-defstruct)
    (set-pprint-dispatch '(cons (member defgeneric)) 'pprint-defgeneric)
    (set-pprint-dispatch '(cons (member pushnew)) 'pprint-fun-call)
    *print-pprint-dispatch*))

(defparameter *pprint-dispatch* *print-pprint-dispatch*)

(defun pprint-defclass (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-block.
                    "~W~^ ~3I~@_" ; operator
                    "~W~^ ~@_" ; class-name
                    "~:<~@{~W~^ ~@_~}~:>~^ ~1I~:_" ; superclasses
                    "~:<~@{~/trivial-formatter::pprint-fun-call/~^ ~_~}~:>~^ ~_" ; slots
                    "~@{~W~^ ~_~}" ; options
                    "~:>"))
    stream exp))

(defun pprint-ftype (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~:<~W~^ ~1I~@_~:I~^~W~^ ~_~@{~W~^ ~_~}~:>") stream exp))

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
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~3I~:_" ; operator
                      "~W~^ ~1I~_" ; form
                      (list "~@{" ; cluases.
                            (list "~:<" ; each clause logical-block.
                                  "~W~^ ~@_" ; condition.
                                  "~:<~@{~W~^ ~@_~}~:>~^ ~1I~:@_" ; lambda-list
                                  "~@{~W~^ ~_~}" ; clause-body
                                  "~:>")
                            "~^ ~_~}")
                      "~:>"))))
    stream exp))

(defun pprint-define-condition (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~@_" ; op
                      "~W~^ ~3I~@_" ; name
                      "~:<~@{~W~^ ~@_~}~:>~^ ~1I~_" ; superclasses.
                      (list "~:<" ; slots.
                            (list "~@{" ; each slot.
                                  "~/trivial-formatter::pprint-fun-call/~^ ~:@_"
                                  "~}")
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; options.
                      "~:>"))))
    stream exp))

(defun pprint-linear-elt (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~:<~^~W~^ ~:I~@{~W~^ ~_~}~:>") stream exp))

(defun pprint-flet (stream exp)
  (setf stream (or stream *standard-output*))
  (let ((printer (pprint-dispatch exp (copy-pprint-dispatch nil)))
        (local-funs
         (and (listp (second exp))
              (loop :for x :in (second exp)
                    :when (listp x)
                      :collect (car x))))
        (*print-pprint-dispatch* (copy-pprint-dispatch)))
    (set-pprint-dispatch 'list
                         (lambda (stream exp &rest noise)
                           (declare (ignore noise))
                           (if (and (symbolp (car exp))
                                    (not (keywordp (car exp)))
                                    (not (special-operator-p (car exp)))
                                    (not (macro-function (car exp)))
                                    (or (find (car exp) local-funs)
                                        (eq
                                          (pprint-dispatch exp
                                                           *pprint-dispatch*)
                                          (pprint-dispatch exp nil))))
                               (pprint-fun-call stream exp)
                               (funcall (pprint-dispatch exp *pprint-dispatch*)
                                        stream exp))))
    (funcall printer stream exp)))

(defun pprint-cond (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~1I~:_~:I" ; op.
                      (list "~@{" ; clauses.
                            (list "~:<~^" ; each clause logical block.
                                  "~W~^ ~_" ; pred.
                                  "~W~^ ~:@_" ; first body.
                                  "~@{~W~^ ~_~}" ; rest body.
                                  "~:>~^ ~_")
                            "~}")
                      "~:>"))))
    stream exp))

(defun pprint-with-open-file (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" "~W~^ ~3I~@_" ; operator
                      (list "~:<~^" ; Open spec.
                            "~W~^ ~@_" ; var
                            "~W~^ ~:_" ; path
                            "~@{~W~^ ~@_~W~^ ~_~}" ; options.
                            "~:>~^ ~1I~_")
                      "~@{~W~^ ~_~}" ; body
                      "~:>"))))
    stream exp))

(defun pprint-fun-call (stream exp &optional colon? at?)
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    (pprint-exit-if-list-exhausted)
    (multiple-value-bind (pre post)
        (split-keywords exp)
      (funcall (formatter "~W~1I") stream (car pre))
      (when (or (cdr pre) post)
        (if colon?
            (funcall (formatter " ~:@_") stream)
            (funcall (formatter " ~:_") stream))
        (funcall (formatter "~@[~:I~{~W~^ ~:_~}~]") stream (cdr pre)))
      (when post
        (when at?
          (funcall (formatter "~1I") stream))
        (funcall (formatter "~:[~:_~:I~; ~_~]~{~^~W ~@_~W~^ ~_~}") stream
                 (cdr pre) post)))))

(defun pprint-list (stream exp)
  (setf stream (or stream *standard-output*))
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
  (setf stream (or stream *standard-output*))
  (funcall (formatter "~:<~W~^ ~3I~@_~W~^ ~1I~:@_~@{~^~W~^ ~_~}~:>") stream
           exp))

(defun pprint-assert (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      (list "~{" ; Pre args.
                            "~W~^ ~1I~@_" ; op
                            "~W~^ ~:_" ; form
                            "~:<~@{~W~^ ~@_~}~:>~^ ~_" ; lambda-list
                            "~@{~W~^ ~:_~}" ; pre body.
                            "~}")
                      "~@[ ~:I~:_~{~W ~@_~W~^ ~_~}~]" ; key-value pairs.
                      "~:>"))))
    stream (multiple-value-list (split-keywords exp))))

(defun pprint-restart-case (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~1I~@_" ; operator.
                      "~W~^ ~_" ; form.
                      (list "~@{" ; clauses.
                            "~/trivial-formatter:pprint-restart-case-clause/~^ ~_" ; each-clause
                            "~}")
                      "~:>"))))
    stream exp))

(defun pprint-restart-case-clause (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (if (atom exp)
      (write exp :stream stream)
      (pprint-logical-block (stream exp :prefix "(" :suffix ")")
        (pprint-exit-if-list-exhausted)
        (apply
          (formatter
           #.(apply #'concatenate 'string
                    (alexandria:flatten
                      (list "~{~W~^ ~@_~:<~^~W~:>~}" ; pre.
                            (list "~@[" ; if exists.
                                  " ~3I~_~{~W~^ ~@_~W~^ ~_~}" ; keys
                                  "~]")
                            "~^ ~1I" ; if exists body.
                            "~:*~:[~_~;~:@_~]" ; mandatory newline when keys.
                            "~@{~W~^ ~:@_~}")))) ; body.
          stream (parse-restart-clause exp)))))

(defun pprint-restart-bind (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block
                      "~W~^ ~1I~@_" ; operator.
                      (list "~:<" ; pprint-logical-block for bind.
                            (list "~@{" ; Iterate each bind.
                                  "~@/trivial-formatter:pprint-fun-call/~^ ~_"
                                  "~}")
                            "~:>~^ ~_")
                      "~@{~W~^ ~_~}" ; body
                      "~:>"))))
    stream exp))

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

(defun pprint-defstruct (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; pprint-logical-block.
                      "~W~^ ~1I~@_" ; op
                      (list "~:<" ; name&options.
                            "~W~^ ~:I~@_" ; name
                            "~@{~W~^ ~_~}" ; options
                            "~:>~^ ~_")
                      ;; documentation and slots.
                      "~@{~/trivial-formatter::pprint-fun-call/~^ ~_~}"
                      "~:>"))))
    stream exp))

(defun pprint-defgeneric (stream exp)
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    ;; DEFGENERIC
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (funcall (formatter " ~:I~@_") stream)
    ;; name
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (funcall (formatter " ~@_") stream)
    ;; lambda-list
    (write (pprint-pop) :stream stream)
    (pprint-exit-if-list-exhausted)
    (funcall (formatter " ~1I~_") stream)
    ;; options
    (loop :for elt := (pprint-pop)
          :if (or (atom elt) (not (eq :method (car elt))))
            :do (write elt :stream stream)
          :else
            :do (pprint-logical-block (stream elt :prefix "(" :suffix ")")
                  ;; :method
                  (write (pprint-pop) :stream stream)
                  (pprint-exit-if-list-exhausted)
                  (funcall (formatter " ~1I~@_") stream)
                  (loop :for elt := (pprint-pop)
                        :if (listp elt)
                          :do (pprint-method-lambda-list stream elt)
                              (pprint-exit-if-list-exhausted)
                              (write-char #\Space stream)
                              (pprint-newline :linear stream)
                              (loop-finish)
                        :else
                          :do (write elt :stream stream)
                              (pprint-exit-if-list-exhausted)
                              (write-char #\Space stream)
                              (pprint-newline :miser stream))
                  (loop (write (pprint-pop) :stream stream)
                        (pprint-exit-if-list-exhausted)
                        (write-char #\Space stream)
                        (pprint-newline :linear stream)))
          :do (pprint-exit-if-list-exhausted)
              (write-char #\Space stream)
              (pprint-newline :linear stream))))

(defun pprint-method-lambda-list (stream exp)
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream exp :prefix "(" :suffix ")")
    (loop :for elt := (pprint-pop)
          :if (find elt lambda-list-keywords)
            :do (write elt :stream stream)
                (pprint-exit-if-list-exhausted)
                (write-char #\Space stream)
                (pprint-newline :fill stream)
                (loop (write (pprint-pop) :stream stream)
                      (pprint-exit-if-list-exhausted)
                      (write-char #\Space stream)
                      (pprint-newline :fill stream))
          :else
            :do (funcall (formatter "~:<~@{~W~^ ~@_~}~:>") stream elt)
                (pprint-exit-if-list-exhausted)
                (write-char #\Space stream)
                (pprint-newline :fill stream))))

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
                    (#\|
                     (push (core-reader:read-delimited-string #\| stream)
                           line))
                    (#\;
                     (acc (format nil "~A~A" char (read-line stream)))
                     (setf line nil))
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
  (let ((*standard-output* (or stream *standard-output*)) (*print-circle*))
    (if (typep exp '(or block-comment string))
        (tagbody (prin1 exp))
        (loop :for (first . rest)
                   :on (alignment (split-to-lines (string-as-code exp)))
              :if (uiop:string-prefix-p "; " (string-left-trim " " (car rest)))
                :if (uiop:string-prefix-p "; " (string-left-trim " " first))
                  ;; Both are single semicoloned line comment.
                  ;; Integrate it as one for pritty printings.
                  :do (setf (car rest) (format nil "~A ~A" first (car rest)))
                :else
                  ;; Next one is single semicoloned line comment but FIRST.
                  ;; Both should be printed in same line.
                  :do (format t "~A " first)
                      (rplaca rest (string-left-trim " " (car rest)))
              :else :if (uiop:string-prefix-p "; " (string-left-trim " " first))
                ;; Next is not single semicoloned line comment but FIRST.
                ;; Comment should be printed.
                :do (funcall (formatter "~<; ~@;~@{~A~^ ~:_~}~:>~:[~;~%~]")
                             *standard-output*
                             (remove ""
                                     (uiop:split-string first :separator "; ")
                                     :test #'equal)
                             rest) ; To avoid unneeded newline.
              ;; Both are not single semicoloned line comment.
              :else :if (null rest)
                ;; Last line never need newline.
                :do (write-string first)
              :else :if (and (uiop:string-suffix-p first "(")
                             (not (uiop:string-suffix-p first "#\\(")))
                ;; To avoid unneeded newline. Especially &KEY.
                :do (rplaca rest
                            (format nil "~A~A" first
                                    (string-left-trim " " (car rest))))
              :else :if (= (1+ (length first))
                           (loop :for num :upfrom 0
                                 :for char :across (car rest)
                                 :while (char= #\Space char)
                                 :finally (return num)))
                ;; To avoid unneeded newline. Especially for conditional.
                :do (rplaca rest
                            (format nil "~A ~A" first
                                    (string-left-trim " " (car rest))))
              :else
                :do (write-line first)))))

;;;; loop clause

(defvar *print-clause* nil)

(defvar *indent* 5)

;;; CLAUSE

(defstruct (clause (:constructor %make-clause)) keyword forms)

(defmethod print-object ((o clause) stream)
  (if (null *print-clause*)
      (call-next-method)
      (let* ((into
              (let ((last2 (last (clause-forms o) 2)))
                (when (and (symbolp (car last2)) (string= "INTO" (car last2)))
                  last2)))
             (body
              (if into
                  (butlast (clause-forms o) 2)
                  (clause-forms o))))
        (if (clause-keyword o)
            (funcall
              (formatter "~W~@[ ~{~W~^ ~@_~}~]~:[~; ~VI~:_~{~W ~W~}~VI~]")
              stream (clause-keyword o) body into (+ 2 *indent*) into *indent*)
            (funcall (formatter "~{~W~^ ~_~}~:[~; ~VI~:_~{~W ~W~}~VI~]") stream
                     body into (+ 2 *indent*) into *indent*)))))

;;; OPTIONAL

(defstruct (optional (:include clause)))

;;; ADDITIONAL

(defstruct (additional (:include optional)))

(defmethod print-object ((c additional) stream)
  (if (null *print-clause*)
      (call-next-method)
      (let ((*indent*
             (+ 1 (length (prin1-to-string (clause-keyword c))) *indent*)))
        (funcall (formatter "~VI~W~^ ~{~W~^ ~@_~}~5I") stream *indent*
                 (clause-keyword c) (clause-forms c)))))

;;; VAR

(defstruct (var (:include clause)))

(defmethod print-object ((v var) stream)
  (if (null *print-clause*)
      (call-next-method)
      (apply (formatter "~W~^ ~:I~W~@{~^ ~:_~W~^ ~W~}~5I") stream
             (clause-keyword v) (clause-forms v))))

;;; NESTABLE

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; To muffle compiler note.
  (unless (boundp '+unbound+)
    (defconstant +unbound+ '#:unbound)))

(defstruct (nestable (:include clause)) (pred +unbound+) else end)

(defmethod print-object ((c nestable) stream)
  (if (null *print-clause*)
      (call-next-method)
      (progn
       (funcall (formatter "~2:I~W~:[ ~W~;~]") stream (clause-keyword c)
                (eq +unbound+ (nestable-pred c)) (nestable-pred c))
       (when (clause-forms c)
         (let ((*indent* (+ 2 *indent*)))
           (loop :for form :in (clause-forms c)
                 :do (funcall (formatter "~VI~:@_~W") stream *indent* form))))
       (when (nestable-else c)
         (let ((current-indent *indent*) (*indent* (+ 2 *indent*)))
           (funcall (formatter "~VI~:@_~W") stream current-indent
                    (nestable-else c))))
       (when (nestable-end c)
         (funcall (formatter "~VI~:@_~W") stream *indent* (nestable-end c)))
       (pprint-indent :block 5 stream))))

;;; ELSE

(defstruct (else (:include optional)))

(defmethod print-object ((c else) stream)
  (if (null *print-clause*)
      (call-next-method)
      (if (nestable-p (car (clause-forms c)))
          (let ((*indent* (- *indent* 2)))
            (funcall (formatter "~2:I~W ~@_~W~@[~:@_~{~W~^~:@_~}~]~5I") stream
                     (clause-keyword c) (car (clause-forms c))
                     (cdr (clause-forms c))))
          (funcall (formatter "~2:I~W~:@_~{~W~^~:@_~}~5I") stream
                   (clause-keyword c) (clause-forms c)))))

;;; END

(defstruct (end (:include clause)))

;;; OWN-BLOCK

(defstruct (own-block (:include clause)))

(defmethod print-object ((c own-block) stream)
  (if (null *print-clause*)
      (call-next-method)
      (funcall (formatter "~W~@[ ~:I~{~W~^~:@_~}~]~5I") stream
               (clause-keyword c) (clause-forms c))))

;;; CONSTRUCTOR

(defparameter *strict-loop-keyword-p* nil)

(declaim (type boolean *strict-loop-keyword-p*))

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
    :keyword (or (and *strict-loop-keyword-p*
                      (or (separation-keyword-p keyword)
                          (non-separation-keyword-p keyword)))
                 keyword)
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
               :finally :return :else :named)
             :test #'string=)))

(defun non-separation-keyword-p (thing)
  (and (symbolp thing)
       (find thing
             `(:using :being :the :each :hash-keys :hash-key :hash-values
               :hash-value :in :on :by :from :then :upfrom :downfrom :across
               :to :upto :downto :into :of-type :below := :into)
             :test #'string=)))

(defun force-to-keyword (clause)
  (typecase clause
    ((or null own-block) clause)
    (nestable
     (setf (clause-forms clause)
             (mapcar #'force-to-keyword (clause-forms clause))
           (nestable-else clause) (force-to-keyword (nestable-else clause)))
     clause)
    (optional
     (setf (clause-forms clause)
             (mapcar #'force-to-keyword (clause-forms clause)))
     clause)
    (t
     (setf (clause-forms clause)
             (loop :for (first . rest) :on (clause-forms clause) :by #'cddr
                   :collect first
                   :if rest
                     :collect (or (separation-keyword-p (car rest))
                                  (non-separation-keyword-p (car rest))
                                  (car rest))))
     clause)))

(defun parse-loop-body (body)
  (labels ((rec (list &optional acc)
             (if (endp list)
                 (nreverse acc)
                 (multiple-value-bind (obj rest)
                     (pick-clause list)
                   (rec rest (cons obj acc))))))
    (if *strict-loop-keyword-p*
        (mapcar #'force-to-keyword (rec (make-loop-clauses body)))
        (rec (make-loop-clauses body)))))

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
                          (optional-p temp)
                          (end-p temp)
                          (let ((last (last (clause-forms temp))))
                            (and last
                                 (not
                                   (and (symbolp (car last))
                                        (find (car last)
                                              '(:into :below :upto :to :downto)
                                              :test #'string=)))))))
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

(defgeneric make-nest (nestable first rest)
  (:method ((nestable nestable) (first null) rest) (values nestable nil))
  (:method ((nestable nestable) (end end) rest)
    (cond
      ((null (nestable-end nestable))
       (setf (nestable-end nestable) end)
       (values nestable rest))
      (t (values nestable (cons end rest)))))
  (:method ((nestable nestable) (nest nestable) rest)
    (cond
      ((null (clause-forms nestable))
       (multiple-value-bind (obj list)
           (make-nest nest (car rest) (cdr rest))
         (setf (clause-forms nestable) (list obj))
         (make-nest nestable (car list) (cdr list))))
      ((let ((last-form (car (last (clause-forms nestable)))))
         (and (additional-p last-form) (null (clause-forms last-form))))
       (multiple-value-bind (obj list)
           (make-nest nest (car rest) (cdr rest))
         (setf (clause-forms (car (last (clause-forms nestable)))) (list obj))
         (make-nest nestable (car list) (cdr list))))
      ((let ((else (nestable-else nestable)))
         (and else (null (clause-forms else))))
       (multiple-value-bind (obj list)
           (make-nest nest (car rest) (cdr rest))
         (setf (clause-forms (nestable-else nestable)) (list obj))
         (make-nest nestable (car list) (cdr list))))
      ((and (nestable-else nestable)
            (additional-p (car (last (clause-forms (nestable-else nestable)))))
            (null (car (last (clause-forms (nestable-else nestable))))))
       (multiple-value-bind (obj list)
           (make-nest nest (car rest) (cdr rest))
         (setf (clause-forms (nestable-else nestable))
                 (append (clause-forms (nestable-else nestable)) (list obj)))
         (make-nest nestable (car list) (cdr list))))
      (t (values nestable (cons nest rest)))))
  (:method ((nestable nestable) (additional additional) rest)
    (cond
      ((and (nestable-else nestable) (clause-forms (nestable-else nestable)))
       (setf (clause-forms (nestable-else nestable))
               (append (clause-forms (nestable-else nestable))
                       (list additional)))
       (make-nest nestable (car rest) (cdr rest)))
      ((and (nestable-forms nestable) (null (nestable-else nestable)))
       (setf (clause-forms nestable)
               (append (clause-forms nestable) (list additional)))
       (make-nest nestable (car rest) (cdr rest)))
      (t (values nestable (cons additional rest)))))
  (:method ((nestable nestable) (else else) rest)
    (cond
      ((null (nestable-else nestable))
       (when (and (clause-forms else) (every #'comment-p (clause-forms else)))
         (setf (clause-forms else)
                 (nconc (clause-forms else) (list (pop rest)))))
       (setf (nestable-else nestable) else)
       (make-nest nestable (car rest) (cdr rest)))
      (t (values nestable (cons else rest)))))
  (:method ((nestable nestable) first rest)
    (cond
      ((null (clause-forms nestable))
       (when (every #'comment-p (clause-forms first))
         (setf (clause-forms first)
                 (nconc (clause-forms first) (list (pop rest)))))
       (setf (clause-forms nestable) (list first))
       (make-nest nestable (car rest) (cdr rest)))
      ((and (additional-p (car (last (clause-forms nestable))))
            (null (clause-forms (car (last (clause-forms nestable))))))
       (setf (clause-forms (car (last (clause-forms nestable)))) (list first))
       (make-nest nestable (car rest) (cdr rest)))
      ((and (nestable-else nestable)
            (null (clause-forms (nestable-else nestable))))
       (setf (clause-forms (nestable-else nestable)) (list first))
       (make-nest nestable (car rest) (cdr rest)))
      ((and (nestable-else nestable)
            (additional-p (car (last (clause-forms (nestable-else nestable)))))
            (null
              (clause-forms
                (car (last (clause-forms (nestable-else nestable)))))))
       (setf (clause-forms
               (car (last (clause-forms (nestable-else nestable)))))
               (list first))
       (make-nest nestable (car rest) (cdr rest)))
      (t (values nestable (cons first rest))))))

(defun pprint-extended-loop (stream list)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (funcall (formatter "~W~:[~; ~:I~]") stream (car list) (cdr list))
    (let ((*print-clause* t) (*indent* 5))
      (funcall (formatter "~{~W~^~:@_~}") stream
               (parse-loop-body (cdr list))))))

;;;; Package TRIVIAL-FORMATTER-USER

(defpackage :trivial-formatter-user
  (:use :cl)
  (:shadow set-pprint-dispatch)
  (:import-from :trivial-formatter
                #:deformatter
                #:pprint-fun-call
                #:pprint-linear-elt))

(in-package :trivial-formatter-user)

(defun set-pprint-dispatch (type function)
  (cl:set-pprint-dispatch type function)
  (setf trivial-formatter::*pprint-dispatch*
          (copy-pprint-dispatch trivial-formatter::*pprint-dispatch*))
  (cl:set-pprint-dispatch type function 0 trivial-formatter::*pprint-dispatch*))