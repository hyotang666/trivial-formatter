(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export
    ;; Main api
    #:fmt
    ;; Useful helpers
    #:read-as-code
    #:print-as-code
    ))
(in-package :trivial-formatter)

;;;; FMT
(declaim (ftype (function ((or symbol string asdf:system)
                           &optional
                           (member nil :append :supersede :rename :error :new-version
                                   :rename-and-delete :overwrite))
                          (values null &optional))
                fmt))
(defun fmt (system &optional(if-exists nil supplied-p))
  (asdf:load-system system)
  (dolist (component (asdf:component-children (asdf:find-system system)))
    (if supplied-p
      (with-open-file(s (asdf:component-pathname component
                                                 :direction :output
                                                 :if-does-not-exist :create
                                                 :if-exists if-exists))
        (debug-printer component))
      (debug-printer component))))

;;;; READ-AS-CODE
(declaim (ftype (function (&optional
                            (or null stream)
                            boolean
                            T
                            boolean)
                          (values t &optional))
                read-as-code))
(defun read-as-code (&optional
                      stream
                      (eof-error-p T)
                      (eof-value nil)
                      (recursive-p nil))
  (let* ((*readtable*
           (named-readtables:find-readtable 'as-code))
         (*standard-input*
           (or stream *standard-input*))
         (char
           (peek-char t nil eof-error-p eof-value)))
    (if (eq char eof-value)
      eof-value
      (if(get-macro-character char)
        (read *standard-input* eof-error-p eof-value recursive-p)
        (let((notation
               (read-as-string:read-as-string nil eof-error-p eof-value recursive-p)))
          (handler-case(values(read-from-string notation))
            #+ecl
            (error(c)
              (if(search "There is no package with the name"
                         (princ-to-string c))
                (make-broken-symbol :notation notation)
                (error c)))
            (package-error()
              (make-broken-symbol :notation notation))))))))

;;;; META-OBJECT
;;; DOT
(defstruct dot)
(defmethod print-object ((dot dot) stream)
  (princ #\. stream))

;;; COMMENT
(defstruct comment content)
(defstruct (line-comment (:include comment)))
(defmethod print-object ((c line-comment)stream)
  (if (uiop:string-prefix-p #\; (comment-content c))
    (progn (pprint-newline :mandatory stream)
           (format stream ";~A" (comment-content c)))
    (format stream "; ~A" (comment-content c)))
  (pprint-newline :mandatory stream)
  (write-char #\null stream))
(defstruct (block-comment (:include comment)))
(defmethod print-object((c block-comment) stream)
  (format stream "~A" (comment-content c)))

;;; CONDITIONAL
(defstruct conditional
  char
  condition)
(defmethod print-object ((c conditional) stream)
  (format stream "~_#~A~A"
          (conditional-char c)
          (conditional-condition c)))

;;; BROKEN-SYMBOL
(defstruct broken-symbol
  notation)
(defmethod print-object ((symbol broken-symbol)stream)
  (write-string (broken-symbol-notation symbol) stream))

;;; READ-TIME-EVAL
(defstruct read-time-eval
  form)
(defmethod print-object((form read-time-eval)stream)
  (format stream "~<#.~;~^~@{~A~^~:@_~}~:>"
          (uiop:split-string (prin1-to-string (read-time-eval-form form))
                             :separator '(#\newline))))

;;; SHARED-OBJECT
(defstruct shared-object number exp)
(defmethod print-object((obj shared-object)stream)
  (format stream "#~D=~S"
          (shared-object-number obj)
          (shared-object-exp obj)))

;;; SHARED-REFERENCE
(defstruct shared-reference number)
(defmethod print-object((ref shared-reference)stream)
  (format stream "#~D#" (shared-reference-number ref)))

;;;; MACRO CHARS
(defun |dot-reader| (stream character)
  (declare(ignore stream character))
  (make-dot))

(defun |paren-reader|(stream character)
  (declare(ignore character))
  (loop :for char = (peek-char t stream)
        ;; end check.
        :if (char= #\) char)
        :do (read-char stream)
        (loop-finish)
        ;; The default.
        :else :collect (read-as-code stream t t t)))

(defun |line-comment-reader| (stream character)
  (declare (ignore character))
  (make-line-comment :content(string-trim " "(read-line stream))))

(defun |block-comment-reader| (stream number character)
  (make-block-comment :content (funcall #'read-as-string::|#\|reader| stream number character)))

(defun |#+-reader|(stream character number)
  (when number
    (warn "A numeric argument is ignored in #~A~A."
          number character))
  (make-conditional :char character
                    :condition (read-as-code stream)))

(defun |#.reader|(stream character number)
  (declare(ignore character))
  (when number
    (warn "A numeric argument is ignored in read time eval."))
  (make-read-time-eval :form (read-as-code stream t t t)))

(defun |#=reader|(stream character number)
  (declare(ignore character))
  (make-shared-object :number number :exp (read-as-code stream)))

(defun |##reader|(stream character number)
  (declare(ignore stream character))
  (make-shared-reference :number number))

;;;; NAMED-READTABLE
(named-readtables:defreadtable as-code
  (:merge :common-lisp)
  (:macro-char #\( '|paren-reader|)
  (:macro-char #\. '|dot-reader| t)
  (:macro-char #\; '|line-comment-reader|)
  (:dispatch-macro-char #\# #\| '|block-comment-reader|)
  (:dispatch-macro-char #\# #\+ '|#+-reader|)
  (:dispatch-macro-char #\# #\- '|#+-reader|)
  (:dispatch-macro-char #\# #\. '|#.reader|)
  (:dispatch-macro-char #\# #\= '|#=reader|)
  (:dispatch-macro-char #\# #\# '|##reader|)
  )

;;;; DEBUG-PRINTER
(defun file-exp(pathname)
  (with-open-file(input pathname)
    (loop :with tag = '#:end
          :for exp = (read-as-code input nil tag)
          :until (eq exp tag)
          :collect exp)))

(defun debug-printer (component)
  (loop :for (exp . rest) :on (file-exp (asdf:component-pathname component))
        :do (when(and (comment-p exp)
                      (not (uiop:string-prefix-p #\; (comment-content exp))))
              (write-char #\space))
        (print-as-code exp)
        (typecase exp
          (block-comment
            (format t "~2%"))
          (line-comment
            (terpri)
            (when(not (comment-p (car rest)))
              (terpri)))
          (conditional
            (terpri))
          (t
            (typecase (car rest)
              (null)
              (line-comment
                (if(uiop:string-prefix-p #\; (comment-content (car rest)))
                  (format t "~2%")))
              (t
                (format t "~2%")))))))

;;;; PRETTY PRINTERS
(defun shortest-package-name (package)
  (reduce (lambda(champion challenger)
            (if(< (length champion)
                  (length challenger))
              champion
              challenger))
          (cons (package-name package)
                (package-nicknames package))))

(defun symbol-printer (stream object)
  (let((default-style
         (let((*print-pprint-dispatch*
                (copy-pprint-dispatch nil)))
           (prin1-to-string object))))
    (if (or (null (symbol-package object))
            (eq #.(find-package :keyword)
                (symbol-package object))
            (nth-value 1 (find-symbol (symbol-name object))))
      (write-string default-style stream)
      (progn
        (write-string (string-downcase
                        (shortest-package-name
                          (symbol-package object)))
                      stream)
        (write-string default-style
                      stream
                      :start
                      ;; Please do not ever use collon as package name!
                      (position #\: default-style))))))

#+sbcl
(defun pprint-handler-case(stream exp &rest noise)
  (declare(ignore noise))
  (funcall (formatter "~:<~^~W~^~3I ~:_~W~1I~@{ ~_~/sb-pretty::pprint-data-list/~}~:>") stream exp))

#+sbcl
(defun pprint-define-condition(stream exp &rest noise)
  (declare(ignore noise))
  (pprint-logical-block(stream exp :prefix "(" :suffix ")")
    (flet((output()
            (write (pprint-pop) :stream stream)
            (pprint-exit-if-list-exhausted)
            (write-char #\space stream)))
      (output) ; operater
      (output) ; name
      (pprint-indent :block 3 stream)
      (format stream "~@_~:<~@{~W~^ ~}~:>" (pprint-pop)) ; superclasses
      (pprint-exit-if-list-exhausted)
      (pprint-indent :block 0 stream)
      (write-char #\space stream)
      (format stream "~@_~:<~^~W~^~_~:>" (pprint-pop)) ; slots
      (pprint-exit-if-list-exhausted)
      (pprint-newline :linear stream)
      (write-char #\space stream)
      (loop (write (pprint-pop) :stream stream)
            (pprint-exit-if-list-exhausted)
            (pprint-newline :mandatory stream)))))

;; Two functions below are copied from sbcl, and modified.
#+sbcl
(defun pprint-loop (stream list &rest noise)
  (declare (ignore noise))
  (destructuring-bind (loop-symbol . clauses) list
    (declare (ignore loop-symbol))
    (if (or (atom clauses) (consp (car clauses)))
        (sb-pretty::pprint-spread-fun-call stream list)
        (pprint-extended-loop stream list))))

#+sbcl
(defvar +loop-separating-clauses+
  '(:and
     :with :for
     :collect :collecting
     :append :appending
     :nconc :nconcing
     :count :counting
     :sum :summing
     :maximize :maximizing
     :minimize :minimizing
     :if :when :unless :end
     :for :while :until :repeat :always :never :thereis
     ))

(defparameter *pprint-dispatch*
  (let((*print-pprint-dispatch*
         (copy-pprint-dispatch)))

    (set-pprint-dispatch '(eql #\space)
                         (lambda(stream object)
                           (format stream "#\\~:C" object)))

    (set-pprint-dispatch 'symbol 'symbol-printer)

    #+sbcl
    (set-pprint-dispatch '(cons (member handler-case)) 'pprint-handler-case)
    #+sbcl
    (set-pprint-dispatch '(cons (member loop)) 'pprint-loop)
    #+sbcl
    (set-pprint-dispatch '(cons (member define-condition)) 'pprint-define-condition)

    *print-pprint-dispatch*))

;;;; PRINT-AS-CODE
(defun split-to-lines (string)
  (mapcan (lambda(line)
            (setf line (remove #\nul
                               (ppcre:regex-replace #.(format nil " ~C[^)]" #\nul)
                                                    line
                                                    "")))
            (unless (every (lambda(char)
                             (char= #\space char))
                           line)
              (list line)))
          (uiop:split-string string :separator '(#\newline))))

(declaim (ftype (function (T &optional (or null stream))
                          (values null &optional))
                print-as-code))
(defun print-as-code (exp &optional stream)
  (let*((*print-case*
          :downcase)
        (*print-pprint-dispatch*
          *pprint-dispatch*)
        (*print-pretty*
          t)
        (string
          (prin1-to-string exp))
        (*standard-output*
          (or stream *standard-output*)))
    (loop :for (first . rest) :on (split-to-lines string)
          :do (if(uiop:string-prefix-p "; " (string-left-trim " " (car rest)))
                (if(uiop:string-prefix-p "; " (string-left-trim " " first))
                  ;; Both are single semicoloned line comment.
                  ;; Integrate it as one for pritty printings.
                  (setf (car rest)
                        (format nil "~A ~A" first (car rest)))
                  ;; Next one is single semicoloned line comment but FIRST.
                  ;; Both should be printed in same line.
                  (progn (format t "~A " first)
                         (rplaca rest (string-left-trim " " (car rest)))))
                (if(uiop:string-prefix-p "; " (string-left-trim " " first))
                  ;; Next is not single semicoloned line comment but FIRST.
                  ;; Comment should be printed.
                  (format t "~<; ~@;~@{~A~^ ~:_~}~:>~:[~;~%~]"
                          (remove "" (uiop:split-string first :separator "; ")
                                  :test #'equal)
                          rest) ; To avoid unneeded newline.
                  ;; Both are not single semicoloned line comment.
                  (if rest
                    ;; To avoid unneeded newline. Especially for conditional.
                    (if(= (1+ (length first))
                          (loop :for num :upfrom 0
                                :for char :across (car rest)
                                :while (char= #\space char)
                                :finally (return num)))
                      (progn (format t "~A " first)
                             (rplaca rest (string-left-trim " " (car rest))))
                      (write-line first))
                    ;; Last line never need newline.
                    (write-string first)))))))

;;;; loop clause
(defvar *print-clause* nil)

;;; CLAUSE
(defstruct (clause (:constructor %make-clause))
  keyword forms)
(defmethod print-object((o clause)stream)
  (if (null *print-clause*)
    (call-next-method)
    (if (clause-keyword o)
      (format stream "~W~@[ ~{~W~^ ~@_~}~]"
              (clause-keyword o)
              (clause-forms o))
      (format stream "~{~W~^ ~_~}" (clause-forms o)))))

;;; OPTIONAL
(defstruct (optional (:include clause)))

;;; ADDITIONAL
(defstruct (additional (:include optional)))

;;; ELSE
(defstruct (else (:include optional)))
(defmethod print-object ((c else)stream)
  (if (null *print-clause*)
    (call-next-method)
    (format stream "~2:I~W~:@_~{~W~^~:@_~}~5I"
            (clause-keyword c)
            (clause-forms c))))

;;; VAR
(defstruct (var (:include clause)))
(defmethod print-object((v var) stream)
  (if (null *print-clause*)
    (call-next-method)
    (apply #'format stream "~W~^ ~:I~W~@{~^ ~:_~W~^ ~W~}~5I"
            (clause-keyword v)
            (clause-forms v))))

;;; NESTABLE
(defvar *depth* 0)
(defstruct (nestable (:include clause))
  pred
  else)
(defmethod print-object((c nestable)stream)
  (if (null *print-clause*)
    (call-next-method)
    (progn (format stream "~2:I~W ~W"
                   (clause-keyword c)
                   (nestable-pred c))
           (when (clause-forms c)
             (let((*depth*(1+ *depth*)))
               (loop :for form :in (clause-forms c)
                     :do (format stream "~VI~:@_~W"
                                 (+ 5 (* 2 *depth*))
                                 form))))
           (when (nestable-else c)
             (let((*depth* *depth*))
               (loop :for form :in (nestable-else c)
                     :do (format stream "~VI~:@_~W"
                                 (+ 5 (* 2 *depth*))
                                 form)))))))

;;; OWN-BLOCK
(defstruct (own-block (:include clause)))
(defmethod print-object((c own-block)stream)
  (if (null *print-clause*)
    (call-next-method)
    (format stream "~W~@[ ~:I~{~W~^~:@_~}~]~5I"
            (clause-keyword c)
            (clause-forms c))))

;;; CONSTRUCTOR
(defun make-clause(&key keyword forms)
  (funcall (case (separation-keyword-p keyword)
             ((:for :with :as) #'make-var)
             ((:if :when) #'make-nestable)
             ((:do :doing :finally :initially) #'make-own-block)
             ((:else) #'make-else)
             ((:and) #'make-additional)
             (otherwise #'%make-clause))
           :keyword keyword
           :forms forms))

(defun print-clause(thing)
  (let((*print-clause* t))
    (pprint-logical-block(nil nil)
      (prin1 thing))))

(defun separation-keyword-p(thing)
  (and (symbolp thing)
       (find thing '(:and
                      :with :for :as
                      :collect :collecting
                      :append :appending
                      :nconc :nconcing
                      :count :counting
                      :sum :summing
                      :maximize :maximizing
                      :minimize :minimizing
                      :if :when :unless :end
                      :while :until :repeat :always :never :thereis
                      :do :doing :initially :finally
                      :return :else
                      )
             :test #'string=)))

(defun parse-loop-body(body)
  (make-nest(make-loop-clauses body)))

(defun make-loop-clauses(body)
  (labels((rec(list &optional temp acc)
            (if(endp list)
              (if temp
                (progn (reverse-form temp nil)
                       (nreconc acc (list temp)))
                (nreverse acc))
              (body (car list)(cdr list) temp acc)))
          (body(first rest temp acc)
            (if(separation-keyword-p first)
              (rec rest
                   (make-clause :keyword first)
                   (if(null temp)
                     acc
                     (if(and (optional-p(car acc))
                             (null (clause-forms (car acc))))
                       (progn (append-forms (car acc) (reverse-form temp first))
                              acc)
                       (cons (reverse-form temp nil) acc))))
              (let((next(separation-keyword-p (car rest))))
                (case next
                  ((nil)
                   (rec rest
                        (if (null temp)
                          (make-clause :forms (list first))
                          (if(nestable-p temp)
                            (if(null (nestable-forms temp))
                              (progn (setf (nestable-pred temp) first)
                                     temp)
                              (progn (push first (clause-forms temp))
                                     temp))
                            (progn (push first (clause-forms temp))
                                   temp)))
                        acc))
                  (otherwise
                    (rec (cdr rest)
                         (make-clause :keyword (car rest))
                         (if (null temp)
                           (cons (make-clause :forms (list first)) acc)
                           (typecase (car acc)
                             (nestable
                               (append-forms (car acc) (reverse-form temp first))
                               acc)
                             (optional
                               (if(null (clause-forms (car acc)))
                                 (progn (setf (clause-forms (car acc))
                                              (list (reverse-form temp first)))
                                        acc)
                                 (cons (reverse-form temp first)acc)))
                             (t
                               (if(nestable-p temp)
                                 (if(null (clause-forms temp))
                                   (cons (progn (setf (nestable-pred temp) first)
                                                temp)
                                         acc)
                                   (cons (reverse-form temp first) acc))
                                 (cons (reverse-form temp first) acc)))))))))))
          (append-forms(clause other)
            (setf (clause-forms clause)
                  (append (clause-forms clause)
                          (list other))))
          (reverse-form(clause tail)
            (setf (clause-forms clause)
                  (nreconc (clause-forms clause)
                           (when tail
                             (list tail))))
            clause))
    (rec body)))

(defun make-nest (list)
  (do*((list list (cdr list))
       (first (car list)(car list))
       when
       acc)
    ((endp list)
     (if when
       (nreconc acc (list when))
       (nreverse acc)))
    (typecase first
      (additional
        (if(null when)
          (push first acc)
          (if(nestable-else when)
            (setf (nestable-else when)
                  (append (nestable-else when)
                          (list first)))
            (setf (nestable-forms when)
                  (append (nestable-forms when)
                          (list first))))))
      (else (if(null when)
              (push first acc)
              (if(nestable-else when)
                (setq acc (list* first when acc))
                (setf (nestable-else when)
                      (append (nestable-else when)
                              (list first))))))
      (nestable
        (setf when first))
      (otherwise
        (if when
          (setq acc (list* first when acc)
                when nil)
          (push first acc))))))

(defun pprint-extended-loop (stream list)
  (pprint-logical-block(stream nil :prefix "(" :suffix ")")
    (format stream "~W~:[~; ~:I~]" (car list)(cdr list))
    (do*((list (parse-loop-body (cdr list))(cdr list))
         (first (car list)(car list))
         (*print-clause* t))
      ((null list))
      (format stream "~W~:[~;~:@_~]" first (cdr list)))))
