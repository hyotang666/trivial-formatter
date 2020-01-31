(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export
    ;; Main api
    #:fmt
    ;; Useful helpers
    #:read-as-code
    #:print-as-code
    ;; Hooker
    #:debug-printer
    #:renamer
    #:appender
    #:replacer
    ))
(in-package :trivial-formatter)

;;;; FMT
(declaim (ftype (function ((or symbol string asdf:system)
                           &optional
                           (or symbol function))
                          (values null &optional))
                fmt))
(defun fmt (system &optional(formatter 'debug-printer))
  (asdf:load-system system)
  (dolist (component (asdf:component-children (asdf:find-system system)))
    (funcall formatter component)))

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
            (package-error(c)
              (let((package
                     (package-error-package c)))
                (if (stringp package) ; No such package exists.
                  (make-broken-symbol :notation notation)
                  (error c))))))))))

;;;; META-OBJECT
;;; DOT
(defstruct dot)
(defmethod print-object ((dot dot) stream)
  (princ #\. stream))

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

(defstruct conditional
  char
  condition)
(defmethod print-object ((c conditional) stream)
  (format stream "~_#~A~A"
          (conditional-char c)
          (conditional-condition c)))

(defstruct broken-symbol
  notation)
(defmethod print-object ((symbol broken-symbol)stream)
  (write-string (broken-symbol-notation symbol) stream))

(defstruct read-time-eval
  form)
(defmethod print-object((form read-time-eval)stream)
  (format stream "~<#.~;~^~@{~A~^~:@_~}~:>"
          (uiop:split-string (prin1-to-string (read-time-eval-form form))
                             :separator '(#\newline))))

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
  )

;;;; Hookers
(declaim (ftype (function (asdf:component)
                          (values null &optional))
                debug-printer
                renamer
                appender
                replacer))

(defun renamed-pathname(pathname)
  (make-pathname
    :name (with-output-to-string(out)
            (princ (pathname-name pathname)out)
            (dolist(num (cdddr (nreverse(multiple-value-list (get-decoded-time)))))
              (format out "~2,,,'0@A" num)))
    :defaults pathname))

(defun file-exp(pathname)
  (with-open-file(input pathname)
    (loop :with tag = '#:end
          :for exp = (read-as-code input nil tag)
          :until (eq exp tag)
          :collect exp)))

(defun renamer(component)
  (let*((pathname
          (asdf:component-pathname component))
        (old
          (uiop:read-file-string pathname)))
    (with-open-file(*standard-output*
                     (renamed-pathname pathname)
                     :direction :output
                     :if-does-not-exist :create)
      (write-line old))
    (with-open-file(*standard-output* pathname
                                      :direction :output
                                      :if-exists :supersede)
      (debug-printer component))))

(defun replacer(component)
  (let*((pathname
          (asdf:component-pathname component)))
    (with-open-file(*standard-output* pathname :direction :output :if-exists :supersede)
      (debug-printer component))))

(defun appender(component)
  (let*((pathname
          (asdf:component-pathname component)))
    (with-open-file(*standard-output* pathname :direction :output :if-exists :append)
      (debug-printer component))))

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

;;;; PRINT-AS-CODE
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

#+sbcl
(defun pprint-extended-loop (stream list)
  (pprint-logical-block (stream list :prefix "(" :suffix ")")
      (sb-kernel:output-object (pprint-pop) stream)
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (pprint-indent :current 0 stream)
      (sb-kernel:output-object (pprint-pop) stream)
    (prog(thing after-do indent before-key-p)
      :top
      (setf thing (pprint-pop))
      :update-vars
      (or (and (symbolp thing)
               (if (find thing '(:do :doing :initially :finally) :test #'string=)
                 (setf after-do 0
                       indent (length (prin1-to-string thing))
                       before-key-p t)
                 (when (member thing +loop-separating-clauses+ :test #'string=)
                   (setf after-do nil
                         indent nil
                         before-key-p t))))
          (setf before-key-p nil))
      :newline-check
      (when(or before-key-p
               (and after-do
                    (if (zerop after-do)
                      (tagbody (incf after-do))
                      t)))
        (pprint-newline :mandatory stream))
      :output
      (sb-kernel:output-object thing stream)
      :end-check
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (go :top))))

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

    *print-pprint-dispatch*))

(declaim (ftype (function (T &optional (or null stream))
                          (values null &optional))
                print-as-code))
(defun print-as-code (exp &optional stream)
  (let*((*print-case*
          :downcase)
        (*print-pprint-dispatch*
          *pprint-dispatch*)
        (string
          (prin1-to-string exp))
        (*standard-output*
          (or stream *standard-output*)))
    (loop :for (first . rest) :on (mapcan (lambda(line)
                                            (setf line (remove #\nul
                                                               (ppcre:regex-replace #.(format nil " ~C[^)]" #\nul)
                                                                                    line
                                                                                    "")))
                                            (unless (every (lambda(char)
                                                             (char= #\space char))
                                                           line)
                                              (list line)))
                                          (uiop:split-string string :separator '(#\newline)))
          :do (if(uiop:string-prefix-p "; " (string-left-trim " " (car rest)))
                (if(uiop:string-prefix-p "; " (string-left-trim " " first))
                  (setf (car rest)
                        (format nil "~A ~A" first (car rest)))
                  (progn (format t "~A " first)
                         (rplaca rest (string-left-trim " " (car rest)))))
                (if(uiop:string-prefix-p "; " (string-left-trim " " first))
                  (format t "~<; ~@;~@{~A~^ ~:_~}~:>~%"
                          (remove "" (uiop:split-string first :separator "; ")
                                  :test #'equal))
                  (if rest
                    (write-line first)
                    (write-string first)))))))
