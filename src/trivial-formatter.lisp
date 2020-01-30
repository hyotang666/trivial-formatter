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

(defstruct comment content)
(defmethod print-object ((object comment) stream)
  (write-char #\Null stream) ; as place holder.
  )

(defstruct (line-comment (:include comment)))
(defstruct (block-comment (:include comment)))

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
  (let((string
         (with-output-to-string(s)
           (print-as-code (read-time-eval-form form) s))))
    (format stream "~<#.~;~^~@{~A~^~:@_~}~:>"
            (uiop:split-string (string-trim '(#\newline) string)
                               :separator '(#\newline)))))

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

(defparameter *pprint-dispatch*
  (let((*print-pprint-dispatch*
         (copy-pprint-dispatch)))

    (set-pprint-dispatch '(eql #\space)
                         (lambda(stream object)
                           (format stream "#\\~:C" object)))

    (set-pprint-dispatch 'symbol 'symbol-printer)

    #+sbcl
    (set-pprint-dispatch '(cons (member handler-case)) 'pprint-handler-case)

    *print-pprint-dispatch*))

(defun collect-comments (exp)
  (remove-if-not #'comment-p
                 (alexandria:flatten exp) ; NIY sbcl backquote.
                 ))

(defun position-of-not-space-char(string)
  (position-if-not (lambda(char)
                     (char= #\space char))
                   string))

(defun comment-at-first-p(string)
  (loop :for index :upfrom 0
        :until (char= #\null (char string index))
        :always (char= #\space (char string index))))

(defun close-paren-after-comment-p(string)
  (let((position
         (position #\null string)))
    (and (array-in-bounds-p string (1+ position))
         (char= #\) (char string (1+ position))))))

(defun only-comment-line-p (string)
  (string= "" (string-trim '(#\null #\space)
                           string)))

(defun print-commented-line(comment first rest comments)
  (typecase comment
    (line-comment
      (cond
        ((uiop:string-prefix-p #\; (comment-content comment))
         ;; Comment as ";; hoge" or ";;; hoge" etc..
         (format t "~%~A~&~VT;~A~%"
                 (string-right-trim '(#\null #\space)
                                    first)
                 (some #'position-of-not-space-char rest)
                 (comment-content comment)))
        ;; Comment as '; hoge'.
        ((close-paren-after-comment-p first)
         (if (comment-at-first-p first)
           (format t " ~<; ~@;~^~@{~A~^ ~:_~}~:>~%~A"
                   (uiop:split-string(comment-content comment))
                   (remove #\null first))
           (progn
             (fresh-line)
             (write-string first nil
                           :end (position #\null first))
             (format t "; ~A~%" (comment-content comment))
             (format t "~VT"
                     (1+ (position #\space first
                                   :start (1+ (position-of-not-space-char first)))))
             (write-string first nil :start (1+ (position #\null first))))))
        ((only-comment-line-p first)
         (if (and (car rest)
                  (only-comment-line-p (car rest))
                  (not(uiop:string-prefix-p #\; (comment-content (car comments)))))
           (setf (comment-content (car comments))
                 (format nil "~A ~A"
                         (comment-content comment)
                         (comment-content (car comments))))
           (format t " ~<; ~@;~^~@{~A~^ ~:_~}~:>"
                   (uiop:split-string(comment-content comment)))))
        ((comment-at-first-p first)
         (format t " ~<; ~@;~^~@{~A~^ ~:_~}~:>~%~A" (uiop:split-string(comment-content comment))
                 (remove #\null first)))
        (t
          (let((exp
                 (mapcar (lambda(line)
                           (string-right-trim " " line))
                         (uiop:split-string first :separator '(#\nul)))))
            (format t "~%~{~A~} ~<; ~@;~^~@{~A~^ ~:_~}~:>"
                    exp
                    (uiop:split-string(comment-content comment)))))))
    (block-comment
      (write-string (string-right-trim '(#\null #\space)
                                       first))
      (format t " ~A" (comment-content comment)))))

(defun print-some-comment-line(comments first)
  (fresh-line)
  (let((list
         (uiop:split-string first :separator '(#\nul))))
    (loop :initially (write-string (car list))
          :with indent = (length (car list))
          :for elt :in (mapcar (lambda(elt)
                                 (string-trim " " elt))
                               (cdr list))
          :if (block-comment-p (car comments))
          :do (format t "~A ~A"
                      (comment-content (pop comments))
                      elt)
          :else :do
          (when(uiop:string-prefix-p #\; (comment-content (car comments)))
            (format t "~%~VT" indent))
          (print-as-code (pop comments))
          (format t "~VT~A" indent elt)))
  comments)

(declaim (ftype (function (T &optional (or null stream))
                          (values null &optional))
                print-as-code))
(defun print-as-code (exp &optional stream)
  (let*((*print-case*
          :downcase)
        (*print-pprint-dispatch*
          *pprint-dispatch*)
        (string
          (with-output-to-string(*standard-output*)
            (typecase exp
              (line-comment
                (let((content(comment-content exp)))
                  (if(uiop:string-prefix-p #\; content)
                    (format t ";~A" content)
                    (format t "; ~A" content))))
              (block-comment
                (format t "~A" (comment-content exp)))
              (conditional
                (format t "~A" exp))
              (t
                (let((comments
                       (collect-comments exp)))
                  (if(null comments)
                    (format t "~S" exp)
                    (let((code(prin1-to-string exp)))
                      (loop :for (first . rest) :on (uiop:split-string code :separator '(#\newline))
                            :do
                            (let((count (count #\null first)))
                              (case count ; how many comment in line?
                                (0 (format t "~&~A" first))
                                (1 (print-commented-line (pop comments) first rest comments))
                                (otherwise
                                  (setf comments (print-some-comment-line comments first)))))))))))))
        (*standard-output*
          (or stream *standard-output*)))
    (format t "~{~A~^~%~}"
            (remove-if (lambda(line)
                         (every (lambda(char)
                                  (char= #\space char))
                                line))
                       (uiop:split-string string :separator '(#\newline))))))
