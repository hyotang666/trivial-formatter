(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export))
(in-package :trivial-formatter)

(defparameter *output-hook* 'debug-printer)

(defun fmt (system)
  (asdf:load-system system)
  (dolist (component (asdf:component-children (asdf:find-system system)))
    (funcall *output-hook* component)))

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
          (handler-case(read-from-string notation)
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
  char)
(defmethod print-object ((c conditional) stream)
  (format stream "#~A"
          (conditional-char c)))

(defstruct broken-symbol
  notation)
(defmethod print-object ((symbol broken-symbol)stream)
  (write-string (broken-symbol-notation symbol) stream))

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
  (make-line-comment :content (format nil ";~A~%"(read-line stream))))

(defun |block-comment-reader| (stream number character)
  (make-block-comment :content (funcall #'read-as-string::|#\|reader| stream number character)))

(defun |#+-reader|(stream number character)
  (declare (ignore stream))
  (warn "A numeric argument is ignored in #~A~A."
        number character)
  (make-conditional :char character))

(named-readtables:defreadtable as-code
  (:merge :common-lisp)
  (:macro-char #\( '|paren-reader|)
  (:macro-char #\. '|dot-reader| t)
  (:macro-char #\; '|line-comment-reader|)
  (:dispatch-macro-char #\# #\| '|block-comment-reader|)
  (:dispatch-macro-char #\# #\+ '|#+-reader|)
  (:dispatch-macro-char #\# #\- '|#+-reader|)
  )

(defun renamed-pathname(pathname)
  (make-pathname
    :name (with-output-to-string(out)
            (princ (pathname-name pathname)out)
            (dolist(num (cdddr (nreverse(multiple-value-list (get-decoded-time)))))
              (format out "~2,,,'0@A" num)))
    :defaults pathname))

(defun call-with-file-exp(pathname callback)
  (with-open-file(input pathname)
    (loop :with tag = '#:end
          :for exp = (read-as-code input nil tag)
          :until (eq exp tag)
          :do (funcall callback exp))))

(defun renamer(component)
  (let*((pathname
          (asdf:component-pathname component))
        (old
          (uiop:read-file-string pathname))
        (new
          (uiop:while-collecting(acc)
            (call-with-file-exp pathname #'acc))))
    (with-open-file(*standard-output*
                     (renamed-pathname pathname)
                     :direction :output
                     :if-does-not-exist :create)
      (mapc #'write-line old))
    (with-open-file(*standard-output* pathname
                                      :direction :output
                                      :if-exists :supersede)
      (mapc #'print-as-code new))))

(defun replacer(component)
  (let*((pathname
          (asdf:component-pathname component))
        (new
          (uiop:while-collecting(acc)
            (call-with-file-exp pathname #'acc))))
    (with-open-file(*standard-output* pathname :direction :output :if-exists :supersede)
      (mapc #'print-as-code new))))

(defun debug-printer (component)
  (call-with-file-exp (asdf:component-pathname component)
                      #'print-as-code))

(defun shortest-package-name (package)
  (reduce (lambda(chanpion challenger)
            (if(< (length chanpion)
                  (length challenger))
              chanpion
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

(defparameter *pprint-dispatch*
  (let((*print-pprint-dispatch*
         (copy-pprint-dispatch)))

    (set-pprint-dispatch '(eql #\space)
                         (lambda(stream object)
                           (format stream "#\\~:C" object)))

    (set-pprint-dispatch 'symbol 'symbol-printer)

    *print-pprint-dispatch*))

(defun collect-comments (exp)
  (remove-if-not #'comment-p
                 (alexandria:flatten exp) ; NIY sbcl backquote.
                 ))

(defun position-of-not-space-char(string)
  (position-if-not (lambda(char)
                     (char= #\space char))
                   string))

(defun always-space-till-null-p(string)
  (loop :for index :upfrom 0
        :until (char= #\null (char string index))
        :always (char= #\space (char string index))))

(defun close-paren-after-comment-p(string)
  (let((position
         (position #\null string)))
    (and (array-in-bounds-p string (1+ position))
         (char= #\) (char string (1+ position))))))

(defun print-commented-line(comment first rest)
  (typecase comment
    (line-comment
      (cond
        ((char= #\; (char (comment-content comment) 1))
         ;; Comment as ";; hoge" or ";;; hoge" etc..
         (format t "~%~A~&~VT~A"
                 (string-right-trim '(#\null #\space)
                                    first)
                 (position-of-not-space-char (car rest))
                 (comment-content comment)))
        ;; Comment as '; hoge'.
        ((close-paren-after-comment-p first)
         (if (always-space-till-null-p first)
           (format t " ~A~A"
                   (comment-content comment)
                   (remove #\null first))
           (progn
             (fresh-line)
             (write-string first nil
                           :end (position #\null first))
             (format t "~A" (comment-content comment))
             (format t "~VT"
                     (1+ (position #\space first
                                   :start (1+ (position-of-not-space-char first)))))
             (write-string first nil :start (1+ (position #\null first))))))
        ((string= "" (string-trim '(#\null #\space)
                                  first))
         (format t " ~A" (comment-content comment)))
        (t
          (format t "~A ~A"
                  (string-right-trim '(#\null)
                                     first)
                  (comment-content comment)))))
    (block-comment
      (write-string (string-right-trim '(#\null #\space)
                                       first))
      (format t " ~A" (comment-content comment)))))

(defun print-as-code (exp)
  (let((*print-case*
         :downcase)
       (*print-pprint-dispatch*
         *pprint-dispatch*))
    (typecase exp
      (comment
        (format t "~A" (comment-content exp)))
      (t
        (let((comments
               (collect-comments exp)))
          (if(null comments)
            (format t "~&~S~2%" exp)
            (let((code(format nil "~&~S~2%" exp)))
              (loop :for (first . rest) :on (uiop:split-string code :separator '(#\newline))
                    :with previous
                    :do
                    (let((count (count #\null first)))
                      (case count ; how many comment in line?
                        (0 (format t "~&~A" first))
                        (1 (print-commented-line (pop comments) first rest))
                        (otherwise
                          (error "NIY"))))
                    (setf previous first)
                    :finally (terpri)))))))))
