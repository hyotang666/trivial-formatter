(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export))
(in-package :trivial-formatter)

(defparameter *output-hook* 'debug-printer)

(defun fmt (system)
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
           (or stream *standard-input*)))
    (read *standard-input* eof-error-p eof-value recursive-p)))

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

(defun debug-printer (component)
  (with-open-file(input (asdf:component-pathname component))
    (loop :with tag = '#:end
          :for exp = (read-as-code input nil tag)
          :until (eq exp tag)
          :do (print-as-code exp))))

(defun shortest-package-name (package)
  (car (sort (cons (package-name package)
                   (copy-list(package-nicknames package)))
             #'<
             :key #'length)))

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
            (let*((code(format nil "~&~S~2%" exp))
                  (lines (uiop:split-string code :separator '(#\newline))))
              (loop :for (first . rest) :on lines
                    :with previous
                    :do
                    (let((count (count #\null first)))
                      (case count ; how many comment in line?
                        (0 (format t "~&~A" first))
                        (1 (let((comment
                                  (pop comments)))
                             (typecase comment
                               (line-comment
                                 (if (char= #\; (char (comment-content comment) 1)) ; i.e. ;; or ;;; etc.
                                   (format t "~%~A~&~VT~A"
                                           (string-right-trim '(#\null #\space)
                                                              first)
                                           (position-of-not-space-char (car rest))
                                           (comment-content comment))
                                   ;; Comment as '; hoge'.
                                   (cond
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
                                                  (1+ (position
                                                        #\space first
                                                        :start (1+ (position-of-not-space-char first)))))
                                          (write-string first nil :start (1+ (position #\null first))))))
                                     ((string= "" (string-trim '(#\null #\space)
                                                               first))
                                      (format t " ~A" (comment-content comment)))
                                     (t
                                       (format t "~A ~A"
                                               (string-right-trim '(#\null)
                                                                  first)
                                               (comment-content comment))))))
                               (block-comment
                                 (write-string (string-right-trim '(#\null #\space)
                                                                  first))
                                 (format t " ~A" (comment-content comment))))))
                        (otherwise
                          (error "NIY"))))
                    (setf previous first))
              (terpri))))))))
