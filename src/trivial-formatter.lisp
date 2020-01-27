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
          :do (format t "~:[~&~(~S~)~2%~;~S~]"
                      (or (stringp exp)
                          (comment-p exp))
                      exp))))

(defun initialize-pprint-dispatch()
  (set-pprint-dispatch '(eql #\space)
                       (lambda(stream object)
                         (format stream "#\\~:C" object))))

(defun print-as-code (exp)
  (let((*print-case*
         :downcase)
       (*print-pprint-dispatch*
         (copy-pprint-dispatch)))
    (initialize-pprint-dispatch)
    (typecase exp
      (comment
        (format t "~A" (comment-content exp)))
      (t
        (let((comments
               (remove-if-not #'comment-p
                              (alexandria:flatten exp) ; NIY sbcl backquote.
                              )))
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
                                           (position-if-not (lambda(char)
                                                              (char= #\space char))
                                                            (car rest))
                                           (comment-content comment))
                                   ;; Comment as '; hoge'.
                                   (cond
                                     ((let((position
                                             (position #\null first)))
                                        (and (array-in-bounds-p first (1+ position))
                                             (char= #\) (char first (1+ position)))))
                                      (if (loop :for index :upfrom 0
                                                :until (char= #\null (char first index))
                                                :always (char= #\space (char first index)))
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
                                                        :start (1+ (position-if
                                                                     (lambda(char)
                                                                       (not (char= #\space char)))
                                                                     first)))))
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
