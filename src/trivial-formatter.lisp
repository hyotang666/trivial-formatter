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
  (flet ((may-peek ()
           (let ((return
                   (peek-char (null recursive-p)
                              nil
                              eof-error-p
                              eof-value
                              recursive-p)))
             (if (eq return eof-value)
               (return-from read-as-code eof-value)
               return))))
    (let* ((*readtable*
             (named-readtables:find-readtable 'as-code))
           (*standard-input*
             (or stream *standard-input*))
           (char
             (may-peek)))
      (if (get-macro-character (if recursive-p
                                 (may-peek)
                                 char))
        (read *standard-input* eof-error-p eof-value recursive-p)
        (read)))))

;;;; META-OBJECT
;;; DOT
(defstruct dot)
(defmethod print-object ((dot dot) stream)
  (princ #\. stream))

(defstruct comment content)

(defstruct (line-comment (:include comment)))
(defmethod print-object ((line line-comment) stream)
  (format stream ";~@[~A~]"
          (comment-content line)))

(defstruct (block-comment (:include comment)))
(defmethod print-object ((comment block-comment) stream)
  (write-line (comment-content comment)))

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
  (loop :for char = (peek-char nil stream)
        ;; end check.
        :if (char= #\) char)
        :do (read-char stream)
        (loop-finish)
        ;; The default.
        :else :collect (read-as-code stream t t t)))

(defun |line-comment-reader| (stream character)
  (declare (ignore character))
  (make-line-comment :content (read-line stream)))

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
          :unless (eq exp tag)
          :do (format t "~%~:[~(~S~)~%~;~S~]"
                      (or (stringp exp)
                          (comment-p exp))
                      exp))))

