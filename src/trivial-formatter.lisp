(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export))
(in-package :trivial-formatter)

(defparameter *output-hook* 'debug-printer)

(defun fmt (system)
  (dolist (component (asdf:component-children (asdf:find-system system)))
    (funcall *output-hook* component)))

;;;; META-OBJECT
;;; DOT
(defstruct dot)
(defmethod print-object ((dot dot) stream)
  (princ #\. stream))

(defstruct line-comment
  content)
(defmethod print-object ((line line-comment) stream)
  (format stream ";~@[~A~]~%"
          (line-comment-content line)))

(defstruct block-comment
  content)
(defmethod print-object ((comment block-comment) stream)
  (write-line (block-comment-content comment)))

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
        :collect (read-char stream)
        :and :do (loop-finish)
        ;; The default.
        :else :collect (read-as-code stream t t t)))

(defun |line-comment-reader| (stream character)
  (declare (ignore character))
  (make-line-comment :content (read-line stream)))

(defun |block-comment-reader| (stream number character)
  (make-block-comment :content (funcall #'read-as-string::|#\|reader| stream number character)))
