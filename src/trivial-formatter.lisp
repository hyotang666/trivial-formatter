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
