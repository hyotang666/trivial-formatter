(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export))
(in-package :trivial-formatter)

(defparameter *output-hook* 'debug-printer)

(defun fmt (system)
  (dolist (component (asdf:component-children (asdf:find-system system)))
    (funcall *output-hook* component)))

(defun component-codes (component)
  (let ((pathname
          (asdf:component-pathname component)))
    (with-open-file(input pathname)
      (loop :for code = (read-as-string:read-as-string input nil nil)
            :while code
            :collect code))))

