(in-package :cl-user)
(defpackage :trivial-formatter
  (:use :cl)
  (:export))
(in-package :trivial-formatter)

(defun fmt (system)
  (dolist (component (asdf:component-children (asdf:find-system system)))
    (funcall *output-hook* component)))
