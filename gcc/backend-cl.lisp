(in-package :gcc)

;; Common Lisp backend

(defclass backend-cl (backend) ())

;;; Convinience functions

(defun translate-cl (ast)
  (translate-walker (make-instance 'backend-cl) ast '()))

(defun translate-file-cl (filename)
  (with-open-file (source filename)
    (translate-cl (read source))))

