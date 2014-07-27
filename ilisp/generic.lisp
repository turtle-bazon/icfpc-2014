(in-package :ilisp.impl)

(defclass translation-phase () ())

(defgeneric translate-walk (translator ast &rest other))
(defgeneric translate-lambda (translator proc-args proc-body))
(defgeneric translate-let (translator bindings body))
(defgeneric translate-letrec (translator bindings body))
(defgeneric translate-invoke (translator proc-name proc-args))
(defgeneric translate-binop (translator op lhs rhs))
(defgeneric translate-unop (translator op arg))
(defgeneric translate-atom (translator atom))
(defgeneric translate-if (translator cond then else))

;; defun

(defmacro ilisp:defun (name (&rest arg-list) &body body*)
  )

;; defmacro

(defun gcc-macro-fn-name (macro-name)
  (intern (format nil "GCC-MACRO/~A" macro-name)))

(defun gcc-macro-fn (macro-name)
  (symbol-function (gcc-macro-fn-name macro-name)))

(defun gcc-macro-p (sym)
  (fboundp (gcc-macro-fn-name sym)))

(defmacro ilisp:defmacro (name (&rest arg-list) &body body*)
  "Defines a macro in ILisp"
  `(defun ,(gcc-macro-fn-name name) ,arg-list
     ,@body*))

