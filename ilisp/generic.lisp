(in-package :ilisp.impl)

(defclass translation-phase () ())

(defgeneric translate-walk (translator ast &rest other))
(defgeneric translate-lambda (translator proc-args proc-body))
(defgeneric translate-let (translator bindings body))
(defgeneric translate-letrec (translator bindings body))
(defgeneric translate-binop (translator op lhs rhs))
(defgeneric translate-unop (translator op arg))
(defgeneric translate-atom (translator atom))
(defgeneric translate-if (translator cond then else))
