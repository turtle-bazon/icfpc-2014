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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ilisp-fn-library* (make-hash-table)))

;;(setq *gcc-ai-library (make-hash-table))

(defmacro il:defun (name (&rest lambda-list) &body body*)
  "Defines library function for AI code. The all the functions will be
linked into resulting AI GCC code."
  `(setf (gethash ',name *ilisp-fn-library*)
         '(il:lambda ,lambda-list ,@body*)))

;; defmacro
(defun gcc-macro-fn-name (sym)
  (intern (format nil "GCC-MACRO/~A/~A" 
                  (package-name (symbol-package sym)) 
                  (symbol-name sym))
          (symbol-package sym)))

(defun gcc-macro-fn (sym)
  (symbol-function (gcc-macro-fn-name sym)))

(defun gcc-macro-p (sym)
  (fboundp (gcc-macro-fn-name sym)))

(defmacro ilisp:defmacro (name (&rest arg-list) &body body*)
  "Defines a macro in ILisp"
  `(defun ,(gcc-macro-fn-name name) ,arg-list
     ,@body*))

