(in-package :gcc)

;; Backend API
(defclass backend () ())

(defgeneric translate-let (backend binding-forms let-body env rec-env))
(defgeneric translate-letrec (backend binding-forms let-body env))
(defgeneric translate-op (backend op form-a form-b env))
(defgeneric translate-seq (backend type body env))
(defgeneric translate-if (backend condition-form true-form false-form env))
(defgeneric translate-proc-invocation (backend proc-name proc-args env))
(defgeneric translate-atom (backend atom env))
(defgeneric translate-lambda (backend ast env))

;; Frondend body

(defun translate-file (backend filename)
  (with-open-file (source filename)
    (translate (read source))))

(defun translate (backend ast)
  (translate-walker ast '()))

(defmacro with-match ((ast proc) &rest clauses)
  (with-gensyms (assume form)
    `(flet ((,assume (,form) (return-from ,proc ,form)))
       (progn ,@(iter (for (pattern action) in clauses)
                      (collect `(if-match ,pattern ,ast (,assume ,action))))))))

(defun translate-walker (backend ast env)
  (declare (optimize (debug 3)))
  (with-match (ast translate-walker)
    ((let ?bindings ?body) (translate-let backend ?bindings ?body env nil))
    ((letrec ?bindings ?body) (translate-letrec backend ?bindings ?body env))
    ((+ ?form-a ?form-b) (translate-op backend :add ?form-a ?form-b env))
    ((- ?form-a ?form-b) (translate-op backend :sub ?form-a ?form-b env))
    ((* ?form-a ?form-b) (translate-op backend :mul ?form-a ?form-b env))
    ((/ ?form-a ?form-b) (translate-op backend :div ?form-a ?form-b env))
    ((= ?form-a ?form-b) (translate-op backend :ceq ?form-a ?form-b env))
    ((> ?form-a ?form-b) (translate-op backend :cgt ?form-a ?form-b env))
    ((>= ?form-a ?form-b) (translate-op backend :cgte ?form-a ?form-b env))
    ((cons ?form-a ?form-b) (translate-op backend :cons ?form-a ?form-b env))
    ((car ?form) (translate-op backend :car ?form nil env))
    ((cdr ?form) (translate-op backend :cdr ?form nil env))
    ((integerp ?form) (translate-op backend :atom ?form nil env))
    ((list . ?forms) (translate-seq backend :list ?forms env))
    ((tuple . ?forms) (translate-seq backend :tuple ?forms env))
    ((if ?condition-form ?true-form ?false-form)
     (translate-if backend
                   ?condition-form
                   ?true-form ?false-form
                   env))
    ((?proc-name . ?proc-args) (translate-proc-invocation backend ?proc-name ?proc-args env))
    (?atom (translate-atom  backend ?atom env)))
  (error "Invalid AST: ~s" ast))
