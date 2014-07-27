(in-package :ilisp.impl)

(defclass translation-phase () ())

(defgeneric translate (translator ast &rest other))
(defgeneric translate-lambda (translator proc-args proc-body))
(defgeneric translate-let (translator bindings body))
(defgeneric translate-letrec (translator bindings body))
(defgeneric translate-binop (translator op lhs rhs))
(defgeneric translate-atom (translator atom))

; Let form
(defmethod translate-node (phase (node (eql 'ilisp:let)) form)
  (bind (((_ (&rest bindings) &body body-forms) form))
    (mapcar #'(lambda (binding)
                (translate-lexical-binding phase name value-form))
            bindings)
    (call-next-method)))

; Letrec form



(defun tranlate-let (obj bindings body)
  )

()

(defun process-lambda (args &rest body)
  env = (add-args-to-env args env)
  (add-lambda-body somewhere `((:LABEL (GENSYM))
                               ,@(process body env)
                               (:RTN)))
  ()

  `((:LABEL (GENSYM))
    ,@(process body)
    (:RTN)))



(defun process-let (bindings &rest body)
  )

(defun process-letrec (bindings &rest body)
  )
