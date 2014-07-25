(in-package :gcc)

;; Common Lisp backend

(defclass backend-cl (backend) ())

(defmethod translate-let ((backend backend-cl) binding-forms let-body env rec-env)
  (iter (for binding-form in binding-forms)
        (for (binding proc-args proc-body) = (parse-binding binding-form))
        (collect binding into bindings)
        (collect (translate-lambda backend proc-body (cons (append proc-args rec-env) env)) into codes)
        (finally
         (return
           (append (translate-lambda backend let-body (cons rec-env env))
                   (iter outer
                         (for proc-label in (nreverse bindings))
                         (for proc-code in (nreverse codes))
                         (collect `(:label ,proc-label))
                         (iter (for form in proc-code) (in outer (collect form))))))))
  `(flet ,bindinig-forms))

(defmethod translate-letrec ((backend backend-cl) binding-forms let-body env)
  )

(defmethod translate-op ((backend backend-cl) op form-a form-b env)
  (let ((lisp-op (ecase op
                   (:add '+)
                   (:sub '-)
                   (:mul '*)
                   (:div '/)
                   (:ceq '=)
                   (:cgt '>)
                   (:cgte '>=)
                   (:cons 'cons)
                   (:car 'car)
                   (:cdr 'cdr)
                   (:atom 'integerp))))
    `(,lisp-op ,(translate-walker backend form-a env)
          ,@(when form-b
                  (list (translate-walker backend form-b env))))))

(defmethod translate-seq ((backend backend-cl) type body env)
  (ecase type
    (:list `(list ,@body))
    (:tuple (error "Not implemented yet"))))


(defmethod translate-if ((backend backend-cl) condition-form true-form false-form env)
  `(if (/= ,(translate-walker backend condition-form env) 0)
       ,(translate-walker backend true-form env)
       ,(translate-walker backend false-form env)))

(defmethod translate-proc-invocation ((backend backend-cl) proc-name proc-args env)
  `(,proc-name ,@(mapcar (lambda (arg)
                           (translate-walker backend arg env))
                         proc-args)))

(defmethod translate-atom ((backend backend-cl) atom env)
  atom)

(defmethod translate-lambda ((backend backend-cl) ast env)
  )


;;; Convinience functions

(defun translate-cl (ast)
  (translate-walker (make-instance 'backend-cl) ast '()))

(defun translate-file-cl (filename)
  (with-open-file (source filename)
    (translate-cl (read source))))

