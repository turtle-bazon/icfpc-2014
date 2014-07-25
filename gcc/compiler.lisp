(in-package :gcc)

(defun translate-file (filename)
  (with-open-file (source filename)
    (translate (read source))))

(defun translate (ast)
  (translate-walker ast '() '()))

(defmacro with-match ((ast proc) &rest clauses)
  (with-gensyms (assume form)
    `(flet ((,assume (,form) (return-from ,proc ,form)))
       (progn ,@(iter (for (pattern action) in clauses)
                      (collect `(if-match ,pattern ,ast (,assume ,action))))))))

(defun translate-walker (ast env rec-env)
  (declare (optimize (debug 3)))
  (with-match (ast translate-walker)
    ((let ?bindings ?body) (translate-let ?bindings ?body env rec-env))
    ((letrec ?bindings ?body) (translate-letrec ?bindings ?body env rec-env))
    ((+ ?form-a ?form-b) (translate-op :add ?form-a ?form-b env rec-env))
    ((- ?form-a ?form-b) (translate-op :sub ?form-a ?form-b env rec-env))
    ((* ?form-a ?form-b) (translate-op :mul ?form-a ?form-b env rec-env))
    ((/ ?form-a ?form-b) (translate-op :div ?form-a ?form-b env rec-env))
    ((?proc-name . ?proc-args) (translate-proc-invocation ?proc-name ?proc-args env rec-env))
    (?atom (translate-atom ?atom env rec-env)))
  (error "Invalid AST: ~s" ast))

(defun translate-letrec (binding-forms let-body env rec-env)
  (translate-let binding-forms let-body env rec-env))

(defun translate-let (binding-forms let-body env rec-env)
  (declare (optimize (debug 3)))
  (flet ((parse-binding (binding-form)
           (with-match (binding-form parse-binding)
             ((?binding (lambda ?proc-args ?proc-body)) (list ?binding ?proc-args ?proc-body)))))
    (iter (for binding-form in binding-forms)
          (for (binding proc-args proc-body) = (parse-binding binding-form))
          (collect binding into bindings)
          (collect (translate-walker proc-body (cons proc-args env) rec-env) into codes)
          (finally
           (return
             (append (translate-walker let-body (cons bindings env) rec-env)
                     (iter outer
                           (for proc-label in bindings)
                           (for proc-code in codes)
                           (collect `(:label ,proc-label))
                           (iter (for form in proc-code) (in outer (collect form))))
                     '((:rtn))))))))
                
(defun translate-atom (atom env rec-env)
  (etypecase atom
    (integer (translate-const atom env rec-env))
    (atom (translate-variable atom env rec-env))))

(defun translate-const (const env rec-env)
  (declare (ignore env rec-env))
  (assert (typep const 'integer))
  `((:ldc ,const)))

(defun locate-within-env (bind env)
  (iter (for n from 0)
        (for frame in env)
        (for i = (position bind frame :test 'eq))
        (when i
          (return-from locate-within-env (values n i))))
  (error "Variable ~s unbound" bind))

(defun translate-variable (var env rec-env)
  (declare (ignore rec-env))
  (multiple-value-bind (n i) (locate-within-env var env)
    `((:ld ,n ,i))))

(defun translate-op (op form-a form-b env rec-env)
  `(,@(translate-walker form-b env rec-env) ,@(translate-walker form-a env rec-env) (,op)))

(defun translate-proc-invocation (proc-name proc-args env rec-env)
  (multiple-value-bind (n i) (locate-within-env proc-name env)
    `(,@(apply #'append (mapcar (lambda (form) (translate-walker form env rec-env)) (reverse proc-args)))
      (:ld ,n ,i)
      (:ap ,(length proc-args)))))
  
