(in-package :gcc)

(defun translate-file (filename &key unlabel)
  (with-open-file (source filename)
    (translate (read source) :unlabel unlabel)))

(defun translate (ast &key unlabel)
  (let ((ast (translate-walker ast '())))
    (if unlabel
        (unlabel ast)
        ast)))

(defun unlabel (opcode-list)
  (let ((label-map (iter (for op-code in opcode-list)
                         (for pc from 0)
                         (when (eq (first op-code) :label)
                           (collect (list (second op-code) pc))
                           (decf pc)))))
    (iter (for op-code in opcode-list)
	  (collect (unlabel-op-code op-code label-map)))))

(defun unlabel-op-code (op-code label-map)
  (cons 'unlabeled
	(iter (for op-code-part in op-code)
	      (collect (let ((label-place (find op-code-part label-map :key #'car)))
			 (if label-place
			     (second label-place)
			     op-code-part))))))

(defmacro with-match ((ast proc) &rest clauses)
  (with-gensyms (assume form)
    `(flet ((,assume (,form) (return-from ,proc ,form)))
       (progn ,@(iter (for (pattern action) in clauses)
                      (collect `(if-match ,pattern ,ast (,assume ,action))))))))


(defun translate-walker (ast env)
  (declare (optimize (debug 3)))
  (with-match (ast translate-walker)
    ((let ?bindings ?body) (translate-let ?bindings ?body env nil))
    ((letrec ?bindings ?body) (translate-letrec ?bindings ?body env))
    ((+ ?form-a ?form-b) (translate-op :add ?form-a ?form-b env))
    ((- ?form-a ?form-b) (translate-op :sub ?form-a ?form-b env))
    ((* ?form-a ?form-b) (translate-op :mul ?form-a ?form-b env))
    ((/ ?form-a ?form-b) (translate-op :div ?form-a ?form-b env))
    ((= ?form-a ?form-b) (translate-op :ceq ?form-b ?form-a env))
    ((> ?form-a ?form-b) (translate-op :cgt ?form-b ?form-a env))
    ((>= ?form-a ?form-b) (translate-op :cgte ?form-b ?form-a env))
    ((cons ?form-a ?form-b) (translate-op :cons ?form-b ?form-a env))
    ((car ?form) (translate-op :car ?form nil env))
    ((cdr ?form) (translate-op :cdr ?form nil env))
    ((integerp ?form) (translate-op :atom ?form nil env))
    ((list . ?forms) (translate-seq :list ?forms env))
    ((tuple . ?forms) (translate-seq :tuple ?forms env))
    ((if ?condition-form ?true-form ?false-form)
     (translate-if ?condition-form
                   ?true-form ?false-form
                   env))
    ((?proc-name . ?proc-args) (translate-proc-invocation ?proc-name ?proc-args env))
    (?atom (translate-atom ?atom env)))
  (error "Invalid AST: ~s" ast))

(defun parse-binding (binding-form)
  (with-match (binding-form parse-binding)
    ((?binding (lambda ?proc-args ?proc-body)) (list ?binding ?proc-args ?proc-body))))

(defun translate-lambda (ast env)
  (append (translate-walker ast env) '((:rtn))))

(defun translate-letrec (binding-forms let-body env)
  (declare (optimize (debug 3)))
  (iter (with helper-proc = (gensym))
        (for binding-form in binding-forms)
        (for (binding proc-args proc-body) = (parse-binding binding-form))
        (collect binding into bindings)
        (finally
         (return
           `((:dum ,(length bindings))
             ,@(iter (for binding in bindings) (collect `(:ldf ,binding)))
             (:ldf ,helper-proc)
             (:rap ,(length bindings))
             (:rtn)
             (:label ,helper-proc)
             ,@(translate-let binding-forms let-body env bindings))))))

(defun translate-let (binding-forms let-body env rec-env)
  (declare (optimize (debug 3)))
  (iter (for binding-form in binding-forms)
        (for (binding proc-args proc-body) = (parse-binding binding-form))
        (collect binding into bindings)
        (collect (translate-lambda proc-body (cons proc-args (cons rec-env env))) into codes)
        (finally
         (return
           (append (translate-lambda let-body (cons rec-env env))
                   (iter outer
                         (for proc-label in (nreverse bindings))
                         (for proc-code in (nreverse codes))
                         (collect `(:label ,proc-label))
                         (iter (for form in proc-code) (in outer (collect form)))))))))

(defun translate-atom (atom env)
  (etypecase atom
    (integer (translate-const atom env))
    (atom (translate-variable atom env))))

(defun translate-const (const env)
  (declare (ignore env))
  (assert (typep const 'integer))
  `((:ldc ,const)))

(defun locate-within-env (bind env)
  (iter (for n from 0)
        (for frame in env)
        (for i = (position bind frame :test 'eq))
        (when i
          (return-from locate-within-env (values n i))))
  (error "Variable ~s unbound" bind))

(defun translate-variable (var env)
  (declare (ignore))
  (multiple-value-bind (n i) (locate-within-env var env)
    `((:ld ,n ,i))))

(defun translate-op (op form-a form-b env)
  `(,@(translate-walker form-b env) ,@(translate-walker form-a env) (,op)))

(defun translate-proc-invocation (proc-name proc-args env)
  (declare (optimize (debug 3)))
  (multiple-value-bind (n i) (ignore-errors (locate-within-env proc-name env))
    `(,@(apply #'append (mapcar (lambda (form) (translate-walker form env)) (reverse proc-args)))
      ,@(if (and n i) `((:ld ,n ,i)) `((:ldf ,proc-name)))
      (:ap ,(length proc-args)))))

(defun translate-seq (type body environment)
  (reduce (lambda (l r)
            `(,@r ,@(translate-walker l environment)
                  ,@(when r '((:cons)))))
          (nconc body (when (eql type :list) (list 0)))
          :from-end t :initial-value nil))

(defun translate-if (condition-form true-form false-form environment)
  (let ((true-label (gensym "true"))
 	(false-label (gensym "false"))
 	(end-label (gensym "end")))
    `(,@(translate-walker condition-form environment)
 	(:sel ,true-label ,false-label)
 	(:ldc 1)
 	(:tsel ,end-label ,end-label)
 	(:label ,true-label)
        ,@(translate-walker true-form environment)
 	(:join)
 	(:label ,false-label)
        ,@(translate-walker false-form environment)
        (:join)
 	(:label ,end-label))))
