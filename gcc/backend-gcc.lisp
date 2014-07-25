(in-package :gcc)

;; GCC backend

(defclass backend-gcc (backend) ())

(defun parse-binding (binding-form)
  (with-match (binding-form parse-binding)
    ((?binding (lambda ?proc-args ?proc-body)) (list ?binding ?proc-args ?proc-body))))

(defmethod translate-lambda ((backend backend-gcc) ast env)
  (append (translate-walker backend ast env) '((:rtn))))

(defmethod translate-letrec ((backend backend-gcc) binding-forms let-body env)
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
             ,@(translate-let backend binding-forms let-body env bindings))))))

(defmethod translate-let ((backend backend-gcc) binding-forms let-body env rec-env)
  (declare (optimize (debug 3)))
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
                         (iter (for form in proc-code) (in outer (collect form)))))))))

(defun translate-variable (var env)
  (declare (ignore))
  (multiple-value-bind (n i) (locate-within-env var env)
    `((:ld ,n ,i))))

(defun translate-const (const env)
  (declare (ignore env))
  (assert (typep const 'integer))
  `((:ldc ,const)))

(defmethod translate-atom ((backend backend-gcc) atom env)
  (etypecase atom
    (integer (translate-const atom env))
    (atom (translate-variable atom env))))

(defun locate-within-env (bind env)
  (iter (for n from 0)
        (for frame in env)
        (for i = (position bind frame :test 'eq))
        (when i
          (return-from locate-within-env (values n i))))
  (error "Variable ~s unbound" bind))

(defmethod translate-op ((backend backend-gcc) op form-a form-b env)
  `(,@(translate-walker backend form-b env) ,@(translate-walker backend form-a env) (,op)))

(defmethod translate-proc-invocation ((backend backend-gcc) proc-name proc-args env)
  (declare (optimize (debug 3)))
  (multiple-value-bind (n i) (ignore-errors (locate-within-env proc-name env))
    `(,@(apply #'append (mapcar (lambda (form) (translate-walker backend form env)) (reverse proc-args)))
      ,@(if (and n i) `((:ld ,n ,i)) `((:ldf ,proc-name)))
      (:ap ,(length proc-args)))))

(defmethod translate-seq (backend type body environment)
  (reduce (lambda (l r)
            `(,@r ,@(translate-walker backend l environment)
                  ,@(when r '((:cons)))))
          (nconc body (when (eql type :list) (list 0)))
          :from-end t :initial-value nil))

(defmethod translate-if ((backend backend-gcc) condition-form true-form false-form environment)
  (let ((true-label (gensym "true"))
 	(false-label (gensym "false"))
 	(end-label (gensym "end")))
    `(,@(translate-walker backend  condition-form environment)
 	(:sel ,true-label ,false-label)
 	(:ldc 1)
 	(:tsel ,end-label ,end-label)
 	(:label ,true-label)
        ,@(translate-walker backend true-form environment)
 	(:join)
 	(:label ,false-label)
        ,@(translate-walker backend false-form environment)
        (:join)
 	(:label ,end-label))))


;;; Labels management

(defun unlabel (opcode-list)
  (let ((label-map (iter (for op-code in opcode-list)
                         (for pc from 0)
                         (when (eq (first op-code) :label)
                           (collect (list (second op-code) pc))
                           (decf pc)))))
    (iter (for op-code in opcode-list)
	  (unless (eq (first op-code) :label)
	    (collect (unlabel-op-code op-code label-map))))))

(defun unlabel-op-code (op-code label-map)
  (iter (for op-code-part in op-code)
	(collect (let ((label-place (find op-code-part label-map :key #'car)))
		   (if label-place
		       (second label-place)
		       op-code-part)))))


;;; Convinience functions

(defun translate-gcc (ast &key unlabel)
  (let ((ast (translate-walker (make-instance 'backend-gcc)
                               ast '())))
    (if unlabel
        (unlabel ast)
        ast)))

(defun translate-file-gcc (filename &key unlabel)
  (with-open-file (source filename)
    (translate-gcc (read source) :unlabel unlabel)))

