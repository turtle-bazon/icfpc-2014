(in-package :gcc)

(defun translate-file (filename &key unlabel)
  (with-open-file (source filename)
    (translate (read source) :unlabel unlabel)))

(defun translate (ast &key unlabel)
  (let ((form (translate-walker ast '())))
    (if unlabel (unlabel form) form)))

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
			     (cons :label label-place)
			     op-code-part))))))

(defmacro with-match ((ast proc) &rest clauses)
  (with-gensyms (assume form)
    `(flet ((,assume (,form) (return-from ,proc ,form)))
       (progn ,@(iter (for (pattern action) in clauses)
                      (collect `(if-match ,pattern ,ast (,assume ,action))))))))

(defun translate-walker (ast env)
  (declare (optimize (debug 3)))
  (with-match (ast translate-walker)
    ((lambda ?proc-args ?proc-body) (translate-lambda ?proc-body (cons ?proc-args env)))
    ((let ?bindings ?body) (translate-let ?bindings ?body env nil))
    ((letrec ?bindings ?body) (translate-letrec ?bindings ?body env))
    ((+ ?form-a ?form-b) (translate-op :add ?form-a ?form-b env))
    ((- ?form-a ?form-b) (translate-op :sub ?form-a ?form-b env))
    ((* ?form-a ?form-b) (translate-op :mul ?form-a ?form-b env))
    ((/ ?form-a ?form-b) (translate-op :div ?form-a ?form-b env))
    ((= ?form-a ?form-b) (translate-op :ceq ?form-a ?form-b env))
    ((> ?form-a ?form-b) (translate-op :cgt ?form-a ?form-b env))
    ((>= ?form-a ?form-b) (translate-op :cgte ?form-a ?form-b env))
    ((and . ?forms) (translate-and ?forms env))
    ((or . ?forms) (translate-or ?forms env))
    ((not ?form) (translate-not ?form env))
    ((cons ?form-a ?form-b) (translate-op :cons ?form-a ?form-b env))
    ((car ?form) (translate-op :car ?form nil env))
    ((cdr ?form) (translate-op :cdr ?form nil env))
    ((integerp ?form) (translate-op :atom ?form nil env))
    ((list . ?forms) (translate-list ?forms env))
    ((tuple . ?forms) (translate-tuple ?forms env))
    ((if ?condition-form ?true-form ?false-form)
     (translate-if ?condition-form
                   ?true-form ?false-form
                   env))
    ((when ?condition-form ?true-form)
     (translate-when ?condition-form ?true-form env))
    ((cond . ?forms)
     (translate-cond ?forms env))
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
        (collect (translate-lambda proc-body (cons proc-args (if rec-env (cons rec-env env) env))) into codes)
        (finally
         (return
           (append (translate-lambda let-body (if rec-env (cons rec-env env) env))
                   (iter outer
                         (for proc-label in (nreverse bindings))
                         (for proc-code in (nreverse codes))
                         (collect `(:label ,proc-label))
                         (iter (for form in proc-code) (in outer (collect form)))))))))

(defun translate-atom (atom env)
  (etypecase atom
    (integer (translate-const atom env))
    (keyword (translate-const (ecase atom
				(:up 0)
				(:down 2)
				(:left 3)
				(:rigth 1)

				(:wall 0)
				(:empty 1)
				(:pill 2)
				(:power-pill 3)
				(:fruit-loc 4)
				(:lm-start 5)
				(:ghost-start 6)) env))
    (atom (translate-variable atom env))))

(defun translate-const (const env)
  (declare (ignore env))
  (assert (typep const 'integer))
  `((:ldc ,const)))

(defun locate-within-env (bind env)
  (declare (optimize (debug 3)))
  (iter (for n from 0)
        (for frame in env)
        (for i = (position bind frame :test 'eq))
        (when i
          (return-from locate-within-env (values n i))))
  (error "Variable ~s unbound" bind))

(defun translate-variable (var env)
  (declare (optimize (debug 3)))
  (multiple-value-bind (n i) (locate-within-env var env)
    `((:ld ,n ,i))))

(defun translate-and (forms env)
  (if (= 1 (length forms))
      (translate-walker (first forms) env)
      (let ((labeled-forms (iter (for form in forms)
				 (collect `(,(gensym "and") ,form))))
	    (false-end (gensym "false"))
	    (end (gensym "end")))
	(append
	 (iter (for (label form) in labeled-forms)
	       (for form-number from 0)
	       (collect `(:label ,label))
	       (appending (translate-walker form env))
	       (let* ((next-form-number (+ form-number 1))
		      (next-form-label (first (nth next-form-number labeled-forms))))
		 (if next-form-label
		     (collect `(:tsel ,next-form-label ,false-end))
		     (appending `((:ldc 1)
				  (:tsel ,end ,end))))))
	 `((:label ,false-end)
	   (:ldc 0)
	   (:label ,end))))))

(defun translate-or (forms env)
  (if (= 1 (length forms))
      (translate-walker (first forms) env)
      (let ((labeled-forms (iter (for form in forms)
                                 (collect `(,(gensym "and") ,form))))
            (true-end (gensym "true"))
            (end (gensym "end")))
        (append
         (iter (for (label form) in labeled-forms)
               (for form-number from 0)
               (collect `(:label ,label))
               (appending (translate-walker form env))
               (let* ((next-form-number (+ form-number 1))
                      (next-form-label (first (nth next-form-number labeled-forms))))
                 (if next-form-label
                     (collect `(:tsel ,true-end ,next-form-label))
                     (appending `((:ldc 1)
				  (:tsel ,end ,end))))))
         `((:label ,true-end)
           (:ldc 1)
           (:label ,end))))))

(defun translate-not (form env)
  `((:ldc 1)
    ,@(translate-walker form env)
    (:cgt)))

(defun translate-op (op form-a form-b env)
  `(,@(translate-walker form-a env)
    ,@(when form-b (translate-walker form-b env))
    (,op)))

(defun translate-proc-invocation (proc-name proc-args env)
  (declare (optimize (debug 3)))
  (case proc-name
    ((cadr caar) (translate-macro proc-name proc-args env))
    (t (multiple-value-bind (n i) (ignore-errors (locate-within-env proc-name env))
         `(,@(apply #'append (mapcar (lambda (form) (translate-walker form env)) proc-args))
             ,@(if (and n i) `((:ld ,n ,i)) `((:ldf ,proc-name)))
             (:ap ,(length proc-args)))))))

(defun translate-macro (macro-name macro-args env)
  (translate-walker
   (ecase macro-name
     (cadr `(car (cdr ,@macro-args)))
     (caar `(car (car ,@macro-args))))
   env))

(defun translate-list (body environment)
  (labels ((build-cons-chain (lst)
             (case (length lst)
               (0 0)
               (1 `(cons ,(car lst) 0))
               (t `(cons ,(car lst) ,(build-cons-chain (cdr lst)))))))
    (translate-walker (build-cons-chain body) environment)))

(defun translate-tuple (body environment)
  ; There couldn't be 0-tuple or 1-tuple
  (assert (>= (length body) 2) (body) "Tuple length cannot be less 2 but ~a given" (length body))

  (labels ((build-cons-chain (lst)
             (case (length lst)
               (2 `(cons ,(car lst) ,(second lst)))
               (t `(cons ,(car lst) ,(build-cons-chain (cdr lst)))))))
    (translate-walker (build-cons-chain body) environment)))

(defun translate-if (condition-form true-form false-form environment)
  (let ((true-label (intern (symbol-name (gensym "TRUE")) 
                            (find-package :keyword)))
 	(false-label (intern (symbol-name (gensym "FALSE")) 
                             (find-package :keyword)))
 	(end-label (intern (symbol-name (gensym "END")) 
                           (find-package :keyword))))
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

(defun translate-when (condition-form true-form environment)
  (translate-walker `(if ,condition-form ,true-form 0) environment))

(defun translate-cond (forms environment)
  (labels ((build-if (forms)
             (cond 
               ((= (length forms) 0) 0)
               ((= (length forms) 1) 
                (bind (((c f) (first forms)))
                  (if (eql c 't)
                      f
                      `(when ,c ,f))))
               (t (bind ((((c f) . tail-forms) forms))
                    `(if ,c
                         ,f
                         ,(build-if tail-forms)))))))
    (translate-walker (build-if forms) environment)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *gcc-ai-library* (make-hash-table)))

;;(setq *gcc-ai-library (make-hash-table))

(defmacro deflib/gcc (name (&rest arg-list) &body body*)
  "Defines library function for AI code. The all the functions will be
linked into resulting AI GCC code."
  `(setf (gethash ',name *gcc-ai-library*)
         '(lambda ,arg-list ,@body*)))

;; Usage samples
;; (deflib/gcc test (x)
;;   (+ x x))

;; (deflib/gcc test2 (x)
;;   (+ x x))


(defun build-ai-core (ast &key (debug t))
  (translate
   `(lambda (initial-state unknown)
      (letrec ,(iter (for (fn-name fn-body) in-hashtable *gcc-ai-library*)
                     (collect `(,fn-name ,fn-body)))
        ,ast))
   :unlabel (not debug)))
