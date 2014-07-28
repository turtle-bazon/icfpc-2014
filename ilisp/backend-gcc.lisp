(in-package :ilisp.impl)

(defclass gcc-translator (translation-phase) 
  ((lambdas :initform (list) :accessor lambdas)
   (scopes :initform (list) :accessor scopes)
   (functions :initform (list) :accessor functions)
   (macros :initform (list) :accessor macros)))

(defun gcc-macro-fn (translator sym)
  (getf (macros translator) sym))

(defmacro with-scope ((translator) &body body)
  (with-gensyms (trans-var old-scope-var)
    `(bind ((,trans-var ,translator)
            (,old-scope-var (copy-seq (scopes ,trans-var))))
       (setf (scopes ,trans-var)
             (cons (list)
                   (copy-seq (scopes ,trans-var))))
       (prog1
           (progn ,@body)
         (setf (scopes ,trans-var) ,old-scope-var)))))


(defun scope-add-var (translator name)
  (setf (car (scopes translator))
        (nconc (car (scopes translator)) (list name))))

(defun scope-lookup-var-index (translator name)
  (declare (optimize (debug 3)))
  (iter (for frame in (scopes translator))
        (for frame-num from 0)
        (for idx = (position name frame :test 'eq))
        (when idx
          (return-from scope-lookup-var-index (values frame-num idx))))
  (error "Variable ~s unbound" name))


(defmacro with-match ((ast proc) &rest clauses)
  (with-gensyms (assume form)
    `(flet ((,assume (,form) (return-from ,proc ,form)))
       (progn ,@(iter (for (pattern action) in clauses)
                      (collect `(if-match ,pattern ,ast (,assume ,action))))))))

(defun unlabel-op-code (op-code label-map)
  (cons 'unlabeled
	(iter (for op-code-part in op-code)
	      (collect (let ((label-place (find op-code-part label-map :key #'car)))
			 (if label-place
			     (cons :label label-place)
			     op-code-part))))))

(defun unlabel (opcode-list)
  (let ((label-map (iter (for op-code in opcode-list)
                         (for pc from 0)
                         (when (eq (first op-code) :label)
                           (collect (list (second op-code) pc))
                           (decf pc)))))
    (iter (for op-code in opcode-list)
	  (collect (unlabel-op-code op-code label-map)))))

(defun translate (translator ast &key (unlabel nil))
  (bind ((top-level (translate-walk translator ast))
         (lambdas-code (iter 
                         (for code in (lambdas translator))
                         (appending code)))
         (full-code (nconc top-level (list '(:RTN)) lambdas-code)))
    
    (if unlabel (unlabel full-code) full-code)))

(defun translate-gcc (ast &key (unlabel nil))
  (translate (make-instance 'gcc-translator) ast :unlabel unlabel))

(defmethod translate-walk ((translator gcc-translator) ast &rest unused)
  (declare (optimize (debug 3))
           (ignore unused))
  (with-match (ast translate-walk)
    ((lambda ?proc-args . ?proc-body) (translate-lambda translator ?proc-args ?proc-body))
    ((let ?bindings . ?body) (translate-let translator ?bindings ?body))
    ((letrec ?bindings . ?body) (translate-letrec translator ?bindings ?body))

    ((setq ?name ?form) (translate-setq translator ?name ?form))
    ((in-package ?name) (list))
    ((declare . ?unused) (list))
    ((defun ?name ?lambda-list . ?body) (translate-defun translator ?name ?lambda-list ?body))
    ((defmacro ?name ?lambda-list . ?body) (translate-defmacro translator ?name ?lambda-list ?body))
    ((function ?obj) (translate-walk translator ?obj))
    ((trace ?obj) (translate-trace translator ?obj))
    ((break) `((:BRK)))
    ;; ((funcall ?fn . ?args) (translate-walk translator `(,?fn ,?args)))

    ((+ ?form-a ?form-b) (translate-binop translator :add ?form-a ?form-b))
    ((- ?form-a ?form-b) (translate-binop translator :sub ?form-a ?form-b))
    ((* ?form-a ?form-b) (translate-binop translator :mul ?form-a ?form-b))
    ((/ ?form-a ?form-b) (translate-binop translator :div ?form-a ?form-b))
    ((= ?form-a ?form-b) (translate-binop translator :ceq ?form-a ?form-b))
    ((> ?form-a ?form-b) (translate-binop translator :cgt ?form-a ?form-b))
    ((>= ?form-a ?form-b) (translate-binop  translator :cgte ?form-a ?form-b))
    ((cons ?form-a ?form-b) (translate-binop translator :cons ?form-a ?form-b))

    ((car ?form) (translate-unop translator :car ?form))
    ((cdr ?form) (translate-unop translator :cdr ?form))
    ((integerp ?form) (translate-unop translator :atom ?form))

    ((if ?condition-form ?true-form ?false-form)
     (translate-if translator ?condition-form ?true-form ?false-form))
    ((when ?condition-form ?true-form)
     (translate-if translator ?condition-form ?true-form 0))

    ((?proc-name . ?proc-args) (let ((macro-fn (gcc-macro-fn translator ?proc-name)))
                                 (if macro-fn
                                     (translate-walk translator (apply macro-fn ?proc-args))
                                     (translate-invoke translator ?proc-name ?proc-args))))

    ;; ((ilisp:list . ?forms) (translate-list translator ?forms))
    ;; ((ilisp:tuple . ?forms) (translate-tuple translator ?forms))

    ;; ((and . ?forms) (translate-and ?forms env))
    ;; ((or . ?forms) (translate-or ?forms env))
    ;; ((not ?form) (translate-not ?form env))
    ;; ((when ?condition-form ?true-form)
    ;;  (translate-when ?condition-form ?true-form env))
    ;; ((cond . ?forms)
    ;;  (translate-cond ?forms env))

    ;; ((?proc-name . ?proc-args) (if (gcc-macro-p ?proc-name)
    ;;                                (translate-walker (apply (gcc-macro-fn ?proc-name) ?proc-args) env)
    ;;                                (translate-proc-invocation ?proc-name ?proc-args env)))
    (?atom (translate-atom translator ?atom))))

(defmethod translate-lambda ((translator gcc-translator) proc-args proc-body)
  (bind ((lambda-name (gensym "LAMBDA")))
    (with-scope (translator)
      (iter (for arg in proc-args)
            (scope-add-var translator arg))

      (push `((:LABEL ,lambda-name)
              ,@(mapcan (named-lambda translate-lambda-each-body-form (form)
                          (translate-walk translator form))
                        proc-body)
              (:RTN))
            (lambdas translator)))

    `((:LDF ,lambda-name))))

(defmethod translate-let ((translator gcc-translator) bindings body)
  (bind (((args params) (iter (for binding in bindings)
                              (collect (if (listp binding)
                                           (first binding)
                                           binding)
                                into argz)
                              (collect (if (listp binding)
                                           (second binding)
                                           nil)
                                into paramz)
                              
                              (finally (return
                                         (list argz paramz))))))
    
    `(,@(mapcan (named-lambda translate-let-prepare-params (param)
                  (translate-walk translator param))
                params)
        ,@(translate-lambda translator args body)
        (:AP ,(length bindings)))))

(defmethod translate-letrec ((translator gcc-translator) bindings body)
  (bind (((args params) (iter (for binding in bindings)
                              (collect (if (listp binding)
                                           (first binding)
                                           binding)
                                into argz)
                              (collect (if (listp binding)
                                           (second binding)
                                           nil)
                                into paramz)
                              
                              (finally (return
                                         (list argz paramz))))))
    
    (nconc `((:DUM ,(length bindings)))
           (with-scope (translator)
             (iter (for arg in args)
                   (scope-add-var translator arg))
             
             (mapcan (named-lambda translate-letrec-prepare-params (param)
                       (translate-walk translator param))
                     params))
           (translate-lambda translator args body)
           `((:RAP ,(length bindings))))))

(defmethod translate-invoke ((translator gcc-translator) proc-name proc-args)
  (declare (optimize (debug 3)))
  `(,@(apply #'append (mapcar (named-lambda invoke-process-args (form)
                                (translate-walk translator form))
                              proc-args))
      ,@(translate-atom translator proc-name)
      (:ap ,(length proc-args))))

(defmethod translate-binop ((translator gcc-translator) op lhs rhs)
  `(,@(translate-walk translator lhs)
    ,@(translate-walk translator rhs)
    (,op)))

(defmethod translate-unop ((translator gcc-translator) op arg)
  `(,@(translate-walk translator arg)
   (,op)))

(defun translate-const (const)
  (assert (typep const 'integer))
  `((:ldc ,const)))

(defmethod translate-atom ((translator gcc-translator) atom)
  (etypecase atom
    (null (translate-const 0))
    (integer (translate-const atom))
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
				(:ghost-start 6))))
    (atom (bind (((:values frame index) (scope-lookup-var-index translator atom)))
            `((:LD ,frame ,index))))))

(defmethod translate-if ((translator gcc-translator) condition-form true-form false-form)
  (let ((true-label (intern (symbol-name (gensym "TRUE")) 
                            (find-package :keyword)))
 	(false-label (intern (symbol-name (gensym "FALSE")) 
                             (find-package :keyword)))
 	(end-label (intern (symbol-name (gensym "END")) 
                           (find-package :keyword))))
    `(,@(translate-walk translator condition-form)
 	(:sel ,true-label ,false-label)
 	(:ldc 1)
 	(:tsel ,end-label ,end-label)
 	(:label ,true-label)
        ,@(translate-walk translator true-form)
 	(:join)
 	(:label ,false-label)
        ,@(translate-walk translator false-form)
        (:join)
 	(:label ,end-label))))

(defmethod translate-defun ((translator gcc-translator) name lambda-list body*)
  (setf (getf (functions translator) name)
        `(lambda ,lambda-list ,@body*)))

(defmethod translate-defmacro ((translator gcc-translator) name lambda-list body*)
  (setf (getf (macros translator) name)
        (eval `(lambda ,lambda-list ,@body*)))
  nil)

(defmethod translate-setq ((translator gcc-translator) name form)
  (bind (((:values frame index) (scope-lookup-var-index translator name)))
    `(,@(translate-walk translator form)
        (:ST ,frame ,index))))

(defmethod translate-trace ((translator gcc-translator) obj)
  `(,@(translate-walk translator obj)
      (:DBUG)))

;; build system
(defun pretty-print-gcc (gcc &key (stream *standard-output*) (minimize nil)
			       (pad t))
  (iter (for form in gcc)
        (let ((unlabeled (eq 'unlabeled (first form))))
	  (bind (((op &rest args) (if unlabeled
				      (rest form)
				      form)))
	    (case op
	      (:label (unless minimize
			(format stream (if unlabeled
					   ";~a:~%"
					   "~a:~%") (first args))))
	      (t (format stream "~a~a~{ ~a~}~%"
			 (if pad "    " "") op
			 (mapcar #'(lambda (x)
				     (if (and (listp x) (eql (car x) :label))
					(caddr x) x)) args))))))))

(defun build-ai (input-files gcc-output-file entry-point &key (debug t))
  (with-open-file (fo gcc-output-file 
                      :direction :output 
                      :if-exists :supersede
		      :if-does-not-exist :create)

    (let ((top-level-forms (list))
          (translator (make-instance 'gcc-translator)))
        (with-scope (translator)
          (scope-add-var translator 'world-state)
          (scope-add-var translator 'ghosts)

          ;; For all the files...
          (dolist (gcc-input-file input-files)
            ;; Read top-level forms
            (with-open-file (fi gcc-input-file)
              (iter 
                (for form in-stream fi)
                (push (translate-walk translator form) top-level-forms))))
          
          (pretty-print-gcc
           (translate translator
                      `(letrec ,(iter (generating el in (functions translator))
                                      (for fn-name = (next el)) (for fn-body = (next el))
                                      (collect `(,fn-name ,fn-body)))
                         (,entry-point world-state ghosts))
                      :unlabel (not debug))
           :stream fo)))))

