
(in-package :gcc)

(defun translate (ast)
  (translate-walker ast '()))

(defun translate-walker (ast environment)
  (flet ((assume (form) (return-from translate-walker form)))
    (macrolet ((with-match (() &rest clauses)
                 `(progn ,@(iter (for (pattern action) in clauses)
                                 (collect `(if-match ,pattern ast (assume ,action)))))))
      (with-match ()
        ((define (?proc-name . ?proc-args) ?forms)
	 (translate-define
	  ?proc-name ?proc-args ?forms environment))
        ((+ ?form-a ?form-b) (translate-op :add ?form-a ?form-b environment))
        ((- ?form-a ?form-b) (translate-op :sub ?form-a ?form-b environment))
        ((* ?form-a ?form-b) (translate-op :mul ?form-a ?form-b environment))
        ((/ ?form-a ?form-b) (translate-op :div ?form-a ?form-b environment))
	((= ?form-a ?form-b) (translate-op :ceq ?form-a ?form-b environment))
	((> ?form-a ?form-b) (translate-op :cgt ?form-a ?form-b environment))
	((>= ?form-a ?form-b) (translate-op :cgte ?form-a ?form-b environment))
	((cons ?form-a ?form-b) (translate-op :cons ?form-a ?form-b environment))
	((car ?form) (translate-op :car ?form nil environment))
	((cdr ?form) (translate-op :cdr ?form nil environment))
	((integerp ?form) (translate-op :atom ?form nil environment))
	((list . ?forms) (translate-seq :list ?forms environment))
	((tuple . ?forms) (translate-seq :tuple ?forms environment))
        (?atom (translate-atom ?atom environment))))
    (error "Invalid AST: ~s" ast)))

(defun translate-define (proc-name proc-args proc-body environment)
  `((:label ,proc-name)
    ,@(translate-walker proc-body (cons proc-args environment))
    (:rtn)))

(defun translate-atom (atom environment)
  (etypecase atom
    (integer (translate-const atom environment))
    (atom (translate-variable atom environment))))

(defun translate-const (const environment)
  (declare (ignore environment))
  (assert (typep const 'integer))
  `((:ldc ,const)))

(defun translate-variable (var environment)
  (iter (for n from 0)
        (for frame in environment)
        (for i = (position var frame :test 'eq))
        (when i
          (return-from translate-variable
            `((:ld ,n ,i)))))
  (error "Variable ~s unbound" var))

(defun translate-op (op form-a form-b environment)
  `(,@(when form-b
	      (translate-walker form-b environment))
    ,@(translate-walker form-a environment)
       (,op)))

(defun translate-seq (type body environment)
  (reduce (lambda (l r)
            `(,@r ,@(translate-walker l environment)
                  ,@(when r '((:cons)))))
          (nconc body (when (eql type :list) (list 0)))
          :from-end t :initial-value nil))
