
(in-package :gcc)

(defun translate (ast)
  (translate-walker ast '()))

(defun translate-walker (ast environment)
  (flet ((assume (form) (return-from translate-walker form)))
    (macrolet ((with-match (() &rest clauses)
                 `(progn ,@(iter (for (pattern action) in clauses)
                                 (collect `(if-match ,pattern ast (assume ,action)))))))
      (with-match ()
        ((define (?proc-name . ?proc-args) . ?forms) (translate-define ?proc-name ?proc-args ?forms environment))
        ((+ ?form-a ?form-b) (translate-op :add ?form-a ?form-b environment))
        ((- ?form-a ?form-b) (translate-op :sub ?form-a ?form-b environment))
        ((* ?form-a ?form-b) (translate-op :mul ?form-a ?form-b environment))
        ((/ ?form-a ?form-b) (translate-op :div ?form-a ?form-b environment))
        (?atom (translate-atom ?atom environment))))
    (error "Invalid AST: ~s" ast)))

(defun translate-define (proc-name proc-args proc-body environment)
  (let* ((rec-bindings (reduce (lambda (bindings form)
                                 (if-match (define (?proc-name . ?proc-args) ?forms) form
                                           (cons ?proc-name bindings)
                                           bindings))
                               proc-body
                               :initial-value '()))
         (new-environment (cons (list :rec (nreverse rec-bindings)
                                      :std (list proc-name)
                                      :var proc-args)
                                environment)))
    `((:label ,proc-name)
      ,@(apply #'append (mapcar (lambda (form) (translate-walker form new-environment)) proc-body))
      (:rtn))))
                
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
        (for i = (position var (getf frame :var) :test 'eq))
        (when i
          (return-from translate-variable
            `((:ld ,n ,i)))))
  (error "Variable ~s unbound" var))

(defun translate-op (op form-a form-b environment)
  `(,@(translate-walker form-a environment) ,@(translate-walker form-b environment) (,op)))

