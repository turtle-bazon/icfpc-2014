;; DOES NOT WORK YET

(in-package :gcc)

(defun translate-file (filename)
  (with-open-file (source filename)
    (translate (read source))))

(defun translate (ast)
  (translate-walker ast '()))

(defun translate-walker (ast environment)
  (flet ((assume (form) (return-from translate-walker form)))
    (macrolet ((with-match (() &rest clauses)
                 `(progn ,@(iter (for (pattern action) in clauses)
                                 (collect `(if-match ,pattern ast (assume ,action)))))))
      (with-match ()
        ((entrypoint . ?forms) (translate-define nil nil ?forms environment))
        ((define (?proc-name . ?proc-args) . ?forms) (translate-define ?proc-name ?proc-args ?forms environment))
        ((+ ?form-a ?form-b) (translate-op :add ?form-a ?form-b environment))
        ((- ?form-a ?form-b) (translate-op :sub ?form-a ?form-b environment))
        ((* ?form-a ?form-b) (translate-op :mul ?form-a ?form-b environment))
        ((/ ?form-a ?form-b) (translate-op :div ?form-a ?form-b environment))
        ((?proc-name . ?proc-args) (translate-proc-invocation ?proc-name ?proc-args environment))
        (?atom (translate-atom ?atom environment))))
    (error "Invalid AST: ~s" ast)))

(defun reduce-rec-bindings (collector forms)
  (reduce (lambda (acc form)
            (if-match (define (?proc-name . ?proc-args) . ?proc-forms) form
                      (funcall collector ?proc-name ?proc-args ?proc-forms acc)
                      acc))
          forms
           :initial-value '()))
  
(defun translate-define (proc-name proc-args proc-body environment)
  (let* ((rec-bindings (nreverse (reduce-rec-bindings
                                  (lambda (proc-name proc-args proc-forms bindings)
                                    (declare (ignore proc-args proc-forms))
                                    (cons proc-name bindings))
                                  proc-body)))
         (new-environment (cons (append rec-bindings proc-args) environment))
         (codes+rec-binds (reduce-rec-bindings
                           (lambda (proc-name proc-args proc-forms codes+rec-binds)
                             (multiple-value-bind (code rec-bindings)
                                 (translate-define proc-name proc-args proc-forms new-environment)
                               (cons (cons code rec-bindings) codes+rec-binds)))
                           proc-body)))
    `(,@(when proc-name `((:label ,proc-name)))
        
        
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

(defun locate-within-env (bind namespace environment)
  (iter (for n from 0)
        (for frame in environment)
        (for i = (position bind frame :test (lambda (bind elem) (and (eq (car elem) namespace) (eq (cdr elem) bind)))))
        (when i
          (return-from locate-within-env (values n i))))
  (error "Variable ~s unbound" bind))

(defun translate-variable (var environment)
  (multiple-value-bind (n i) (locate-within-env var :var environment)
    `((:ld ,n ,i))))

(defun translate-op (op form-a form-b environment)
  `(,@(translate-walker form-b environment) ,@(translate-walker form-a environment) (,op)))

(defun translate-proc-invocation (proc-name proc-args environment)
  (multiple-value-bind (n i) (locate-within-env proc-name :fun environment)
    (let* ((parameters (apply #'append (mapcar (lambda (form) (translate-walker form environment)) (reverse proc-args))))
           (frame-rec-bindings (
          (invocation (if (zerop n)
                          `((:ld ,
    
    ,@(when rec-bindings
            `(((:dum ,(length rec-bindings))
               ,@(iter (for bind in rec-bindings)
                       (collect `(:ldf ,bind))))))
    `(,@(apply #'append (mapcar (lambda (form) (translate-walker form environment)) (reverse proc-args)))
      (:ld ,n ,i)
      (:ap ,(length proc-args)))))
  
