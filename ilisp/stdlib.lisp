(defmacro list (&rest body)
  (if body
      `(cons ,(car body) (list . ,(cdr body)))
      0))

(defun cadr (x)
  (car (cdr x)))

(defun cddr (x)
  (cdr (cdr x)))

(defmacro not (x)
  `(> ,x 1 ))

(defmacro < (a b)
  `(not (>= ,a ,b)))

(defmacro <= (a b)
  `(not (> ,a ,b)))

(defmacro and (&rest args)
  (case (length args)
    (0 0)
    (1 (first args))
    (t `(if ,(first args) 
            (and ,@(cdr args))
            0))))

(defmacro or (&rest args)
  (case (length args)
    (0 0)
    (1 (first args))
    (t (with-gensyms (x-var)
         `(let ((,x-var ,(first args)))
            (if ,x-var
                ,x-var
                (and ,@(cdr args))))))))

(defmacro labels (bindings &rest body)
  `(letrec ,(iter (for (name lambda-list &rest fn-body) in bindings)
                  (collect `(,name (lambda ,lambda-list ,@fn-body))))
     ,@body))

(defmacro funcall (fn &rest args)
  (with-gensyms (fn-var)
    `(let ((,fn-var ,fn))
       (,fn-var ,@args))))

(defmacro function (x)
  x)
