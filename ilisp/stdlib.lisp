(defmacro list (&rest body)
  (if body
      `(cons ,(car body) (list . ,(cdr body)))
      0))

(defun cadr (x)
  (car (cdr x)))

(defun cddr (x)
  (cdr (cdr x)))
