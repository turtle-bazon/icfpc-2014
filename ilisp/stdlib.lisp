(in-package :ilisp)

(defmacro list (cl:&rest body)
  (cl:if body
         `(cons ,(cl:car body) (list . ,(cl:cdr body)))
         0))



