(defpackage #:ilisp
  (:use)
  (:shadowing-import-from :metatilities minimize finish)
  (:export #:let 
           #:letrec 
           #:lambda
           #:+
           #:-
           #:*
           #:/
           #:=
           #:>
           #:>=
           #:cons
           #:car
           #:cdr
           #:integerp
           #:if
           #:when
           #:list
           #:tuple))

(defpackage #:ilisp.impl
  (:use :cl :iterate :metatilities)
  (:shadowing-import-from :metatilities minimize finish)
  (:export ))


