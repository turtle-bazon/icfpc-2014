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
           #:cons))

(defpackage #:ilisp.impl
  (:use :cl :iterate :metatilities)
  (:shadowing-import-from :metatilities minimize finish)
  (:export ))


