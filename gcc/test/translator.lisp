(in-package :gcc-test)

(deftestsuite gcc-translator-core () ())

(iter (for bin-op in '(:add :sub :mul :div :ceq :cgt :cgte :cons))
      (for bin-op-lisp = (getf (list :add '+ 
                                     :sub '- 
                                     :mul '*
                                     :div '/
                                     :ceq '=
                                     :cgt '>
                                     :cgte '>=
                                     :cons 'cons)
                               bin-op))
      
      (bind ((test-prefix (intern (format nil "BINOP-~A" bin-op)))
             (test-const-1 (intern (format nil "~A-CONST-1" test-prefix)))
             (test-var-1 (intern (format nil "~A-VAR-1" test-prefix)))
             (test-var-2 (intern (format nil "~A-VAR-2" test-prefix)))
             (test-var-3 (intern (format nil "~A-VAR-3" test-prefix))))
        (eval `(progn 
                 (addtest (gcc-translator-core)
                   ,test-const-1
                   (ensure-same (translate '(,bin-op-lisp 1 2))
                                '((:ldc 1) (:ldc 2) (,bin-op))))
                 
                 (addtest (gcc-translator-core) 
                   ,test-var-1
                   (ensure-same (translate '(lambda (x) (,bin-op-lisp x 1)))
                                '((:ld 0 0) (:ldc 1) (,bin-op) (:rtn))))
                 
                 (addtest (gcc-translator-core) 
                   ,test-var-2
                   (ensure-same (translate '(lambda (x) (,bin-op-lisp 1 x)))
                                '((:ldc 1) (:ld 0 0) (,bin-op) (:rtn))))
                 
                 (addtest (gcc-translator-core) 
                   ,test-var-3
                   (ensure-same (translate '(lambda (x y) (,bin-op-lisp x y)))
                                '((:ld 0 0) (:ld 0 1) (,bin-op) (:rtn))))))))
