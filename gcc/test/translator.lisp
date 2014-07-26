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

(addtest (gcc-translator-core)
  if-1
  (ensure-same (bind ((*gensym-counter* 0))
                 (translate '(if 0 1 2)))
               '((:LDC 0) 
                 (:SEL :TRUE0 :FALSE1)
                 (:LDC 1) 
                 (:TSEL :END2 :END2)
                 (:LABEL :TRUE0)
                 (:LDC 1) 
                 (:JOIN) 
                 (:LABEL :FALSE1)
                 (:LDC 2) 
                 (:JOIN) 
                 (:LABEL :END2))))

(addtest (gcc-translator-core)
  when-1
  (ensure-same (bind ((*gensym-counter* 0))
                 (translate '(when 0 1)))
               '((:LDC 0) 
                 (:SEL :TRUE0 :FALSE1) 
                 (:LDC 1)
                 (:TSEL :END2 :END2)
                 (:LABEL :TRUE0)
                 (:LDC 1)
                 (:JOIN)
                 (:LABEL :FALSE1)
                 (:LDC 0)
                 (:JOIN)
                 (:LABEL :END2))))

(addtest (gcc-translator-core)
  cond-1
  (ensure-same (bind ((*gensym-counter* 0))
                 (translate '(cond)))
               '((:LDC 0))))

(addtest (gcc-translator-core)
  cond-2
  (ensure-same (bind ((*gensym-counter* 0))
                 (translate '(cond (t 0))))
               '((:LDC 0))))

(addtest (gcc-translator-core)
  cond-3
  (ensure-same (bind ((*gensym-counter* 0))
                 (translate '(cond (0 0))))
               '((:LDC 0)
                 (:SEL :TRUE0 :FALSE1)
                 (:LDC 1)
                 (:TSEL :END2 :END2)
                 (:LABEL :TRUE0)
                 (:LDC 0)
                 (:JOIN)
                 (:LABEL :FALSE1)
                 (:LDC 0)
                 (:JOIN)
                 (:LABEL :END2))))

(addtest (gcc-translator-core)
  cond-4
  (ensure-same (bind ((*gensym-counter* 0))
                 (translate '(cond (0 0) (1 1))))
               '((:LDC 0)
                 (:SEL :TRUE0 :FALSE1)
                 (:LDC 1)
                 (:TSEL :END2 :END2)
                 (:LABEL :TRUE0)
                 (:LDC 0)
                 (:JOIN)
                 (:LABEL :FALSE1)
                 (:LDC 1)
                 (:SEL :TRUE3 :FALSE4)
                 (:LDC 1)
                 (:TSEL :END5 :END5)
                 (:LABEL :TRUE3)
                 (:LDC 1)
                 (:JOIN)
                 (:LABEL :FALSE4)
                 (:LDC 0)
                 (:JOIN)
                 (:LABEL :END5)
                 (:JOIN)
                 (:LABEL :END2))))


(addtest (gcc-translator-core)
  tuple-0
  (ensure-error (translate '(tuple))))

(addtest (gcc-translator-core)
  tuple-1
  (ensure-error (translate '(tuple 1))))

(addtest (gcc-translator-core)
  tuple-2
  (ensure-same (translate '(tuple 1 2))
               '((:LDC 1) 
                 (:LDC 2) 
                 (:CONS))))

(addtest (gcc-translator-core)
  tuple-3
  (ensure-same (translate '(tuple 1 2 3))
               '((:LDC 1) 
                 (:LDC 2) 
                 (:LDC 3) 
                 (:CONS)
                 (:CONS))))

(addtest (gcc-translator-core)
  list-0
  (ensure-same (translate '(list))
               '((:LDC 0))))

(addtest (gcc-translator-core)
  list-1
  (ensure-same (translate '(list 1))
               '((:LDC 1) (:LDC 0) (:CONS))))

(addtest (gcc-translator-core)
  list-2
  (ensure-same (translate '(list 1 2))
               '((:LDC 1) (:LDC 2) (:LDC 0) (:CONS) (:CONS))))

(addtest (gcc-translator-core)
  list-2
  (ensure-same (translate '(list 1 2 3))
               '((:LDC 1) (:LDC 2) (:LDC 3) (:LDC 0) (:CONS) (:CONS) (:CONS))))
