(letrec ((inner-a (lambda (x) (+ x 1)))
         (inner-b (lambda (x)
                    (let ((dummy (lambda () (inner-a (+ x 2)))))
                      (dummy)))))
  (inner-b 3))

