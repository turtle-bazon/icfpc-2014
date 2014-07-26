(letrec ((rec (lambda (x) (+ x 44))))
  (let ((f (lambda (y) (rec y))))
    (let ((g (lambda (x) (f x))))
      (g 77))))


