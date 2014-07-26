(letrec ((nth (lambda (x) (+ x 1)))
         (init (lambda (x)
                 (let ((f (lambda (y) (nth y))))
                   (let ((g (lambda (z) (f z))))
                     (g x))))))
  (init 1))

