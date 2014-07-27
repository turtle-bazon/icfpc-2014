(let ((f (lambda (x)
           (let ((g (lambda (y) (+ x y))))
             (let ((h (lambda (y) (g y))))
               (+ (h 4) (g 4)))))))
  (f 17))

