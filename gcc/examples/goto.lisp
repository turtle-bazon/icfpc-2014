(let ((main (lambda ()
              (letrec ((go (lambda (n)
                             (to (+ n 1))))
                       (to (lambda (n)
                             (go (- n 1)))))
                (go 1)))))
  (main))

