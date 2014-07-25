(letrec ((loop (lambda (i)
                 (if (>= i 16) i (loop (+ i 1))))))
  (loop 0))

