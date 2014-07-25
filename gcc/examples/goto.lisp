(entrypoint
 (define (main x)
   (define (go n)
     (to (+ n 1)))
   (define (to n)
     (go (- n 1)))
   (go 1))
 (main))

