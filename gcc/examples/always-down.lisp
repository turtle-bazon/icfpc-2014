(lambda (initial-state unknown)
  (letrec ((step (lambda (ai-state world-state)
                   (cons (+ ai-state 1) 2))))
    (let ((init (lambda (initial-state unknown)
                  (cons 42 step))))
      (init initial-state unknown))))

